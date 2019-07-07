-- |
-- Module      :  Codec.Audio.LAME
-- Copyright   :  © 2017–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- The module provides an interface to LAME MP3 encoder. All you need to do
-- to encode a WAVE (or RF64) file is to call 'encodeMp3', which see.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Codec.Audio.LAME
  ( encodeMp3
  , EncoderSettings (..)
  , defaultEncoderSettings
  , Compression (..)
  , VbrMode (..)
  , MetadataPlacement (..)
  , FilterSetup (..)
  , I.LameException (..) )
where

import Codec.Audio.Wave
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Word
import System.Directory
import System.FilePath
import System.IO
import qualified Codec.Audio.LAME.Internal as I
import qualified Data.Text                 as T

-- | LAME encoder settings.
--
-- Currently some noise-shaping and psycho-acoustic parameters are omitted,
-- but can be added on request.

data EncoderSettings = EncoderSettings
  { encoderCompression :: !Compression
    -- ^ How to determine amount of compression to apply, see 'Compression'.
    -- Default value: @'CompressionRatio' 11@ (corresponds to 128 kbps).
  , encoderQuality :: !(Maybe Word)
    -- ^ Select algorithm. This variable will effect quality by selecting
    -- expensive or cheap algorithms. 0 gives the best quality (very slow). 9 is
    -- very fast, but gives worst quality.
    --
    --     * 2—near-best quality, not too slow.
    --     * 5—good quality, fast.
    --     * 7—OK quality, really fast.
    --
    -- Default value: 'Nothing', meaning that LAME will select the best
    -- value itself.
  , encoderNoGap :: !(Maybe (Word32, Word32))
    -- ^ Enable “no gap” encoding. The first value in the tuple specifies
    -- current index and the second value specifies total number of tracks.
    -- Default value: 'Nothing'—disabled.
  , encoderErrorProtection :: !Bool
    -- ^ Whether to enable error protection. Error protection means that 2
    -- bytes from each frame for CRC checksum. Default value: 'False'.
  , encoderStrictISO :: !Bool
    -- ^ Whether to enforce strict ISO compliance. Default value: 'False'.
  , encoderSampleRate :: !(Maybe Word32)
    -- ^ Sample rate of output file. Default value: 'Nothing', which means
    -- use sample rate of input file.
  , encoderScale :: !Float
    -- ^ Scale input stream by multiplying samples by this value. Default
    -- value: 1 (no scaling).
  , encoderFreeFormat :: !Bool
    -- ^ Use free format. Default value: 'False'.
  , encoderCopyright :: !Bool
    -- ^ Mark as copyright protected. Default value: 'False'.
  , encoderOriginal :: !Bool
    -- ^ Mark as original. Default value: 'True'.
  , encoderWriteVbrTag :: !Bool
    -- ^ Whether to write Xing VBR header frame. Default value: 'True'.
  , encoderMetadataPlacement :: !MetadataPlacement
    -- ^ Which container to use to write metadata. Default value:
    -- 'Id3v2Only'.
  , encoderTagTitle :: !(Maybe Text)
    -- ^ Title tag to write. Default value: 'Nothing'.
  , encoderTagArtist :: !(Maybe Text)
    -- ^ Artist tag to write. Default value: 'Nothing'.
  , encoderTagAlbum :: !(Maybe Text)
    -- ^ Album tag to write. Default value: 'Nothing'.
  , encoderTagYear :: !(Maybe Text)
    -- ^ Year tag to write. Default value: 'Nothing'.
  , encoderTagComment :: !(Maybe Text)
    -- ^ Comment tag to write. Default value: 'Nothing'.
  , encoderTagTrack :: !(Maybe (Word8, Maybe Word8))
    -- ^ Track number (first in tuple) and (optionally) total number of
    -- tracks (second in tuple). Default value: 'Nothing'.
  , encoderTagGenre :: !(Maybe Text)
    -- ^ Genre tag to write. Default value: 'Nothing'.
  , encoderAlbumArt :: !(Maybe ByteString)
    -- ^ Album art (a picture) to write (JPEG\/PNG\/GIF). Default value:
    -- 'Nothing'.
  , encoderLowpassFilter :: !FilterSetup
    -- ^ Settings for low-pass filter. Default value: 'FilterAuto'.
  , encoderHighpassFilter :: !FilterSetup
    -- ^ Settings for high-pass filter. Default value: 'FilterAuto'.
  } deriving (Show, Read, Eq, Ord)

-- | Default value of 'EncoderSettings'.
--
-- @since 0.2.0

defaultEncoderSettings :: EncoderSettings
defaultEncoderSettings = EncoderSettings
  { encoderScale           = 1
  , encoderSampleRate      = Nothing
  , encoderFreeFormat      = False
  , encoderNoGap           = Nothing
  , encoderCopyright       = False
  , encoderOriginal        = True
  , encoderErrorProtection = False
  , encoderStrictISO       = False
  , encoderWriteVbrTag     = True
  , encoderQuality         = Nothing
  , encoderCompression     = CompressionRatio 11
  , encoderMetadataPlacement = Id3v2Only
  , encoderTagTitle        = Nothing
  , encoderTagArtist       = Nothing
  , encoderTagAlbum        = Nothing
  , encoderTagYear         = Nothing
  , encoderTagComment      = Nothing
  , encoderTagTrack        = Nothing
  , encoderTagGenre        = Nothing
  , encoderAlbumArt        = Nothing
  , encoderLowpassFilter   = FilterAuto
  , encoderHighpassFilter  = FilterAuto
  }

-- | The data type represents supported options for compression. You can
-- specify either fixed bitrate, compression ratio, or use the VBR mode.

data Compression
  = CompressionBitrate Word
    -- ^ Specify compression by setting a fixed bitrate, e.g.
    -- @'CompressionBitrate' 320@.
  | CompressionRatio Float
    -- ^ Specify compression ratio.
  | CompressionVBR VbrMode Word
    -- ^ VBR. Here you can specify which mode to use and the second argument
    -- is VBR quality from 0 (highest), to 9 (lowest). Good default is 4.
  deriving (Show, Read, Eq, Ord)

-- | Variable bitrate (VBR) modes with their associated parameters.

data VbrMode
  = VbrRh
    -- ^ VBR RH.
  | VbrAbr (Maybe Word) (Maybe Word) (Maybe Word) Bool
    -- ^ VBR ABR. Parameters of the data constructor in order:
    --
    --     * Minimal bitrate
    --     * Mean bitrate
    --     * Maximal bitrate
    --     * “Hard minimal bitrate”, if 'True', then the specified minimal
    --       bitrate will be enforced in all cases (normally it's violated
    --       for silence).
  | VbrMtrh
    -- ^ VBR MTRH
  deriving (Show, Read, Eq, Ord)

-- | Which container to use to write tags\/metadata.

data MetadataPlacement
  = Id3v1Only          -- ^ Only write ID3v1 metadata
  | Id3v2Only          -- ^ Only write ID3v2 metadata
  | Id3Both            -- ^ Write ID3v1 and ID3v2
  deriving (Show, Read, Eq, Ord)

-- | Settings for a filter.

data FilterSetup
  = FilterAuto         -- ^ Filter settings are chosen automatically
  | FilterDisabled     -- ^ Filter is disabled
  | FilterManual Word (Maybe Word)
    -- ^ The first parameter is the filter's frequency (cut-off frequency)
    -- in Hz and the second parameter is width of transition band in Hz (if
    -- 'Nothing', it's chosen automatically).
  deriving (Show, Read, Eq, Ord)

-- | Encode a WAVE file or RF64 file to MP3.
--
-- If the input file is not a valid WAVE file, 'WaveException' will be
-- thrown. 'I.LameException' is thrown when underlying LAME encoder reports
-- a problem.
--
-- Not all sample formats and bit depth are currently supported. Supported
-- sample formats include:
--
--     * PCM with samples represented as signed integers > 8 bit and <= 16
--       bit per mono-sample.
--     * IEEE floating point (32 bit) samples.
--     * IEEE floating point (64 bit) samples.
--
-- If you feed the encoder with something else, expect
-- 'I.LameUnsupportedSampleFormat' to be thrown.

encodeMp3 :: MonadIO m
  => EncoderSettings   -- ^ Encoder settings
  -> FilePath          -- ^ WAVE file to encode
  -> FilePath          -- ^ Where to save the resulting MP3 file
  -> m ()
encodeMp3 EncoderSettings {..} ipath' opath' = liftIO . I.withLame $ \l -> do
  ipath <- makeAbsolute ipath'
  opath <- makeAbsolute opath'
  wave@Wave {..}  <- readWaveFile ipath
  case waveSampleFormat of
    SampleFormatPcmInt bps ->
      when (bps > 16 || bps <= 8) $
        throwM (I.LameUnsupportedSampleFormat waveSampleFormat)
    _ -> return ()
  I.setNumSamples l waveSamplesTotal
  I.setInputSampleRate l (fromIntegral waveSampleRate)
  I.setNumChannels l (fromIntegral $ waveChannels wave)
  I.setScale l encoderScale
  let targetSampleRate = fromMaybe waveSampleRate encoderSampleRate
  I.setOutputSampleRate l (fromIntegral targetSampleRate)
  I.setFreeFormat l encoderFreeFormat
  forM_ encoderNoGap $ \(currentIndex, total) -> do
    I.setNoGapCurrentIndex l (fromIntegral currentIndex)
    I.setNoGapTotal        l (fromIntegral total)
  I.setCopyright l encoderCopyright
  I.setOriginal l encoderOriginal
  I.setErrorProtection l encoderErrorProtection
  I.setStrictISO l encoderStrictISO
  I.setWriteVbrTag l encoderWriteVbrTag
  forM_ encoderQuality (I.setQuality l . fromIntegral . min 9)
  case encoderCompression of
    CompressionBitrate brate ->
      I.setBitrate l (fromIntegral brate)
    CompressionRatio ratio ->
      I.setCompressionRatio l ratio
    CompressionVBR vbrMode vbrQuality -> do
      (I.setVBRQ l . fromIntegral . min 9) vbrQuality
      case vbrMode of
        VbrRh -> I.setVBR l I.VbrRh
        VbrAbr minRate meanRate maxRate hardMin -> do
          I.setVBR l I.VbrAbr
          forM_ minRate  (I.setVBRMinBitrate  l . fromIntegral)
          forM_ meanRate (I.setVBRMeanBitrate l . fromIntegral)
          forM_ maxRate  (I.setVBRMaxBitrate  l . fromIntegral)
          I.setVBRHardMin l hardMin
        VbrMtrh -> I.setVBR l I.VbrMtrh
  I.id3TagInit l
  case encoderMetadataPlacement of
    Id3v1Only -> I.id3TagV1Only l
    Id3v2Only -> I.id3TagV2Only l
    Id3Both   -> I.id3TagAddV2  l
  forM_ encoderTagTitle   (I.id3TagSetTextInfo l "TIT2")
  forM_ encoderTagArtist  (I.id3TagSetTextInfo l "TPE1")
  forM_ encoderTagAlbum   (I.id3TagSetTextInfo l "TALB")
  forM_ encoderTagYear    (I.id3TagSetTextInfo l "TYER")
  forM_ encoderTagComment (I.id3TagSetComment l)
  forM_ encoderTagTrack   (uncurry renderTrackNumber >=> I.id3TagSetTextInfo l "TRCK")
  forM_ encoderTagGenre   (I.id3TagSetTextInfo l "TCON")
  forM_ encoderAlbumArt   (I.id3TagSetAlbumArt l)
  setupFilter I.setLowpassFreq  I.setLowpassWidth  l encoderLowpassFilter
  setupFilter I.setHighpassFreq I.setHighpassWidth l encoderHighpassFilter
  I.initParams l
  withTempFile' opath $ \otemp -> do
    I.encodingHelper l wave ipath otemp
    renameFile otemp opath

----------------------------------------------------------------------------
-- Helpers

-- | Setup a filter for given 'I.Lame'.

setupFilter
  :: (I.Lame -> Int -> IO ()) -- ^ How to set cut-off frequncy
  -> (I.Lame -> Int -> IO ()) -- ^ How to set width of transition band
  -> I.Lame            -- ^ Settings to edit
  -> FilterSetup       -- ^ Filter setup to apply
  -> IO ()
setupFilter setFreq _ l FilterAuto = setFreq l 0
setupFilter setFreq _ l FilterDisabled = setFreq l (-1)
setupFilter setFreq setWidth l (FilterManual freq mwidth) = do
  setFreq l (fromIntegral freq)
  forM_ mwidth (setWidth l . fromIntegral)

-- | A custom wrapper for creating temporary files in the same directory as
-- given file. 'Handle' is not opened, you only get 'FilePath' in the
-- callback.

withTempFile' :: FilePath -> (FilePath -> IO a) -> IO a
withTempFile' path = bracketOnError acquire cleanup
  where
    acquire = do
      (path',h) <- openBinaryTempFile dir file
      hClose h
      return path'
    cleanup = ignoringIOErrors . removeFile -- in case exception strikes
              -- before we create the actual file
    dir     = takeDirectory path
    file    = takeFileName  path

-- | Perform specified action ignoring IO exceptions it may throw.

ignoringIOErrors :: IO () -> IO ()
ignoringIOErrors ioe = ioe `catch` handler
  where
    handler :: IOError -> IO ()
    handler = const (return ())

-- | Render track number and optionally total number of tracks as a strict
-- 'Text' value.

renderTrackNumber :: Word8 -> Maybe Word8 -> IO Text
renderTrackNumber 0 t          = throwM (I.LameInvalidTrackNumber 0 t)
renderTrackNumber n t@(Just 0) = throwM (I.LameInvalidTrackNumber n t)
renderTrackNumber n t = return . T.pack $ show n ++ maybe "" (("/" ++) . show) t
