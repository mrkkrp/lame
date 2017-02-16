-- |
-- Module      :  Codec.Audio.LAME
-- Copyright   :  © 2017 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- The module provides an interface to LAME MP3 encoder.

{-# LANGUAGE RecordWildCards #-}

module Codec.Audio.LAME
  ( encodeMp3
  , EncoderSettings (..)
  , Compression (..)
  , VbrMode (..)
  , I.LameException (..) )
where

import Codec.Audio.Wave
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Default.Class
import Data.Maybe (fromMaybe)
import Data.Word
import System.Directory
import System.FilePath
import System.IO
import qualified Codec.Audio.LAME.Internal as I

-- | LAME encoder settings.

data EncoderSettings = EncoderSettings
  { encoderCompression :: !Compression
    -- ^ How to determine amount of compression to apply, see 'Compression'.
    -- Default value: @'CompressionRatio' 11@.
  , encoderQuality :: !(Maybe Word)
    -- ^ Select algorithm. This variable will effect quality by selecting
    -- expensive or cheap algorithms. 0 gives the best quality (very slow). 9 is
    -- very fast, but gives worst quality.
    --
    --     * 2 — near-best quality, not too slow.
    --     * 5 — good quality, fast.
    --     * 7 — OK quality, really fast.
    --
    -- Default value: 'Nothing', meaning that LAME will select the best
    -- value itself.
  , encoderNoGap :: !(Maybe (Word32, Word32))
    -- ^ Enable “no gap” encoding. The first value in the tuple specifies
    -- current index and the second value specifies total number of tracks.
    -- Default value: 'Nothing' — disabled.
  , encoderFindReplayGain :: !Bool
    -- ^ Whether to calculated ReplayGain and write it to headers. Default
    -- value: 'False'.
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
    -- ^ Mark as original.
  , encoderWriteVbrTag :: !Bool
    -- ^ Whether to write Xing VBR header frame. Default value: 'True'.
  } deriving (Show, Read, Eq, Ord)

-- | The data type represents supported options for compression. You can
-- specify either fixed bitrate, or compression ratio, or use the VBR mode.

data Compression
  = CompressionBitrate Word
    -- ^ Specify compression by setting a fixed bitrate, e.g.
    -- @'CompressionBitrate' 320@.
  | CompressionRatio Word
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
    --     * Minimal bitrate
    --     * Mean bitrate
    --     * Maximal bitrate
    --     * “Hard minimal bitrate”, if 'True', then the specified minimal
    --       bitrate will be enforced in all cases (normally it's violated
    --       for silence).
  | VbrMtrh
    -- ^ VBR MTRH
  deriving (Show, Read, Eq, Ord)

instance Default EncoderSettings where
  def = EncoderSettings
    { encoderScale           = 1
    , encoderSampleRate      = Nothing
    , encoderFreeFormat      = False
    , encoderFindReplayGain  = False
    , encoderNoGap           = Nothing
    , encoderCopyright       = False
    , encoderOriginal        = False
    , encoderErrorProtection = False
    , encoderStrictISO       = False
    , encoderWriteVbrTag     = True
    , encoderQuality         = Nothing
    , encoderCompression     = CompressionRatio 11 }

-- | Encode a WAVE file or RF64 file to MP3.
--
-- If the input file is not a valid WAVE file, 'WaveException' will be
-- thrown. 'LameException' is thrown when underlying LAME encoder reports a
-- problem.

encodeMp3 :: MonadIO m
  => EncoderSettings   -- ^ Encoder settings
  -> FilePath          -- ^ File to encode
  -> FilePath          -- ^ Where to save the resulting FLAC file
  -> m ()
encodeMp3 EncoderSettings {..} ipath' opath' = liftIO . I.withLame $ \l -> do
  ipath <- makeAbsolute ipath'
  opath <- makeAbsolute opath'
  wave@Wave {..}  <- readWaveFile ipath
  I.setNumSamples l waveSamplesTotal
  I.setInputSampleRate l (fromIntegral waveSampleRate)
  I.setNumChannels l (fromIntegral $ waveChannels wave)
  I.setScale l encoderScale
  let targetSampleRate = fromMaybe waveSampleRate encoderSampleRate
  I.setOutputSampleRate l (fromIntegral targetSampleRate)
  I.setFreeFormat l encoderFreeFormat
  I.setFindReplayGain l encoderFindReplayGain
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
    CompressionBitrate brate -> I.setBitrate l (fromIntegral brate)
    CompressionRatio   ratio -> I.setCompressionRatio l (fromIntegral ratio)
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
  I.initParams l
  withTempFile' opath $ \otemp -> do
    I.encodingHelper l
      (fromIntegral waveDataOffset)
      waveDataSize
      ipath
      opath
    renameFile otemp opath

----------------------------------------------------------------------------
-- Helpers

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
