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
  , I.VbrMode (..)
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
  { encoderScale :: !Float
    -- ^ Scale input stream by multiplying samples by this value. Default
    -- value: 1 (no scaling).
  , encoderSampleRate :: !(Maybe Word32)
    -- ^ Sample rate of output file. Default value: 'Nothing', which means
    -- use sample rate of input file.
  , encoderFreeFormat :: !Bool
    -- ^ Use free format. Default value: 'False'.
  , encoderFindReplayGain :: !Bool
    -- ^ Whether to calculated ReplayGain and write it to headers. Default
    -- value: 'False'.
  , encoderNoGap :: !(Maybe (Word32, Word32))
    -- ^ Enable “no gap” encoding. The first value in the tuple specifies
    -- current index and the second value specifies total number of tracks.
    -- Default value: 'Nothing' — disabled.
  , encoderCopyright :: !Bool
    -- ^ Mark as copyright protected. Default value: 'False'.
  , encoderOriginal :: !Bool
    -- ^ Mark as original.
  , encoderErrorProtection :: !Bool
    -- ^ Whether to enable error protection. Error protection means that 2
    -- bytes from each frame for CRC checksum. Default value: 'False'.
  , encoderStrictISO :: !Bool
    -- ^ Whether to enforce strict ISO compliance. Default value: 'False'.
  } deriving (Show, Read, Eq, Ord)

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
    }

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
