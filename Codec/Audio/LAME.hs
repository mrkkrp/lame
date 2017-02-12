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
import Control.Monad.IO.Class
import Data.Default.Class
import System.Directory
import qualified Codec.Audio.LAME.Internal as I

-- | LAME encoder settings.

data EncoderSettings = EncoderSettings
  { encoderScale :: !Float
  } deriving (Show, Read, Eq, Ord)

instance Default EncoderSettings where
  def = EncoderSettings
    { encoderScale = 1
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

  -- TODO
  I.initParams l

  return ()
