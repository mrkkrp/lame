-- |
-- Module      :  Codec.Audio.LAME.Internal
-- Copyright   :  © 2017 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Low-level non-public helpers.

{-# LANGUAGE ForeignFunctionInterface #-}

module Codec.Audio.LAME.Internal
  ( -- * Types
    Lame
  , LameException (..)
    -- * Low-level API
  , withLame
  , initParams
    -- ** Input stream description
  , setNumSamples
  , setInputSampleRate
  , setNumChannels
  , setScale
  , setOutputSampleRate
    -- ** General control parameters
  , setQuality
  , setFreeFormat
  , setFindReplayGain
  , setNoGapTotal
  , setNoGapCurrentIndex
  , setBitrate
  , setCompressionRatio
    -- ** Frame parameters
  , setCopyright
  , setOriginal
  , setErrorProtection
  , setStrictISO
    -- ** VBR control
  , setVbr
  , setVbrQ
  , setVbrMeanBitrate
  , setVbrMinBitrate
  , setVbrMaxBitrate
  , setVBRHardMin
    -- ** Filtering control
  , setLowpassFreq
  , setLowpassWidth
  , setHighpassFreq
  , setHighpassWidth
    -- ** Psycho-acoustics
  , setAthOnly
  , setAthShort
  , setNoAth
  , setAthType
  , setAthLower
  , setAthAAType
  , setAthAASensitivity
  , setAllowDiffShort
  , setUseTemporal
  , setInterChRatio
  , setNoShortBlocks
  , setForceShortBlocks
    -- * Encoding
  , encodingHelper )
where

import Control.Monad.Catch
import Data.Void
import Foreign
import Unsafe.Coerce

----------------------------------------------------------------------------
-- Types

-- | An opaque newtype wrapper around @'Ptr' 'Void'@, serves to represent
-- pointer to the structure that does all the bookkeeping in LAME.

newtype Lame = Lame (Ptr Void)

-- | Enumeration of problems you can have with LAME.

data LameException
  = LameGenericError   -- ^ A “generic error” happened
  | LameNoMemory       -- ^ Memory allocation issue
  | LameBadBitrate     -- ^ Unsupported bitrate
  | LameBadSampleFreq  -- ^ Unsupported sample rate
  | LameInternalError  -- ^ An “Internal error” happened
  deriving (Eq, Show, Read)

instance Exception LameException

----------------------------------------------------------------------------
-- Low-level API

-- | Create and use a 'Lame' (pointer structure needed for talking to the
-- LAME API).
--
-- If memory cannot be allocated, corresponding 'LameException' is raised.

withLame :: (Lame -> IO a) -> IO a
withLame f = bracket lameInit (mapM_ lameClose) $ \mlame ->
  case mlame of
    Nothing -> throwM LameNoMemory
    Just x -> f x

-- | Create a new 'Lame'. In the case of memory allocation problem 'Nothing'
-- is returned.

lameInit :: IO (Maybe Lame)
lameInit = maybePtr <$> c_lame_init

foreign import ccall unsafe "lame_init"
  c_lame_init :: IO Lame

-- | Free 'Lame' structure and all associated buffers.

lameClose :: Lame -> IO ()
lameClose = c_lame_close

foreign import ccall unsafe "lame_close"
  c_lame_close :: Lame -> IO ()

-- | Set more internal configuration based on previously set parameters.
-- Should be called when all the other stuff is set.

initParams :: Lame -> IO ()
initParams = handleErrors . c_lame_init_params

foreign import ccall unsafe "lame_init_params"
  c_lame_init_params :: Lame -> IO Int

----------------------------------------------------------------------------
-- Input stream description

-- | Set total number of samples to encode.

setNumSamples :: Lame -> Word64 -> IO ()
setNumSamples l x = handleErrors (c_lame_set_num_samples l x)

foreign import ccall unsafe "lame_set_num_samples"
  c_lame_set_num_samples :: Lame -> Word64 -> IO Int

-- | Set sample rate of the input stream.

setInputSampleRate :: Lame -> Int -> IO ()
setInputSampleRate l x = handleErrors (c_lame_set_input_samplerate l x)

foreign import ccall unsafe "lame_set_input_samplerate"
  c_lame_set_input_samplerate :: Lame -> Int -> IO Int

-- | Set number of channels in input stream.

setNumChannels :: Lame -> Int -> IO ()
setNumChannels l x = handleErrors (c_lame_set_num_channels l x)

foreign import ccall unsafe "lame_set_num_channels"
  c_lame_set_num_channels :: Lame -> Int -> IO Int

-- | Scale the input by this amount before encoding.

setScale :: Lame -> Float -> IO ()
setScale l x = handleErrors (c_lame_set_scale l x)

foreign import ccall unsafe "lame_set_scale"
  c_lame_set_scale :: Lame -> Float -> IO Int

-- | Set output sample rate in Hz. 0 (default) means that LAME will pick
-- this value automatically.

setOutputSampleRate :: Lame -> Int -> IO ()
setOutputSampleRate l x = handleErrors (c_lame_set_out_samplerate l x)

foreign import ccall unsafe "lame_set_out_samplerate"
  c_lame_set_out_samplerate :: Lame -> Int -> IO Int

----------------------------------------------------------------------------
-- General control parameters

-- | Select algorithm. This variable will effect quality by selecting
-- expensive or cheap algorithms. 0 gives the best quality (very slow). 9 is
-- very fast, but gives worst quality. worst compression.
--
--     * 2 — near-best quality, not too slow.
--     * 5 — good quality, fast.
--     * 7 — OK quality, really fast.

setQuality :: Lame -> Int -> IO ()
setQuality l x = handleErrors (c_lame_set_quality l x)

foreign import ccall unsafe "lame_set_quality"
  c_lame_set_quality :: Lame -> Int -> IO Int

----------------------------------------------------------------------------
-- Frame parameters

----------------------------------------------------------------------------
-- VBR control

----------------------------------------------------------------------------
-- Filtering control

----------------------------------------------------------------------------
-- Psycho-acoustics

----------------------------------------------------------------------------
-- Encoding

----------------------------------------------------------------------------
-- Helpers

-- | Coerce to 'Ptr' and check if it's a null pointer, return 'Nothing' if
-- it is, otherwise return the given pointer unchanged. Needless to say that
-- this thing is unsafe.

maybePtr :: a -> Maybe a
maybePtr a
  | unsafeCoerce a == nullPtr = Nothing
  | otherwise                 = Just a

-- | Treat the 'Int' value as a error code. Unless it's 0, throw
-- corresponding 'LameException', otherwise just return the unit.

handleErrors :: IO Int -> IO ()
handleErrors m = do
  n <- m
  case n of
    0   -> return ()
    -10 -> throwM LameNoMemory
    -11 -> throwM LameBadBitrate
    -12 -> throwM LameBadSampleFreq
    -13 -> throwM LameInternalError
    _   -> throwM LameGenericError

-- | Convert 'Bool' into 'Int'.

fromBool :: Bool -> Int
fromBool False = 0
fromBool True  = 1
