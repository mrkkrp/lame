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
  , VbrMode (..)
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
  , setWriteVbrTag
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
    -- ** Quantization\/noise shaping
  -- , setQuantComp
  -- , setQuantCompShort
  -- , setExpNspsytune
  -- , setMsfix
    -- ** VBR control
  , setVBR
  , setVBRQ
  , setVBRMinBitrate
  , setVBRMeanBitrate
  , setVBRMaxBitrate
  , setVBRHardMin
    -- ** Filtering control
  -- , setLowpassFreq
  -- , setLowpassWidth
  -- , setHighpassFreq
  -- , setHighpassWidth
    -- ** Psycho-acoustics
  -- , setAthOnly
  -- , setAthShort
  -- , setNoAth
  -- , setAthType
  -- , setAthLower
  -- , setAthAAType
  -- , setAthAASensitivity
  -- , setAllowDiffShort
  -- , setUseTemporal
  -- , setInterChRatio
  -- , setNoShortBlocks
  -- , setForceShortBlocks
    -- * Encoding
  , encodingHelper )
where

import Control.Monad.Catch
import Data.Void
import Foreign
import Foreign.C.String
import Unsafe.Coerce

----------------------------------------------------------------------------
-- Types

-- | An opaque newtype wrapper around @'Ptr' 'Void'@, serves to represent
-- pointer to the structure that does all the bookkeeping in LAME.

newtype Lame = Lame (Ptr Void)

-- | Enumeration of VBR modes.

data VbrMode
  = VbrRh              -- ^ VBR RH
  | VbrAbr             -- ^ VBR ABR
  | VbrMtrh            -- ^ VBR MTRH
  deriving (Show, Read, Eq, Ord, Bounded, Enum)

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

-- | Set whether to write Xing VBR header frame.

setWriteVbrTag :: Lame -> Bool -> IO ()
setWriteVbrTag l x = handleErrors (c_lame_set_bWriteVbrTag l (fromBool x))

foreign import ccall unsafe "lame_set_bWriteVbrTag"
  c_lame_set_bWriteVbrTag :: Lame -> Int -> IO Int

-- | Select algorithm. This variable will effect quality by selecting
-- expensive or cheap algorithms. 0 gives the best quality (very slow). 9 is
-- very fast, but gives worst quality.
--
--     * 2 — near-best quality, not too slow.
--     * 5 — good quality, fast.
--     * 7 — OK quality, really fast.

setQuality :: Lame -> Int -> IO ()
setQuality l x = handleErrors (c_lame_set_quality l x)

foreign import ccall unsafe "lame_set_quality"
  c_lame_set_quality :: Lame -> Int -> IO Int

-- | Set whether we should use free format.

setFreeFormat :: Lame -> Bool -> IO ()
setFreeFormat l x = handleErrors (c_lame_set_free_format l (fromBool x))

foreign import ccall unsafe "lame_set_free_format"
  c_lame_set_free_format :: Lame -> Int -> IO Int

-- | Set whether we should do ReplayGain analysis.

setFindReplayGain :: Lame -> Bool -> IO ()
setFindReplayGain l x = handleErrors (c_lame_set_findReplayGain l (fromBool x))

foreign import ccall unsafe "lame_set_findReplayGain"
  c_lame_set_findReplayGain :: Lame -> Int -> IO Int

-- | Set total number of tracks to encode in “no gap” mode.

setNoGapTotal :: Lame -> Int -> IO ()
setNoGapTotal l x = handleErrors (c_lame_set_nogap_total l x)

foreign import ccall unsafe "lame_set_nogap_total"
  c_lame_set_nogap_total :: Lame -> Int -> IO Int

-- | Set index of current track to encode in “no gap” mode.

setNoGapCurrentIndex :: Lame -> Int -> IO ()
setNoGapCurrentIndex l x = handleErrors (c_lame_set_nogap_currentindex l x)

foreign import ccall unsafe "lame_set_nogap_currentindex"
  c_lame_set_nogap_currentindex :: Lame -> Int -> IO Int

-- | Set bitrate.

setBitrate :: Lame -> Int -> IO ()
setBitrate l x = handleErrors (c_lame_set_brate l x)

foreign import ccall unsafe "lame_set_brate"
  c_lame_set_brate :: Lame -> Int -> IO Int

-- | Set compression ratio.

setCompressionRatio :: Lame -> Int -> IO ()
setCompressionRatio l x = handleErrors (c_lame_set_compression_ratio l x)

foreign import ccall unsafe "lame_set_compression_ratio"
  c_lame_set_compression_ratio :: Lame -> Int -> IO Int

----------------------------------------------------------------------------
-- Frame parameters

-- | Mark as copyright protected.

setCopyright :: Lame -> Bool -> IO ()
setCopyright l x = handleErrors (c_lame_set_copyright l (fromBool x))

foreign import ccall unsafe "lame_set_copyright"
  c_lame_set_copyright :: Lame -> Int -> IO Int

-- | Mark as original.

setOriginal :: Lame -> Bool -> IO ()
setOriginal l x = handleErrors (c_lame_set_original l (fromBool x))

foreign import ccall unsafe "lame_set_original"
  c_lame_set_original :: Lame -> Int -> IO Int

-- | Set whether to use 2 bytes from each frame for CRC checksum.

setErrorProtection :: Lame -> Bool -> IO ()
setErrorProtection l x =
  handleErrors (c_lame_set_error_protection l (fromBool x))

foreign import ccall unsafe "lame_set_error_protection"
  c_lame_set_error_protection :: Lame -> Int -> IO Int

-- | Enforce strict ISO compliance.

setStrictISO :: Lame -> Bool -> IO ()
setStrictISO l x = handleErrors (c_lame_set_strict_ISO l (fromBool x))

foreign import ccall unsafe "lame_set_strict_ISO"
  c_lame_set_strict_ISO :: Lame -> Int -> IO Int

----------------------------------------------------------------------------
-- Quantization/noize shaping

----------------------------------------------------------------------------
-- VBR control

-- | Set type of VBR.

setVBR :: Lame -> VbrMode -> IO ()
setVBR l x' = handleErrors (c_lame_set_VBR l x)
  where
    x = case x' of
          VbrRh   -> 2
          VbrAbr  -> 3
          VbrMtrh -> 4

foreign import ccall unsafe "lame_set_VBR"
  c_lame_set_VBR :: Lame -> Int -> IO Int

-- | Set VBR quality level, 0 is highest, 9 is lowest.

setVBRQ :: Lame -> Int -> IO ()
setVBRQ l x = handleErrors (c_lame_set_VBR_q l x)

foreign import ccall unsafe "lame_set_VBR_q"
  c_lame_set_VBR_q :: Lame -> Int -> IO Int

-- | Only for VBR ABR: set min bitrate in kbps.

setVBRMinBitrate :: Lame -> Int -> IO ()
setVBRMinBitrate l x = handleErrors (c_lame_set_VBR_min_bitrate_kbps l x)

foreign import ccall unsafe "lame_set_VBR_min_bitrate_kbps"
  c_lame_set_VBR_min_bitrate_kbps :: Lame -> Int -> IO Int

-- | Only for VBR ABR: set mean bitrate in kbps.

setVBRMeanBitrate :: Lame -> Int -> IO ()
setVBRMeanBitrate l x = handleErrors (c_lame_set_VBR_mean_bitrate_kbps l x)

foreign import ccall unsafe "lame_set_VBR_mean_bitrate_bkps"
  c_lame_set_VBR_mean_bitrate_kbps :: Lame -> Int -> IO Int

-- | Only for VBR ABR: set max bitrate in kbps.

setVBRMaxBitrate :: Lame -> Int -> IO ()
setVBRMaxBitrate l x = handleErrors (c_lame_set_VBR_max_bitrate_kbps l x)

foreign import ccall unsafe "lame_set_VBR_max_bitrate_kbps"
  c_lame_set_VBR_max_bitrate_kbps :: Lame -> Int -> IO Int

-- | Set whether to strictly enforce VBR min bitrate. Normally it will be
-- violated for analog silence.

setVBRHardMin :: Lame -> Bool -> IO ()
setVBRHardMin l x = handleErrors (c_lame_set_VBR_hard_min l (fromBool x))

foreign import ccall unsafe "lame_set_VBR_hard_min"
  c_lame_set_VBR_hard_min :: Lame -> Int -> IO Int

----------------------------------------------------------------------------
-- Filtering control

----------------------------------------------------------------------------
-- Psycho-acoustics

----------------------------------------------------------------------------
-- Encoding

-- | Encode given input file.

encodingHelper
  :: Lame              -- ^ The settings
  -> Word64            -- ^ Offset of data chunk
  -> Word64            -- ^ Size of data chunk
  -> FilePath          -- ^ Location of input file (normalized)
  -> FilePath          -- ^ Location of output file (normalized)
  -> IO ()
encodingHelper l dataOffset dataSize ipath opath =
  withCString ipath $ \ipathPtr ->
    withCString opath $ \opathPtr ->
      c_lame_encoding_helper
        l              -- lame settings stucture
        dataOffset     -- offset of data chunk
        dataSize       -- size of data chunk
        ipathPtr       -- path to input file
        opathPtr       -- path to output file

foreign import ccall unsafe "lame_encoding_helper"
  c_lame_encoding_helper
    :: Lame -> Word64 -> Word64 -> CString -> CString -> IO ()

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
