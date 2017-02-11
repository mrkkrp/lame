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
  , withLame )
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

----------------------------------------------------------------------------
-- Helpers

-- | Coerce to 'Ptr' and check if it's a null pointer, return 'Nothing' if
-- it is, otherwise return the given pointer unchanged. Needless to say that
-- this thing is unsafe.

maybePtr :: a -> Maybe a
maybePtr a
  | unsafeCoerce a == nullPtr = Nothing
  | otherwise                 = Just a
