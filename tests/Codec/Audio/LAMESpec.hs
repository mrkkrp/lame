--
-- Tests for the ‘lame’ package.
--
-- Copyright © 2017 Mark Karpov <markkarpov@openmailbox.org>
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are
-- met:
--
-- * Redistributions of source code must retain the above copyright notice,
--   this list of conditions and the following disclaimer.
--
-- * Redistributions in binary form must reproduce the above copyright
--   notice, this list of conditions and the following disclaimer in the
--   documentation and/or other materials provided with the distribution.
--
-- * Neither the name Mark Karpov nor the names of contributors may be used
--   to endorse or promote products derived from this software without
--   specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS “AS IS” AND ANY
-- EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
-- WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY
-- DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
-- STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
-- ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Codec.Audio.LAMESpec
  ( spec )
where

import Codec.Audio.LAME
import Control.Monad
import Data.Default.Class
import Data.Text (Text)
import Data.Word (Word8)
import Sound.HTagLib
import System.Directory
import System.FilePath
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import qualified Data.Text as T

data Info = Info
  { infoTitle       :: Title
  , infoArtist      :: Artist
  , infoAlbum       :: Album
  , infoComment     :: Comment
  , infoGenre       :: Genre
  , infoYear        :: Maybe Year
  , infoTrackNumber :: Maybe TrackNumber
  , infoDuration    :: Duration
  , infoBitRate     :: BitRate
  , infoSampleRate  :: SampleRate
  , infoChannels    :: Channels
  } deriving (Eq, Show)

spec :: Spec
spec =
  describe "encodeMp3" $
    withVariousWaves $
      it "produces correct MP3 file" $ \(ipath, opath) -> do
        encodeMp3 def
          { encoderTagTitle   = pure tagTitle
          , encoderTagArtist  = pure tagArtist
          , encoderTagAlbum   = pure tagAlbum
          , encoderTagYear    = pure tagYear
          , encoderTagComment = pure tagComment
          , encoderTagTrack   = pure tagTrack
          , encoderTagGenre   = pure tagGenre }
          ipath
          opath
        Info {..} <- getTags' opath MPEG $ Info
          <$> titleGetter
          <*> artistGetter
          <*> albumGetter
          <*> commentGetter
          <*> genreGetter
          <*> yearGetter
          <*> trackNumberGetter
          <*> durationGetter
          <*> bitRateGetter
          <*> sampleRateGetter
          <*> channelsGetter
        unTitle infoTitle `shouldBe` tagTitle
        unArtist infoArtist `shouldBe` tagArtist
        unAlbum infoAlbum `shouldBe` tagAlbum
        fmap (T.pack . show . unYear) infoYear  `shouldBe` pure tagYear
        unComment infoComment `shouldBe` tagComment
        fmap unTrackNumber infoTrackNumber `shouldBe`
          (pure . fromIntegral . fst) tagTrack
        unGenre infoGenre `shouldBe` tagGenre
        unDuration infoDuration `shouldBe` 1
        unBitRate infoBitRate `shouldBe` 128
        unSampleRate infoSampleRate `shouldBe` 44100
        unChannels infoChannels `shouldBe` 2

----------------------------------------------------------------------------
-- Helpers

-- | Run given test with various WAVE files.

withVariousWaves :: SpecWith (FilePath, FilePath) -> SpecWith ()
withVariousWaves m =
  forM_ waveFiles $ \(path, desc) ->
    context ("when given " ++ desc) (around (withSandbox path) m)

-- | Make a temporary copy of given file and provide the path to the file in
-- a sandbox directory. Automatically remove the files when the test
-- finishes.

withSandbox :: FilePath -> ActionWith (FilePath, FilePath) -> IO ()
withSandbox path action =
  withSystemTempDirectory "lame-test" $ \dir -> do
    let ipath = dir </> "файл" -- testing Unicode
        opath = dir </> "результат"
    copyFile path ipath
    action (ipath, opath)

waveFiles :: [(FilePath, String)]
waveFiles =
  [ ( "audio-samples/16bit-int.wav"
    , "2 channels 44100 Hz 16 bit PCM" )
  , ( "audio-samples/32bit-float.wav"
    , "2 channels 44100 Hz 32 bit float PCM" )
  , ( "audio-samples/64bit-float.wav"
    , "2 channels 44100 Hz 64 bit float PCM" )
  ]

tagTitle, tagArtist, tagAlbum, tagYear, tagComment, tagGenre :: Text
tagTitle   = "Название"
tagArtist  = "Исполнитель"
tagAlbum   = "Альбом"
tagYear    = "2017"
tagComment = "Комментарий тут…"
tagGenre   = "Жанр"

tagTrack :: (Word8, Maybe Word8)
tagTrack = (1, Just 10)
