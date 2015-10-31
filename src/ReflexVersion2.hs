{-# LANGUAGE RankNTypes #-}

module ReflexVersion2 where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Builder
import Reflex.MiniSpider
import Shared
import System.IO (hSetEcho, hSetBuffering, stdout, BufferMode (NoBuffering))
import Control.Monad.IO.Class

exec :: IO ()
exec = do
    (sampleEvent, fireSample) <- newEventWithFire
    (freqEvent, fireFreq) <- newEventWithFire

    let guest hz samples = do
          phase <- foldDyn ($) 0 (attachWith stepPhase hz samples)
          return $ tag (sampleWaveform SawWave <$> snd phase) samples

    freq <- runHostFrame $ hold 0 freqEvent
    resultEvent <- runHostFrame $ guest freq sampleEvent
    resultEventHandle <- subscribeEvent resultEvent

    
    forM_ furEliseNotes $ \(hz, time) -> do
      fireEvents [fireFreq hz]
      
      let sampleRate = 44100
      let numSamples = floor (time * sampleRate)
      forM_ (replicate numSamples (1 / sampleRate)) $ \inputSample -> do
        resultSample <- fireEventsAndRead [fireSample inputSample] (readEvent resultEventHandle)
        maybe (return ()) (liftIO . hPutBuilder stdout . int16LE . toInt) resultSample
  where
    toInt = toPCM . (* 0.1)

