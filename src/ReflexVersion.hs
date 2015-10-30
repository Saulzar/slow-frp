{-# LANGUAGE RankNTypes #-}

module ReflexVersion where

import Control.Applicative
import Control.Concurrent (forkIO)
import Control.Monad
import Control.Monad.Fix (MonadFix)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString.Builder
import Data.Dependent.Sum (DSum ((:=>)))
import Data.IORef (IORef, readIORef)
import Reflex
import Reflex.Host.Class
import Shared
import System.IO (hSetEcho, hSetBuffering, stdout, BufferMode (NoBuffering))
import Control.Monad.IO.Class

exec :: IO ()
exec = runSpiderHost $ do
    (sampleEvent, sampleTriggerRef) <- newEventWithTriggerRef
    (freqEvent, freqTriggerRef) <- newEventWithTriggerRef

    let guest hz samples = do
          phase <- foldDyn ($) 0 (attachWith stepPhase hz samples)
          return $ tag (sampleWaveform SawWave <$> current phase) samples

    freq <- runHostFrame $ hold 0 freqEvent
    resultEvent <- runHostFrame $ guest freq sampleEvent
    resultEventHandle <- subscribeEvent resultEvent

    forM_ furEliseNotes $ \(hz, time) -> do
      fireEventRef freqTriggerRef hz
      let sampleRate = 44100
      let numSamples = floor (time * sampleRate)
      forM_ (replicate numSamples (1 / sampleRate)) $ \inputSample -> do
        resultSample <- fireEventRefAndRead sampleTriggerRef inputSample resultEventHandle
        maybe (return ()) (liftIO . hPutBuilder stdout . int16LE . toInt) resultSample
  where
    toInt = toPCM . (* 0.1)

