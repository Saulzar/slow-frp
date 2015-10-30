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
      fire freqTriggerRef hz
      let sampleRate = 44100
      let numSamples = floor (time * sampleRate)
      forM_ (replicate numSamples (1 / sampleRate)) $ \inputSample -> do
        resultSample <- fireAndRead sampleTriggerRef inputSample resultEventHandle
        maybe (return ()) (liftIO . hPutBuilder stdout . int16LE . toInt) resultSample
  where
    toInt = toPCM . (* 0.1)

fire
    :: (MonadReflexHost t m, MonadIO m)
    => IORef (Maybe (EventTrigger t a))
    -> a
    -> m ()
fire mtRef input = do
    mt <- liftIO (readIORef mtRef)
    maybe (return ()) (\trigger -> fireEvents [trigger :=> input]) mt

fireAndRead
    :: (MonadReflexHost t m, MonadIO m)
    => IORef (Maybe (EventTrigger t a))
    -> a
    -> EventHandle t b
    -> m (Maybe b)
fireAndRead mtRef input e = do
    mt <- liftIO (readIORef mtRef)
    case mt of
      Nothing -> return Nothing
      Just trigger -> do
          fireEventsAndRead [trigger :=> input] $ do
              mAction <- readEvent e
              case mAction of
                Nothing -> return Nothing
                Just action -> Just <$> action

