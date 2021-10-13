{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
-- How to use:
--
-- > import Control.Watchdog
-- > import Data.Time
-- >
-- > errorProneTask :: IO (Either String ())
-- > errorProneTask = do
-- >     getCurrentTime >>= print
-- >     return $ Left "some error"
-- >
-- > main = watchdog $ watch errorProneTask
--
-- Result:
--
-- @
-- 2012-07-09 21:48:19.592252 UTC
-- Watchdog: Error executing task (some error) - waiting 1s before trying again.
-- 2012-07-09 21:48:20.594381 UTC
-- Watchdog: Error executing task (some error) - waiting 2s before trying again.
-- 2012-07-09 21:48:22.597069 UTC
-- Watchdog: Error executing task (some error) - waiting 4s before trying again.
-- ...
-- @
--
-- Alternatively the watchdog can stop after a certain number of attempts:
--
-- > import Control.Watchdog
-- > import Data.Time
-- >
-- > errorProneTask :: IO (Either String ())
-- > errorProneTask = do
-- >     getCurrentTime >>= print
-- >     return $ Left "some error"
-- >
-- > main = do
-- >     result <- watchdog $ do
-- >         setMaximumRetries 2
-- >         watchImpatiently errorProneTask
-- >     print result
--
-- Result:
--
-- @
-- 2012-07-09 21:55:41.046432 UTC
-- Watchdog: Error executing task (some error) - waiting 1s before trying again.
-- 2012-07-09 21:55:42.047246 UTC
-- Watchdog: Error executing task (some error) - waiting 2s before trying again.
-- 2012-07-09 21:55:44.049993 UTC
-- Left \"some error\"
-- @
--
-- The watchdog will execute the task and check the return value, which should
-- be an 'Either' value where 'Left' signals an error and 'Right' signals success.
--
-- The watchdog will backoff exponentially (up to a maximum delay) in case of
-- persisting errors, but will reset after the task has been running for a while
-- without problems (see 'setResetDuration') and start a new cycle of
-- exponential backoff should new errors arise.
--
-- The module is intended to be used in different watchdog settings. For example
-- to keep an eye on a server process (use 'watch' and only return a succesful
-- result when the server is doing a clean shutdown) or to retry an action
-- multiple times, if necessary, before giving up (use 'watchImpatiently').  A
-- monadic approach is used to modify the various settings. Below is a code
-- sample with all possible configuration options and their default values:
--
-- > import Control.Watchdog
-- > import Data.Time
-- >
-- > errorProneTask :: IO (Either String ())
-- > errorProneTask = do
-- >     getCurrentTime >>= print
-- >     return $ Left "some error"
-- >
-- > main = watchdog $ do
-- >         setInitialDelay $ 1 * 10^6      -- 1 second
-- >         setMaximumDelay $ 300 * 10^6    -- 300 seconds
-- >         setMaximumRetries 10            -- has no effect when using 'watch'
-- >         setResetDuration $ 30 * 10^6    -- 30 seconds
-- >         setLoggingAction defaultLogger
-- >         watch errorProneTask
--

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Control.Watchdog
    ( watchdog
    , watchdogBlank
    , watch
    , watchImpatiently
    , setInitialDelay
    , setMaximumDelay
    , setMaximumRetries
    , setResetDuration
    , setLoggingAction
    , defaultLogger
    , showLogger
    , silentLogger
    , formatWatchdogError
    , WatchdogLogger
    , WatchdogAction
    ) where

import Control.Applicative
import Control.Concurrent
import Control.Monad.State.Strict
#if !MIN_VERSION_base(4,9,0)
import Data.Monoid                ((<>))
#endif
import Data.String                (IsString, fromString)
import Data.Time                  (NominalDiffTime, diffUTCTime, getCurrentTime)

data WatchdogState e = WatchdogState { wcInitialDelay   :: Int
                                     , wcMaximumDelay   :: Int
                                     , wcResetDuration  :: Int
                                     , wcMaximumRetries :: Integer
                                     , wcLoggingAction  :: WatchdogLogger e
                                     }

data WatchdogTaskStatus e a = FailedImmediately e
                            | FailedAfterAWhile e
                            | CompletedSuccessfully a

newtype WatchdogAction e a = WA { runWA :: StateT (WatchdogState e) IO a }
                             deriving ( Functor, Applicative, Alternative
                                      , Monad, MonadIO, MonadState (WatchdogState e))

-- | Type synonym for a watchdog logger.
type WatchdogLogger e = e          -- ^ Error value returned by the task.
                      -> Maybe Int -- ^ Waiting time - if any - before trying again.
                      -> IO ()

defaultConf :: WatchdogState String
defaultConf = blankConf { wcLoggingAction = defaultLogger }

blankConf :: WatchdogState e
blankConf = WatchdogState { wcInitialDelay   = 1 * 10 ^ (6::Integer)
                          , wcMaximumDelay   = 300 * 10 ^ (6::Integer)
                          , wcMaximumRetries = 10
                          , wcResetDuration  = 30 * 10 ^ (6::Integer)
                          , wcLoggingAction  = silentLogger
                          }

-- | The Watchdog monad. Used to configure and eventually run a watchdog.
watchdog :: WatchdogAction String a -> IO a
watchdog = watchdogWith defaultConf

-- | As with 'watchdog', but don't specify a default logging action to
--   allow it to be polymorphic.
watchdogBlank :: WatchdogAction e a -> IO a
watchdogBlank = watchdogWith blankConf

watchdogWith :: WatchdogState e -> WatchdogAction e a -> IO a
watchdogWith conf action = evalStateT (runWA action) conf

-- | Set the initial delay in microseconds. The first time the watchdog pauses
-- will be for this amount of time. The default is 1 second.
setInitialDelay :: Int -> WatchdogAction e ()
setInitialDelay delay = modify (\conf -> conf { wcInitialDelay = delay })

-- | Set the maximum delay in microseconds. When a task fails to execute
-- properly multiple times in quick succession, the delay is doubled each time
-- until it stays constant at the maximum delay. The default is 300 seconds.
setMaximumDelay :: Int -> WatchdogAction e ()
setMaximumDelay delay = modify (\conf -> conf { wcMaximumDelay = delay })

-- | If a task has been running for some time, the watchdog will consider
-- the next failure to be something unrelated and reset the waiting time
-- back to the initial delay. This function sets the amount of time in
-- microseconds that needs to pass before the watchdog will consider a task to
-- be successfully running. The default is 30 seconds.
setResetDuration :: Int -> WatchdogAction e ()
setResetDuration duration = modify (\conf -> conf { wcResetDuration = duration })

-- | Set the number of retries after which the watchdog will give up and
-- return with a permanent error. This setting is only used in combination with
-- 'watchImpatiently'. The default is 10.
setMaximumRetries :: Integer -> WatchdogAction e ()
setMaximumRetries retries = modify (\conf -> conf { wcMaximumRetries = retries })

-- | Set the logging action that will be called by the watchdog. The supplied
-- function of type 'WatchdogLogger' will be provided with the error message of
-- the task and either 'Nothing' if the watchdog will retry immediately or 'Just
-- delay' if the watchdog will now pause for the specified amount of time before
-- trying again.  The default is 'defaultLogger'.
setLoggingAction :: WatchdogLogger e -> WatchdogAction e ()
setLoggingAction f = modify (\conf -> conf { wcLoggingAction = f })

-- | Watch a task, restarting it potentially forever or until it returns with a
-- result. The task should return an 'Either', where 'Left' in combination with
-- an error message signals an error and 'Right' with an arbitrary result
-- signals success.
watch :: IO (Either e a) -> WatchdogAction e a
watch task = do
    conf <- get
    liftIO $ go conf (wcInitialDelay conf)
  where
    go conf errorDelay = do
        status <- timeTask (wcResetDuration conf) task
        case status of
            CompletedSuccessfully result -> return result
            FailedAfterAWhile err -> do
                let errorDelay' = wcInitialDelay conf
                    loggingAction = wcLoggingAction conf
                loggingAction err Nothing -- log that we will retry immediately
                go conf errorDelay'
            FailedImmediately err -> do
                let errorDelay' =
                        min (errorDelay * 2) (wcMaximumDelay conf)
                    loggingAction = wcLoggingAction conf
                loggingAction err (Just errorDelay) -- log that we will
                                                    -- retry after a delay
                threadDelay errorDelay
                go conf errorDelay'

-- | Watch a task, but only restart it a limited number of times (see
-- 'setMaximumRetries'). If the failure persists, it will be returned as a 'Left',
-- otherwise it will be 'Right' with the result of the task.
watchImpatiently :: IO (Either e b) -> WatchdogAction e (Either e b)
watchImpatiently task = do
    conf <- get
    liftIO $ go conf 0 (wcInitialDelay conf)
  where
    go conf retries errorDelay = do
        status <- timeTask (wcResetDuration conf) task
        case status of
            CompletedSuccessfully result -> return $ Right result
            FailedAfterAWhile err ->
                if retries >= wcMaximumRetries conf
                    then return $ Left err
                    else do
                        let errorDelay' = wcInitialDelay conf
                            loggingAction = wcLoggingAction conf
                        loggingAction err Nothing
                        go conf (retries + 1) errorDelay'
            FailedImmediately err ->
                if retries >= wcMaximumRetries conf
                    then return $ Left err
                    else do
                        let errorDelay' =
                                min (errorDelay * 2) (wcMaximumDelay conf)
                            loggingAction = wcLoggingAction conf
                        loggingAction err (Just errorDelay)
                        threadDelay errorDelay
                        go conf (retries + 1) errorDelay'

-- | The default logging action. It will call 'formatWatchdogError' and display
-- the result on STDOUT.
defaultLogger :: WatchdogLogger String
defaultLogger taskErr delay = putStrLn $ formatWatchdogError taskErr delay

-- | As with 'defaultLogger', but calls 'show' on the value provided.
showLogger :: (Show e) => WatchdogLogger e
showLogger err = defaultLogger (show err)

-- | Disable logging by passing this function to 'setLoggingAction'.
silentLogger :: WatchdogLogger e
silentLogger _ _ = return ()

-- | Format the watchdog status report. Will produce output like this:
--
-- @
-- Watchdog: Error executing task (some error) - trying again immediately.
-- Watchdog: Error executing task (some error) - waiting 1s before trying again.
-- @
formatWatchdogError :: (IsString str, Monoid str)
                       => str       -- ^ Error message returned by the task.
                       -> Maybe Int -- ^ Waiting time - if any - before trying again.
                       -> str
formatWatchdogError taskErr Nothing =
    "Watchdog: Error executing task (" <> taskErr <> ") - trying again immediately."
formatWatchdogError taskErr (Just delay) =
    let asNominalDiffTime :: NominalDiffTime    -- just to display it properly
        asNominalDiffTime = fromIntegral delay / 10 ^ (6 :: Integer)
    in "Watchdog: Error executing task (" <> taskErr <> ") - waiting"
        <> " " <> fromString (show asNominalDiffTime) <> " before trying again."

-- | Helper class which can be wrapped around a task.
-- The task should return an 'Either', where Left in combination
-- with an error message signals an error and Right with an arbitrary
-- result signals success. In case of failure, the wrapper will check whether
-- the function ran less than 30 seconds and then return 'FailedImmediately'
-- or 'FailedAfterAWhile' accordingly.
timeTask :: Int -> IO (Either e a) -> IO (WatchdogTaskStatus e a)
timeTask resetDuration task = do
    start <- getCurrentTime
    status <- task
    stop <- getCurrentTime
    case status of
        Right result -> return $ CompletedSuccessfully result
        Left err ->
            let cutOff = fromIntegral resetDuration / 10 ^ (6 :: Integer)
            in if diffUTCTime stop start < cutOff
                        then return $ FailedImmediately err
                        else return $ FailedAfterAWhile err
