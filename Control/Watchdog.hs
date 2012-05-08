-- |
-- How to use:
--
-- > import Control.Watchdog
-- >
-- > main = do
-- >     let conf = defaultWatchdogConfig
-- >         hopelessTask = putStrLn "I give up."
-- >                         >> return (Left "gave up too early")
-- >     watchdog conf (timeTask hopelessTask)
--
-- Result:
--
-- @
-- I give up.
-- Watchdog: Error executing task (gave up too early) - waiting 1 second(s) before retrying.
-- I give up.
-- Watchdog: Error executing task (gave up too early) - waiting 2 second(s) before retrying.
-- I give up.
-- Watchdog: Error executing task (gave up too early) - waiting 4 second(s) before retrying.
-- ...
-- @
--
-- The watchdog will execute the task and check the return value.
-- 
--  * If the value is 'CompletedSuccessfully' the watchdog terminates.
-- 
--  * If the value is 'FailedImmediately' the watchdog will sleep and then retry.
--    Upon repeated 'FailedImmediately' the timeout is doubled until it reaches
--    the maximum timeout.
--
--  * If the value is 'FailedAfterAWhile' the watchdog will reset the current timeout delay
--    and then retry immediately.
--
-- The result should be, that the watchdog will backoff exponentially in case of persisting
-- error, but will reset after the task has been running successfully (again) for a while.
--
-- The helper function 'timeTask' can be used, to check how long a task has been
-- running and set the return value accordingly.
--
module Control.Watchdog
    ( defaultWatchdogConfig
    , customWatchdogConfig
    , watchdog
    , timeTask
    , WatchdogTaskStatus(..)
    ) where

import Control.Concurrent
import Data.Time.Clock

data WatchdogConfig = WatchdogConfig { wcInitialErrorTimeout :: Int
                                     , wcMaxErrorTimeout :: Int
                                     , wcLogAction :: String -> Maybe Int -> IO ()
                                     }

-- | Data structure to signal failure (with error messages)
-- or success (with an arbitrary result).
data WatchdogTaskStatus a = FailedImmediately String | FailedAfterAWhile String
                             | CompletedSuccessfully a

-- | Returns the default watchdog configuration:
--
--   * Initial timeout of 1 second
--
--   * Maximum timeout of 300 seconds
--
--   * Actions are logged to STDOUT
--
defaultWatchdogConfig :: WatchdogConfig
defaultWatchdogConfig = WatchdogConfig { wcInitialErrorTimeout = 1 * 1000000
                                       , wcMaxErrorTimeout = 300 * 1000000
                                       , wcLogAction = logAction
                                       }
  where
    logAction err Nothing = putStrLn $ "Watchdog: Error executing task (" ++ err ++ ") - retrying immediately"
    logAction err (Just timeout) = putStrLn $ "Watchdog: Error executing task (" ++ err ++ ") - waiting"
                                            ++ " " ++ displayErrorTimeout timeout ++ " second(s)"
                                            ++ " before retrying."
    displayErrorTimeout :: Int -> String
    displayErrorTimeout timeout =
        let completeOutput = show timeout
        in take (length completeOutput - 6) completeOutput

-- | Build a custom watchdog configuration.
customWatchdogConfig :: Int -- ^ initial timeout (in microseconds)
                        -> Int -- ^ maximum timeout (in microseconds)
                        -> (String -> Maybe Int -> IO ())
                            -- ^ logging function, which will receive the error
                            -- message and how long the watchdog will wait (if it all)
                        -> WatchdogConfig
customWatchdogConfig initialErrorTimeout maxErrorTimeout logAction =
    WatchdogConfig { wcInitialErrorTimeout = initialErrorTimeout
                   , wcMaxErrorTimeout = maxErrorTimeout
                   , wcLogAction = logAction
                   }

-- | Helper class which can be wrapped around a task.
-- The task should return an 'Either', where Left in combination
-- with an error message signals an error and Right with an arbitrary
-- result signals success. In case of failure, the wrapper will check whether
-- the function ran less than 30 seconds and then return 'FailedImmediately'
-- or 'FailedAfterAWhile' accordingly.
timeTask :: IO (Either String a) -> IO (WatchdogTaskStatus a)
timeTask task = do
    start <- getCurrentTime
    status <- task
    stop <- getCurrentTime
    case status of
        Right result -> return $ CompletedSuccessfully result
        Left err -> if diffUTCTime stop start < 30 {- seconds -}
                        then return $ FailedImmediately err
                        else return $ FailedAfterAWhile err

-- | Run the watchdog.
watchdog :: WatchdogConfig  -- ^ configuration to use
            -> IO (WatchdogTaskStatus a) -- ^ task to run; needs to return a 'WatchdogTaskStatus'
            -> IO a
watchdog conf task = go (wcInitialErrorTimeout conf)
  where
    go errorTimeout = do
        status <- task
        case status of
            CompletedSuccessfully result -> return result
            FailedAfterAWhile err -> do
                let errorTimeout' = wcInitialErrorTimeout conf
                wcLogAction conf err Nothing   -- log that we will retry immediatly
                go errorTimeout'
            FailedImmediately err -> do
                let errorTimeout' =
                        min (errorTimeout * 2) (wcMaxErrorTimeout conf)
                wcLogAction conf err (Just errorTimeout)   -- log that we will retry after delay
                threadDelay errorTimeout
                go errorTimeout'
