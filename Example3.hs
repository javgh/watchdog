import Control.Watchdog
import Data.Time

errorProneTask :: IO (Either String ())
errorProneTask = do
    getCurrentTime >>= print
    return $ Left "some error"

main = watchdog $ do
        setInitialDelay $ 1 * 10^6      -- 1 second
        setMaximumDelay $ 300 * 10^6    -- 300 seconds
        setMaximumRetries 10            -- has no effect when using 'watch'
        setResetDuration $ 30 * 10^6    -- 30 seconds
        setLoggingAction defaultLogger
        watch errorProneTask
