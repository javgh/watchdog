import Control.Watchdog
import Data.Time

errorProneTask :: IO (Either String ())
errorProneTask = do
    getCurrentTime >>= print
    return $ Left "some error"

main = do
    result <- watchdog $ do
        setMaximumRetries 2
        watchImpatiently errorProneTask
    print result
