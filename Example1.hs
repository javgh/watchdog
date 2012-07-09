import Control.Watchdog
import Data.Time

errorProneTask :: IO (Either String ())
errorProneTask = do
    getCurrentTime >>= print
    return $ Left "some error"

main = watchdog $ watch errorProneTask
