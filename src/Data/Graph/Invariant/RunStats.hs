module Data.Graph.Invariant.RunStats
  ( RunTime(..)
  , currentRunTime
  ) where

import           Control.Exception              ( throwIO )
import           Control.Monad                  ( when )
import qualified Data.ByteString.Char8         as BS
import           System.Posix.Unistd
import           Text.Read

data RunTime = RunTime
  { runTimeKernel :: Double
  , runTimeUser   :: Double
  }
  deriving (Eq, Ord, Show, Read)

parseNumber :: BS.ByteString -> IO Double
parseNumber bs = case readEither (BS.unpack bs) of
  Left err ->
    throwIO
      .  userError
      $  "Cannot parse number '"
      ++ BS.unpack bs
      ++ "': "
      ++ show err
  Right x -> return x

currentRunTime :: IO RunTime
currentRunTime = do
  tick <- fromIntegral <$> getSysVar ClockTick
  stat <- BS.words <$> BS.readFile "/proc/self/stat"
  when (length stat < 15) . throwIO $ userError
    "/proc/self/stat doesn't have the expected utime/stime fields"
  utime <- parseNumber (stat !! 13)
  stime <- parseNumber (stat !! 14)
  return $ RunTime { runTimeKernel = stime / tick, runTimeUser = utime / tick }
