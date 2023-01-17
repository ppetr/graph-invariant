-- Copyright 2023 Google LLC
--
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
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
