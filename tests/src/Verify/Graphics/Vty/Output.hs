module Verify.Graphics.Vty.Output where

import Control.Applicative ((<$>))

import Graphics.Vty.Output.Mock

import Data.IORef
import qualified Data.String.UTF8 as UTF8

import Test.QuickCheck.Property

compareMockOutput :: MockData -> String -> IO Result
compareMockOutput mockData expectedStr = do
    outStr <- UTF8.toString <$> readIORef mockData
    if outStr /=  expectedStr
        then return $ failed { reason = "bytes\n" ++ outStr
                                      ++ "\nare not the expected bytes\n"
                                      ++ expectedStr
                             }
        else return succeeded
