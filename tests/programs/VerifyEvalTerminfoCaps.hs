{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module VerifyEvalTerminfoCaps where

import Graphics.Vty.Platform.Windows.WindowsCapabilities

import Blaze.ByteString.Builder.Internal.Write (runWrite, getBound)
import Data.Terminfo.Eval
import Data.Terminfo.Parse
import Control.DeepSeq

import Verify
import Verify.Graphics.Vty.Output

import Control.Applicative ( (<$>) )
import Control.Exception ( try, SomeException(..) )

import Control.Monad ( mapM_, forM, forM_ )

import Data.Maybe ( fromJust )
import Data.Word

import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Ptr (Ptr, minusPtr)
import Numeric

-- If a terminal defines one of the caps then it's expected to be
-- parsable.
capsOfInterest =
    [ "cup"
    , "sc"
    , "rc"
    , "setf"
    , "setb"
    , "setaf"
    , "setab"
    , "op"
    , "cnorm"
    , "civis"
    , "smcup"
    , "rmcup"
    , "clear"
    , "hpa"
    , "vpa"
    , "sgr"
    , "sgr0"
    ]

tests :: IO [Test]
tests = do
    -- 1 MB should be big enough for any termcaps ;-)
    evalBuffer :: Ptr Word8 <- mallocBytes (1024 * 1024)
    fmap concat $ forM capsOfInterest $ \capName -> do
        case getStringCapability capName of
            Just capDef -> do
                putStrLn $ "\tadding test for cap: " ++ capName
                let testName = "(" ++ capName ++ ")"
                case parseCapExpression capDef of
                    Left error -> return [verify testName (failed {reason = "parse error " ++ show error})]
                    Right !cap_expr -> return [verify testName (verifyEvalCap evalBuffer cap_expr)]
            Nothing      -> do
                return []

{-# NOINLINE verifyEvalCap #-}
verifyEvalCap :: Ptr Word8 -> CapExpression -> Int -> Property
verifyEvalCap evalBuffer expr !junkInt = do
    forAll (vector 9) $ \inputValues ->
        let write = writeCapExpr expr inputValues
            !byteCount = getBound write
        in liftIOResult $ do
            let startPtr :: Ptr Word8 = evalBuffer
            forM_ [0..100] $ \i -> runWrite write startPtr
            endPtr <- runWrite write startPtr
            case endPtr `minusPtr` startPtr of
                count | count < 0        ->
                            return $ failed { reason = "End pointer before start pointer." }
                      | toEnum count > byteCount ->
                            return $ failed { reason = "End pointer past end of buffer by "
                                                       ++ show (toEnum count - byteCount)
                                            }
                      | otherwise        ->
                            return succeeded
