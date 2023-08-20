{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
module VerifyParseTerminfoCaps where

import Graphics.Vty.Platform.Windows.WindowsCapabilities ( getStringCapability )
import Prelude hiding ( catch )

import Verify.Data.Terminfo.Parse
import Verify.Graphics.Vty.Output
import Verify

import Data.Maybe ( catMaybes, fromJust )
import Data.Word ()

import Numeric

-- If a terminal defines one of the caps then it's expected to be parsable.
-- TODO: reduce duplication with terminfo terminal implementation.
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
    parseTests <- concat <$> forM capsOfInterest (\capName -> do
                        let caseName = "\tparsing cap: " ++ capName
                        liftIO $ putStrLn caseName
                        return $ case getStringCapability capName of
                            Just capDef -> [verify (caseName ++ " -> " ++ show capDef)
                                                   (verifyParseCap capDef $ const succeeded)]
                            Nothing      -> []
                    )
        
    return $ [ verify "parse_nonParamaterizedCaps" nonParamaterizedCaps
             , verify "parse cap string with literal %" literalPercentCaps
             , verify "parse cap string with %i op" incFirstTwoCaps
             , verify "parse cap string with %pN op" pushParamCaps
             ] ++ parseTests

verifyParseCap capString onParse =
    case parseCapExpression capString of
        Left error -> failed { reason = "parse error " ++ show error }
        Right e    -> onParse e

nonParamaterizedCaps (NonParamCapString cap) = do
    verifyParseCap cap $ \e ->
        let expectedBytes = map (toEnum . fromEnum) cap
            outBytes = bytesForRange e 0 (length cap)
        in verifyBytesEqual outBytes expectedBytes

literalPercentCaps (LiteralPercentCap capString expectedBytes) = do
    verifyParseCap capString $ \e -> verifyBytesEqual (collectBytes e) expectedBytes

incFirstTwoCaps (IncFirstTwoCap capString expectedBytes) = do
    verifyParseCap capString $ \e -> verifyBytesEqual (collectBytes e) expectedBytes

pushParamCaps (PushParamCap capString expectedParamCount expectedBytes) = do
    verifyParseCap capString $ \e ->
        let outBytes = collectBytes e
            outParamCount = paramCount e
        in if outParamCount == expectedParamCount
            then verifyBytesEqual outBytes expectedBytes
            else failed { reason = "out param count /= expected param count" }

decPrintParamCaps (DecPrintCap capString expectedParamCount expectedBytes) = do
    verifyParseCap capString $ \e ->
        let outBytes = collectBytes e
            outParamCount = paramCount e
        in if outParamCount == expectedParamCount
            then verifyBytesEqual outBytes expectedBytes
            else failed { reason = "out param count /= expected param count" }

printCap ti capName = do
    putStrLn $ capName ++ ": " ++ show (getStringCapability capName)

printExpression ti capName = do
    let parseResult = parseCapExpression . fromJust $ getStringCapability capName
    putStrLn $ capName ++ ": " ++ show parseResult
