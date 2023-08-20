{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Generate some input bytes and delays between blocks of input bytes.
-- Verify the events produced are as expected.
module Main where

-- import Graphics.Vty.Platform.Windows.Settings
--     ( defaultSettings,
--       WindowsSettings(..)
--     )

import Graphics.Vty hiding (resize)

import Graphics.Vty.Platform.Windows.Input.Loop ( initInput )
import Graphics.Vty.Platform.Windows.Input.Terminfo
import Graphics.Vty.Platform.Windows.WindowsInterfaces ( configureInput )
import WindowsPseudoConsole

import Control.Concurrent ( forkOS, threadDelay, newEmptyMVar, putMVar, takeMVar )
import Control.Concurrent.STM ( atomically, readTChan )
import Control.Exception ( finally )
import Control.Monad ( forM_, void )
import Data.IORef ( modifyIORef, newIORef, readIORef )
import Data.List (intersperse, nubBy)
import System.IO (Handle, hPutStr, hClose)
import System.Timeout ( timeout )
import Test.Framework.Providers.SmallCheck ( testProperty )
import Test.Framework ( defaultMain )
import Test.SmallCheck ( forAll, monadic, over, Property, Testable )
import Test.SmallCheck.Series ( generate, localDepth, Serial(..) )
import Text.Printf ( printf )

main :: IO ()
main = return ()
-- main = defaultMain
--     [ testProperty "synthesized typing of single visible chars translates to expected events"
--         verifyVisibleSynInputToEvent
--     , testProperty "synthesized typing of hard coded special keys translates to expected events"
--         verifySpecialSynInputToEvent
--     , testProperty "synthesized typing of keys from capabilities tables translates to expected events"
--         verifyCapsSynInputToEvent
--     , testProperty "synthesized typing of any key in the table translates to its paired event"
--         verifyFullSynInputToEvent
--     , testProperty "synthesized typing of 2x any key in the table translates to 2x paired event"
--         verifyFullSynInputToEvent_2x
--     ]

verifyVisibleSynInputToEvent :: Property IO
verifyVisibleSynInputToEvent = forAll $ \(InputBlocksUsingTable gen) -> monadic $ do
    let table    = commonVisibleChars
        inputSeq = gen table
        events   = map snd inputSeq
        keydowns = map (Bytes . fst) inputSeq
        input    = intersperse (Delay testKeyDelay) keydowns ++ [Delay testKeyDelay]
    assertEventsFromSynInput universalTable input events

verifySpecialSynInputToEvent :: Property IO
verifySpecialSynInputToEvent = forAll $ \(InputBlocksUsingTable gen) -> monadic $ do
    let table         = specialSupportKeys
        inputSeq      = gen table
        events        = map snd inputSeq
        keydowns      = map (Bytes . fst) inputSeq
        input         = intersperse (Delay testKeyDelay) keydowns ++ [Delay testKeyDelay]
    assertEventsFromSynInput universalTable input events

verifyCapsSynInputToEvent :: Property IO
verifyCapsSynInputToEvent = forAll $ \(InputBlocksUsingTable gen) -> monadic $ do
        let table         = classifyMapForTerm
            inputSeq      = gen table
            events        = map snd inputSeq
            keydowns      = map (Bytes . fst) inputSeq
            input         = intersperse (Delay testKeyDelay) keydowns ++ [Delay testKeyDelay]
        assertEventsFromSynInput table input events

verifyFullSynInputToEvent :: Property IO
verifyFullSynInputToEvent = forAll $ \(InputBlocksUsingTable gen) -> monadic $ do
        let table         = classifyMapForTerm
            inputSeq      = gen table
            events        = map snd inputSeq
            keydowns      = map (Bytes . fst) inputSeq
            input         = intersperse (Delay testKeyDelay) keydowns ++ [Delay testKeyDelay]
        assertEventsFromSynInput table input events

verifyFullSynInputToEvent_2x :: Property IO
verifyFullSynInputToEvent_2x = forAll $ \(InputBlocksUsingTable gen) -> monadic $ do
        let table         = classifyMapForTerm
            inputSeq      = gen table
            events        = concatMap ((\s -> [s,s]) . snd) inputSeq
            keydowns      = map (Bytes . (\s -> s ++ s) . fst) inputSeq
            input         = intersperse (Delay testKeyDelay) keydowns ++ [Delay testKeyDelay]
        assertEventsFromSynInput table input events

-- processing a block of 16 chars is the largest I can do without taking
-- too long to run the test.
-- maxBlockSize :: Int
-- maxBlockSize = 16

maxTableSize :: Int
maxTableSize = 28

forEachOf :: (Show a, Testable m b) => [a] -> (a -> b) -> Property m
forEachOf l = over (generate (\n -> take n l))

data InputEvent
    = Bytes String
    -- ^ Input sequence encoded as a string. Regardless, the input is
    -- read a byte at a time.
    | Delay Int
    -- ^ Microsecond delay
    deriving Show

type InputSpec = [InputEvent]

type ExpectedSpec = [Event]

synthesizeInput :: InputSpec -> Handle -> IO ()
synthesizeInput input outHandle = forM_ input f >> void (hPutStr outHandle "\xFFFD")
    where
        f (Bytes str) = void $ hPutStr outHandle str
        f (Delay t) = threadDelay t

minDetectableDelay :: Int
minDetectableDelay = 4000

minTimout :: Int
minTimout = 4000000

testKeyDelay :: Int
testKeyDelay = minDetectableDelay * 4

testEscSampleDelay :: Int
testEscSampleDelay = minDetectableDelay * 2

genEventsUsingIoActions :: Int -> IO () -> IO () -> IO ()
genEventsUsingIoActions maxDuration inputAction outputAction = do
    let maxDuration' = max minTimout maxDuration
    readComplete <- newEmptyMVar
    writeComplete <- newEmptyMVar
    _ <- forkOS $ inputAction `finally` putMVar writeComplete ()
    _ <- forkOS $ outputAction `finally` putMVar readComplete ()
    Just () <- timeout maxDuration' $ takeMVar writeComplete
    Just () <- timeout maxDuration' $ takeMVar readComplete
    return ()

compareEvents :: (Show a1, Show a, Eq a1) => a -> [a1] -> [a1] -> IO Bool
compareEvents inputSpec expectedEvents outEvents = compareEvents' expectedEvents outEvents
    where
        compareEvents' [] []         = return True
        compareEvents' [] outEvents' = do
            printf "extra events %s\n" (show outEvents') :: IO ()
            return False
        compareEvents' expectedEvents' [] = do
            printf "events %s were not produced for input %s\n" (show expectedEvents') (show inputSpec) :: IO ()
            printf "expected events %s\n" (show expectedEvents) :: IO ()
            printf "received events %s\n" (show outEvents) :: IO ()
            return False
        compareEvents' (e : expectedEvents') (o : outEvents')
            | e == o    = compareEvents' expectedEvents' outEvents'
            | otherwise = do
                printf "%s expected not %s for input %s\n" (show e) (show o) (show inputSpec) :: IO ()
                printf "expected events %s\n" (show expectedEvents) :: IO ()
                printf "received events %s\n" (show outEvents) :: IO ()
                return False

assertEventsFromSynInput :: ClassifyMap -> InputSpec -> ExpectedSpec -> IO Bool
assertEventsFromSynInput table inputSpec expectedEvents = do
    let maxDuration = sum [t | Delay t <- inputSpec] + minDetectableDelay
        eventCount = length expectedEvents
    (writeFd, readFd) <- openPseudoTerminal
    (setAttrs, unsetAttrs) <- configureInput readFd
    setAttrs

    input <- initInput defaultConfig readFd table
    eventsRef <- newIORef []
    let writeWaitClose = do
            synthesizeInput inputSpec writeFd
            threadDelay minDetectableDelay
            shutdownInput input
            threadDelay minDetectableDelay
            hClose writeFd
            hClose readFd
    -- drain output pipe
    let readEvents = readLoop eventCount
        readLoop 0 = return ()
        readLoop n = do
            e <- atomically $ readTChan $ eventChannel input
            case e of
                InputEvent ev -> modifyIORef eventsRef ((:) ev)
                ResumeAfterInterrupt -> return ()
            readLoop (n - 1)
    genEventsUsingIoActions maxDuration writeWaitClose readEvents
    outEvents <- reverse <$> readIORef eventsRef
    unsetAttrs
    compareEvents inputSpec expectedEvents outEvents

newtype InputBlocksUsingTable event
    = InputBlocksUsingTable ([(String,event)] -> [(String, event)])

instance Show (InputBlocksUsingTable event) where
    show (InputBlocksUsingTable _g) = "InputBlocksUsingTable"

instance Monad m => Serial m (InputBlocksUsingTable event) where
    series = do
        n :: Int <- localDepth (const maxTableSize) series
        return $ InputBlocksUsingTable $ \raw_table ->
                 let table = reverse $ nubBy (\(s0,_) (s1,_) -> s0 == s1) $ reverse raw_table
                 in concat (take n (selections table))
        where
            selections []     = []
            selections (x:xs) = let z = selections xs in [x] : (z ++ map ((:) x) z)
