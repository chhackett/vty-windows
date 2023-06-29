-- | This module provides parsers for mouse events for both "normal" and
-- "extended" modes. This implementation was informed by
--
-- http://invisible-island.net/xterm/ctlseqs/ctlseqs.html#h2-Mouse-Tracking
module Graphics.Vty.Platform.Windows.Input.Mouse
  ( requestMouseEvents
  , disableMouseEvents
  , isMouseEvent
  , classifyMouseEvent
  )
where

import Graphics.Vty.Input.Events
import Graphics.Vty.Platform.Windows.Input.Classify.Types
import Graphics.Vty.Platform.Windows.Input.Classify.Parse

import Control.Monad
import Data.Maybe (catMaybes)
import Data.Bits ((.&.))

import qualified Data.ByteString.Char8 as BS8
import Data.ByteString.Char8 (ByteString)

-- A mouse event in SGR extended mode is
--
-- '\ESC' '[' '<' B ';' X ';' Y ';' ('M'|'m')
--
-- where
--
-- * B is the number with button and modifier bits set,
-- * X is the X coordinate of the event starting at 1
-- * Y is the Y coordinate of the event starting at 1
-- * the final character is 'M' for a press, 'm' for a release

-- | These sequences set xterm-based terminals to send mouse event
-- sequences.
requestMouseEvents :: ByteString
requestMouseEvents = BS8.pack "\ESC[?1000h\ESC[?1002h\ESC[?1006h"

-- | These sequences disable mouse events.
disableMouseEvents :: ByteString
disableMouseEvents = BS8.pack "\ESC[?1000l\ESC[?1002l\ESC[?1006l"

-- | Does the specified string begin with a mouse event?
isMouseEvent :: ByteString -> Bool
isMouseEvent s = isSGREvent s || isNormalEvent s

isSGREvent :: ByteString -> Bool
isSGREvent = BS8.isPrefixOf sgrPrefix

sgrPrefix :: ByteString
sgrPrefix = BS8.pack "\ESC[M"

isNormalEvent :: ByteString -> Bool
isNormalEvent = BS8.isPrefixOf normalPrefix

normalPrefix :: ByteString
normalPrefix = BS8.pack "\ESC[<"

-- Modifier bits:
shiftBit :: Int
shiftBit = 4

metaBit :: Int
metaBit = 8

ctrlBit :: Int
ctrlBit = 16

-- These bits indicate the buttons involved:
buttonMask :: Int
buttonMask = 67

leftButton :: Int
leftButton = 0

middleButton :: Int
middleButton = 1

rightButton :: Int
rightButton = 2

scrollUp :: Int
scrollUp = 64

scrollDown :: Int
scrollDown = 65

hasBitSet :: Int -> Int -> Bool
hasBitSet val bit = val .&. bit > 0

-- | Attempt to classify an input string as a mouse event.
classifyMouseEvent :: ByteString -> KClass
classifyMouseEvent s = runParser s $ do
    when (not $ isMouseEvent s) failParse

    expectChar '\ESC'
    expectChar '['
    ty <- readChar
    case ty of
        '<' -> classifySGRMouseEvent
        'M' -> classifyNormalMouseEvent
        _   -> failParse

-- Given a modifier/button value, determine which button was indicated
getSGRButton :: Int -> Parser Button
getSGRButton mods =
    let buttonMap = [ (leftButton,   BLeft)
                    , (middleButton, BMiddle)
                    , (rightButton,  BRight)
                    , (scrollUp,     BScrollUp)
                    , (scrollDown,   BScrollDown)
                    ]
    in case lookup (mods .&. buttonMask) buttonMap of
        Nothing -> failParse
        Just b -> return b

getModifiers :: Int -> [Modifier]
getModifiers mods =
    catMaybes [ if mods `hasBitSet` shiftBit then Just MShift else Nothing
              , if mods `hasBitSet` metaBit  then Just MMeta  else Nothing
              , if mods `hasBitSet` ctrlBit  then Just MCtrl  else Nothing
              ]

-- Attempt to classify a control sequence as a "normal" mouse event. To
-- get here we should have already read "\ESC[M" so that will not be
-- included in the string to be parsed.
classifyNormalMouseEvent :: Parser Event
classifyNormalMouseEvent = do
    statusChar <- readChar
    xCoordChar <- readChar
    yCoordChar <- readChar

    let xCoord = fromEnum xCoordChar - 32
        yCoord = fromEnum yCoordChar - 32
        status = fromEnum statusChar
        modifiers = getModifiers status

    let press = status .&. buttonMask /= 3
    case press of
            True -> do
                button <- getSGRButton status
                return $ EvMouseDown (xCoord-1) (yCoord-1) button modifiers
            False -> return $ EvMouseUp (xCoord-1) (yCoord-1) Nothing

-- Attempt to classify a control sequence as an SGR mouse event. To
-- get here we should have already read "\ESC[<" so that will not be
-- included in the string to be parsed.
classifySGRMouseEvent :: Parser Event
classifySGRMouseEvent = do
    mods <- readInt
    expectChar ';'
    xCoord <- readInt
    expectChar ';'
    yCoord <- readInt
    final <- readChar

    let modifiers = getModifiers mods
    button <- getSGRButton mods
    case final of
        'M' -> return $ EvMouseDown (xCoord-1) (yCoord-1) button modifiers
        'm' -> return $ EvMouseUp   (xCoord-1) (yCoord-1) (Just button)
        _ -> failParse
