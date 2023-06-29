module Graphics.Vty.Platform.Windows.Input.Terminfo
  ( classifyMapForTerm
  , specialSupportKeys
  , capsClassifyMap
  , keysFromCapsTable
  , universalTable
  , visibleChars
  )
where

import Graphics.Vty.Input.Events
import qualified Graphics.Vty.Platform.Windows.Input.Terminfo.ANSIVT as ANSIVT
import Graphics.Vty.Platform.Windows.WindowsCapabilities
    ( getStringCapability )

import Control.Arrow ( Arrow(first) )

-- | Queries the terminal for all capability-based input sequences and
-- then adds on a terminal-dependent input sequence mapping.
--
-- For reference see:
--
-- * http://vimdoc.sourceforge.net/htmldoc/term.html
--
-- * vim74/src/term.c
--
-- * http://invisible-island.net/vttest/
--
-- * http://aperiodic.net/phil/archives/Geekery/term-function-keys.html
--
-- Terminfo is incomplete. The vim source implies that terminfo is also
-- incorrect. Vty assumes that the internal terminfo table added to the
-- system-provided terminfo table is correct.
--
-- The procedure used here is:
--
-- 1. Build terminfo table for all caps. Missing caps are not added.
--
-- 2. Add tables for visible chars, esc, del, ctrl, and meta.
--
-- 3. Add internally-defined table for given terminal type.
--
-- Precedence is currently implicit in the 'compile' algorithm.
classifyMapForTerm :: ClassifyMap
classifyMapForTerm =
    concat $ capsClassifyMap keysFromCapsTable
           : universalTable
           : termSpecificTables

-- | The key table applicable to all terminals.
--
-- Note that some of these entries are probably only applicable to
-- ANSI/VT100 terminals.
universalTable :: ClassifyMap
universalTable = concat [visibleChars, ctrlChars, ctrlMetaChars, specialSupportKeys]

capsClassifyMap :: [(String,Event)] -> ClassifyMap
capsClassifyMap table = [(x,y) | (Just x,y) <- map extractCap table]
    where extractCap = first getStringCapability

-- | Tables specific to a given terminal that are not derivable from
-- terminfo.
--
-- Note that this adds the ANSI/VT100/VT50 tables regardless of term
-- identifier.
termSpecificTables :: [ClassifyMap]
termSpecificTables = ANSIVT.classifyTable

-- | Visible characters in the ISO-8859-1 and UTF-8 common set.
--
-- We limit to < 0xC1. The UTF8 sequence detector will catch all values
-- 0xC2 and above before this classify table is reached.
visibleChars :: ClassifyMap
visibleChars = [ ([x], EvKey (KChar x) [])
               | x <- [' ' .. toEnum 0xC1]
               ]

-- | Non-printable characters in the ISO-8859-1 and UTF-8 common set
-- translated to ctrl + char.
--
-- This treats CTRL-i the same as tab.
ctrlChars :: ClassifyMap
ctrlChars =
    [ ([toEnum x],EvKey (KChar y) [MCtrl])
    | (x,y) <- zip [0..31] ('@':['a'..'z']++['['..'_'])
    , y /= 'i'  -- Resolve issue #3 where CTRL-i hides TAB.
    , y /= 'h'  -- CTRL-h should not hide BS
    ]

-- | Ctrl+Meta+Char
ctrlMetaChars :: ClassifyMap
ctrlMetaChars = map (\(s,EvKey c m) -> ('\ESC':s, EvKey c (MMeta:m))) ctrlChars

-- | Esc, meta-esc, delete, meta-delete, enter, meta-enter.
specialSupportKeys :: ClassifyMap
specialSupportKeys =
    [ ("\ESC\ESC[5~",EvKey KPageUp [MMeta])
    , ("\ESC\ESC[6~",EvKey KPageDown [MMeta])
    -- special support for ESC
    , ("\ESC",EvKey KEsc []), ("\ESC\ESC",EvKey KEsc [MMeta])
    -- Special support for backspace
    , ("\DEL",EvKey KBS []), ("\ESC\DEL",EvKey KBS [MMeta]), ("\b",EvKey KBS [])
    -- Special support for Enter
    , ("\ESC\^J",EvKey KEnter [MMeta]), ("\^J",EvKey KEnter [])
    -- explicit support for tab
    , ("\t", EvKey (KChar '\t') [])
    ]

-- | A classification table directly generated from terminfo cap
-- strings.  These are:
--
-- * ka1 - keypad up-left
--
-- * ka3 - keypad up-right
--
-- * kb2 - keypad center
--
-- * kbs - keypad backspace
--
-- * kbeg - begin
--
-- * kcbt - back tab
--
-- * kc1 - keypad left-down
--
-- * kc3 - keypad right-down
--
-- * kdch1 - delete
--
-- * kcud1 - down
--
-- * kend - end
--
-- * kent - enter
--
-- * kf0 - kf63 - function keys
--
-- * khome - KHome
--
-- * kich1 - insert
--
-- * kcub1 - left
--
-- * knp - next page (page down)
--
-- * kpp - previous page (page up)
--
-- * kcuf1 - right
--
-- * kDC - shift delete
--
-- * kEND - shift end
--
-- * kHOM - shift home
--
-- * kIC - shift insert
--
-- * kLFT - shift left
--
-- * kRIT - shift right
--
-- * kcuu1 - up
keysFromCapsTable :: ClassifyMap
keysFromCapsTable =
    [ ("ka1",   EvKey KUpLeft    [])
    , ("ka3",   EvKey KUpRight   [])
    , ("kb2",   EvKey KCenter    [])
    , ("kbs",   EvKey KBS        [])
    , ("kbeg",  EvKey KBegin     [])
    , ("kcbt",  EvKey KBackTab   [])
    , ("kc1",   EvKey KDownLeft  [])
    , ("kc3",   EvKey KDownRight [])
    , ("kdch1", EvKey KDel       [])
    , ("kcud1", EvKey KDown      [])
    , ("kend",  EvKey KEnd       [])
    , ("kent",  EvKey KEnter     [])
    , ("khome", EvKey KHome      [])
    , ("kich1", EvKey KIns       [])
    , ("kcub1", EvKey KLeft      [])
    , ("knp",   EvKey KPageDown  [])
    , ("kpp",   EvKey KPageUp    [])
    , ("kcuf1", EvKey KRight     [])
    , ("kDC",   EvKey KDel       [MShift])
    , ("kEND",  EvKey KEnd       [MShift])
    , ("kHOM",  EvKey KHome      [MShift])
    , ("kIC",   EvKey KIns       [MShift])
    , ("kLFT",  EvKey KLeft      [MShift])
    , ("kRIT",  EvKey KRight     [MShift])
    , ("kcuu1", EvKey KUp        [])
    ] ++ functionKeyCapsTable

-- | Cap names for function keys.
functionKeyCapsTable :: ClassifyMap
functionKeyCapsTable = flip map [0..63] $ \n -> ("kf" ++ show n, EvKey (KFun n) [])
