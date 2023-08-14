-- | Input mappings for ANSI/VT100/VT50 terminals that is missing from
-- terminfo.
--
-- Or that are sent regardless of terminfo by terminal emulators. EG:
-- Terminal emulators will often use VT50 input bytes regardless of
-- declared terminal type. This provides compatibility with programs
-- that don't follow terminfo.
module Graphics.Vty.Platform.Windows.Input.Terminfo.ANSIVT
  ( classifyTable
  )
where

import Graphics.Vty.Input.Events

classifyTable :: [ClassifyMap]
classifyTable =
    [ navKeys0
    , navKeys1
    , navKeys2
    , navKeys3
    , functionKeysMap
    , [("\ESC[Z", EvKey KBackTab [])]  -- don't forget poor little backtab
    ]

-- | Encoding for navigation keys.
navKeys0 :: ClassifyMap
navKeys0 =
    [ k "G" KCenter
    , k "P" KPause
    , k "A" KUp
    , k "B" KDown
    , k "C" KRight
    , k "D" KLeft
    , k "H" KHome
    , k "F" KEnd
    , k "E" KBegin
    ]
    where k c s = ("\ESC[" ++ c, EvKey s [])

-- | encoding for shift, meta and ctrl plus arrows/home/end
navKeys1 :: ClassifyMap
navKeys1 =
   [("\ESC[" ++ charCnt ++ show mc ++ c, EvKey s m)
    | charCnt <- ["1;", ""], -- we can have a count or not
    (m,mc) <- [([MShift],2::Int), ([MCtrl],5), ([MMeta],3),
               -- modifiers and their codes
               ([MShift, MCtrl],6), ([MShift, MMeta],4)],
    -- directions and their codes
    (c,s) <- [("A", KUp), ("B", KDown), ("C", KRight), ("D", KLeft), ("H", KHome), ("F", KEnd)]
   ]

-- | encoding for ins, del, pageup, pagedown, home, end
navKeys2 :: ClassifyMap
navKeys2 =
    let k n s = ("\ESC[" ++ show n ++ "~", EvKey s [])
    in zipWith k [2::Int,3,5,6,1,4]
                 [KIns,KDel,KPageUp,KPageDown,KHome,KEnd]

-- | encoding for ctrl + ins, del, pageup, pagedown, home, end
navKeys3 :: ClassifyMap
navKeys3 =
    let k n s = ("\ESC[" ++ show n ++ ";5~", EvKey s [MCtrl])
    in zipWith k [2::Int,3,5,6,1,4]
                 [KIns,KDel,KPageUp,KPageDown,KHome,KEnd]


-- pulled directly from https://learn.microsoft.com/en-us/windows/console/console-virtual-terminal-sequences#numpad--function-keysfunctionKeysMap :: ClassifyMap
functionKeysMap :: ClassifyMap
functionKeysMap =
  [ ("\ESCOP", EvKey (KFun 1) []),
    ("\ESCOQ", EvKey (KFun 2) []),
    ("\ESCOR", EvKey (KFun 3) []),
    ("\ESCOS", EvKey (KFun 4) []),
    ("\ESC[15~", EvKey (KFun 5) []),
    ("\ESC[17~", EvKey (KFun 6) []),
    ("\ESC[18~", EvKey (KFun 7) []),
    ("\ESC[19~", EvKey (KFun 8) []),
    ("\ESC[20~", EvKey (KFun 9) []),
    ("\ESC[21~", EvKey (KFun 10) []),
    ("\ESC[23~", EvKey (KFun 11) []), -- in default WT this is get intercepted by "toggle fullscreen"
    ("\ESC[24~", EvKey (KFun 12) [])
  ]
  <> (merger <$> conhostModifiers <*> conhostFnBase)

-- We have both Meta and Alt in the modifier
-- had to pick one, went with MEta for consistency
conhostModifiers :: [(String, [Modifier])]
conhostModifiers =
  [ (";2", [MShift]),
    (";3", [MMeta]),
    (";4", [MMeta, MShift]),
    (";5", [MCtrl]),
    (";6", [MCtrl, MShift]),
    (";7", [MMeta, MCtrl]),
    (";8", [MMeta, MCtrl, MShift])
    -- I do not know whether ";1" is used
  ]

conhostFnBase :: [((String, String), Int)]
conhostFnBase =
  [ (("\ESC[1","P"),  1),
    (("\ESC[1","Q"),  2),
    (("\ESC[1","R"),  3),
    (("\ESC[1","S"),  4),
    (("\ESC[15","~"), 5),
    (("\ESC[17","~"), 6),
    (("\ESC[18","~"), 7),
    (("\ESC[19","~"), 8),
    (("\ESC[20","~"), 9),
    (("\ESC[21","~"), 10),
    (("\ESC[23","~"), 11),
    (("\ESC[24","~"), 12)
  ]

merger :: (String, [Modifier]) -> ((String, String), Int) -> (String, Event)
merger (infx, mods) ((prefix, suffix), btn) = (prefix <> infx <> suffix, EvKey (KFun btn) mods)
