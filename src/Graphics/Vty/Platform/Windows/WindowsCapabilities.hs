-- | This module provides functions to obtain VT escape sequences for the Windows platform
module Graphics.Vty.Platform.Windows.WindowsCapabilities
    ( getStringCapability,
      getIntCapability
    ) where

import qualified Data.Map as M

-- | Lookup for terminal capabilities that have a string value.
-- All Windows supported escape sequences are described here:
-- https://learn.microsoft.com/en-us/windows/console/console-virtual-terminal-sequences
getStringCapability :: String -> Maybe String
getStringCapability cap = M.lookup cap windowsStringCaps

-- | Lookup a terminal capability that has an integer value
getIntCapability :: String -> Maybe Int
getIntCapability cap = M.lookup cap windowsIntCaps

esc :: String -> String
esc code = '\ESC' : code

csi :: String -> String
csi code = esc $ '[' : code

-- | Mapping of capability names to VT escape sequences for the Windows platform
-- All Windows supported escape sequences are described here:
-- https://learn.microsoft.com/en-us/windows/console/console-virtual-terminal-sequences
windowsStringCaps :: M.Map String String
windowsStringCaps = M.fromList
    [ ("bold", csi "1m")
    , ("clear", csi "H" ++ csi "J")
    , ("cup", csi "%i%p1%d;%p2%dH")
    , ("civis", csi "?25l")
    , ("cnorm", csi "34h" ++ csi "?25h")
    , ("dim", csi "2m")
    , ("ed", csi "J")
    , ("el", csi "K")
    -- , ("el1", csi "1K")
    , ("home", csi "H")
    , ("invis", csi "8m")
    , ("kbs", "^H")
    , ("rev", csi "7m")
    , ("rmso", csi "27m")
    , ("rmul", csi "m")
    , ("setab", csi "%?%p1%{8}%<%t4%p1%d%e%p1%{16}%<%t10%p1%{8}%-%d%e48;5;%p1%d%;m")
    , ("setaf", csi "%?%p1%{8}%<%t3%p1%d%e%p1%{16}%<%t9%p1%{8}%-%d%e38;5;%p1%d%;m")
    , ("sgr", csi "0%?%p6%t;1%;%?%p2%t;4%;%?%p1%p3%|%t;7%;%?%p4%t;5%;%?%p5%t;2%;%?%p7%t;8%;m%?%p9%t\SO%e\SI%;")
    , ("sgr0", csi "m\SI")
    , ("smso", csi "7m")
    , ("rmcup", csi "?1049l")
    , ("smcup", csi "?1049h")
    , ("el", csi "K")
    , ("sitm", csi "3m")
    , ("ritm", csi "23m")
    , ("smxx", csi "9m")
    , ("rmxx", csi "29m")
    , ("smul", csi "4m")
    , ("rmul", csi "24m")
    , ("rev", csi "7m")
    , ("dim", csi "2m")
    , ("bold", csi "1m")
    ]

windowsIntCaps :: M.Map String Int
windowsIntCaps = M.fromList 
    [ ("colors", 256)
    ]
