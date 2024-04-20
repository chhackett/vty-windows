`vty-windows` is a plugin for the vty terminal interface library. It provides 
support for vty on the Windows platform. vty-windows is supported on GHC versions
9.4.4 and up.

Install via `git` with:

```
git clone git://github.com/chhackett/vty-windows.git
```

Install via `cabal` with:

```
cabal install vty-windows
```

# Features

* Supports Windows 10 since build 1703 (Creators Update added support for most VT sequences).

* Automatically handles window resizes.

* Supports Unicode output.

* Automatically decodes keyboard keys into (key,[modifier]) tuples.

* Provides extensible input and output interfaces.

* Supports ANSI graphics modes (SGR as defined in `console_codes(4)`)
  with a type-safe interface and graceful fallback for terminals
  with limited or nonexistent support for such modes.

* Properly handles cleanup.

* Supports "normal" and "extended" (SGR) mouse modes as described at
  http://invisible-island.net/xterm/ctlseqs/ctlseqs.html#h2-Mouse-Tracking

* Supports bracketed paste mode as described at
  http://cirw.in/blog/bracketed-paste

* Supports multi-column Unicode characters such as emoji characters. In
  cases where Vty and your terminal emulator disagree on character
  widths, Vty provides a tool `vty-build-width-table` and library
  functionality to build a width table that will work for your terminal
  and load it on application startup.

# Platform Support

`vty-windows` has been tested and all features are supported in the following terminals:
* Command Prompt
* Powershell
* MSYS/MSYS2
* ConEmu
Other terminals have not been tested at this time. Bug reports are welcome!

# Notes
Developers that want to develop cross platform applications using the vty
library should directly depend on [vty-crossplatform]
(https://github.com/jtdaugherty/vty-crossplatform), and not this library.

If you wish to only target the Windows platform, then using vty-windows would
be the correct approach.

# Contributing

If you decide to contribute, that's great! Here are some guidelines you
should consider to make submitting patches easier for all concerned:

 - Please ensure that the examples and test suites build along with the
   library.
 - If you make changes, make them consistent with the syntactic
   conventions already used in the codebase.
 - Please provide Haddock documentation for any changes you make.

# Known Issues

In Powershell, foreground and background colors may render incorrectly.

# Further Reading

Good sources of documentation for terminal programming are:

* https://github.com/b4winckler/vim/blob/master/src/term.c
* http://invisible-island.net/xterm/ctlseqs/ctlseqs.html
* http://ulisse.elettra.trieste.it/services/doc/serial/config.html
* http://www.leonerd.org.uk/hacks/hints/xterm-8bit.html
* http://vt100.net/docs/vt100-ug/chapter3.html