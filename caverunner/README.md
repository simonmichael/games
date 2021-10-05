```
  _________ __   _____  _______  ______  ____  ___  _____
 / ___/ __ `/ | / / _ \/ ___/ / / / __ \/ __ \/ _ \/ ___/
/ /__/ /_/ /| |/ /  __/ /  / /_/ / / / / / / /  __/ /
\___/\__,_/ |___/\___/_/   \__,_/_/ /_/_/ /_/\___/_/
```

A simple one-file cross-platform terminal game in Haskell, built with ansi-terminal-game and stack.

![screencast](caverunner.anim.gif)

## Install

This game is shipped as a single-file script that will run relatively reliably
if you have [stack](https://www.fpcomplete.com/haskell/get-started) in
your PATH. On first run it may seem to hang (perhaps for minutes) if
it needs to download GHC (change its header to --verbosity=info if you
need to see progress).

You can also use cabal and/or your system package manager to install
the haskell packages mentioned in the header, and a suitable GHC
version (eg 8.10), then compile the script.

See also `./caverunner.hs --help`.

To enable sound effects, install [sox](https://sox.sourceforge.net) in PATH:
do `apt install sox`, `brew install sox`, `choco install sox.portable` or similar.

## Project status

Installable/playable game,
occasional development/maintenance,
help welcome,
last notable updates 2021,
developer-hours ~40,
discussion/support [#haskell-game on matrix](https://matrix.to/#/#haskell-game:matrix.org) or [IRC](https://web.libera.chat/#haskell-game).

## Dev notes

This is a one-file stack script, not a cabal project.
See the parent directory for more notes about this setup.

With ansi-terminal-game you describe the whole screen each frame,
and it prints just the minimum changes to the terminal.
[caverunner-fast.anim.gif](caverunner-fast.anim.gif)
shows about the max speed in a 80x25 Terminal.app window on a m1 macbook.

[cave1.hs](old/cave1.hs), built with just print (putStrLn),
went faster ([cave1.anim.gif](old/cave1.anim.gif)),
but couldn't easily get user input.


## Roadmap/Wishlist

- show crash sites
- sound effects
  - game start
  - depth cues
  - crash
  - thrusters
  - high score
  - quit
  - wind noise (in caves with atmospheres, rising with speed)
  - "near miss"
- more use of colour
- game mechanics / increase fun
  - something rewarding at the bottom
  - braking thrusters, with limited fuel
    - activated by a modifier key when those are available
    - otherwise by toggling left/right ?
  - auto speedup/slowdown in next game ?
  - return flight to the top ?
- attract mode
  - high scores
- 1.0 release
- document vs code+hls setup, dev workflows
- more colour
- more varied caves and flight mechanics
- high score sharing / server
- sound check mode (and video check if useful) to help with setup
- try soxlib with graceful fallback
- try sdl-mixer with graceful fallback
