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
developer-hours ~100,
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

### Decisions/goals

- The style is inspired by 70s basic computer games and 80s home computer magazine games.
- The game is a terminal game, to limit cost and scope, increase chance of shipping, and invoke the creativity and fun of early games.
- The programming language is Haskell, because it's powerful, maintainable, and portable, 
  and has untapped potential for game dev and a small but enthusiastic game dev scene.

Installability, usability

- The game is implemented and shipped as a single easy-to-run source file, for simple setup, reading and hacking.
- The game is cross platform, and must work on at least the three big platforms.
- Installing/running should *just work* to the greatest extent possible.
- stack's script support is used to achieve this (a `stack` executable is the only requirement). Binary/web/ssh versions could be explored later.
- Pointers are provided for people who want to run the game without installing stack (using cabal).
- The game does not require a network connection to play (once installed).

Gameplay

- The game should be quick and fun to play.
- The mechanics should (initially) be as simple as possible while still being fun.
- Levels, scores, progress etc. should be stable and comparable between players.

Sound

- The game should use sound if possible, without complicating installation.
- Command-line `sox` is used, as the easiest cross-platform way to achieve this. OpenAL will be explored later.

Persistence

- Persistent state is part of the gameplay and must be reliable.
- Persistent state is extracted from an append-only event log, for robustness and evolvability.


### Roadmap/Wishlist

- show crash sites
- events/state cleanup
- cave-specific colours ?
- light/dark schemes ? cf Terminal.app silver aerogel
- version numbers
- show actual frame rate
- better run/install docs
- beta test
- test sound latency on other machines, windows
- 1.0 release
- multiple named saves ?
- attract mode / in-game high score table
- high score sharing / server
- 3 lives ?
- more interesting caves
- freeform user created caves
- sideways acceleration ?
- braking thrusters, with limited fuel
  - activated by a modifier key when those are available
  - otherwise by toggling left/right ?
- auto speedup/slowdown in next game ?
- return flight to the top ?
- document vs code+hls setup, dev workflows
- sound
  - don't clip end of sounds, or muffle sounds < 100ms
  - thruster sound
  - quit sound
  - soxlib / sdl-mixer with graceful fallback ?
  - sound check mode (and video check if useful) to help with setup
  - continuous/dynamic length sounds (keep playing thruster sound until player stops)
  - wind noise, rising with speed (in caves with atmospheres)
