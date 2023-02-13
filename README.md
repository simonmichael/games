Small games, experiments and game dev notes.

## Games and experiments

✅ = confirmed installable on target platforms as of release date
🕹 = a playable game
🔔 = sound

|     |                                                                                             |              |                             |
|-----|---------------------------------------------------------------------------------------------|--------------|-----------------------------|
| ✅🕹 | [tiny-games-hs](https://github.com/haskell-game/tiny-games-hs)                                           | 2023         | haskell |
| ✅🕹🔔 | [caverunner](caverunner)                                                                    | 2021         | haskell, ansi-terminal-game |
| &#x3000;&#x3000;🔔  | [breakout](https://github.com/simonmichael/breakout)                                        | 2007, 2020   | haskell, sdl                |
| ✅  | [metapong](https://github.com/simonmichael/metapong)                                        | 2019         | haskell, ansi-terminal-game |
| ✅  | [symon](https://github.com/simonmichael/symon)                                              | 2016         | haskell, ansi-terminal      |
| ✅🕹 | [guess-the-number](https://hub.darcs.net/simon/guess-the-number/browse/guess-the-number.hs) | 2013         | haskell, cli                |
|     | [hssdl-osx-template](https://hub.darcs.net/simon/hssdl-osx-template)                        | 2013         | haskell, sdl                |
|     | [hssdl-mac-example](https://github.com/simonmichael/hssdl-mac-example)                      | 2011         | haskell, sdl                |
|     | [animtest-hs](https://hub.darcs.net/simon/animtest-hs)                                      | 2010         | haskell, sdl, opengl        |
|     | [wallofdoom](https://hub.darcs.net/simon/wallofdoom)                                        | 2009         | python, pygame              |
|     | [rocks](https://hub.darcs.net/simon/rocks)                                                  | 2005         | IO                          |
| &#x3000;&#x3000;🔔 | [frozen-bubble-py](https://hub.darcs.net/simon/frozen-bubble-py)                            | 2002         | python, pygame              |
| ✅🕹 | [FunGEn game engine & example games](https://github.com/haskell-game/fungen) (maintenance)  | 2008-present | haskell, opengl             |

## Haskell game development

### See also

- [#haskell-game matrix](https://matrix.to/#/#haskell-game:matrix.org) / [IRC](https://web.libera.chat/#haskell-game) chat (also Discord, Telegram chats)
- https://haskell-game.dev
- http://www.reddit.com/r/haskellgamedev
- http://www.haskell.org/haskellwiki/Game_Development
- http://hackage.haskell.org/packages/#cat:Game
- https://github.com/haskell-game

### How hard is it to make games in Haskell ?

Pretty hard to figure out a good setup; relatively easy and pleasant after that,
for non-resource-intensive games at least.
These notes and examples aim to help.

### Some problems and solutions

(as of 2021-09)

#### Packaging/installation

**Minimising packaging boilerplate and complex/unreliable install instructions**\
Use a [stack script with `script` command](https://docs.haskellstack.org/en/stable/GUIDE/#script-interpreter).

**Getting stack script header just right**

- Specify all extra packages (packages other than `base` that you import from)
  with `--package` options in the stack script header.
  If you forget any, the script may run for you but fail for others.

- If they depend on packages not in stackage, you must also mention each of those
  (the error message will list them.)

- Use a block comment (`{- -}`) so that you can write packages and options on multiple lines.
  (A line comment (`--`) will work only for one line.)

**Avoiding apparent hang when ghc is installed on first run**\
Add `--verbosity=info` to stack options line to show ghc install progress
(but this also shows unwanted "resolver" output on every run.
`--verbosity=warning` hides that, but still shows package install progress.
`--verbosity=error` hides that too.)

**Avoiding recompilation delay on every run**\
Use `script --compile` in stack options line
(or `script --optimize`, which might take longer and/or run faster).

**Running stack script in threaded mode when using packages that require this**\
(ansi-terminal-game, etc.):  add `--ghc-options=-threaded` to stack options line

**Providing ready-to-run binaries that don't require the user to have `stack` or other haskell tools**

- set up Github CI workflows to build binary artifacts on the main platforms 
  (and ideally, statically link the GNU/Linux binary);
  add those to a Github release for stable download url.

- mac: get your app packaged in homebrew

- etc.

**Providing screenshots/screencasts**\
use a convenient screenshot tool on your platform (eg CMD-SHIFT-5 and friends on mac);
or install and use asciinema to record .cast files,
asciicast2gif or similar to convert those to animated GIF images;
add the images to a README.md.

#### Tools

**Getting haskell-language-server to see extra packages, eg for editing in VS Code**\
add this `hie.yaml` in the same directory:
```
cradle:
  stack:
```
and `stack install` each extra package in stack's global project.\
(Still flaky..)

**Configure GHC build options, eg to silence unwanted warnings**\
Add lines above your imports like\
`{-# OPTIONS_GHC -Wno-missing-signatures -Wno-unused-imports #-}`

**Enabling/disabling GHC's optional Haskell language extensions**\
Add lines above your imports like\
`{-# LANGUAGE MultiWayIf, RecordWildCards #-}`

#### Terminal/console games

**Drawing anywhere on the terminal, portably**\
use [ansi-terminal](https://hackage.haskell.org/package/ansi-terminal)

**Getting non-blocking input, a ready-made event loop, and more powerful drawing, portably**\
use [ansi-terminal-game](https://hackage.haskell.org/package/ansi-terminal-game)

**Getting more powerful input/drawing/event loop, if you don't care about Windows**\
use [vty](https://hackage.haskell.org/package/vty) and [brick](https://hackage.haskell.org/package/brick)

**Getting arrow key and modifier key inputs**\
use vty
