# ski

Basic terminal version ([ski1.hs](ski1.hs)):

![screencast](ski1.gif)

ansi-terminal-game version ([ski2.hs](ski2.hs)):

![screencast](ski2.gif)

## Some problems and solutions

How hard is it to make classic terminal/console games in Haskell ?
Pretty hard to figure out a good setup. 
Pretty easy after that.
These notes aim to help.

### Packaging

- minimising complex packaging boilerplate & complex/unreliable install instructions:\
  use a [stack script with `script` command](https://docs.haskellstack.org/en/stable/GUIDE/#script-interpreter)

- getting stack script options just right:\
  specify all extra packages with `--package` options in stack options line,
  including any deps not in stackage,
  and remember stack options must be all on one line 
  (follow-on lines will be silently ignored)

- avoiding apparent hang when ghc/packages are installed on first run:\
  add `--verbosity=info` to stack options line to show progress output
  (but this also shows unwanted output on every run)

- avoiding recompilation delay on every run:\
  use `--compile` in stack options line

- running stack script in threaded mode for ansi-terminal-game:\
  add `--ghc-options=-threaded` to stack options line

### Tools

- getting haskell-language-server to see extra packages, eg for editing in VS Code:\
  add this `hie.yaml` in the same directory:
  ```
  cradle:
    stack:
  ```
  and `stack install` each extra package in stack's global project.\
  (Still flaky..)

### Functionality

- drawing anywhere on the terminal, portably:\
  use [ansi-terminal](https://hackage.haskell.org/package/ansi-terminal)

- getting non-blocking input, a ready-made event loop, and more powerful drawing, portably:\
  use [ansi-terminal-game](https://hackage.haskell.org/package/ansi-terminal-game)

- getting more powerful input/drawing/event loop, if you don't care about Windows:\
  use [vty](https://hackage.haskell.org/package/vty) and [brick](https://hackage.haskell.org/package/brick)

- getting arrow key and modifier key inputs:\
  use vty

