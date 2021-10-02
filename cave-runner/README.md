# cave-runner

A simple one-file terminal game in Haskell.

Basic terminal version using just print (putStrLn): [cave1.hs](old/cave1.hs)

![screencast](old/cave1.anim.gif)

ansi-terminal-game version: [cave2.hs](cave2.hs)

![screencast](cave2.anim.gif)

Here is about the max speed in a 80x25 Terminal.app window on a m1 macbook.
With ansi-terminal-game you describe the whole screen each frame,
and it prints just the minimum changes to the terminal.

![screencast](cave2-fast.anim.gif)

A longer run:

https://asciinema.org/a/f1xjpZx6UuuNBJcs5nGiGiKWo
