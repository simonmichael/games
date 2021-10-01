# ski

A simple one-file terminal game in Haskell.

Basic terminal version using just print (putStrLn): [ski1.hs](ski1.hs)

![screencast](ski1.anim.gif)

ansi-terminal-game version: [ski2.hs](ski2.hs)

![screencast](ski2.anim.gif)

Here is about the max speed in a 80x25 Terminal.app window on a m1 macbook.
With ansi-terminal-game you describe the whole screen each frame,
and it prints just the minimum changes to the terminal.

![screencast](ski2-fast.anim.gif)

A longer run:

https://asciinema.org/a/f1xjpZx6UuuNBJcs5nGiGiKWo
