# caverunner

A simple one-file terminal game in Haskell, built with ansi-terminal-game.

![screencast](caverunner.anim.gif)

In terminals or width 80 or more, the cave mouth is always 40 wide, making the cave, and scores, repeatable. 
Smaller windows (<80 wide) are still playable, but won't give the same cave.
Larger terminals will slow down the action. For competition play and to best feel the rush, 80x25 is recommended :)

With ansi-terminal-game you describe the whole screen each frame,
and it prints just the minimum changes to the terminal.
[caverunner-fast.anim.gif](caverunner-fast.anim.gif)
shows about the max speed in a 80x25 Terminal.app window on a m1 macbook.

[cave1.hs](old/cave1.hs), built with just print (putStrLn),
went faster ([cave1.anim.gif](old/cave1.anim.gif)),
but couldn't easily get user input.

