PROG=caverunner.hs

# keep synced with script's header
PACKAGES=\
  --package ansi-terminal\
  --package ansi-terminal-game\
  --package containers\
  --package directory\
  --package filepath\
  --package pretty-show\
  --package pretty-simple\
  --package safe\
  --package time\
  --package typed-process\

ghc:
	stack ghc $(PACKAGES) $(PROG) -- -threaded

ghci:
	stack ghci $(PACKAGES) $(PROG)

ghcid:
	ghcid -c 'make ghci'

loop:
	while true; do ./$(PROG); done

%.anim.gif: FORCE
	osascript \
		-e "tell app \"Terminal\" to do script \"cd $$PWD; asciinema rec $*.cast --overwrite -c ./$(PROG) -i1 -t '$* cast'; asciicast2gif -S1 $*.cast $@; open -a safari $@; \"" \
		-e "tell application \"Terminal\" to activate"

FORCE:

loc:
	cloc $(PROG)

