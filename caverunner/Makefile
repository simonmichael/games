PROG=caverunner.hs

# keep synced with script's header
PACKAGES=\
  --package ansi-terminal-game\
  --package ansi-terminal\
  --package containers\
  --package directory\
  --package filepath\
  --package linebreak\
  --package process\
  --package safe\
  --package silently\
  --package timers-tick\
  --package unidecode\

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
