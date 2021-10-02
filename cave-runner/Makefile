PROG=cave.hs

# keep synced with script's stack header
PACKAGES=--package random --package ansi-terminal-game --package linebreak --package timers-tick --package unidecode --package safe

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
