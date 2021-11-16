PROG=caverunner.hs

ghci:
	stack ghci

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
