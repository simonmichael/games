PROG=caverunner.hs

# keep synced with script's header
PACKAGES=\
  --package ansi-terminal\
  --package ansi-terminal-game\
  --package containers\
  --package directory\
  --package extra\
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

pkg:
	stack new $@ pkg.hsfiles -p "name:caverunner" -p "version:0.98" \
	&& cd $@ \
	&& ln -sf ../../caverunner.hs src/Main.hs \
	&& ln -sf ../README.md \
	&& ln -sf ../LICENSE \

pkg-rebuild:
	ls $(PROG) pkg.hsfiles | entr -r bash -c 'rm -rf pkg && make pkg && (cd pkg; stack build)'
