# keep synced with script's stack header
PACKAGES=--package random --package ansi-terminal-game --package linebreak --package timers-tick --package unidecode

ghci:
	stack ghci $(PACKAGES) ski.hs

ghcid:
	ghcid -c 'make ghci'
