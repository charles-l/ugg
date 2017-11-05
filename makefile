watch:
	ls game.scm | entr -r sh -c 'echo "(r)" | nc localhost 1234'
