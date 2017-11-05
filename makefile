all: game.o
	csc -c runner.scm
	csc game.o runner.o -o game

game.o: game.scm
	csc -c -j game game.scm

watch:
	ls game.scm | entr -r sh -c 'echo "(r)" | nc localhost 1234'
