all:
	clang++ -g -fPIC -shared g.cc -o g.so -lGL -lSDL2 -lGLEW

