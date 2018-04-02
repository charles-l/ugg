g.so: g.cc
	g++ -g -fPIC -shared g.cc -o g.so -lGL -lSDL2 -lGLEW

