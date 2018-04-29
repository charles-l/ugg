RACKET_FILES=$(shell find . -name "*.rkt")
all: compiled_racket_files g.so

g.so: g.cc
	clang++ -g -fPIC -shared g.cc -o g.so -lGL -lSDL2 -lGLEW

compiled_racket_files: $(RACKET_FILES)
	raco make $(RACKET_FILES)

.PHONY: compiled_racket_files
