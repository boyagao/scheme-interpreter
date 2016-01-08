all: lisp run clean

lisp:
	ghc -package parsec -o repl repl.hs

run: repl
	./repl

clean:
	rm repl repl.hi repl.o
