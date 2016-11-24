all: lisp run clean

lisp:
	ghc -package parsec -o repl Main.hs

run:  repl
	./repl

clean:
	rm repl *.o *.hi

