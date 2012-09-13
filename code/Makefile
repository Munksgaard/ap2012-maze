HC=ghc
IFILE=Maze.hs
OFILE="maze"
FLAGS=-Wall

default: compile run
compile:
	ghc $(FLAGS) -o $(OFILE) $(IFILE)

run:
	./$(OFILE)

test:
	echo "This feature is not yet implemented"

.PHONY: clean

clean:
	rm -f $(OFILE)
