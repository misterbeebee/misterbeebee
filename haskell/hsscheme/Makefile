BUILD=build
BIN=$(BUILD)/bin
SRC=src
SOURCES=$(SRC)/Scheme.hs # This will be wrong when we add more files. how to glob?
OBJECTS=$(SOURCES:.hs=.o)
EXECUTABLE=hsscheme

# $@ is the current target
# $< is the input file

all: build 

build: $(SOURCES)
	echo "build"
	mkdir -p $(BIN)
	ghc --make -o $(BIN)/hsscheme -package parsec -fglasgow-exts src/Scheme.hs

clean:	
	echo "clean"
	rm -rf $(BUILD)
	rm -rf src/*.hi
	rm -rf src/*.o

test: build
	echo "test"
	$(BIN)/hsscheme " $$"
	$(BIN)/hsscheme "a"

