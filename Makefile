all: c go haskell perl

c:
	@${CC} -o brainfuck-c brainfuck.c

go:
	@go build -o brainfuck-go brainfuck.go

haskell:
	@ghc -c -O brainfuck.hs
	@ghc brainfuck.o -o brainfuck-hs
	@rm -f brainfuck.hi brainfuck.o

perl:
	@cp brainfuck.pl brainfuck-pl
	@chmod a+x brainfuck-pl 
	@cp brainfuck_obfu.pl brainfuck-pl_obfu
	@chmod a+x brainfuck-pl_obfu
	@cp brainfuck_to_c.pl brainfuck-to-c
	@chmod a+x brainfuck-to-c

clean:
	@rm -f brainfuck-*
