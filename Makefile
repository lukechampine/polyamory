all: c perl

c:
	@${CC} -o brainfuck-c brainfuck.c

perl:
	@cp brainfuck.pl brainfuck-perl
	@chmod a+x brainfuck-perl 

clean:
	@rm -f brainfuck-*
