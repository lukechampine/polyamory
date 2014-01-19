all: c perl

c:
	@${CC} -o brainfuck-c brainfuck.c

perl:
	@cp brainfuck.pl brainfuck-perl
	@chmod a+x brainfuck-perl 
	@cp brainfuck_obfu.pl brainfuck-perl_obfu
	@chmod a+x brainfuck-perl_obfu

clean:
	@rm -f brainfuck-*
