all: c go perl

c:
	@${CC} -o brainfuck-c brainfuck.c

go:
	@go build -o brainfuck-go brainfuck.go

perl:
	@cp brainfuck.pl brainfuck-perl
	@chmod a+x brainfuck-perl 
	@cp brainfuck_obfu.pl brainfuck-perl_obfu
	@chmod a+x brainfuck-perl_obfu
	@cp brainfuck_to_c.pl brainfuck-to-c
	@chmod a+x brainfuck-to-c

clean:
	@rm -f brainfuck-*
