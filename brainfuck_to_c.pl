#!/usr/bin/perl

# read input, stripping comments
($p .= $_) =~ s/[^\+\-\>\<\.\,\[\]]//g while (<>);

# perform RLE on optimizable characters
while (length $p) {
	$p =~ s/(.)(\1*)//;
	if ($1 ~~ ['+', '-', '<', '>']) {
		push @program, ($1, length ($1.$2));
	}
	else {
		push @program, ($1, 1) for (0..length $2);
	}
}

# write header
print "#include <stdio.h>\n\n";
print "int main() {\n\tunsigned char array[30000];\n\tunsigned char *pointer = array;\n\t";

# write main body
$indent = 1;
for ($i = 0; $i < ~~@program; $i += 2) {
	if    ($program[$i] eq '>') { print "pointer += $program[$i+1];"; }
	elsif ($program[$i] eq '<') { print "pointer -= $program[$i+1];"; }
	elsif ($program[$i] eq '+') { print "*pointer += $program[$i+1];"; }
	elsif ($program[$i] eq '-') { print "*pointer -= $program[$i+1];"; }
	elsif ($program[$i] eq '.') { print "putchar(*pointer);"; }
	elsif ($program[$i] eq ',') { print "*pointer = getchar();"; }
	elsif ($program[$i] eq '[') { print "while (*pointer) {"; }
	elsif ($program[$i] eq ']') { print "}"; }
	$indent++ if ($program[$i] eq '[');
	$indent-- if ($program[$i+2] eq ']');
	print "\n" . "\t"x$indent;
}
print "return 0;\n}\n";