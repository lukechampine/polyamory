#!/usr/bin/perl
use bytes;
binmode(STDOUT, ":utf8");

# global variables
$progpointer = 0;
@array = ();
$pointer = 0;
%bracketlookup = ();

# command dictionary
%commands = (
	'>' => sub { $pointer++; },
	'<' => sub { $pointer--; die "Error: out of bounds\n" if $pointer < 0; },
	'+' => sub { $array[$pointer]++; },
	'-' => sub { $array[$pointer]--; },
	'.' => sub { print chr $array[$pointer]; },
	',' => sub { $c = getc; $array[$pointer] = ord $c if defined $c; },
	'[' => sub { $progpointer = $bracketlookup{$progpointer} if $array[$pointer] == 0; },
	']' => sub { $progpointer = $bracketlookup{$progpointer} if $array[$pointer] != 0; }
);

# read from stdin
push @program, (split '', $_) while (<>);

# build [ ] lookup table and check for unmatched brackets
my @stack;
for (0..$#program) {
	if (@program[$_] eq "[") {
		push @stack, $_;
	}
	elsif (@program[$_] eq "]") {
		die "Error: unmatched ]\n" unless ~~@stack;
		$a = pop @stack;
		$bracketlookup{$a} = $_;
		$bracketlookup{$_} = $a;
	}
}
die "Error: unmatched [\n" if ~~@stack;

# call each character's respective subroutine
while ($progpointer < ~~@program) {
	$com = $commands{@program[$progpointer]};
	\&$com() if $com;
	$progpointer++;
}
print "\n";