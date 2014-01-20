#!/usr/bin/perl
# Warning: this version does not do any error checking!
use bytes;%c=qw(. print+chr$a[$p] + $a[$p]++ > $p++
[ $i=$l{$i}if!$a[$p] ] $i=$l{$i}if$a[$p] - $a[$p]--
, $a[$p]=ord(getc) < $p--);@g=split'',<>;for(0..$#g
){push @s,$_ if$g[$_]eq"[";$a=pop@s,$l{$a}=$_,$l{$_
}=$a if$g[$_]eq"]"}eval$c{$g[$i]},$i++while($i<$#g)