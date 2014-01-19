#!/usr/bin/perl
# Warning: this version does not do any error checking!
push@g,split'',$_ while<>;for(0..$#g){push@s,$_ if(@g[$_]eq"["
);$a=pop@s,$l{$a}=$_,$l{$_}=$a if$g[$_]eq"]"}%c=(62=>'$p++',60
=>'$p--',43=>'$a[$p]++',45=>'$a[$p]--',46=>'print chr$a[$p];',
44=>'$a[$p]=ord(getc)',91=>'$i=$l{$i}if!$a[$p]',93=>'$i=$l{$i'
.'}if$a[$p];');use bytes;eval$c{ord$g[$i]},$i++while($i<~~@g);
