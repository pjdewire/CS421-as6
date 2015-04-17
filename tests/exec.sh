#!/bin/sh
/c/cs421/bin/sml <<POOP 2> /dev/null | egrep -v '\- |val it = |val use =
|Standard|\[(linking|library|loading|scanning)'
    CM.make "../sources.cm";
    Main.compile "$1.tig";
POOP

rm a.out;
gcc -m32 $1.tig.s ../runtime.c 2> /dev/null
