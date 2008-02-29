#! /usr/bin/env mzscheme

#lang scheme/base

(require ffi/c-printf)

(c-printf-is-dangerous!) ; see last example below

(c-printf "|%4d| |%04d| |%-4d|\n" 12 34 56)
(c-printf "|%4d| |%04d| |%-4d|\n" "12" "34" "56")
(c-printf "Bye bye sanity:\n")
(c-printf "%s\n" 0)
(c-printf "%s\n" 1234)
