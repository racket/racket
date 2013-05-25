#lang scribble/doc
@(require scribble/manual scribble/bnf "common.rkt" (for-label racket/base))

@title[#:tag "demod"]{@exec{raco demod}: Demodularizing Programs}

The @exec{raco demod} command takes a racket module and flattens 
all of its dependencies into a single compiled module. 
A file @filepath{@nonterm{name}.rkt} is demodularized into 
@filepath{@nonterm{name}_rkt_merged.zo}.

The demodularized zo file can be run by passing it as an argument to
the racket command-line program.
