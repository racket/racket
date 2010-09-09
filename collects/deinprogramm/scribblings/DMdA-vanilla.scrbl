#lang scribble/doc
@(require scribblings/htdp-langs/common
	  "std-grammar.ss"
	  "prim-ops.ss"
          (for-label deinprogramm/DMdA-vanilla))

@title[#:style 'toc #:tag "DMdA-vanilla"]{Die Macht der Abstraktion}

This is documentation for the language level @italic{Die Macht der
Abstraktion} to go with the German textbook @italic{Die Macht der
Abstraktion}.

@declare-exporting[deinprogramm/DMdA-vanilla]

@schemegrammar*-DMdA[
#:literals ()
() () ()
]

@|prim-nonterms|

@prim-ops['(lib "DMdA-vanilla.ss" "deinprogramm") #'here]

@section[#:tag "vanilla-prim-op"]{Primitive Operationen}

@prim-op-defns['(lib "DMdA-vanilla.ss" "deinprogramm") #'here '()]

@section[#:tag "signatures-vanilla"]{Signaturen}

@subsection{@scheme[list-of]} 
@defform[(list-of sig)]{
Diese Signatur ist dann für einen Wert gültig, wenn dieser eine Liste ist,
für dessen Elemente @scheme[sig] gültig ist.
}

