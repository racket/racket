#lang scribble/doc
@(require scribblings/htdp-langs/common
	  "std-grammar.ss"
	  "prim-ops.ss"
          (for-label deinprogramm/DMdA-vanilla
	             (only-in deinprogramm/DMdA-beginner define)))

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

@section{@scheme[let], @scheme[letrec] und @scheme[let*]}

@defform[(let ((id expr) ...) expr)]{

Bei einem @scheme[let]-Ausdruck werden zunächst die @scheme[expr]s aus
den @scheme[(id expr)]-Paaren ausgewertet. Ihre Werte werden dann im
Rumpf-@scheme[expr] für die Namen @scheme[id] eingesetzt. Dabei können
sich die Ausdrücke nicht auf die Namen beziehen.

@schemeblock[
(define a 3)
(let ((a 16)
      (b a))
  (+ b a))
=> 19]

Das Vorkommen von @scheme[a] in der Bindung von @scheme[b] bezieht
sich also auf das @scheme[a] aus der Definition, nicht das @scheme[a]
aus dem @scheme[let]-Ausdruck.
}

@defform[(letrec ((id expr) ...) expr)]{
Ein @scheme[letrec]-Ausdruck ist
ähnlich zum entsprechenden @scheme[let]-Ausdruck, mit dem Unterschied, daß sich
die @scheme[expr]s aus den Bindungen auf die gebundenen Namen beziehen
dürfen.}

@defform[(let* ((id expr) ...) expr)]{
Ein @scheme[let*]-Ausdruck ist ähnlich zum entsprechenden
@scheme[let]-Ausdruck, mit dem Unterschied, daß sich die @scheme[expr]s
aus den Bindungen auf die Namen beziehen dürfen, die jeweils vor dem
@scheme[expr] gebunden wurden. Beispiel:

@schemeblock[
(define a 3)
(let* ((a 16)
       (b a))
  (+ b a))
=> 32]

Das Vorkommen von @scheme[a] in der Bindung von @scheme[b] bezieht
sich also auf das @scheme[a] aus dem @scheme[let*]-Ausdruck, nicht das
@scheme[a] aus der globalen Definition.
}


