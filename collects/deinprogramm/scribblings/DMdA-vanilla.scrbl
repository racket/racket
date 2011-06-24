#lang scribble/doc
@(require scribblings/htdp-langs/common "std-grammar.rkt" "prim-ops.rkt"
          (for-label deinprogramm/DMdA-vanilla
                     (only-in deinprogramm/DMdA-beginner define)))

@title[#:style 'toc #:tag "DMdA-vanilla"]{Die Macht der Abstraktion}

This is documentation for the language level @italic{Die Macht der
Abstraktion} to go with the German textbook @italic{Die Macht der
Abstraktion}.

@declare-exporting[deinprogramm/DMdA-vanilla #:use-sources (deinprogramm/DMdA)]

@racketgrammar*-DMdA[
#:literals ()
() () ()
]

@|prim-nonterms|

@prim-ops['(lib "DMdA-vanilla.rkt" "deinprogramm") #'here]

@section[#:tag "signatures-vanilla"]{Signaturen}

@subsection{@racket[list-of]} 
@defform[(list-of sig)]{
Diese Signatur ist dann für einen Wert gültig, wenn dieser eine Liste ist,
für dessen Elemente @racket[sig] gültig ist.
}

@section{@racket[let], @racket[letrec] und @racket[let*]}

@defform[(let ((id expr) ...) expr)]{

Bei einem @racket[let]-Ausdruck werden zunächst die @racket[expr]s aus
den @racket[(id expr)]-Paaren ausgewertet. Ihre Werte werden dann im
Rumpf-@racket[expr] für die Namen @racket[id] eingesetzt. Dabei können
sich die Ausdrücke nicht auf die Namen beziehen.

@racketblock[
(define a 3)
(let ((a 16)
      (b a))
  (+ b a))
=> 19]

Das Vorkommen von @racket[a] in der Bindung von @racket[b] bezieht
sich also auf das @racket[a] aus der Definition, nicht das @racket[a]
aus dem @racket[let]-Ausdruck.
}

@defform[(letrec ((id expr) ...) expr)]{
Ein @racket[letrec]-Ausdruck ist
ähnlich zum entsprechenden @racket[let]-Ausdruck, mit dem Unterschied, daß sich
die @racket[expr]s aus den Bindungen auf die gebundenen Namen beziehen
dürfen.}

@defform[(let* ((id expr) ...) expr)]{
Ein @racket[let*]-Ausdruck ist ähnlich zum entsprechenden
@racket[let]-Ausdruck, mit dem Unterschied, daß sich die @racket[expr]s
aus den Bindungen auf die Namen beziehen dürfen, die jeweils vor dem
@racket[expr] gebunden wurden. Beispiel:

@racketblock[
(define a 3)
(let* ((a 16)
       (b a))
  (+ b a))
=> 32]

Das Vorkommen von @racket[a] in der Bindung von @racket[b] bezieht
sich also auf das @racket[a] aus dem @racket[let*]-Ausdruck, nicht das
@racket[a] aus der globalen Definition.
}

@section[#:tag "vanilla-prim-op"]{Primitive Operationen}

@prim-op-defns['(lib "DMdA-vanilla.rkt" "deinprogramm") #'here '()]
