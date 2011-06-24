#lang scribble/doc
@(require scribblings/htdp-langs/common "std-grammar.rkt" "prim-ops.rkt"
          (for-label deinprogramm/DMdA-assignments
                     (only-in deinprogramm/DMdA-beginner
                              define-record-procedures-parametric)))

@title[#:style 'toc #:tag "DMdA-assignments"]{Die Macht der Abstraktion mit Zuweisungen}

This is documentation for the language level @italic{Die Macht der
Abstraktion mit Zuweisungen} to go with the German textbook
@italic{Die Macht der Abstraktion}.

@declare-exporting[deinprogramm/DMdA-assignments]

@racketgrammar*-DMdA[
#:literals (define-record-procedures-2 define-record-procedures-parametric-2 set!)
(
  (define-record-procedures-2 id id id (field-spec ...))
  (define-record-procedures-parametric-2 id id id id id (field-spec ...))
)
(
  [field-spec id (id id)]
)
(
  (set! id expr)
)
]

@|prim-nonterms|

@prim-ops['(lib "DMdA-assignments.rkt" "deinprogramm") #'here]

@section{@racket[define-record-procedures-2]}

@declare-exporting[deinprogramm/DMdA]

@defform[(define-record-procedures-2 t c p (field-spec ...))]{
Die @racket[define-record-procedures-2]-Form ist eine Definition für
einen neuen Record-Typ. Dabei ist @racket[t] der Name der Record-Signatur,
@racket[c] der Name des Konstruktors, @racket[p] der Name des
Prädikats. Jedes @racket[field-spec] kann entweder der Name eines Selektors
oder ein Paar @racket[(id id)] aus dem Namen eines Selektors und dem Namen eines
Mutators sein.
}

@section{@racket[define-record-procedures-parametric-2]}

@declare-exporting[deinprogramm/DMdA]

@defform[(define-record-procedures-parametric-2 t cc c p (field-spec1 ...))]{
Diese Form ist wie @racket[define-record-procedures-2], nur parametrisch 
wie @racket[define-record-procedures-parametric].  Außerdem
werden die Signaturen für die Feldinhalte, anders als bei
@racket[define-record-procedures-parametric], sofort bei der
Konstruktion überprüft und nicht erst beim Aufruf eines Selektors.
}

@section{@racket[begin]}

@declare-exporting[deinprogramm/DMdA]

@defform[(begin expr expr ...)]{
Bei der Auswertung eines @racket[begin]-Ausdrucks werden nacheinander
die Operanden ausgewertet. Der Wert des letzten Ausdrucks wird der
Wert des @racket[begin]-Ausdrucks.
}

@section{@racket[set!]}

@declare-exporting[deinprogramm/DMdA]

@defform[(set! id expr)]{
Ein @racket[set!]-Ausdruck ist eine Zuweisung, und ändert den Inhalt
der Zelle, die an @racket[id] gebunden ist, auf den Wert von @racket[expr].
}

@section[#:tag "assignments-signatures"]{Signaturen}

@declare-exporting[deinprogramm/DMdA]

@defidform[unspecific]{
Signatur für unspezifische Werte, die unwichtig sind - typischerweise für die 
Rückgabewerte von Operationen, die nur Seiteneffekte haben wie @racket[set!]
oder @racket[write-string].
}

@section[#:tag "advanced-definitions"]{Definitionen}
@declare-exporting[deinprogramm/DMdA-deflam]

@defform[(define id expr)]{Diese Form ist wie in den unteren
Sprachebenen, mit dem Unterschied, dass an @racket[id] mit
@racket[set!] zugewiesen werden kann.}

@section[#:tag "advanced-lambda"]{@racket[lambda]}
@declare-exporting[deinprogramm/DMdA-deflam]

@defform[(lambda (id id ... . id) expr)]{Bei @racket[lambda] ist in
dieser Sprachebene in einer Form zulässig, die es erlaubt, eine
Prozedur mit einer variablen Anzahl von Paramern zu erzeugen: Alle
Parameter vor dem Punkt funktionieren wie gewohnt und werden jeweils
an die entsprechenden Argumente gebunden.  Alle restlichen Argumente
werden in eine Liste verpackt und an den Parameter nach dem Punkt
gebunden.}


@section[#:tag "assignments-prim-op"]{Primitive Operationen}

@prim-op-defns['(lib "DMdA-assignments.rkt" "deinprogramm") #'here '()]
