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

@schemegrammar*-DMdA[
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

@section{@scheme[define-record-procedures-2]}

@declare-exporting[deinprogramm/DMdA]

@defform[(define-record-procedures-2 t c p (field-spec ...))]{
Die @scheme[define-record-procedures-2]-Form ist eine Definition für
einen neuen Record-Typ. Dabei ist @scheme[t] der Name der Record-Signatur,
@scheme[c] der Name des Konstruktors, @scheme[p] der Name des
Prädikats. Jedes @scheme[field-spec] kann entweder der Name eines Selektors
oder ein Paar @scheme[(id id)] aus dem Namen eines Selektors und dem Namen eines
Mutators sein.
}

@section{@scheme[define-record-procedures-parametric-2]}

@declare-exporting[deinprogramm/DMdA]

@defform[(define-record-procedures-parametric-2 t cc c p (field-spec1 ...))]{
Diese Form ist wie @scheme[define-record-procedures-2], nur parametrisch 
wie @scheme[define-record-procedures-parametric].  Außerdem
werden die Signaturen für die Feldinhalte, anders als bei
@scheme[define-record-procedures-parametric], sofort bei der
Konstruktion überprüft und nicht erst beim Aufruf eines Selektors.
}

@section{@scheme[begin]}

@declare-exporting[deinprogramm/DMdA]

@defform[(begin expr expr ...)]{
Bei der Auswertung eines @scheme[begin]-Ausdrucks werden nacheinander
die Operanden ausgewertet. Der Wert des letzten Ausdrucks wird der
Wert des @scheme[begin]-Ausdrucks.
}

@section{@scheme[set!]}

@declare-exporting[deinprogramm/DMdA]

@defform[(set! id expr)]{
Ein @scheme[set!]-Ausdruck ist eine Zuweisung, und ändert den Inhalt
der Zelle, die an @scheme[id] gebunden ist, auf den Wert von @scheme[expr].
}

@section[#:tag "assignments-signatures"]{Signaturen}

@declare-exporting[deinprogramm/DMdA]

@defidform[unspecific]{
Signatur für unspezifische Werte, die unwichtig sind - typischerweise für die 
Rückgabewerte von Operationen, die nur Seiteneffekte haben wie @scheme[set!]
oder @scheme[write-string].
}

@section[#:tag "advanced-definitions"]{Definitionen}
@declare-exporting[deinprogramm/DMdA-deflam]

@defform[(define id expr)]{Diese Form ist wie in den unteren
Sprachebenen, mit dem Unterschied, dass an @scheme[id] mit
@scheme[set!] zugewiesen werden kann.}

@section[#:tag "advanced-lambda"]{@scheme[lambda]}
@declare-exporting[deinprogramm/DMdA-deflam]

@defform[(lambda (id id ... . id) expr)]{Bei @scheme[lambda] ist in
dieser Sprachebene in einer Form zulässig, die es erlaubt, eine
Prozedur mit einer variablen Anzahl von Paramern zu erzeugen: Alle
Parameter vor dem Punkt funktionieren wie gewohnt und werden jeweils
an die entsprechenden Argumente gebunden.  Alle restlichen Argumente
werden in eine Liste verpackt und an den Parameter nach dem Punkt
gebunden.}


@section[#:tag "assignments-prim-op"]{Primitive Operationen}

@prim-op-defns['(lib "DMdA-assignments.rkt" "deinprogramm") #'here '()]
