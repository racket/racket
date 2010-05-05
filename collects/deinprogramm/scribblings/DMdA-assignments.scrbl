#lang scribble/doc
@(require scribblings/htdp-langs/common
	  "std-grammar.ss"
	  "prim-ops.ss"
          (for-label deinprogramm/DMdA-assignments (only-in deinprogramm/DMdA-beginner define-record-procedures-parametric)))

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

@prim-ops['(lib "DMdA-assignments.ss" "deinprogramm") #'here]

@section{@scheme[define-record-procedures-2]}

@defform[(define-record-procedures-2 t c p (field-spec ...))]{
Die @scheme[define-record-procedures-2]-Form ist eine Definition für
einen neuen Record-Typ. Dabei ist @scheme[t] der Name des Record-Vertrags,
@scheme[c] der Name des Konstruktors, @scheme[p] der Name des
Prädikats. Jedes @scheme[field-spec] kann entweder der Name eines Selektors
oder ein Paar @scheme[(id id)] aus dem Namen eines Selektors und dem Namen eines
Mutators sein.
}

@section{@scheme[define-record-procedures-parametric-2]}

@defform[(define-record-procedures-parametric-2 t cc c p (field-spec1 ...))]{
Diese Form ist wie @scheme[define-record-procedures-2], nur parametrisch 
wie @scheme[define-record-procedures-parametric].  Außerdem
werden die Verträge für die Feldinhalte, anders als bei
@scheme[define-record-procedures-parametric], sofort bei der
Konstruktion überprüft und nicht erst beim Aufruf eines Selektors.
}

@section{@scheme[set!]}

@defform[(set! id expr)]{
Ein @scheme[set!]-Ausdruck ist eine Zuweisung, und ändert den Inhalt
der Zelle, die an @scheme[id] gebunden ist, auf den Wert von @scheme[expr].
}

@section[#:tag "assignments-prim-op"]{Primitive Operationen}

@prim-op-defns['(lib "DMdA-assignments.ss" "deinprogramm") #'here '()]
