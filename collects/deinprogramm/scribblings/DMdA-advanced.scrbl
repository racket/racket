#lang scribble/doc
@(require scribblings/htdp-langs/common "std-grammar.rkt" "prim-ops.rkt"
          (for-label deinprogramm/DMdA-assignments))

@title[#:style 'toc #:tag "DMdA-advanced"]{Die Macht der Abstraktion fortgeschritten}

This is documentation for the language level @italic{Die Macht der
Abstraktion - fortgeschritten} that goes with the German textbook
@italic{Die Macht der Abstraktion}.

@declare-exporting[deinprogramm/DMdA-advanced]

@racketgrammar*-DMdA[
#:literals (define-record-procedures-2 set!)
(
  (define-record-procedures-2 id id id (field-spec ...))
  (define-record-procedures-parametric-2 id id id id (field-spec ...))
)
(
  [field-spec id (id id)]
  [quoted id
          number
          string
          character
          (quoted ...)
          @#,elem{@racketvalfont{'}@racket[quoted]}]
)
(
  (set! id expr)
  (code:line @#,elem{@racketvalfont{'}@racket[quoted]} (code:comment @#,seclink["advanced-quote"]{Quote-Literal}))
)
]

@|prim-nonterms|

@prim-ops['(lib "DMdA-advanced.rkt" "deinprogramm") #'here]

@section[#:tag "advanced-quote"]{Quote-Literal}

@deftogether[(
@defform/none[(unsyntax @elem{@racketvalfont{'}@racket[quoted]})]
@defform[(quote quoted)]
)]{
Der Wert eines Quote-Literals hat die gleiche externe Repräsentation wie @racket[quoted].
}

@section[#:tag "advanced-signatures"]{Signaturen}

@defidform[symbol]{
Signatur für Symbole.
}

@section[#:tag "advanced-prim-op"]{Primitive Operationen}

@prim-op-defns['(lib "DMdA-advanced.rkt" "deinprogramm") #'here '()]
