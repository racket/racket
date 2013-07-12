#lang scribble/manual
@(require racket/stxparam scribble/base scribble/eval
          unstable/scribblings/utils
          (for-syntax racket/base syntax/srcloc)
          (for-label racket/base racket/contract pict redex unstable/gui/redex))

@(define the-eval (make-base-eval))
@(the-eval '(require redex/reduction-semantics redex/pict unstable/gui/redex pict))

@unstable-title[#:tag "redex"]{Redex}
@unstable-header[]

@defmodule[unstable/gui/redex]

This library provides functions to help typesetting for
@racketmodname[redex] models. The following example program provides
an overview of the features:

@with-eval-preserve-source-locations[
@interaction[#:eval the-eval
(define-language ZF
  [e empty
     (Set e)
     (Union e_1 e_2)
     (Powerset e)
     ZZ
     variable-not-otherwise-mentioned]
  [formula (same? e_1 e_2)
           (in? e_1 e_2)
           true
           false
           (implies formula_1 formula_2)])
]]

By default, Redex models are typeset as S-expressions with some basic
styling that distinguishes literals from nonterminal names, handles
subscripting, etc.

@with-eval-preserve-source-locations[
@interaction[#:eval the-eval
(language->pict ZF)
(term->pict ZF (in? x (Set 1 2 3 ...)))
]]

This library provides helper functions for creating and using
rewriters that transform the S-expression model terms into other
notations.

@with-eval-preserve-source-locations[
@interaction[#:eval the-eval
(add-atomic-rewriters!
 'empty "∅"
 'formula "φ"
 'ZZ (text "Z" '(bold . modern) (default-font-size)) ;; "\u2124" doesn't work with latex/pdf output
 'variable-not-otherwise-mentioned
     (lambda () (text "x, y, z, ..." (literal-style) (default-font-size)))
 'true (lambda () (text "true" '(caps . modern) (default-font-size)))
 'false (lambda () (text "false" '(caps . modern) (default-font-size))))
(add-compound-rewriters!
 'same? (binary-rw " = ")
 'in? (binary-rw " ∈ ")
 'Set (bracket-rw 'curly)
 'Powerset (function-rw "P") ;; "𝒫 " doesn't work with latex/pdf output
 'Union (binary-rw "∪")
 'implies (binary-rw " ⇒ " #:parenthesize-left '(implies)))
(with-rewriters
  (lambda ()
    (language->pict ZF)))
(with-rewriters
  (lambda ()
    (render-term ZF (in? x (Set 1 2 3 ...)))))
]]

@; ----------------------------------------

@defproc[(with-rewriters [proc (-> any)])
         any]{

Calls @racket[proc] with the rewriters of
@racket[current-atomic-rewriters], @racket[current-compound-rewriters],
and @racket[current-unquote-rewriters].
}

@defparam[current-atomic-rewriters rewriters
          (let ([atomic-rewriter/c
                 (or/c string? pict?
                       (-> (or/c string? pict?)))])
            (plistof symbol? atomic-rewriter/c))]{

Parameter of atomic rewriters (as in @racket[with-atomic-rewriter])
used by @racket[with-rewriters].
}

@defparam[current-compound-rewriters rewriters
          (plistof symbol? compound-rewriter/c)]{

Parameter of compound rewriters (as in
@racket[with-compound-rewriter]) used by @racket[with-rewriters].
}

@defparam[current-unquote-rewriters rewriters
          (plistof (-> lw? any/c) (-> lw? lw?))]{

Parameter of unquote rewriters (as in @racket[with-unquote-rewriter])
used by @racket[with-rewriters].
}

@deftogether[[
@defproc[(add-atomic-rewriters! [rewriters
                                 (let ([atomic-rewriter/c
                                        (or/c string? pict?
                                               (-> (or/c string? pict?)))])
                                   (plistof symbol? atomic-rewriter/c))])
         void?]
@defproc[(add-compound-rewriters! [rewriters
                                   (plistof symbol? compound-rewriter/c)])
         void?]
@defproc[(add-unquote-rewriters! [rewriters
                                  (plistof (-> lw? any/c) (-> lw? lw?))])
         void?]]]{

Add rewriters to the @racket[current-atomic-rewriters],
@racket[current-compound-rewriters], or
@racket[current-unquote-rewriters], respectively.
}

@defproc[(plistof [key/c contract?] [value/c contract?])
         contract?]{

Contract for even-length lists of alternating @racket[key/c] and
@racket[value/c] values.

Equivalent to
@racketblock[
(letrec ([ctc
          (recursive-contract
           (or/c '()
                 (cons/c key/c (cons/c value/c ctc))))])
  ctc)
]
}

@defthing[compound-rewriter/c contract?]{

Contract for compound rewriters, which take a list of @racket[lw]
structs and returns a list of @racket[lw]s, @racket[pict]s, or
strings.

Equivalent to 
@racketblock[
(-> (listof lw?)
    (listof (or/c lw? pict? string?)))
]
}

@defproc[(binary-rw [operator (or/c string? pict? (-> (or/c string? pict?)))]
                    [#:parenthesize-arg parenthesize-arg
                                        (or/c #t #f (listof symbol?) (-> lw? any/c))
                                        #f]
                    [#:parenthesize-left parenthesize-left
                                         (or/c #t #f (listof symbol?) (-> lw? any/c))
                                         parenthesize-arg]
                    [#:parenthesize-right parenthesize-right
                                          (or/c #t #f (listof symbol?) (-> lw? any/c))
                                          parenthesize-arg])
         compound-rewriter/c]{

Typesets @racket[(_sym _term1 _term2)] using @racket[operator] as a
binary operator between @racket[_term1] and @racket[_term2].

@with-eval-preserve-source-locations[
@examples[#:eval the-eval
(add-compound-rewriters!
 'plus (binary-rw " + "))
(with-rewriters
  (lambda ()
    (term->pict ZF (plus 1 2))))
]]

Redex terms may become ambiguous when typeset. To avoid ambiguity, use
@racket[#:parenthesize-arg] to direct when arguments should be
parenthesized. If @racket[parenthesize-arg] is @racket[#t], then
arguments are always parenthesized; if it is @racket[#f], never; if it
is a list of symbols, then an argument is parenthesized only if the
argument is a term starting with a symbol in the list; if it is a
procedure, then the argument is parenthesized if the procedure applied
to the argument's @racket[lw] struct returns a true value.

@with-eval-preserve-source-locations[
@interaction[#:eval the-eval
(add-compound-rewriters!
 'times (binary-rw " × "))
(with-rewriters
  (lambda ()
    (term->pict ZF (times (plus 1 2) 3))))
(add-compound-rewriters!
 'times (binary-rw " × " #:parenthesize-arg '(plus)))
(with-rewriters
  (lambda ()
    (term->pict ZF (times (plus 1 2) 3))))
]]

The parenthesization rules for left and right arguments can be
supplied separately through @racket[#:parenthesize-left] and
@racket[#:parenthesize-right], for example to create left-associated
or right-associated operators:

@with-eval-preserve-source-locations[
@interaction[#:eval the-eval
(add-compound-rewriters!
 'arrow (binary-rw " → " #:parenthesize-left '(arrow)))
(with-rewriters
  (lambda ()
    (term->pict ZF (arrow (arrow A B) (arrow C D)))))
]]
}

@defproc[(prefix-rw [prefix (or/c string? pict? (-> (or/c string? pict?)))]
                    [#:parenthesize-arg parenthesize-arg
                                        (or/c #f #t (listof symbol?) (-> lw? any/c))
                                        #f])
         compound-rewriter/c]{

Typesets @racket[(_sym _term)] by placing @racket[prefix] before
@racket[_term].

@with-eval-preserve-source-locations[
@examples[#:eval the-eval
(add-compound-rewriters!
 'not (prefix-rw "¬ "))
(with-rewriters
  (lambda ()
    (term->pict ZF (not (in? x empty)))))
]]
}

@defproc[(postfix-rw [postfix (or/c string? pict? (-> (or/c string? pict?)))]
                     [#:parenthesize-arg parenthesize-arg
                                         (or/c #f #t (listof symbol?) (-> lw? any/c))
                                         #f])
         compound-rewriter/c]{

Typesets @racket[(_sym _term)] by placing @racket[postfix] after
@racket[_term].

@with-eval-preserve-source-locations[
@examples[#:eval the-eval
(add-compound-rewriters!
 'nonempty (postfix-rw " is nonempty"))
(with-rewriters
  (lambda ()
    (term->pict ZF (nonempty (Set x)))))
]]
}

@defproc[(function-rw [function (or/c string? pict? (-> (or/c string? pict?)))])
         compound-rewriter/c]{

Typesets @racket[(_sym _term ...)] by placing @racket[function] before
the parenthesized, comma-separated list of @racket[_term]s.

@with-eval-preserve-source-locations[
@examples[#:eval the-eval
(add-compound-rewriters!
 'f (function-rw "f")
 'max (function-rw (text "max" '(bold . modern) (default-font-size))))
(with-rewriters
  (lambda ()
    (term->pict ZF (max 1 2 (f 3)))))
]]
}

@defproc[(only-first-rw)
         compound-rewriter/c]{

Typesets @racket[(_sym _term1 _term2 ...)] as @racket[_term1]. Useful
for hiding parameters that are necessary for defining the semantics
but can be glossed over in its explanation, such as state parameters
used for generating unique names.

@with-eval-preserve-source-locations[
@examples[#:eval the-eval
(add-compound-rewriters!
 'First (only-first-rw))
(with-rewriters
  (lambda ()
    (term->pict ZF [First (in? x y) counter])))
]]
}

@defproc[(splice-rw)
         compound-rewriter/c]{

Typesets @racket[(_sym _term ...)] by rendering the @racket[_term]s
side-by-side.
}

@defproc[(constant-rw [constant (or/c string? pict? (-> (or/c string? pict?)))])
         compound-rewriter/c]{

Typesets @racket[(_sym _term ...)] as @racket[constant].
}

@defproc[(bracket-rw [brackets (or/c 'round 'square 'curly 'angle
                                     (list (or/c string? pict?)
                                           (or/c string? pict?)))]
                     [#:comma? comma? any/c #t])
         compound-rewriter/c]{

Typesets @racket[(_sym _term ...)] by surrounding the comma-separated
(or space-separated, if @racket[comma?] is false) sequence of
@racket[_term]s with brackets. If @racket[brackets] is a list, the
first element is the left bracket and the second is the right bracket;
@racket['round] is equivalent to @racket['("(" ")")]; @racket['square]
is equivalent to @racket['("[" "]")]; @racket['curly] is equivalent to
@racket['("{" "}")]; and @racket['angle] is equivalent to
@racket['("〈" "〉")].

@with-eval-preserve-source-locations[
@examples[#:eval the-eval
(add-compound-rewriters!
 'Tuple (bracket-rw 'angle))
(with-rewriters
  (lambda ()
    (term->pict ZF (Tuple 1 2 3))))
]]
}

@defproc[(set-cons-rw)
         compound-rewriter/c]{

Rewriter that typesets @racket[(_sym _elem-term _set-term)] as the
union of the singleton set containing @racket[_elem-term] with the set
@racket[_set-term].

@with-eval-preserve-source-locations[
@examples[#:eval the-eval
(add-compound-rewriters!
 'set-cons (set-cons-rw))
(with-rewriters
  (lambda ()
    (term->pict ZF (set-cons x S))))
]]
}
