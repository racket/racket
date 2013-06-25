#lang scribble/doc
@(require "common.rkt" (for-label syntax/kerncase))

@(define-syntax-rule (intro id)
   (begin
     (require (for-label mzscheme))
     (define id (racket if))))
@(intro mzscheme-if)

@; ----------------------------------------------------------------------

@title[#:tag "kerncase"]{Matching Fully-Expanded Expressions}

@defmodule[syntax/kerncase]

@defform[(kernel-syntax-case stx-expr trans?-expr clause ...)]{

A syntactic form like @racket[syntax-case*], except that the literals
are built-in as the names of the primitive Racket forms as
exported by @racketmodname[racket/base], including
@racket[letrec-syntaxes+values]; see @secref[#:doc refman
"fully-expanded"].

The @racket[trans?-expr] boolean expression replaces the comparison
procedure, and instead selects simply between normal-phase comparisons
or transformer-phase comparisons. The @racket[clause]s are the same as in
@racket[syntax-case*].

The primitive syntactic forms must have their normal bindings in the
context of the @racket[kernel-syntax-case] expression. Beware that
@racket[kernel-syntax-case] does not work in a module whose language
is @racketmodname[mzscheme], since the binding of @mzscheme-if from
@racketmodname[mzscheme] is different than the primitive @racket[if].}


@defform[(kernel-syntax-case* stx-expr trans?-expr (extra-id ...) clause ...)]{

A syntactic form like @racket[kernel-syntax-case], except that it
takes an additional list of extra literals that are in addition to the
primitive Racket forms.}


@defform[(kernel-syntax-case/phase stx-expr phase-expr clause ...)]{

Generalizes @racket[kernel-syntax-case] to work at an arbitrary phase
level, as indicated by @racket[phase-expr].}


@defform[(kernel-syntax-case*/phase stx-expr phase-expr (extra-id ..) 
           clause ...)]{

Generalizes @racket[kernel-syntax-case*] to work at an arbitrary phase
level, as indicated by @racket[phase-expr].}


@defproc[(kernel-form-identifier-list) (listof identifier?)]{

Returns a list of identifiers that are bound normally,
@racket[for-syntax], and @racket[for-template] to the primitive 
Racket forms for expressions, internal-definition positions, and
module-level and top-level positions. This function is useful for
generating a list of stopping points to provide to
@racket[local-expand].

In addition to the identifiers listed in @secref[#:doc '(lib
"scribblings/reference/reference.scrbl") "fully-expanded"], the list
includes @racket[letrec-syntaxes+values], which is the core form for
local expand-time binding and can appear in the result of
@racket[local-expand].}
