#lang scribble/doc
@(require scribble/manual
          scribble/struct
          scribble/decode
          scribble/eval
          "parse-common.rkt"
          (for-label racket/class))

@title[#:tag "varied-meanings"]{Variants with Varied Meanings}

As explained in the @seclink["uniform-meanings"]{previous section},
the meaning of a syntax class can be uniform, or it can be varied;
that is, different instances of the syntax class can carry different
kinds of information. This section discusses the latter kind of syntax
class.

A good example of a syntax class with varied meanings is the
@racket[for-clause] of the @racket[for] family of special forms.

@racketgrammar[for-clause
               [id seq-expr]
               [(id ...) seq-expr]
               (code:line #:when guard-expr)]

The first two variants carry the same kind of information; both
consist of identifiers to bind and a sequence expression. The third
variant, however, means something totally different: a condition that
determines whether to continue the current iteration of the loop, plus
a change in scoping for subsequent @racket[seq-expr]s. The information
of a @racket[for-clause] must be represented in a way that a client
macro can do further case analysis to distinguish the ``bind variables
from a sequence'' case from the ``skip or continue this iteration and
enter a new scope'' case.

This section discusses two ways of representing varied kinds of
information.

@section{Syntactic Normalization}

One approach is based on the observation that the syntactic variants
already constitute a representation of the information they carry. So
why not adapt that representation, removing redundancies and
eliminating simplifying the syntax to make subsequent re-parsing
trivial.

@racketblock[
(define-splicing-syntax-class for-clause
  #:attribute (norm)
  (pattern [var:id seq:expr]
           #:with norm #'[(var) seq])
  (pattern [(var:id ...) seq:expr]
           #:with norm #'[(var ...) seq])
  (pattern (~seq #:when guard:expr)
           #:with norm #'[#:when guard]))
]

First, note that since the @racket[#:when] variant consists of two
separate terms, we define @racket[for-clause] as a splicing syntax
class. Second, that kind of irregularity is just the sort of thing
we'd like to remove so we don't have to deal with it again later. Thus
we represent the normalized syntax as a single term beginning with
either a sequence of identifiers (the first two cases) or the keyword
@racket[#:when] (the third case). The two normalized cases are easy to
process and easy to tell apart. We have also taken the opportunity to
desugar the first case into the second.

A normalized syntactic representation is most useful when the
subsequent case analysis is performed by @racket[syntax-parse] or a
similar form.

@section{Non-syntax-valued Attributes}

When the information carried by the syntax is destined for complicated
processing by Racket code, it is often better to parse it into an
intermediate representation using idiomatic Racket data structures,
such as lists, hashes, structs, and even objects.

Thus far we have only used syntax pattern variables and the
@racket[#:with] keyword to bind attribues, and the values of the
attributes have always been syntax. To bind attributes to values other
than syntax, use the @racket[#:attr] keyword.

@racketblock[
(code:comment "A ForClause is either")
(code:comment "  - (bind-clause (listof identifier) syntax)")
(code:comment "  - (when-clause syntax)")
(struct bind-clause (vars seq-expr))
(struct when-clause (guard))

(define-splicing-syntax-class for-clause
  #:attributes (ast)
  (pattern [var:id seq:expr]
           #:attr ast (bind-clause (list #'var) #'seq))
  (pattern [(var:id ...) seq:expr]
           #:attr ast (bind-clause (syntax->list #'(var ...))
                                   #'seq))
  (pattern (~seq #:when guard:expr)
           #:attr ast (when-clause #'guard)))
]

Be careful! If we had used @racket[#:with] instead of @racket[#:attr],
the @racket[#f] would have been coerced to a syntax object before
being matched against the pattern @racket[default].

Attributes with non-syntax values cannot be used in syntax
templates. Use the @racket[attribute] form to get the value of an
attribute.
