#lang scribble/doc
@(require scribble/manual
          scribble/struct
          scribble/decode
          scribble/eval
          "parse-common.rkt"
          (for-label racket/class))

@title[#:tag "uniform-meanings"]{Variants with Uniform Meanings}

Syntax classes not only validate syntax, they also extract some
measure of meaning from it. From the perspective of meaning, there are
essentially two kinds of syntax class. In the first, all of the syntax
class's variants have the same kind of meaning. In the second,
variants may have different kinds of meaning.@margin-note*{In other
words, some syntax classes' meanings are products and others' meanings
are sums.} This section discusses the first kind, syntax classes with
uniform meanings. The next section discusses @secref{varied-meanings}.

If all of a syntax class's variants express the same kind of
information, that information can be cleanly represented via
attributes, and it can be concisely processed using ellipses.

One example of a syntax class with uniform meaning: the
@racket[init-decl] syntax of the @racket[class] macro. Here is the
specification of @racket[init-decl]:

@racketgrammar*[[init-decl
                 id
                 (maybe-renamed)
                 (maybe-renamed default-expr)]
                [maybe-renamed
                 id
                 (internal-id external-id)]]

The @racket[init-decl] syntax class has three variants, plus an
auxiliary syntax class that has two variants of its own. But all forms
of @racket[init-decl] ultimately carry just three pieces of
information: an internal name, an external name, and a default
configuration of some sort. The simpler syntactic variants are just
abbreviations for the full information.

The three pieces of information determine the syntax class's
attributes. It is useful to declare the attributes explicitly using
the @racket[#:attributes] keyword; the declaration acts both as
in-code documentation and as a check on the variants.

@racketblock[
(define-syntax-class init-decl
  #:attributes (internal external default)
  ___)
]

Next we fill in the syntactic variants, deferring the computation of
the attributes:

@racketblock[
(define-syntax-class init-decl
  #:attributes (internal external default)
  (pattern ???:id
           ___)
  (pattern (???:maybe-renamed)
           ___)
  (pattern (???:maybe-renamed ???:expr)
           ___))
]

We perform a similar analysis of @racket[maybe-renamed]:
@racketblock[
(define-syntax-class maybe-renamed
  #:attributes (internal external)
  (pattern ???:id
           ___)
  (pattern (???:id ???:id)
           ___))
]

Here's one straightforward way of matching syntactic structure with
attributes for @racket[maybe-renamed]:

@racketblock[
(define-syntax-class maybe-renamed
  #:attributes (internal external)
  (pattern internal:id
           #:with external #'internal)
  (pattern (internal:id external:id)))
]

Given that definition of @racket[maybe-renamed], we can fill in most
of the definition of @racket[init-decl]:

@racketblock[
(define-syntax-class init-decl
  #:attributes (internal external default)
  (pattern internal:id
           #:with external #:internal
           #:with default ???)
  (pattern (mr:maybe-renamed)
           #:with internal #'mr.internal
           #:with external #'mr.external
           #:with default ???)
  (pattern (mr:maybe-renamed default0:expr)
           #:with internal #'mr.internal
           #:with external #'mr.external
           #:with default ???))
]

At this point we realize we have not decided on a representation for
the default configuration. In fact, it is an example of
@seclink["varied-meanings"]{syntax with varied meanings} (aka sum or
disjoint union). The following section discusses representation
options in greater detail; for the sake of completeness, we present
one of them here.

There are two kinds of default configuration. One indicates that the
initialization argument is optional, with a default value computed
from the given expression. The other indicates that the initialization
argument is mandatory. We represent the variants as a (syntax) list
containing the default expression and as the empty (syntax) list,
respectively. More precisely:

@racketblock[
(define-syntax-class init-decl
  #:attributes (internal external default)
  (pattern internal:id
           #:with external #:internal
           #:with default #'())
  (pattern (mr:maybe-renamed)
           #:with internal #'mr.internal
           #:with external #'mr.external
           #:with default #'())
  (pattern (mr:maybe-renamed default0:expr)
           #:with internal #'mr.internal
           #:with external #'mr.external
           #:with default #'(default0)))
]

Another way to look at this aspect of syntax class design is as the
algebraic factoring of sums-of-products (concrete syntax variants)
into products-of-sums (attributes and abstract syntax variants). The
advantages of the latter form are the ``dot'' notation for data
extraction, avoiding or reducing additional case analysis, and the
ability to concisely manipulate sequences using ellipses.
