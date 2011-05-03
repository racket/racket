#lang scribble/doc
@(require scribble/manual
          scribble/struct
          scribble/decode
          scribble/eval
          "parse-common.rkt")

@title{Literal sets and Conventions}

Sometimes the same literals are recognized in a number of different
places. The most common example is the literals for fully expanded
programs, which are used in many analysis and transformation
tools. Specifying literals individually is burdensome and error-prone.
As a remedy, @schememodname[syntax/parse] offers @deftech{literal
sets}. A literal set is defined via @scheme[define-literal-set] and
used via the @scheme[#:literal-set] option of @scheme[syntax-parse].

@defform/subs[(define-literal-set id maybe-phase maybe-imports (literal ...))
              ([literal literal-id
                        (pattern-id literal-id)]
               [maybe-phase (code:line)
                            (code:line #:for-template)
                            (code:line #:for-syntax)
                            (code:line #:for-label)
                            (code:line #:phase phase-level)]
               [maybe-imports (code:line)
                              (code:line #:literal-sets (imported-litset-id ...))])]{

Defines @scheme[id] as a @tech{literal set}. Each @scheme[literal] can
have a separate @scheme[pattern-id] and @scheme[literal-id]. The
@scheme[pattern-id] determines what identifiers in the pattern are
treated as literals. The @scheme[literal-id] determines what
identifiers the literal matches. If the @racket[#:literal-sets] option
is present, the contents of the given @racket[imported-litset-id]s are
included.

@myexamples[
(define-literal-set def-litset
  (define-values define-syntaxes))
(syntax-parse #'(define-syntaxes (x) 12)
  #:literal-sets (def-litset)
  [(define-values (x:id ...) e:expr) 'v]
  [(define-syntaxes (x:id ...) e:expr) 's])
]

The literals in a literal set always refer to the bindings at phase
@scheme[phase-level] @emph{relative to the enclosing module}. If the
@scheme[#:for-template] option is given, @scheme[phase-level] is
@scheme[-1]; @scheme[#:for-syntax] means @racket[1], and
@racket[#:for-label] means @racket[#f]. If no phase keyword option is
given, then @racket[phase-level] is @racket[0].

For example:

@myexamples[
(module common racket/base
  (define x 'something)
  (provide x))

(module lits racket/base
  (require syntax/parse 'common)
  (define-literal-set common-lits (x))
  (provide common-lits))
]

In the literal set @scheme[common-lits], the literal @scheme[x] always
recognizes identifiers bound to the variable @scheme[x] defined in
module @schememodname['common].

The following module defines an equivalent literal set, but imports
the @racket['common] module for-template instead:

@myexamples[
(module lits racket/base
  (require syntax/parse (for-template 'common))
  (define-literal-set common-lits #:for-template (x))
  (provide common-lits))
]

When a literal set is @emph{used} with the @scheme[#:phase phase-expr]
option, the literals' fixed bindings are compared against the binding of
the input literal at the specified phase. Continuing the example:

@myexamples[
(require syntax/parse 'lits (for-syntax 'common))
(syntax-parse #'x #:literal-sets ([common-lits #:phase 1])
  [x 'yes]
  [_ 'no])
]

The occurrence of @scheme[x] in the pattern matches any identifier
whose binding at phase 1 is the @scheme[x] from module
@schememodname['common].
}

@defform/subs[(define-conventions name-id convention-rule ...)
              ([convention-rule (name-pattern syntax-class)]
               [name-pattern exact-id
                             name-rx]
               [syntax-class syntax-class-id
                             (syntax-class-id expr ...)])]{

Defines @deftech{conventions} that supply default syntax classes for
pattern variables. A pattern variable that has no explicit syntax
class is checked against each @scheme[id-pattern], and the first one
that matches determines the syntax class for the pattern. If no
@scheme[id-pattern] matches, then the pattern variable has no syntax
class.

@myexamples[
(define-conventions xyz-as-ids
  [x id] [y id] [z id])
(syntax-parse #'(a b c 1 2 3)
  #:conventions (xyz-as-ids)
  [(x ... n ...) (syntax->datum #'(x ...))])
(define-conventions xn-prefixes
  [#rx"^x" id]
  [#rx"^n" nat])
(syntax-parse #'(a b c 1 2 3)
  #:conventions (xn-prefixes)
  [(x0 x ... n0 n ...)
   (syntax->datum #'(x0 (x ...) n0 (n ...)))])
]

Local conventions, introduced with the @scheme[#:local-conventions]
keyword argument of @scheme[syntax-parse] and syntax class
definitions, may refer to local bindings:

@myexamples[
(define-syntax-class (nat> bound)
  (pattern n:nat
           #:fail-unless (> (syntax-e #'n) bound)
                         (format "expected number > ~s" bound)))

(define-syntax-class (natlist> bound)
  #:local-conventions ([N (nat> bound)])
  (pattern (N ...)))

(define (parse-natlist> bound x)
  (syntax-parse x
    #:local-conventions ([NS (natlist> bound)])
    [NS 'ok]))
(parse-natlist> 0 #'(1 2 3))
(parse-natlist> 5 #'(8 6 4 2))
]

}
