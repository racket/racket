#lang scribble/doc
@(require scribble/manual
          scribble/struct
          scribble/decode
          scribble/eval
          "parse-common.rkt")

@title{Literal Sets and Conventions}

Sometimes the same literals are recognized in a number of different
places. The most common example is the literals for fully expanded
programs, which are used in many analysis and transformation
tools. Specifying literals individually is burdensome and error-prone.
As a remedy, @racketmodname[syntax/parse] offers @deftech{literal
sets}. A literal set is defined via @racket[define-literal-set] and
used via the @racket[#:literal-set] option of @racket[syntax-parse].

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

Defines @racket[id] as a @tech{literal set}. Each @racket[literal] can
have a separate @racket[pattern-id] and @racket[literal-id]. The
@racket[pattern-id] determines what identifiers in the pattern are
treated as literals. The @racket[literal-id] determines what
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
@racket[phase-level] @emph{relative to the enclosing module}. If the
@racket[#:for-template] option is given, @racket[phase-level] is
@racket[-1]; @racket[#:for-syntax] means @racket[1], and
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

In the literal set @racket[common-lits], the literal @racket[x] always
recognizes identifiers bound to the variable @racket[x] defined in
module @racketmodname['common].

The following module defines an equivalent literal set, but imports
the @racket['common] module for-template instead:

@myexamples[
(module lits racket/base
  (require syntax/parse (for-template 'common))
  (define-literal-set common-lits #:for-template (x))
  (provide common-lits))
]

When a literal set is @emph{used} with the @racket[#:phase phase-expr]
option, the literals' fixed bindings are compared against the binding of
the input literal at the specified phase. Continuing the example:

@myexamples[
(require syntax/parse 'lits (for-syntax 'common))
(syntax-parse #'x #:literal-sets ([common-lits #:phase 1])
  [x 'yes]
  [_ 'no])
]

The occurrence of @racket[x] in the pattern matches any identifier
whose binding at phase 1 is the @racket[x] from module
@racketmodname['common].
}

@defform[(literal-set->predicate litset-id)]{

Given the name of a literal set, produces a predicate that recognizes
identifiers in the literal set. The predicate takes one required
argument, an identifier @racket[_id], and one optional argument, the
phase @racket[_phase] at which to examine the binding of @racket[_id];
the @racket[_phase] argument defaults to
@racket[(syntax-local-phase-level)].

@myexamples[
(define kernel? (literal-set->predicate kernel-literals))
(kernel? #'lambda)
(kernel? #'#%plain-lambda)
(kernel? #'define-values)
(kernel? #'define-values 4)
]
}

@defform/subs[(define-conventions name-id convention-rule ...)
              ([convention-rule (name-pattern syntax-class)]
               [name-pattern exact-id
                             name-rx]
               [syntax-class syntax-class-id
                             (syntax-class-id expr ...)])]{

Defines @deftech{conventions} that supply default syntax classes for
pattern variables. A pattern variable that has no explicit syntax
class is checked against each @racket[name-pattern], and the first one
that matches determines the syntax class for the pattern. If no
@racket[name-pattern] matches, then the pattern variable has no syntax
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

Local conventions, introduced with the @racket[#:local-conventions]
keyword argument of @racket[syntax-parse] and syntax class
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
