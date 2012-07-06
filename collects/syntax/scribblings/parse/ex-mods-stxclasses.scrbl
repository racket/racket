#lang scribble/doc
@(require scribble/manual
          scribble/struct
          scribble/decode
          scribble/eval
          "parse-common.rkt"
          (for-label racket/class))

@title{Phases and Reusable Syntax Classes}

As demonstrated in the @secref{stxparse-intro}, the simplest place to
define a syntax class is within the macro definition that uses it. But
that limits the scope of the syntax class to the one client macro, and
it makes for very large macro definitions. Creating reusable syntax
classes requires some awareness of the Racket @tech[#:doc '(lib
"scribblings/reference/reference.scrbl")]{phase level} separation. A
syntax class defined immediately within a module cannot be used by
macros in the same module; it is defined at the wrong phase.

@myinteraction[
(module phase-mismatch-mod racket
  (require syntax/parse (for-syntax syntax/parse))
  (define-syntax-class foo
    (pattern (a b c)))
  (define-syntax (macro stx)
    (syntax-parse stx
      [(_ f:foo) #'(+ f.a f.b f.c)])))
]

In the module above, the syntax class @racket[foo] is defined at phase
level 0. The reference to @racket[foo] within @racket[macro], however,
is at phase level 1, being the implementation of a macro
transformer. (Needing to require @racketmodname[syntax/parse] twice,
once normally and once @racket[for-syntax] is a common warning sign of
phase level incompatibility.)

The phase level mismatch is easily remedied by putting the syntax
class definition within a @racket[begin-for-syntax] block:

@myinteraction[
(module phase-ok-mod racket
  (require (for-syntax syntax/parse))
  (begin-for-syntax
   (define-syntax-class foo
     (pattern (a b c))))
  (define-syntax (macro stx)
    (syntax-parse stx
      [(_ f:foo) #'(+ f.a f.b f.c)])))
]

In the revised module above, @racket[foo] is defined at phase 1, so it
can be used in the implementation of the macro.

An alternative to @racket[begin-for-syntax] is to define the syntax
class in a separate module and require that module
@racket[for-syntax].

@myinteraction[
(module stxclass-mod racket
  (require syntax/parse)
  (define-syntax-class foo
    (pattern (a b c)))
  (provide foo))
(module macro-mod racket
  (require (for-syntax syntax/parse
                       'stxclass-mod))
  (define-syntax (macro stx)
    (syntax-parse stx
      [(_ f:foo) #'(+ f.a f.b f.c)]))
  (provide macro))
(require 'macro-mod)
(macro (1 2 3))
]

If a syntax class refers to literal identifiers, or if it computes
expressions via syntax templates, then the module containing the
syntax class must generally require @racket[for-template] the bindings
referred to in the patterns and templates.

@myinteraction[
(module arith-keywords-mod racket
  (define-syntax plus (syntax-rules ()))
  (define-syntax times (syntax-rules ()))
  (provide plus times))

(module arith-stxclass-mod racket
  (require syntax/parse
           (for-template 'arith-keywords-mod
                         racket))
  (define-syntax-class arith
    #:literals (plus times)
    (pattern n:nat
             #:with expr #'n)
    (pattern (plus a:arith b:arith)
             #:with expr #'(+ a.expr b.expr))
    (pattern (times a:arith b:arith)
             #:with expr #'(* a.expr b.expr)))
  (provide arith))

(module arith-macro-mod racket
  (require (for-syntax syntax/parse
                       'arith-stxclass-mod)
           'arith-keywords-mod)
  (define-syntax (arith-macro stx)
    (syntax-parse stx
      [(_ a:arith)
       #'(values 'a.expr a.expr)]))
  (provide arith-macro
           (all-from-out 'arith-keywords-mod)))

(require 'arith-macro-mod)
(arith-macro (plus 1 (times 2 3)))
]

In @racket['arith-stxclass-mod], the module
@racket['arith-keywords-mod] must be required @racket[for-template]
because the keywords are used in phase-0 expressions. Likewise, the
module @racketmodname[racket] must be required @racket[for-template]
because the syntax class contains syntax templates involving
@racket[+] and @racket[*] (and, in fact, the implicit @racket[#%app]
syntax). All of these identifiers (the keywords @racket[plus] and
@racket[times]; the procedures @racket[+] and @racket[*]; and the
implicit syntax @racket[#%app]) must be bound at ``absolute'' phase
level 0. Since the module @racket['arith-stxclass-mod] is required
with a phase level offset of 1 (that is, @racket[for-syntax]), it must
compensate with a phase level offset of -1, or @racket[for-template].
