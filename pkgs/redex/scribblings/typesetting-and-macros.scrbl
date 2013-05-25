#lang scribble/base

@(require scribble/manual
          scribble/bnf
          scribble/struct
          scribble/eval
          scribble/racket
          (for-syntax racket/base)
          (for-label racket/base
                     (except-in racket/gui make-color)
                     racket/pretty
                     racket/contract
                     mrlib/graph
                     (except-in 2htdp/image make-pen text)
                     (only-in pict pict? text dc-for-text-size text-style/c
                              vc-append)
                     redex))

@(define redex-macros-eval (make-base-eval))
@(interaction-eval #:eval redex-macros-eval (require redex/reduction-semantics
                                                     redex/pict
                                                     (for-syntax racket/base)))

@title{Macros and Typesetting}

When you have a macro that abstracts over variations in 
Redex programs, then typesetting is unlikely to work
without some help from your macros.

To see the issue, consider this macro abstraction over
a Redex grammar:
@interaction[#:eval 
             redex-macros-eval
             (define-syntax-rule
               (def-my-lang L prim ...)
               (define-language L
                 (e ::=
                    (λ (x) e)
                    (e e)
                    prim ...
                    x)
                 (x ::= variable-not-otherwise-mentioned)))

             (def-my-lang L + - *)
             (render-language L)]
Redex thinks that the grammar is going ``backwards'' because 
of the way macro expansion synthesizes source locations.
In particular, in the result of the macro expansion, the
third production for @racket[_e] appears to come later
in the file than the fourth production and this confuses
Redex, making it unable to typeset this language.

One simple, not-very-general work-around is to just avoid 
typesetting the parts that come from the macro arguments. For
example if you move the primitives into their own non-terminal
and then just avoid typesetting that, Redex can cope:

@(define exp
   (quote-syntax
    (define-syntax-rule
      (def-my-lang L prim ...)
      (define-language L
        (e ::=
           (λ (x) e)
           (e e)
           prims
           x)
        (prims ::= prim ...)
        (x ::= variable-not-otherwise-mentioned)))))

@(redex-macros-eval exp)
@(to-paragraph exp)

@interaction[#:eval 
             redex-macros-eval
             (def-my-lang L + - *)
             (render-language L #:nts '(e x))]

You can also, however, exploit Racket's macro system to rewrite the
source locations in a way that tells Redex where the macro-introduced
parts of the language are supposed to be, and then typesetting
will work normally. For example, here is one way to do this with
the original language:

@(require redex/reduction-semantics (for-syntax racket/base))
@(define fancy-exp
  (quote-syntax
   (define-syntax (def-my-lang stx)
      (syntax-case stx ()
        [(_ L a ...)
         (let ()
           (define template
             #'(define-language L
                 (e (λ (x) e)
                    (e e)
                    HERE
                    x)
                 (x variable-not-otherwise-mentioned)))
           (car
            (let loop ([stx template])
              (syntax-case stx (HERE)
                [HERE 
                 (let loop ([as (syntax->list #'(a ...))]
                            [pos (syntax-position stx)]
                            [col (syntax-column stx)])
                   (cond
                     [(null? as) '()]
                     [else 
                      (define a (car as))
                      (define span 
                        (string-length
                         (symbol->string (syntax-e a))))
                      (define srcloc
                        (vector (syntax-source stx)
                                (syntax-line stx)
                                col
                                pos
                                span))
                      (cons
                       (datum->syntax a 
                                      (syntax-e a)
                                      srcloc
                                      a)
                       (loop (cdr as)
                             (+ pos span 1)
                             (+ col span 1)))]))]
                [(a ...)
                 (list
                  (datum->syntax
                   stx
                   (apply append (map loop (syntax->list #'(a ...))))
                   stx
                   stx))]
                [a
                 (list stx)]))))]))))

@(redex-macros-eval fancy-exp)
@(to-paragraph fancy-exp)

@interaction[#:eval redex-macros-eval
                    (def-my-lang L + - *)]
@interaction[#:eval redex-macros-eval
             (render-language L)]


And one final caveat: when Racket compiles source files to bytecode format, 
it discards source location information in syntax constants, 
which means that if a file containing
a macro like the one above is compiled to bytecode, then it cannot properly
adjust the source locations and the typeset language will not look right 
(the important constant whose source locations are lost is @racket[template], above).

The simplest way to avoid this problem is to just avoid creating bytecode for
these files.

It is possible to write the constant in the source code, however, and then
process it so the compiled version of the file contains different data structures
that record the source locations of the expressions. Redex does this internally so that 
compiled files that use, e.g., @racket[define-language], still correctly typeset.
But when you write macros that expand into @racket[define-language], you must
also take this step yourself (or avoid @tt{.zo} files).
