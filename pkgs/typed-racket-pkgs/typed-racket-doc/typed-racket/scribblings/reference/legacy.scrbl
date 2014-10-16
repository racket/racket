#lang scribble/manual

@begin[(require "../utils.rkt")
       (require scribble/eval
                (for-label (only-meta-in 0 [except-in typed/racket for])))]

@(define the-eval (make-base-eval))
@(the-eval '(require (except-in typed/racket #%top-interaction #%module-begin)))
@(define the-top-eval (make-base-eval))
@(the-top-eval '(require (except-in typed/racket #%module-begin)))

@(define-syntax-rule (ex . args)
   (examples #:eval the-top-eval . args))

@title{Legacy Forms}

The following forms are provided by Typed Racket for backwards
compatibility.

@defform/subs[(lambda: formals . body)
              ([formals ([v : t] ...)
                        ([v : t] ... v : t *)
                        ([v : t] ... v : t ooo bound)])]{
A function of the formal arguments @racket[v], where each formal
argument has the associated type.  If a rest argument is present, then
it has type @racket[(Listof t)].}

@defform[(λ: formals . body)]{
An alias for the same form using @racket[lambda:].}

@defform*[[(plambda: (a ...) formals . body)
           (plambda: (a ... b ooo) formals . body)]]{
A polymorphic function, abstracted over the type variables
@racket[a]. The type variables @racket[a] are bound in both the types
of the formal, and in any type expressions in the @racket[body].}

@defform/subs[(opt-lambda: formals . body)
([formals ([v : t] ... [v : t default] ...)
          ([v : t] ... [v : t default] ... v : t *)
          ([v : t] ... [v : t default] ... v : t ooo bound)])]{
A function with optional arguments.}

@defform*[[(popt-lambda: (a ...) formals . body)
           (popt-lambda: (a ... a ooo) formals . body)]]{
A polymorphic function with optional arguments.}

@defalias[case-lambda: case-lambda]

@defform*[[(pcase-lambda: (a ...) [formals body] ...)
           (pcase-lambda: (a ... b ooo) [formals body] ...)]]{
A polymorphic function of multiple arities.}

@defform*[[
  (let: ([v : t e] ...) . body)
  (let: loop : t0 ([v : t e] ...) . body)]]{
Local bindings, like @racket[let], each with
associated types.  In the second form, @racket[_t0] is the type of the
result of @racket[_loop] (and thus the result of the entire
                              expression as well as the final
                                expression in @racket[body]).
                                Type annotations are optional.
@ex[(: filter-even : (Listof Natural) (Listof Natural) -> (Listof Natural))
    (define (filter-even lst accum)
      (if (null? lst)
          accum
          (let: ([first : Natural (car lst)]
                 [rest  : (Listof Natural) (cdr lst)])
                (if (even? first)
                    (filter-even rest (cons first accum))
                    (filter-even rest accum)))))
    (filter-even (list 1 2 3 4 5 6) null)]

@ex[(: filter-even-loop : (Listof Natural) -> (Listof Natural))
    (define (filter-even-loop lst)
      (let: loop : (Listof Natural)
            ([accum : (Listof Natural) null]
             [lst   : (Listof Natural) lst])
            (cond
              [(null? lst)       accum]
              [(even? (car lst)) (loop (cons (car lst) accum) (cdr lst))]
              [else              (loop accum (cdr lst))])))
    (filter-even-loop (list 1 2 3 4))]}
@defform[(plet: (a ...) ([v : t e] ...) . body)]{
A polymorphic version of @racket[let:], abstracted over the type variables
@racket[a]. The type variables @racket[a] are bound in both the types
of the formal, and in any type expressions in the @racket[body].
Does not support the looping form of let.}

@deftogether[[
@defform[(letrec: ([v : t e] ...) . body)]
@defform[(let*: ([v : t e] ...) . body)]
@defform[(let-values: ([([v : t] ...) e] ...) . body)]
@defform[(letrec-values: ([([v : t] ...) e] ...) . body)]
@defform[(let*-values: ([([v : t] ...) e] ...) . body)]]]{
Type-annotated versions of
@racket[letrec], @racket[let*], @racket[let-values],
@racket[letrec-values], and @racket[let*-values]. As with
@racket[let:], type annotations are optional.}

@deftogether[[
@defform[(let/cc: v : t . body)]
@defform[(let/ec: v : t . body)]]]{
Type-annotated versions of @racket[let/cc] and @racket[let/ec].
As with @racket[let:], the type annotation is optional.
}

@defform*[[(define: v : t e)
           (define: (a ...) v : t e)
           (define: (a ... a ooo) v : t e)
           (define: (f . formals) : t . body)
           (define: (a ...) (f . formals) : t . body)
           (define: (a ... a ooo) (f . formals) : t . body)]]{
These forms define variables, with annotated types.  The first form
defines @racket[v] with type @racket[t] and value @racket[e]. The second
form does the same, but allows the specification of type variables. The third
allows for polydotted variables. The fourth, fifth, and sixth forms define a
function @racket[f] with appropriate types. In most cases, use of @racket[:] is
preferred to use of @racket[define:].

@ex[(define: foo : Integer 10)

    (define: (A) mt-seq : (Sequenceof A) empty-sequence)

    (define: (add [first : Integer]
                  [rest  : Integer]) : Integer
      (+ first rest))

    (define: (A) (poly-app [func : (A A -> A)]
                           [first : A]
                           [rest  : A]) : A
      (func first rest))]
}

@defalias[struct: struct]
@defalias[define-struct: define-struct]
@defalias[define-struct/exec: define-struct/exec]

@defalias[for: for]
@deftogether[(@defidform[for*/and:]
              @defidform[for*/first:]
              @defidform[for*/flvector:]
              @defidform[for*/extflvector:]
              @defidform[for*/fold:]
              @defidform[for*/hash:]
              @defidform[for*/hasheq:]
              @defidform[for*/hasheqv:]
              @defidform[for*/last:]
              @defidform[for*/list:]
              @defidform[for*/lists:]
              @defidform[for*/or:]
              @defidform[for*/product:]
              @defidform[for*/sum:]
              @defidform[for*/vector:]
              @defidform[for*:]
              @defidform[for/and:]
              @defidform[for/first:]
              @defidform[for/flvector:]
              @defidform[for/extflvector:]
              @defidform[for/fold:]
              @defidform[for/hash:]
              @defidform[for/hasheq:]
              @defidform[for/hasheqv:]
              @defidform[for/last:]
              @defidform[for/list:]
              @defidform[for/lists:]
              @defidform[for/or:]
              @defidform[for/product:]
              @defidform[for/sum:]
              @defidform[for/vector:])]{
 Aliases for the same iteration forms without a @racket[_:].
}
@defalias[do: do]

@defidform[define-type-alias]{Equivalent to @racket[define-type].}
@defidform[define-typed-struct]{Equivalent to @racket[define-struct:]}
@defidform[require/opaque-type]{Similar to using the @racket[opaque]
keyword with @racket[require/typed].}
@defidform[require-typed-struct]{Similar to using the @racket[struct]
keyword with @racket[require/typed].}
@defidform[require-typed-struct/provide]{Similar to
@racket[require-typed-struct], but also provides the imported identifiers.}
@defidform[pdefine:]{Defines a polymorphic function.}
@defform[(pred t)]{Equivalent to @racket[(Any -> Boolean : t)].}

@defalias[Un U]
@defalias[mu Rec]
@defalias[Tuple List]
@defalias[Parameter Parameterof]
@defalias[Pair Pairof]
@defalias[values Values]
