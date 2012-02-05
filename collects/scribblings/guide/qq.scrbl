#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@(define qq (racket quasiquote))
@(define uq (racket unquote))

@title[#:tag "qq"]{Quasiquoting: @racket[quasiquote] and @racketvalfont{`}}

@refalso["quasiquote"]{@racket[quasiquote]}

The @racket[quasiquote] form is similar to @racket[quote]:

@specform[(#,qq datum)]

However, for each @racket[(#,uq _expr)]
that appears within the @racket[_datum], the @racket[_expr] is
evaluated to produce a value that takes the place of the
@racket[unquote] sub-form.

@examples[
(eval:alts (#,qq (1 2 (#,uq (+ 1 2)) (#,uq (- 5 1))))
           `(1 2 ,(+ 1 2), (- 5 1)))
]

This form can be used to write functions that build lists according to
certain patterns.

@examples[
(eval:alts (define (deep n)
             (cond
               [(zero? n) 0]
               [else
                (#,qq ((#,uq n) (#,uq (deep (- n 1)))))]))
           (define (deep n)
             (cond
               [(zero? n) 0]
               [else
                (quasiquote ((unquote n) (unquote (deep (- n 1)))))])))
(deep 8)
]

Or even to cheaply construct expressions programmatically. (Of course, 9 times out of 10,
you should be using a @seclink["macros"]{macro} to do this 
(the 10th time being when you're working through
a textbook like @hyperlink["http://www.cs.brown.edu/~sk/Publications/Books/ProgLangs/"]{PLAI}).)

@examples[(define (build-exp n)
            (add-lets n (make-sum n)))
          
          (eval:alts
           (define (add-lets n body)
             (cond
               [(zero? n) body]
               [else
                (#,qq 
                 (let ([(#,uq (n->var n)) (#,uq n)])
                   (#,uq (add-lets (- n 1) body))))]))
           (define (add-lets n body)
             (cond
               [(zero? n) body]
               [else
                (quasiquote 
                 (let ([(unquote (n->var n)) (unquote n)])
                   (unquote (add-lets (- n 1) body))))])))
          
          (eval:alts
           (define (make-sum n)
             (cond
               [(= n 1) (n->var 1)]
               [else
                (#,qq (+ (#,uq (n->var n))
                         (#,uq (make-sum (- n 1)))))]))
           (define (make-sum n)
             (cond
               [(= n 1) (n->var 1)]
               [else
                (quasiquote (+ (unquote (n->var n))
                               (unquote (make-sum (- n 1)))))])))
          (define (n->var n) (string->symbol (format "x~a" n)))
          (build-exp 3)]

The @racket[unquote-splicing] form is similar to @racket[unquote], but
its @racket[_expr] must produce a list, and the
@racket[unquote-splicing] form must appear in a context that produces
either a list or a vector. As the name suggests, the resulting list
is spliced into the context of its use.

@examples[
(eval:alts (#,qq (1 2 (#,(racket unquote-splicing) (list (+ 1 2) (- 5 1))) 5))
           `(1 2 ,@(list (+ 1 2) (- 5 1)) 5))
]

Using splicing we can revise the construction of our example expressions above
to have just a single @racket[let] expression and a single @racket[+] expression.

@examples[(eval:alts
           (define (build-exp n)
             (add-lets 
              n
              (#,qq (+ (#,(racket unquote-splicing) 
                        (build-list
                         n
                         (λ (x) (n->var (+ x 1)))))))))
           (define (build-exp n)
             (add-lets
              n
              (quasiquote (+ (unquote-splicing 
                              (build-list 
                               n
                               (λ (x) (n->var (+ x 1))))))))))
          (eval:alts
           (define (add-lets n body)
             (#,qq
              (let (#,uq
                    (build-list
                     n
                     (λ (n)
                       (#,qq 
                        [(#,uq (n->var (+ n 1))) (#,uq (+ n 1))]))))
                (#,uq body))))
           (define (add-lets n body)
             (quasiquote
              (let (unquote
                    (build-list 
                     n
                     (λ (n) 
                       (quasiquote
                        [(unquote (n->var (+ n 1))) (unquote (+ n 1))]))))
                (unquote body)))))
          (define (n->var n) (string->symbol (format "x~a" n)))
          (build-exp 3)]

If a @racket[quasiquote] form appears within an enclosing
@racket[quasiquote] form, then the inner @racket[quasiquote]
effectively cancels one layer of @racket[unquote] and
@racket[unquote-splicing] forms, so that a second @racket[unquote]
or @racket[unquote-splicing] is needed.

@examples[
(eval:alts (#,qq (1 2 (#,qq (#,uq (+ 1 2)))))
           `(1 2 (,(string->uninterned-symbol "quasiquote")
                  (,(string->uninterned-symbol "unquote") (+ 1 2)))))
(eval:alts (#,qq (1 2 (#,qq (#,uq (#,uq (+ 1 2))))))
           `(1 2 (,(string->uninterned-symbol "quasiquote")
                  (,(string->uninterned-symbol "unquote") 3))))
(eval:alts (#,qq (1 2 (#,qq ((#,uq (+ 1 2)) (#,uq (#,uq (- 5 1)))))))
           `(1 2 (,(string->uninterned-symbol "quasiquote")
                  ((,(string->uninterned-symbol "unquote") (+ 1 2))
                   (,(string->uninterned-symbol "unquote") 4)))))
]

The evaluations above will not actually print as shown. Instead, the
shorthand form of @racket[quasiquote] and @racket[unquote] will be
used: @litchar{`} (i.e., a backquote) and @litchar{,} (i.e., a comma).
The same shorthands can be used in expressions:

@examples[
`(1 2 `(,(+ 1 2) ,,(- 5 1)))
]

The shorthand form of @racket[unquote-splicing] is @litchar[",@"]:

@examples[
`(1 2 ,@(list (+ 1 2) (- 5 1)))
]
