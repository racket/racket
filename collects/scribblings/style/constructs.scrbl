#lang scribble/base

@(require "shared.rkt")

@title{Choosing the Right Construct}

Racket provides a range of constructs for the same or similar purposes.
Although the Racket designers don't think that there is one right way for
everything, we prefer certain constructs in certain situations for consistency
and readability.

@; -----------------------------------------------------------------------------
@section{Definitions}

Racket comes with quite a few definitional constructs, including
@scheme[let], @scheme[let*], @scheme[letrec], and @scheme[define]. Except
for the last one, definitional construct increase the indentation level. We
therefore request that you favor @scheme[define] when feasible.

@compare[
@racketmod[#:file
@tt{good}
racket

(define (swap x y)
  (define t (unbox x))
  (set-box! x (unbox y))
  (set-box! y t))
]
@; -----------------------------------------------------------------------------
@racketmod[#:file
@tt{bad}
racket

(define (swap x y)
  (let ([t (unbox x)])
    (set-box! x (unbox y))
    (set-box! y t)))
]
]

@compare[
@racketmod[#:file
@tt{good}
racket

(define-syntax (increment! stx)
  (syntax-case stx ()
    [(_ s sn fn i)
     (with-syntax ([w (r #'s)])
       (define g (ff #'sn #'w))
       ...)]))
]
@; -----------------------------------------------------------------------------
@racketmod[#:file
@tt{bad}
racket

(define-syntax (increment! stx)
  (syntax-case stx ()
    [(_ s sn fn i)
     (with-syntax ([w (r #'s)])
       (let ([g (ff #'sn #'w)])
         ...))]))
]
]

@; -----------------------------------------------------------------------------
@section{Conditionals}

Like definitional constructs, conditionals come in many flavors,
too. Because @scheme[cond] and its relatives (@scheme[case],
@scheme[match], etc) now allow local uses of @scheme[define], you should
prefer them over @scheme[if].

@compare[
@racketmod[#:file
@tt{good}
racket

(cond
  [(empty? l) true]
  [else
   (define f (fir l))
   (define r (rest l))
   (and (flat-rate f)
        (curved f (chk r)))])
]
@racketmod[#:file
@tt{bad}
racket

(if (empty? l)
    true
    (let ([f (fir l)]
	  [r (rest l)])
      (and (flat-rate f)
	   (curved f (chk r)))))
]
]

Of course you should also favor @scheme[cond] (and its relatives) over
@scheme[if] to match the shape of the data definition.

@; -----------------------------------------------------------------------------
@section{Expressions}

Keep expressions small. Name intermediate results.

@compare[
@racketmod[#:file
@tt{good}
racket
(define (distance0 p)
  (define x (posn-x p))
  (define y (posn-y p))
  (sqrt (+ (sqr x) (sqr y))))
]
@; -----------------------------------------------------------------------------
@racketmod[#:file
@tt{bad}
racket
(define (distance0 p)
  (sqrt
    (+ (sqr (posn-x p))
       (sqr (posn-y p)))))
]
]

@; -----------------------------------------------------------------------------
@section{Structs vs Lists}

Use @racket[struct]s when you represent a combination of a fixed number of
values.  Don't use lists.

@; -----------------------------------------------------------------------------
@section{Lambda vs Define}

While nobody denies that @racket[lambda] is cute, @racket[define]d
functions have names that tell you what they compute and that helps
accelerate reading.

@compare[
@racketmod[#:file
@tt{good}
racket

(define (process f)
  (define (complex-step x)
    ... 10 lines ...)
  (map complext-step
       (to-list f)))
]
@; -----------------------------------------------------------------------------
@racketmod[#:file
@tt{bad}
racket

(define (process f)
  (map (lambda (x)
	 ... 10 lines ...)
       (to-list f)))
]
]


@; -----------------------------------------------------------------------------
@section{List Traversals}

With the availability of @racket[for/fold], @racket[for/list],
 @racket[for/vector], and friends, programming with for @racket[for] loops
 has become just as functional as programming with @racket[map] and
 @racket[foldr]. With @racket[for*] loops, filter, and termination clauses
 in the iteration specification, these loops are also far more concise than
 explicit traversal combinators. And with @racket[for] loops, you can
 decouple the traversal from lists.

@compare[
@;%
@(begin
#reader scribble/comment-reader
[racketmod #:file
@tt{good}
racket

;; [Sequence X] -> Number
(define (sum-up s)
  (for/fold ((sum 0)) ((x s))
    (+ sum x)))

;; examples:
(sum-up '(1 2 3))
(sum-up #(1 2 3))
(sum-up (open-input-string "1 2 3"))
])

@; -----------------------------------------------------------------------------
@;%
@(begin
#reader scribble/comment-reader
[racketmod #:file
@tt{bad}
racket

;; [Listof X] -> Number
(define (sum-up s)
  (for/fold ((sum 0)) ((x s))
    (+ sum x)))

;; example:
(sum-up '(1 2 3))
])
]

 Note: @racket[for] traversals of user-defined sequences tend to be
 slow. If performance matters in these cases, you may wish to fall back on
 your own traversal functions.

@; -----------------------------------------------------------------------------
@section{Functions vs Macros}

Use functions when possible; do not introduce macros.

@compare[
@racketmod[#:file
@tt{good}
racket
...
;; Message -> String
(define (message-name msg)
  (first (second msg)))
]
@; -----------------------------------------------------------------------------
@racketmod[#:file
@tt{bad}
racket
...
;; Message -> String
(define-syntax-rule
  (message-name msg)
  ;; ===>
  (first (second msg)))
]
]

@; -----------------------------------------------------------------------------
@section{Parameters}

If you need to set a parameter, use @racket[parameterize]:

@compare[
@racketmod[#:file
@tt{good}
racket
...
;; String OutputPort -> Void
(define (send-to msg op)
  (parameterize
    ((current-output-port op))
    (format-and-display msg))
  (record-message-in-log msg))
]
@; -----------------------------------------------------------------------------
@racketmod[#:file
@tt{bad}
racket
...
;; String OutputPort -> Void
(define (send-to msg op)
  (define cp
    (current-output-port))
  (current-output-port op)
  (format-and-display msg)
  (current-output-port cp)
  (record-message-in-log msg))
]
]
