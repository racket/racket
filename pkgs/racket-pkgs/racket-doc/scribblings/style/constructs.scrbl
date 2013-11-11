#lang scribble/base

@(require "shared.rkt" scribble/eval)

@title{Choosing the Right Construct}

Racket provides a range of constructs for the same or similar purposes.
Although the Racket designers don't think that there is one right way for
everything, we prefer certain constructs in certain situations for consistency
and readability.

@; -----------------------------------------------------------------------------
@section{Comments}

Following Lisp and Scheme tradition, we use a single semicolon for in-line
comments (to the end of a line) and two semicolons for comments that start
a line. @margin-note*{This request does not contradict the programs in this
document. They use two semicolons for full-line comments in source but
scribble renders only one.} Think of the second semicolon as making an
emphatic point.

Seasoned Schemers, not necessarily Racketeers, also use triple and
quadruple semicolons. This is considered a courtesy to distinguish file
headers from section headers.

In addition to ``;'', we have two other mechanisms for commenting code:
 ``#|...|#'' for blocks and ``#;'' to comment out an expression.
 @defterm{Block comments} are for those rare cases when an entire block of
 definitions and/or expressions must be commented out at once.
 @defterm{Expression comments}---``#;''---apply to the following
 S-expression.  This makes them a useful tool for debugging.  They can even
 be composed in interesting ways with other comments, for example, ``#;#;''
 will comment two expressions, and a line with just ``;#;'' gives you a
 single-character ``toggle'' for the expression that starts on the next
 line.  But on the flip side, many tools don't process them
 properly---treating them instead as a ``#'' followed by a commented line.
 For example, in DrRacket S-expression comments are ignored when it comes
 to syntax coloring, which makes it easy to miss them. In Emacs, the
 commented text is colored like a comment and treated as text, which makes
 it difficult to edit as code.  The bottom line here is that ``#;''
 comments are useful for debugging, but try to avoid leaving them in
 committed code.  If you really want to use ``#;'', clarify their use with
 a line comment (``;'').

@; -----------------------------------------------------------------------------
@section{Definitions}

Racket comes with quite a few definitional constructs, including
@scheme[let], @scheme[let*], @scheme[letrec], and @scheme[define]. Except
for the last one, definitional constructs increase the indentation level.
Therefore, favor @scheme[define] when feasible.

@compare[
@racketmod0[#:file
@tt{good}
racket

(define (swap x y)
  (define t (unbox x))
  (set-box! x (unbox y))
  (set-box! y t))
]
@; -----------------------------------------------------------------------------
@racketmod0[#:file
@tt{bad}
racket

(define (swap x y)
  (let ([t (unbox x)])
    (set-box! x (unbox y))
    (set-box! y t)))
]
]

@bold{Warning} A @racket[let*] binding block is not easily replaced with a
series of @racket[define]s because the former has @emph{sequential} scope
and the latter has @emph{mutually recursive} scope.
@compare[
@racketmod0[#:file
@tt{works}
racket
(define (print-two f)
  (let* ([_ (print (first f))]
	 [f (rest f)]
	 [_ (print (first f))]
	 [f (rest f)])
    (code:comment "IN")
    f))
]
@; -----------------------------------------------------------------------------
@racketmod0[#:file

@tt{does @bold{not}}
racket

(define (print-two f)
   (print (first f))
   (define f (rest f))
   (print (first f))
   (define f (rest f))
   (code:comment "IN")
   f)
]
]

@; -----------------------------------------------------------------------------
@section{Conditionals}

Like definitional constructs, conditionals come in many flavors,
too. Because @scheme[cond] and its relatives (@scheme[case],
@scheme[match], etc) now allow local uses of @scheme[define], you should
prefer them over @scheme[if].

@compare[
@racketmod0[#:file
@tt{good}
racket

(cond
  [(empty? l) #false]
  [else
   (define f (fir l))
   (define r (rest l))
   (if (discounted? f)
       (rate f)
       (curved (g r)))])
]
@racketmod0[#:file
@tt{bad}
racket

(if (empty? l)
    #false
    (let ([f (fir l)]
	  [r (rest l)])
      (if (discounted? f)
          (rate f)
          (curved (g r)))))
]
]

Also, use @racket[cond] instead of @racket[if] to eliminate explicit
 @racket[begin].

The above ``good'' example would be even better with @racket[match]. In
 general, use @racket[match] to destructure complex pieces of data.

You should also favor @scheme[cond] (and its relatives) over @scheme[if] to
 match the shape of the data definition. In particular, the above examples
 could be formulated with @racket[and] and @racket[or] but doing so would
 not bring across the recursion as nicely.

@; -----------------------------------------------------------------------------
@section{Expressions}

Don't nest expressions too deeply. Instead name intermediate results. With
well-chosen names your expression becomes easy to read.

@compare[
@racketmod0[#:file
@tt{good}
racket
(define (next-month date)
  (define day (first date))
  (define month (second date))
  (if (= month 12)
      `(,(+ day 1) 1)
      `(,day ,(+ month 1))))
]
@; -----------------------------------------------------------------------------
@racketmod0[#:file
@tt{bad}
racket
(define (next-month d)
  (if (= (cadr d) 12)
      `(,(+ (car d) 1)
	1
	,(caddr d))
      `(,(car d)
	,(+ (cadr d) 1))))
]
]
 Clearly ``too deeply'' is subjective. On occasion it also isn't the
 nesting that makes the expression unreadable but the sheer number of
 subexpressions. Consider using local definitions for this case, too.

@; -----------------------------------------------------------------------------
@section{Structs vs Lists}

Use @racket[struct]s when you represent a combination of a small and fixed
number of values.  For fixed length (long) lists, add a comment or even a
contract that states the constraints.

If a function returns several results via @racket[values], consider using
@racket[struct]s or lists when you are dealing with four or more values.

@; -----------------------------------------------------------------------------
@section{Lambda vs Define}

While nobody denies that @racket[lambda] is cute, @racket[define]d
functions have names that tell you what they compute and that help
accelerate reading.

@compare[
@racketmod0[#:file
@tt{good}
racket

(define (process f)
  (define (complex-step x)
    ... 10 lines ...)
  (map complext-step
       (to-list f)))
]
@; -----------------------------------------------------------------------------
@racketmod0[#:file
@tt{bad}
racket

(define (process f)
  (map (lambda (x)
	 ... 10 lines ...)
       (to-list f)))
]
]

Even a curried function does not need @racket[lambda].
@compare[
@racketmod0[#:file
@tt{good}
racket

(define ((cut fx-image) image2)
  ...)
]
@; -----------------------------------------------------------------------------
@racketmod0[#:file
@tt{acceptable}
racket

(define (cut fx-image)
  (lambda (image2)
    ...))
]
]
 The left side signals currying in the very first line of the function,
 while the reader must read two lines for the version on the right side.

Of course, many constructs (call-with ..) or higher-order functions
(filter) are made for short @racket[lambda]; don't hesitate to use
@racket[lambda] for such cases.


@; -----------------------------------------------------------------------------
@section{Identity Functions}

The identity function is @racket[values]:

 @examples[
 (map values '(a b c))
 (values 1 2 3)
 ]

@; -----------------------------------------------------------------------------
@section{Traversals}

With the availability of @racket[for/fold], @racket[for/list],
 @racket[for/vector], and friends, programming with @racket[for] loops
 has become just as functional as programming with @racket[map] and
 @racket[foldr]. With @racket[for*] loops, filter, and termination clauses
 in the iteration specification, these loops are also far more concise than
 explicit traversal combinators. And with @racket[for] loops, you can
 decouple the traversal from lists.

@margin-note*{See also @racket[for/sum] and @racket[for/product] in Racket.}
@compare[
@;%
@(begin
#reader scribble/comment-reader
[racketmod0 #:file
@tt{good}
racket

;; [Sequence X] -> Number
(define (sum-up s)
  (for/fold ((sum 0)) ((x s))
    (+ sum x)))

;; examples:
(sum-up '(1 2 3))
(sum-up #(1 2 3))
(sum-up
  (open-input-string
    "1 2 3"))
])
@; -----------------------------------------------------------------------------
@;%
@(begin
#reader scribble/comment-reader
[racketmod0 #:file
@tt{bad}
racket

;; [Listof X] -> Number
(define (sum-up alist)
  (foldr (lambda (x sum)
            (+ sum x))
          0
          alist))

;; example:
(sum-up '(1 2 3))
])
]
 In this example, the @racket[for] loop on the left comes with two
 advantages. First, a reader doesn't need to absorb an intermediate
 @racket[lambda]. Second, the @racket[for] loop naturally generalizes to
 other kinds of sequences. Naturally, the trade-off here is a loss of
 efficiency; using @racket[in-list] to restrict the @tt{good} example to
 the same range of data as the @tt{bad} one speeds up the former.

 @bold{Note}: @racket[for] traversals of user-defined sequences tend to be
 slow. If performance matters in these cases, you may wish to fall back on
 your own traversal functions.

@; -----------------------------------------------------------------------------
@section{Functions vs Macros}

Define functions when possible, Or, do not introduce macros when functions
will do.

@compare[
@racketmod0[#:file
@tt{good}
racket
...
;; Message -> String
(define (name msg)
  (first (second msg)))
]
@; -----------------------------------------------------------------------------
@(begin
#reader scribble/comment-reader
[racketmod0 #:file
@tt{bad}
racket
...
;; Message -> String
(define-syntax-rule (name msg)
  (first (second msg)))
]
)
]
 A function is immediately useful in a higher-order context. For a macro,
 achieving the same goal takes a lot more work.


@; -----------------------------------------------------------------------------
@section{Exceptions}

When you handle exceptions, specify the exception as precisely as
possible.

@compare[
@racketmod0[#:file
@tt{good}
racket
...
(code:comment #, @t{FN [X -> Y] FN -> Void})
(define (convert in f out)
  (with-handlers
      ((exn:fail:read? X))
    (with-output-to out
      (writer f))))

(code:comment #, @t{may raise exn:fail:read})
(define ((writer f))
 (with-input-from in
   (reader f)))

(code:comment #, @t{may raise exn:fail:read})
(define ((reader f))
 ... f ...)
]
@; -----------------------------------------------------------------------------
@racketmod0[#:file
@tt{bad}
racket
...
(code:comment #, @t{FN [X -> Y] FN -> Void})
(define (convert in f out)
  (with-handlers
      (((code:hilite (lambda _ #t)) X))
    (with-output-to out
      (writer f))))

(code:comment #, @t{may raise exn:fail:read})
(define ((writer f))
 (with-input-from in
   (reader f)))

(code:comment #, @t{may raise exn:fail:read})
(define ((reader f))
 ... f ...)
]
]
 Using @racket[(lambda _ #t)] as an exception predicate suggests to the
 reader that you wish to catch every possible exception, including failure
 and break exceptions. Worse, the reader may think that you didn't remotely
 consider what exceptions you @emph{should} be catching.

It is equally bad to use @racket[exn?] as the exception predicate  even if
 you mean to catch all kinds of failures. Doing so catches break
 exceptions, too. To catch all failures, use @racket[exn:fail?] as shown on
 the left:
@compare[
@racketmod0[#:file
@tt{good}
racket
...
(code:comment #, @t{FN [X -> Y] FN -> Void})
(define (convert in f out)
  (with-handlers
      ((exn:fail? X))
    (with-output-to out
      (writer f))))

(code:comment #, @t{may raise exn:fail:read})
(define ((writer f))
 (with-input-from in
   (reader f)))

(code:comment #, @t{may raise exn:fail:read})
(define ((reader f))
 ... f ...)
]
@racketmod0[#:file
@tt{bad}
racket
...
(code:comment #, @t{FN [X -> Y] FN -> Void})
(define (convert in f out)
  (with-handlers
      (((code:hilite exn?) X))
    (with-output-to out
      (writer f))))

(code:comment #, @t{may raise exn:fail:read})
(define ((writer f))
 (with-input-from in
   (reader f)))

(code:comment #, @t{may raise exn:fail:read})
(define ((reader f))
 ... f ...)
]
]

Finally, a handler for a @racket[exn:fail?] clause should never
 succeed for all possible failures because it silences all kinds of
 exceptions that you probably want to see:
@codebox[
@racketmod0[#:file
@tt{bad}
racket
...
(code:comment #, @t{FN [X -> Y] FN -> Void})
(define (convert in f out)
  (with-handlers ((exn:fail? handler))
    (with-output-to out
      (writer f))))

(code:comment #, @t{Exn -> Void})
(define (handler e)
  (cond
    [(exn:fail:read? e)
     (displayln "drracket is special")]
    [else (void)]))

(code:comment #, @t{may raise exn:fail:read})
(define ((writer f))
 (with-input-from in
   (reader f)))

(code:comment #, @t{may raise exn:fail:read})
(define ((reader f))
 ... f ...)
]
]
 If you wish to deal with several different kind of failures, say
 @racket[exn:fail:read?] and @racket[exn:fail:network?], use distinct
 clauses in @racket[with-handlers] to do so and distribute the branches of
 your conditional over these clauses.

@; -----------------------------------------------------------------------------
@section{Parameters}

If you need to set a parameter, use @racket[parameterize]:

@compare[
@racketmod0[#:file
@tt{good}
racket
...
(define cop
  current-output-port)

(code:comment #, @t{String OPort -> Void})
(define (send msg op)
  (parameterize ((cop op))
    (display msg))
  (record msg))
]
@; -----------------------------------------------------------------------------
@racketmod0[#:file
@tt{bad}
racket
...
(define cop
  current-output-port)

(code:comment #, @t{String OPort -> Void})
(define (send msg op)
  (define cp (cop))
  (cop op)
  (display msg)
  (cop cp)
  (record msg))
]
]

As the comparison demonstrates, @racket[parameterize] clearly delimits the
extent of the change, which is an important idea for the reader. In
addition, @racket[parameterize] ensures that your code is more likely to
work with continuations and threads, an important idea for Racket
programmers.


@section{Plural}

Avoid plural when naming collections and libraries. Use @racketmodname[racket/contract]
and @racketmodname[data/heap], not @tt{racket/contracts} or @tt{data/heaps}.
