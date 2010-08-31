#lang scribble/doc
@(require scribble/manual
          scribble/struct
          scribble/decode
          scribble/eval
          "parse-common.rkt"
          (for-label racket/class))

@title{Optional keyword arguments}

This section explains how to write a macro that accepts (simple)
optional keyword arguments. We use the example @scheme[mycond], which
is like Racket's @scheme[cond] except that it takes an optional
keyword argument that controls what happens if none of the clauses
match.

Optional keyword arguments are supported via @tech{head
patterns}. Unlike normal patterns, which match one term, head patterns
can match a variable number of subterms in a list. Some important
head-pattern forms are @scheme[~seq], @scheme[~or], and
@scheme[~optional].

Here's one way to do it:

@myinteraction[
(define-syntax (mycond stx)
  (syntax-parse stx
    [(mycond (~or (~seq #:error-on-fallthrough who:expr) (~seq))
             clause ...)
     (with-syntax ([error? (if (attribute who) #'#t #'#f)]
                   [who (or (attribute who) #'#f)])
       #'(mycond* error? who clause ...))]))

(define-syntax mycond*
  (syntax-rules ()
    [(mycond error? who [question answer] . clauses)
     (if question answer (mycond* error? who . clauses))]
    [(mycond #t who)
     (error who "no clauses matched")]
    [(mycond #f _)
     (void)]))
]

We cannot write @scheme[#'who] in the macro's right-hand side, because
the @scheme[who] attribute does not receive a value if the keyword
argument is omitted. Instead we must write @scheme[(attribute who)],
which produces @scheme[#f] if matching did not assign a value to the
attribute.

@myinteraction[
(mycond [(even? 13) 'blue]
        [(odd? 4) 'red])
(mycond #:error-on-fallthrough 'myfun
        [(even? 13) 'blue]
        [(odd? 4) 'red])
]

There's a simpler way of writing the @scheme[~or] pattern above:
@schemeblock[
(~optional (~seq #:error-on-fallthrough who:expr))
]

Yet another way is to introduce a @tech{splicing syntax class}, which
is like an ordinary syntax class but for head patterns.
@myinteraction[
(define-syntax (mycond stx)

  (define-splicing-syntax-class maybe-fallthrough-option
    (pattern (~seq #:error-on-fallthough who:expr)
             #:with error? #'#t)
    (pattern (~seq)
             #:with error? #'#f
             #:with who #'#f))

  (syntax-parse stx
    [(mycond fo:maybe-fallthrough-option clause ...)
     #'(mycond* fo.error? fo.who clause ...)]))
]

Defining a splicing syntax class also makes it easy to eliminate the
case analysis we did before using @scheme[attribute] by defining
@scheme[error?] and @scheme[who] as attributes within both of the
syntax class's variants. (This is possible to do in the inline pattern
version too, using @scheme[~and] and @scheme[~parse], just less
convenient.) Splicing syntax classes also closely parallel the style
of grammars in macro documentation.
