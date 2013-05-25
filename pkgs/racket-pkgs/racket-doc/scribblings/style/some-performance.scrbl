#lang scribble/base

@(require "shared.rkt")

@; -----------------------------------------------------------------------------

@title{Language and Performance}

When you write a module, you first pick a language. In Racket you can
 choose a lot of languages. The most important choice concerns @rkt/base[]
 vs @rkt[].

For scripts, use @rkt/base[]. The @rkt/base[] language loads significantly
 faster than the @rkt[] language because it is much smaller than the
 @rkt[].

If your module is intended as a library, stick to @rkt/base[]. That way
 script writers can use it without incurring the overhead of loading all of
 @rkt[] unknowingly.

Conversely, you should use @rkt[] (or even @rkt/gui[]) when you just want a
 convenient language to write some program. The @rkt[] language comes with
 almost all the batteries, and @rkt/gui[] adds the rest of the GUI base.

@; -----------------------------------------------------------------------------
@section{Macros: Space and Performance}

Macro copy code. Also, Racket is really a tower of macro-implemented
 languages. Hence, a single line of source code may expand into a rather
 large core expression. As you and others keep adding macros, even the
 smallest functions generate huge expressions and consume a lot of space.
 This kind of space consumption may affect the performance of your project
 and is therefore to be avoided.

When you design your own macro with a large expansion, try to factor it
 into a function call that consumes small thunks or procedures.

@compare[
@racketmod0[#:file
@tt{good}
racket
...
(define-syntax (search s)
  (syntax-parse s
    [(_ x (e:expr ...)
        (~datum in)
        b:expr)
     #'(sar/λ (list e ...)
              (λ (x) b))]))

(define (sar/λ l p)
  (for ((a '())) ((y l))
    (unless (bad? y)
      (cons (p y) a))))

(define (bad? x)
  ... many lines ...)
...
]
@; -----------------------------------------------------------------------------
@(begin
#reader scribble/comment-reader
[racketmod0 #:file
@tt{bad}
racket
...
(define-syntax (search s)
  (syntax-parse s
    [(_ x (e:expr ...)
       (~datum in)
       b:expr)
     #'(begin
         (define (bad? x)
           ... many lines ...)
         (define l
	   (list e ...))
         (for ((a '())) ((x l))
           (unless (bad? x)
             (cons b a))))]))
]
)
]

As you can see, the macro on the left calls a function with a list of the
searchable values and a function that encapsulates the body. Every
expansion is a single function call. In contrast, the macro on the right
expands to many nested definitions and expressions every time it is used.
