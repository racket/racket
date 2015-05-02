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
@section{Library Interfaces}

As the set of modules for a library grows, you may find that not all
features and modules are likely to be used by all programs that use
your library. If, by default, your library includes all features, then
you can cause unnecessary mental stress and runtime cost for features
that the programs do not actually use. In building the Racket
language, we've found it useful to separate out libraries into
different layers so users can decide which bundles they need.

In Racket, the practice is to make the default (i.e. the most
prominent name) the module that includes "everything", namely
@rkt[]. Then, if users wish to have a smaller set of bindings, they
can choose to use @rkt/base[], which gives an explicit name to the
basic foundation of the library. Finally, some features are not even
included in @rkt[], such as @racketmodname[racket/require], and must
be required specially in all programs.

Other libraries in Racket choose the default name to be the small core
and give a special name for including everything.

We encourage library developers to think critically about these
decisions and decide on a practice that fits their taste and
understanding of the users of their library. We encourage developers
to use the following names for different places on the "size"
hierarchy:

@itemlist[

@item{@racket[library/kernel] -- The bare minimal conceievable for
the library to be usable. (Most libraries will not need this level.)}

@item{@racket[library/base] -- A basic set of functionality.}

@item{@racket[library] -- An appropriate "default" of functionality
corresponding to either @racket[library/base] or
@racket[library/full].}

@item{@racket[library/full] -- All available functionality.}

]

If all Racket developers use similar names and think deeply about
these decisions, we can make it easier for Racket users to make wise
dependency decisions.

Finally, the advice of the previous section, to use @rkt/base[] when
building a library, generalizes to other libraries: by being more
specific in your dependencies, you are a responsible citizen and
enable others to have a small (transitive) dependency set.

@; -----------------------------------------------------------------------------
@section{Macros: Space and Performance}

Macros copy code. Also, Racket is really a tower of macro-implemented
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
