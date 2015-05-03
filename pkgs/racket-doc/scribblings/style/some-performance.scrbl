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

Imagine you are working on a library. You start with one module, but before
you know it the set of modules grows to a decent size. Client programs are
unlikely to use all of your library's exports and modules. If, by default,
your library includes all features, you may cause unnecessary mental stress
and run-time cost that clients do not actually use. 

In building the Racket language, we have found it useful to factor
libraries into different layers so that client programs can selectively
import from these bundles. The specific Racket practice is to use the most
prominent name as the default for the module that includes everything. When
it comes to languages, this is the role of @rkt[]. A programmer who wishes
to depend on a small part of the language chooses to @rkt/base[] instead;
this name refers to the basic foundation of the language. Finally, some of
Racket's constructs are not even included in @rkt[]---consider
@racketmodname[racket/require] for example---and must be required
explicitly in programs.

Other Racket libraries choose to use the default name for the small
core. Special names then refer to the complete library. 

We encourage library developers to think critically about these
decisions and decide on a practice that fits their taste and
understanding of the users of their library. We encourage developers
to use the following names for different places on the "size"
hierarchy:

@itemlist[

@item{@racket[library/kernel], the bare minimal conceievable for the
library to be usable;}

@item{@racket[library/base], a basic set of functionality.}

@item{@racket[library], an appropriate "default" of functionality
corresponding to either @racket[library/base] or @racket[library/full].}

@item{@racket[library/full], the full library functionality.}  
] 
Keep two considerations in mind as you decide which parts of your library
should be in which files: dependency and logical ordering.  The smaller
files should depend on fewer dependencies. Try to organize the levels so
that, in principle, the larger libraries can be implemented in terms of the
public interfaces of the smaller ones. 

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
