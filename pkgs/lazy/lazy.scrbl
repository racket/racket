#lang scribble/doc
@(require (for-label (except-in lazy)
                     (only-in lazy/force ! !! !list !!list)
                     racket/contract
                     (only-in racket/promise promise?)))

@(define-syntax-rule (deflazy mod def id)
   (begin
     (def-mz-req mod id mz-id)
     @def[id]{Lazy variant of @|mz-id|.}))

@(define-syntax-rule (def-mz-req mod id in-mz-id)
   (begin
     (define-syntax-rule (intro mz-id)
       (begin
         (require (for-label (only-in mod id)))
         (define mz-id (racket id))))
     (intro in-mz-id)))

@(define-syntax-rule (defprocthing* mod id ...)
   (begin
     (deflazy mod defprocthing id)
     ...))

@(define-syntax-rule (defprocthing id . rest)
   (defthing id procedure? . rest))

@(define-syntax-rule (defidform* mod id ...)
   (begin
     (deflazy mod defidform id)
     ...))

@; ----------------------------------------

@(require scribble/manual)

@title{Lazy Racket}

@author["Eli Barzilay"]

@defmodulelang[lazy]

Lazy Racket is available as both a language level and a module that
can be used to write lazy code. To write lazy code, simply use
@racketmodname[lazy] as your module's language:

@racketmod[
lazy
... @#,elem{lazy code here}...]

Function applications are delayed, and promises are automatically
forced. The language provides bindings that are equivalent to most of
the @racketmodname[racket/base] and @racketmodname[racket/list]
libraries. Primitives are strict in the expected places; struct
constructors are lazy; @racket[if], @racket[and], @racket[or] @|etc|
are plain (lazy) functions. Strict functionality is provided as-is:
@racket[begin], I/O, mutation, parameterization, etc. To have your
code make sense, you should chain side effects in @racket[begin]s,
which will sequence things properly. (Note: This is similar to
threading monads through your code---only use @racket[begin] where
order matters.)

Mixing lazy and strict code is simple: you just write the lazy code in
the lazy language, and strict code as usual. The lazy language treats
imported functions (those that were not defined in the lazy language)
as strict, and on the strict side you only need to force (possibly
recursively) through promises.

A few side-effect bindings are provided as-is. For example,
@racket[read] and @racket[printf] do the obvious thing---but note that
the language is a call-by-need, and you need to be aware when promises
are forced. There are also bindings for @racket[begin] (delays a
computation that forces all sub-expressions), @racket[when],
@racket[unless], etc. There are, however, less reliable and might
change (or be dropped) in the future.

There are a few additional bindings, the important ones are special
forms that force strict behaviour---there are several of these that
are useful in forcing different parts of a value in different ways, as
described in @secref["forcing"].

@; ----------------------------------------

@section{Lazy Forms and Functions}

@defidform*[mzscheme
lambda
define
]

@defidform*[scheme
let
let*
letrec
parameterize
define-values
let-values
let*-values
letrec-values
if
set!
begin begin0 when unless
cond case
]

@defprocthing*[scheme
   values make-struct-type
   cons list list* vector box
   and or
   set-mcar! set-mcdr! vector-set! set-box!
   error printf fprintf display write print
   eq? eqv? equal?
   list? length list-ref list-tail append map for-each andmap ormap
   member memq memv assoc assq assv reverse
   caar cadr cdar cddr caaar caadr cadar caddr cdaar cdadr cddar cdddr caaaar
   caaadr caadar caaddr cadaar cadadr caddar cadddr cdaaar cdaadr cdadar cdaddr
   cddaar cddadr cdddar cddddr
   first second third fourth fifth sixth seventh eighth rest cons? empty empty?
   foldl foldr last-pair remove remq remv remove* remq* remv* memf assf filter
   sort
   true false boolean=? symbol=? compose build-list
   take
]

@defprocthing[identity]{Lazy identity function.}

@defprocthing[cycle]{Creates a lazy infinite list that repeats its
input arguments in order.}

@; ----------------------------------------

@section[#:tag "forcing"]{Forcing Values}

@defmodule[lazy/force]

The bindings of @racketmodname[lazy/force] are re-provided by
@racketmodname[lazy].

@defproc[(! [expr any/c]) any/c]{

Evaluates @racket[expr] strictly. The result is always forced, over
and over until it gets a non-promise value.}


@defproc[(!! [expr any/c]) any/c]{

Similar to @racket[!], but recursively forces a structure (e.g:
lists).}


@defproc[(!list [expr (or/c promise? list?)]) list?]{

Forces the @racket[expr] which is expected to be a list, and forces
the @racket[cdr]s recursively to expose a proper list structure.}


@defproc[(!!list [expr (or/c promise? list?)]) list?]{

Similar to @racket[!list] but also forces (using @racket[!]) the
elements of the list.}


@;{ This moved into lazy.rkt, and all the other forces will move there too.

@subsection{Multiple values}

To avoid dealing with multiple values, they are treated as a single
tuple in the lazy language. This is implemented as a
@racket[multiple-values] struct, with a @racket[values] slot.

@defproc[(split-values [x multiple-values?]) any]{

Used to split such a tuple to actual multiple values. (This may change
in the future.)}


@defproc[(!values [expr (or/c promise? multiple-values?)]) any]{

Forces @racket[expr] and uses @racket[split-values] on the result.}


@defproc[(!!values [expr (or/c promise? multiple-values?)]) any]{

Similar to @racket[!values], but forces each of the values
recursively.}

;}
