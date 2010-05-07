#lang scribble/doc
@(require (for-label (except-in lazy delay force)
                     (only-in lazy/force ! !! !list !!list)
                     scheme/contract
                     (only-in scheme/promise promise?)))

@(define-syntax-rule (deflazy mod def id)
   (begin
     (def-mz-req mod id mz-id)
     @def[id]{Lazy variant of @|mz-id|.}))

@(define-syntax-rule (def-mz-req mod id in-mz-id)
   (begin
     (define-syntax-rule (intro mz-id)
       (begin
         (require (for-label (only-in mod id)))
         (define mz-id (scheme id))))
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

@title{@bold{Lazy Racket}}

@author["Eli Barzilay"]

@defmodulelang[lazy]

Lazy Racket is available as both a language level and a module that
can be used to write lazy code. To write lazy code, simply use
@schememodname[lazy] as your module's language:

@schememod[
lazy
... @#,elem{lazy code here}...]

Function applications are delayed, and promises are automatically
forced. The language provides bindings that are equivalent to most of
the @schememodname[mzscheme] and @schememodname[scheme/list]
libraries. Primitives are strict in the expected places; struct
constructors are lazy; @scheme[if], @scheme[and], @scheme[or] @|etc|
are plain (lazy) functions. Strict functionality is provided as-is:
@scheme[begin], I/O, mutation, parameterization, etc. To have your
code make sense, you should chain side effects in @scheme[begin]s,
which will sequence things properly. (Note: This is similar to
threading monads through your code---only use @scheme[begin] where
order matters.)

Mixing lazy and strict code is simple: you just write the lazy code in
the lazy language, and strict code as usual. The lazy language treats
imported functions (those that were not defined in the lazy language)
as strict, and on the strict side you only need to force (possibly
recursively) through promises.

A few side-effect bindings are provided as-is. For example,
@scheme[read] and @scheme[printf] do the obvious thing---but note that
the language is a call-by-need, and you need to be aware when promises
are forced. There are also bindings for @scheme[begin] (delays a
computation that forces all sub-expressions), @scheme[when],
@scheme[unless], etc. There are, however, less reliable and might
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

@defprocthing[cycle]{Creates a lazy infinite list given a list of
elements to repeat in order.}

@; ----------------------------------------

@section[#:tag "forcing"]{Forcing Values}

@defmodule[lazy/force]

The bindings of @schememodname[lazy/force] are re-provided by
@schememodname[lazy].

@defproc[(! [expr any/c]) any/c]{

Evaluates @scheme[expr] strictly. The result is always forced, over
and over until it gets a non-promise value.}


@defproc[(!![expr any/c]) any/c]{

Similar to @scheme[!], but recursively forces a structure (e.g:
lists).}


@defproc[(!list [expr (or/c promise? list?)]) list?]{

Forces the @scheme[expr] which is expected to be a list, and forces
the @scheme[cdr]s recursively to expose a proper list structure.}


@defproc[(!!list [expr (or/c promise? list?)]) list?]{

Similar to @scheme[!list] but also forces (using @scheme[!]) the
elements of the list.}


@;{ This moved into lazy.ss, and all the other forces will move there too.

@subsection{Multiple values}

To avoid dealing with multiple values, they are treated as a single
tuple in the lazy language. This is implemented as a
@scheme[multiple-values] struct, with a @scheme[values] slot.

@defproc[(split-values [x multiple-values?]) any]{

Used to split such a tuple to actual multiple values. (This may change
in the future.)}


@defproc[(!values [expr (or/c promise? multiple-values?)]) any]{

Forces @scheme[expr] and uses @scheme[split-values] on the result.}


@defproc[(!!values [expr (or/c promise? multiple-values?)]) any]{

Similar to @scheme[!values], but forces each of the values
recursively.}

;}
