#lang scribble/doc
@(require (for-label (except-in lazy delay force promise?)
                     (only-in lazy/force
                              ! !! !!!
                              !list !!list
                              split-values !values !!values)
                     (only-in lazy/promise
                              delay force lazy promise?)))

@(begin
  (define-syntax-rule (def-scheme scheme-force scheme-delay scheme-promise?)
    (begin
     (require (for-label scheme/promise))
     (define scheme-force (scheme force))
     (define scheme-delay (scheme delay))
     (define scheme-promise? (scheme promise?))))
  (def-scheme scheme-force scheme-delay scheme-promise?))

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

@title{@bold{Lazy Scheme}}

@defmodulelang[lazy]

Lazy Scheme is available as both a language level and a module that
can be used to write lazy code. To write lazy code, simply use
@schememodname[lazy] as your module's language:

@schememod[
lazy
... #, @elem{lazy code here}...]

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


@defproc[(!!! [expr any/c]) any/c]{

Similar to @scheme[!!], but also wraps procedures that if finds so
their outputs are forced (so they are useful in a strict world).}


@defproc[(!list [expr (or/c promise? list?)]) list?]{

Forces the @scheme[expr] which is expected to be a list, and forces
the @scheme[cdr]s recursively to expose a proper list structure.}


@defproc[(!!list [expr (or/c promise? list?)]) list?]{

Similar to @scheme[!list] but also forces (using @scheme[!]) the
elements of the list.}


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

@; ----------------------------------------

@section{Promises}

@defmodule[lazy/promise]

The @schememodname[lazy/promise] module implements lazy promises as
implicitly used by the @schememodname[lazy] language.

Note: this module implements a new kind of promises.  MzScheme's
promises are therefore treated as other values---which means that they
are not forced by this module's @scheme[force].

Generally speaking, if you use only @scheme[delay], @scheme[force],
and @scheme[promise?], you get the same functionality as in Scheme.
See below for two (relatively minor) differences.

@scheme[lazy] implements a new kind of promise. When used with
expressions, it behaves like @scheme[delay].  However, when
@scheme[lazy] is used with an expression that already evaluates to a
promise, it combines with it such that @scheme[force] will go through
both promises.  In other words, @scheme[(lazy _expr)] is equivalent to
@scheme[(lazy (lazy _expr))].  The main feature of this implementation
of promises is that @scheme[lazy] is safe-for-space (see
@link["http://srfi.schemers.org/srfi-45/"]{SRFI-45} for
details)---this is crucial for tail-recursion in Lazy Scheme.

To summarize, a sequence of @scheme[lazy]s is forced with a single use
of @scheme[force], and each additional @scheme[delay] requires an
additional @scheme[force]---for example, @scheme[(lazy (delay (lazy
(delay (lazy _expr)))))] requires three @scheme[force]s to evaluate
@scheme[_expr].

Note: @scheme[lazy] cannot be used with an expression that evaluates
to multiple values. @scheme[delay], however, is fine with multiple
values. (This is for efficiency in the lazy language, where multiple
values are avoided.)

As mentioned above, using @scheme[delay] and @scheme[force] is as in
Scheme, except for two differences. The first is a
technicality---@scheme[force] is an identity for non-promise values.
This makes it more convenient in implementing the lazy language, where
there is no difference between a value and a promise.

The second difference is that circular (re-entrant) promises are not
permitted (i.e., when a promise is being forced, trying to force it in
the process will raise an error).  For example, the following code
(see srfi-45 for additional examples):

@schemeblock[
(let ([count 5])
  (define p
    (delay (if (<= count 0)
               count
               (begin (set! count (- count 1))
                      (force p)))))
  (force p))]

returns 0 with Scheme's @|scheme-delay|/@|scheme-force|, but aborts
with an error with this module's promises. This restriction leads to
faster code (see
@link["http://srfi.schemers.org/srfi-45/post-mail-archive/msg00011.html"]{a
SRFI-45 discussion post} for some additional details), while
preventing diverging code (the only reasonable way to use circular
promises is using mutation as above).


@defform[(delay expr)]{Similar in functionality to Scheme's @|scheme-delay|}


@defform[(lazy expr)]{Creates a ``lazy'' promise. See above for
details.}


@defproc[(force [x any/c]) any/c]{

Forces a promise that was generated by @scheme[delay] or
@scheme[lazy]. Similar to Scheme's @|scheme-force|, except that
non-promise values are simply returned.}


@defproc[(promise? [x any/c]) boolean?]{

A predicate for promise values.}

@; ----------------------------------------

@section{MzScheme without Promises}

@defmodule[lazy/mz-without-promises]

The @schememodname[lazy/mz-without-promises] module simply provides
all of @schememodname[mzscheme] except for promise-related
functionality: @|scheme-delay|, @|scheme-force|, and
@|scheme-promise?|. This is because @schememodname[lazy/promise]
defines and provides the same names. It is intended as a helper, but
you can use it together with @schememodname[lazy/promise] to get a
@schememodname[mzscheme]-like language where promises are implemented
by @schememodname[lazy/promise].
