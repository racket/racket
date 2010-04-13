#lang scribble/manual
@(require scribble/eval "utils.ss" (for-label scheme/base unstable/srcloc unstable/location))

@(define unsyntax #f)

@(define (new-evaluator)
   (let* ([e (make-base-eval)])
     (e '(require (for-syntax scheme/base)
                  unstable/srcloc
                  unstable/location))
     e))

@(define evaluator (new-evaluator))

@(define reference-path
   '(lib "scribblings/reference/reference.scrbl"))

@title{Source Locations}

There are two libraries in this collection for dealing with source locations;
one for manipulating representations of them, and the other for quoting the
location of a particular piece of source code.

@section[#:tag "srcloc"]{Representations}

@defmodule[unstable/srcloc]

@unstable[@author+email["Carl Eastlund" "cce@ccs.neu.edu"]]

This module defines utilities for manipulating representations of source
locations, including both @scheme[srcloc] structures and all the values accepted
by @scheme[datum->syntax]'s third argument: syntax objects, lists, vectors, and
@scheme[#f].

@deftogether[(
@defproc[(source-location? [x any/c]) boolean?]{}
@defproc[(source-location-list? [x any/c]) boolean?]{}
@defproc[(source-location-vector? [x any/c]) boolean?]{}
)]{

These functions recognize valid source location representations.  The first,
@scheme[source-location?], recognizes @scheme[srcloc] structures, syntax
objects, lists, and vectors with appropriate structure, as well as @scheme[#f].
The latter predicates recognize only valid lists and vectors, respectively.

@examples[#:eval evaluator
(source-location? #f)
(source-location? #'here)
(source-location? (make-srcloc 'here 1 0 1 0))
(source-location? (make-srcloc 'bad 1 #f 1 0))
(source-location? (list 'here 1 0 1 0))
(source-location? (list* 'bad 1 0 1 0 'tail))
(source-location? (vector 'here 1 0 1 0))
(source-location? (vector 'bad 0 0 0 0))
]

}

@defproc[(check-source-location! [name symbol?] [x any/c]) void?]{

This procedure checks that its input is a valid source location.  If it is, the
procedure returns @scheme[(void)].  If it is not,
@scheme[check-source-location!] raises a detailed error message in terms of
@scheme[name] and the problem with @scheme[x].

@examples[#:eval evaluator
(check-source-location! 'this-example #f)
(check-source-location! 'this-example #'here)
(check-source-location! 'this-example (make-srcloc 'here 1 0 1 0))
(check-source-location! 'this-example (make-srcloc 'bad 1 #f 1 0))
(check-source-location! 'this-example (list 'here 1 0 1 0))
(check-source-location! 'this-example (list* 'bad 1 0 1 0 'tail))
(check-source-location! 'this-example (vector 'here 1 0 1 0))
(check-source-location! 'this-example (vector 'bad 0 0 0 0))
]

}

@deftogether[(
@defproc[(build-source-location [loc source-location?] ...) srcloc?]{}
@defproc[(build-source-location-list [loc source-location?] ...) source-location-list?]{}
@defproc[(build-source-location-vector [loc source-location?] ...) source-location-vector?]{}
@defproc[(build-source-location-syntax [loc source-location?] ...) syntax?]{}
)]{

These procedures combine multiple (zero or more) source locations, merging
locations within the same source and reporting @scheme[#f] for locations that
span sources.  They also convert the result to the desired representation:
@scheme[srcloc], list, vector, or syntax object, respectively.

@examples[#:eval evaluator
(build-source-location)
(build-source-location-list)
(build-source-location-vector)
(build-source-location-syntax)
(build-source-location #f)
(build-source-location-list #f)
(build-source-location-vector #f)
(build-source-location-syntax #f)
(build-source-location (list 'here 1 2 3 4))
(build-source-location-list (make-srcloc 'here 1 2 3 4))
(build-source-location-vector (make-srcloc 'here 1 2 3 4))
(build-source-location-syntax (make-srcloc 'here 1 2 3 4))
(build-source-location (list 'here 1 2 3 4) (vector 'here 5 6 7 8))
(build-source-location-list (make-srcloc 'here 1 2 3 4) (vector 'here 5 6 7 8))
(build-source-location-vector (make-srcloc 'here 1 2 3 4) (vector 'here 5 6 7 8))
(build-source-location-syntax (make-srcloc 'here 1 2 3 4) (vector 'here 5 6 7 8))
(build-source-location (list 'here 1 2 3 4) (vector 'there 5 6 7 8))
(build-source-location-list (make-srcloc 'here 1 2 3 4) (vector 'there 5 6 7 8))
(build-source-location-vector (make-srcloc 'here 1 2 3 4) (vector 'there 5 6 7 8))
(build-source-location-syntax (make-srcloc 'here 1 2 3 4) (vector 'there 5 6 7 8))
]

}

@defproc[(source-location-known? [loc source-location?]) boolean?]{

This predicate reports whether a given source location contains more information
than simply @scheme[#f].

@examples[#:eval evaluator
(source-location-known? #f)
(source-location-known? (make-srcloc #f #f #f #f #f))
(source-location-known? (make-srcloc 'source 1 2 3 4))
(source-location-known? (list #f #f #f #f #f))
(source-location-known? (vector 'source #f #f #f #f))
(source-location-known? (datum->syntax #f null #f))
(source-location-known? (datum->syntax #f null (list 'source #f #f #f #f)))
]

}

@deftogether[(
@defproc[(source-location-source [loc source-location?]) any/c]
@defproc[(source-location-line [loc source-location?])
         (or/c orexact-positive-integer? #f)]
@defproc[(source-location-column [loc source-location?])
         (or/c exact-nonnegative-integer? #f)]
@defproc[(source-location-position [loc source-location?])
         (or/c exact-positive-integer? #f)]
@defproc[(source-location-span [loc source-location?])
         (or/c exact-nonnegative-integer? #f)]
)]{

These accessors extract the fields of a source location.

@examples[#:eval evaluator
(source-location-source #f)
(source-location-line (make-srcloc 'source 1 2 3 4))
(source-location-column (list 'source 1 2 3 4))
(source-location-position (vector 'source 1 2 3 4))
(source-location-span (datum->syntax #f null (list 'source 1 2 3 4)))
]

}

@defproc[(source-location-end [loc source-location?])
         (or/c exact-nonnegative-integer? #f)]{

This accessor produces the end position of a source location (the sum of its
position and span, if both are numbers) or @scheme[#f].

@examples[#:eval evaluator
(source-location-end #f)
(source-location-end (make-srcloc 'source 1 2 3 4))
(source-location-end (list 'source 1 2 3 #f))
(source-location-end (vector 'source 1 2 #f 4))
]

}

@deftogether[(
@defproc[(source-location->string [loc source-location?]) string?]{}
@defproc[(source-location->prefix [loc source-location?]) string?]{}
)]{

These procedures convert source locations to strings for use in error messages.
The first produces a string describing the source location; the second appends
@scheme[": "] to the string if it is non-empty.

@examples[#:eval evaluator
(source-location->string (make-srcloc 'here 1 2 3 4))
(source-location->string (make-srcloc 'here #f #f 3 4))
(source-location->string (make-srcloc 'here #f #f #f #f))
(source-location->string (make-srcloc #f 1 2 3 4))
(source-location->string (make-srcloc #f #f #f 3 4))
(source-location->string (make-srcloc #f #f #f #f #f))
(source-location->prefix (make-srcloc 'here 1 2 3 4))
(source-location->prefix (make-srcloc 'here #f #f 3 4))
(source-location->prefix (make-srcloc 'here #f #f #f #f))
(source-location->prefix (make-srcloc #f 1 2 3 4))
(source-location->prefix (make-srcloc #f #f #f 3 4))
(source-location->prefix (make-srcloc #f #f #f #f #f))
]

}

@section[#:tag "location"]{Quoting}

@defmodule[unstable/location]

@unstable[@author+email["Carl Eastlund" "cce@ccs.neu.edu"]]

This module defines macros that evaluate to various aspects of their own source
location.

@emph{Note:} The examples below illustrate the use of these macros and the
representation of their output.  However, due to the mechanism by which they are
generated, each example is considered a single character and thus does not have
realistic line, column, and character positions.

Furthermore, the examples illustrate the use of source location quoting inside
macros, and the difference between quoting the source location of the macro
definition itself and quoting the source location of the macro's arguments.

@defform*[[(quote-srcloc) (quote-srcloc form) (quote-srcloc form #:module-source expr)]]{

Quotes the source location of @scheme[form] as a @scheme[srcloc]
structure, using the location of the whole @scheme[(quote-srcloc)]
expression if no @scheme[expr] is given. When @scheme[expr] has a
source module (in the sense of @scheme[syntax-source-module]), the
module's source path is used form source location, unless a
@scheme[#:module-source expr] is specified, in which case
@scheme[expr] provides the source.

@defexamples[#:eval (new-evaluator)
(quote-srcloc)
(define-syntax (not-here stx) #'(quote-srcloc))
(not-here)
(not-here)
(define-syntax (here stx) #`(quote-srcloc #,stx))
(here)
(here)
]

}

@deftogether[(
@defform*[[(quote-source-file) (quote-source-file form)]]
@defform*[[(quote-line-number) (quote-line-number form)]]
@defform*[[(quote-column-number) (quote-column-number form)]]
@defform*[[(quote-character-position) (quote-character-position form)]]
@defform*[[(quote-character-span) (quote-character-span form)]]
)]{

Quote various fields of the source location of @scheme[form], or of
the whole macro application if no @scheme[form] is given.

@examples[#:eval (new-evaluator)
(list (quote-source-file)
      (quote-line-number)
      (quote-column-number)
      (quote-character-position)
      (quote-character-span))
(define-syntax (not-here stx)
  #'(list (quote-source-file)
          (quote-line-number)
          (quote-column-number)
          (quote-character-position)
          (quote-character-span)))
(not-here)
(not-here)
(define-syntax (here stx)
  #`(list (quote-source-file #,stx)
          (quote-line-number #,stx)
          (quote-column-number #,stx)
          (quote-character-position #,stx)
          (quote-character-span #,stx)))
(here)
(here)
]

}

@defform[(quote-module-path)]{

Quotes a module path suitable for use with @scheme[require] which
refers to the module in which the macro application occurs.  If executed at the
top level, it may return @scheme['top-level], or it may return a valid module
path if the current namespace was constructed by @scheme[module->namespace]
(such as at the DrScheme interactions window).

The @scheme[quote-module-path] form operates by creating a @tech[#:doc reference-path]{variable
reference} (see @scheme[#%variable-reference]) at the point of its application.
It thus automatically describes its final expanded position, rather than the
module of any macro definition that happens to use it.

@defexamples[#:eval (new-evaluator)
(quote-module-path)
(module A scheme
  (require unstable/location)
  (define-syntax-rule (here) (quote-module-path))
  (define a (here))
  (provide a here))
(require 'A)
a
(module B scheme
  (require unstable/location)
  (require 'A)
  (define b (here))
  (provide b))
(require 'B)
b
[current-namespace (module->namespace (quote 'A))]
(quote-module-path)
]

}

@defform[(quote-module-source)]{

Like @scheme[quote-module-path], but for the enclosing module's source
name, rather than its module path. The module path and source name are
typically the same, but they can be different. For example, a source
file whose name ends with @filepath{.ss} corresponds to a resolved
module path ending with @filepath{.rkt}. The value produced by
@scheme[(quote-module-source)] is either @scheme['top-level] or a
resolved module path, even though the latter may correspond to a
source file rather than a module path.}

@defform[(quote-module-name)]{

Quotes the name (@tech[#:doc reference-path]{path} or @tech[#:doc
reference-path]{symbol}) of the module in which the macro application occurs, or
@scheme[#f] if it occurs at the top level.  As with @scheme[quote-module-path],
@scheme[quote-module-name] uses a @tech[#:doc reference-path]{variable
reference}, so a top level namespace created by @scheme[module->namespace] will
be treated as a module, and the macro will always produce the module name of its
final expanded position.

@defexamples[#:eval (new-evaluator)
(quote-module-name)
(module A scheme
  (require unstable/location)
  (define-syntax-rule (here) (quote-module-name))
  (define a (here))
  (provide a here))
(require 'A)
a
(module B scheme
  (require unstable/location)
  (require 'A)
  (define b (here))
  (provide b))
(require 'B)
b
[current-namespace (module->namespace (quote 'A))]
(quote-module-name)
]

}
