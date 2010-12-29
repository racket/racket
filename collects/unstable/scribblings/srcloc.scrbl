#lang scribble/manual
@(require scribble/eval "utils.rkt" (for-label racket/base unstable/srcloc unstable/location))

@(define unsyntax #f)

@(define (new-evaluator)
   (let* ([e (make-base-eval)])
     (e '(require (for-syntax racket/base)
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
locations, including both @racket[srcloc] structures and all the values accepted
by @racket[datum->syntax]'s third argument: syntax objects, lists, vectors, and
@racket[#f].

@deftogether[(
@defproc[(source-location? [x any/c]) boolean?]{}
@defproc[(source-location-list? [x any/c]) boolean?]{}
@defproc[(source-location-vector? [x any/c]) boolean?]{}
)]{

These functions recognize valid source location representations.  The first,
@racket[source-location?], recognizes @racket[srcloc] structures, syntax
objects, lists, and vectors with appropriate structure, as well as @racket[#f].
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
procedure returns @racket[(void)].  If it is not,
@racket[check-source-location!] raises a detailed error message in terms of
@racket[name] and the problem with @racket[x].

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
locations within the same source and reporting @racket[#f] for locations that
span sources.  They also convert the result to the desired representation:
@racket[srcloc], list, vector, or syntax object, respectively.

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
than simply @racket[#f].

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
position and span, if both are numbers) or @racket[#f].

@examples[#:eval evaluator
(source-location-end #f)
(source-location-end (make-srcloc 'source 1 2 3 4))
(source-location-end (list 'source 1 2 3 #f))
(source-location-end (vector 'source 1 2 #f 4))
]

}

@defproc[(update-source-location
          [loc source-location?]
          [#:source source any/c]
          [#:line line (or/c exact-nonnegative-integer? #f)]
          [#:column column (or/c exact-positive-integer? #f)]
          [#:position position (or/c exact-nonnegative-integer? #f)]
          [#:span span (or/c exact-positive-integer? #f)])
         source-location?]{
Produces a modified version of @racket[loc], replacing its fields with
@racket[source], @racket[line], @racket[column], @racket[position], and/or
@racket[span], if given.

@examples[#:eval evaluator
(update-source-location #f #:source 'here)
(update-source-location (list 'there 1 2 3 4) #:line 20 #:column 79)
(update-source-location (vector 'everywhere 1 2 3 4) #:position #f #:span #f)
]
}

@deftogether[(
@defproc[(source-location->string [loc source-location?]) string?]{}
@defproc[(source-location->prefix [loc source-location?]) string?]{}
)]{

These procedures convert source locations to strings for use in error messages.
The first produces a string describing the source location; the second appends
@racket[": "] to the string if it is non-empty.

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

Quotes the source location of @racket[form] as a @racket[srcloc]
structure, using the location of the whole @racket[(quote-srcloc)]
expression if no @racket[expr] is given.  Uses relative directories
for paths found within the collections tree, the user's collections directory,
or the PLaneT cache.

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

Quote various fields of the source location of @racket[form], or of
the whole macro application if no @racket[form] is given.

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

@deftogether[(
@defform[(quote-module-name)]
@defform[(quote-module-path)]
)]{

Quote the name of the module in which the form is compiled.  The
@racket[quote-module-name] form produces a string or a symbol, while
@racket[quote-module-path] produces a @tech[#:doc reference-path]{module path}.

These forms use relative names for modules found in the collections or PLaneT
cache; their results are suitable for printing, but not for accessing libraries
programmatically, such as via @racket[dynamic-require].

@defexamples[#:eval (new-evaluator)
(module A racket
  (require unstable/location)
  (define-syntax-rule (name) (quote-module-name))
  (define-syntax-rule (path) (quote-module-path))
  (define a-name (name))
  (define a-path (path))
  (provide (all-defined-out)))
(require 'A)
a-name
a-path
(module B racket
  (require unstable/location)
  (require 'A)
  (define b-name (name))
  (define b-path (path))
  (provide (all-defined-out)))
(require 'B)
b-name
b-path
(quote-module-name)
(quote-module-path)
[current-namespace (module->namespace (quote 'A))]
(quote-module-name)
(quote-module-path)
]

}

@close-eval[evaluator]
