#lang scribble/manual
@(require scribble/eval
          scribble/decode
          (for-label racket/base 
                     syntax/srcloc
                     syntax/location
                     setup/path-to-relative))

@(define unsyntax #f)

@(define (new-evaluator)
   (let* ([e (make-base-eval)])
     (e '(require (for-syntax racket/base)
                  syntax/srcloc
                  syntax/location))
     e))

@(define evaluator (new-evaluator))

@(define reference-path
   '(lib "scribblings/reference/reference.scrbl"))

@title{Source Locations}

@author[@author+email["Carl Eastlund" "cce@ccs.neu.edu"]]

There are two libraries in this collection for dealing with source locations;
one for manipulating representations of them, and the other for quoting the
location of a particular piece of source code.

@section[#:tag "srcloc"]{Representations}

@defmodule[syntax/srcloc]

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

@section[#:tag "location"]{Source Location Utilities}

@defmodule[syntax/location]

@deftogether[(
@defproc[(syntax-source-directory [stx syntax?]) (or/c path? #f)]
@defproc[(syntax-source-file-name [stx syntax?]) (or/c path? #f)]
)]{

These produce the directory and file name, respectively, of the path with which
@racket[stx] is associated, or @racket[#f] if @racket[stx] is not associated
with a path.

@defexamples[
#:eval (new-evaluator)
(define loc
  (list (build-path "/tmp" "dir" "somewhere.rkt")
        #f #f #f #f))
(define stx1 (datum->syntax #f 'somewhere loc))
(syntax-source-directory stx1)
(syntax-source-file-name stx1)
(define stx2 (datum->syntax #f 'nowhere #f))
(syntax-source-directory stx2)
(syntax-source-directory stx2)
]

@history[#:added "6.3"]{}
}

@subsection{Quoting}

The following macros evaluate to various aspects of their own source location.

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
@defform*[[(quote-srcloc-string) (quote-srcloc-string form)]]
@defform*[[(quote-srcloc-prefix) (quote-srcloc-prefix form)]]
)]{

Quote the result of @racket[source-location->string] or
@racket[source-location->prefix], respectively, applied to the source location
of @racket[form], or of the whole macro application if no @racket[form] is
given.

@examples[#:eval (new-evaluator)
(list (quote-srcloc-string)
      (quote-srcloc-prefix))
(define-syntax (not-here stx)
  #'(list (quote-srcloc-string)
          (quote-srcloc-prefix)))
(not-here)
(not-here)
(define-syntax (here stx)
  #`(list (quote-srcloc-string #,stx)
          (quote-srcloc-prefix #,stx)))
(here)
(here)
]

}

@(define (p . l) (decode-paragraph l))

@defform[(quote-module-name submod-path-element ...)]{

Quotes the name of the module in a form suitable for printing, but not
necessarily as a valid @tech[#:doc reference-path]{module path}.  See
@racket[quote-module-path] for constructing quoted
@tech[#:doc reference-path]{module path}s.

Returns a path, symbol, list, or @racket['top-level], where @racket['top-level]
is produced when used outside of a module. A list corresponds to a
submodule in the same format as the result of
@racket[variable-reference->module-name]. Any given
@racket[submod-path-element]s (as in a @racket[submod] form) are added
to form a result submodule path.

To produce a name suitable for use in printed messages, apply
@racket[path->relative-string/library] when the result is a path.

@defexamples[#:eval (new-evaluator)
(module A racket
  (require syntax/location)
  (define-syntax-rule (name) (quote-module-name))
  (define a-name (name))
  (module+ C
    (require syntax/location)
    (define c-name (quote-module-name))
    (define c-name2 (quote-module-name ".."))
    (provide c-name c-name2))
  (provide (all-defined-out)))
(require 'A)
a-name
(require (submod 'A C))
c-name
c-name2
(module B racket
  (require syntax/location)
  (require 'A)
  (define b-name (name))
  (provide (all-defined-out)))
(require 'B)
b-name
(quote-module-name)
(current-namespace (module->namespace (quote 'A)))
(quote-module-name)
]

}

@defform[(quote-module-path submod-path-element ...)]{

Quotes the name of the module in which the form is compiled.  When possible,
the result is a valid @tech[#:doc reference-path]{module path} suitable for use
by @racket[dynamic-require] and similar functions.

Builds the result using
@racket[quote], a path, @racket[submod], or @racket['top-level], where
@racket['top-level] is produced when used outside of a module. Any given
@racket[submod-path-element]s (as in a @racket[submod] form) are added
to form a result submodule path.

@defexamples[#:eval (new-evaluator)
(module A racket
  (require syntax/location)
  (define-syntax-rule (path) (quote-module-path))
  (define a-path (path))
  (module+ C
    (require syntax/location)
    (define c-path (quote-module-path))
    (define c-path2 (quote-module-path ".."))
    (provide c-path c-path2))
  (provide (all-defined-out)))
(require 'A)
a-path
(require (submod 'A C))
c-path
c-path2
(module B racket
  (require syntax/location)
  (require 'A)
  (define b-path (path))
  (provide (all-defined-out)))
(require 'B)
b-path
(quote-module-path)
(current-namespace (module->namespace (quote 'A)))
(quote-module-path)
]

}

@close-eval[evaluator]
