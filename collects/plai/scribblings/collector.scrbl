#lang scribble/doc
@(require scribble/manual
          "rkt-exports.rkt"
          "plai-exports.rkt"
          "lang-names.rkt"
          (for-syntax scheme)
          (for-label (except-in scheme
                                error printf)
                     (prefix-in scheme:
                                scheme)
                     (only-in plai/main
                              type-case define-type error
                              test test/pred test/exn test/regexp
                              abridged-test-output
                              plai-catch-test-exn
                              halt-on-errors print-only-errors
                              test-inexact-epsilon plai-ignore-exn-strings
                              plai-all-test-results)
                     (only-in plai/collector
                              root?
                              heap-size
                              location?
                              heap-value?
                              heap-set! heap-ref with-heap
                              get-root-set read-root set-root!
                              procedure-roots)
                     plai/scribblings/fake-collector
                     plai/scribblings/fake-mutator
                     plai/scribblings/fake-web
                     plai/random-mutator
                     (only-in plai/web
                              no-web-browser
                              static-files-path)
                     (only-in plai/mutator
                              set-first!
                              set-rest!
                              import-primitives
                              test/location=?
                              test/value=?
                              printf)))

@title[#:tag "collector"]{@COLLECT-LANG}

@defmodulelang[plai/collector]

@COLLECT-LANG is based on @seclink["plai-scheme"]{PLAI Scheme}.  It provides
additional procedures and syntax for writing garbage collectors.

@section{Garbage Collector Interface}

The @COLLECT-LANG language provides the following functions that provide access
to the heap and root set:

@defproc[(heap-size) exact-nonnegative-integer?]{

Returns the size of the heap.  The size of the heap is specified by the mutator
that uses the garbage collector.  See @racket[allocator-setup] for more
information.
}

@defproc[(location? [v any/c])
         boolean?]{
  Determines if @racket[v] is an integer between @racket[0] and
  @racket[(- (heap-size) 1)] inclusive.
}

@defproc[(root? [v any/c])
         boolean?]{
Determines if @racket[v] is a root.
}

@defproc[(heap-value? [v any/c]) boolean?]{
  A value that may be stored on the heap. Roughly corresponds to the contract
  @racket[(or/c boolean? number? procedure? symbol? empty?)].
}


@defproc[(heap-set! (loc location?) (val heap-value?)) void?]{
  Sets the value at @racket[_loc] to @racket[_val].
}

@defproc[(heap-ref (loc location?)) heap-value?]{
  Returns the value at @racket[_loc].
}

@defform/subs[(get-root-set id ...)()]{
  Returns the current roots as a list.  Local roots are created for the
  identifiers @racket[_id] as well.
}

@defproc[(read-root (root root?)) location?]{
  Returns the location of @racket[_root].
}

@defproc[(set-root! (root root?) (loc location?)) void?]{
  Updates the root to reference the given location.
}

@defproc[(procedure-roots (proc procedure?)) (listof root?)]{
  Given a closure stored on the heap, returns a list of the roots reachable
  from the closure's environment.  If @racket[_proc] is not reachable, the
  empty list is returned.
}

@defform[(with-heap heap-expr body-expr ...)
         #:contracts ([heap-expr (vectorof heap-value?)])]{
 Evaluates each of the @racket[body-expr]s in a context where
 the value of @racket[heap-expr] is used as the heap. Useful in
 tests:
 @racketblock[
  (test (with-heap (make-vector 20) 
          (init-allocator)
          (gc:deref (gc:alloc-flat 2)))
        2)
  ]}
                                                     
@defform[(with-roots roots-expr expr1 expr2 ...)
         #:contracts ([roots-expr (listof location?)])]{
  Evaluates each of @racket[expr1] and the @racket[expr2]s in
  in a context with the result of @racket[roots-expr]
  as additional roots.
  
  This function is intended to be used in test suites
  for collectors. Since your test suites are not running
  in the @racketmod[plai/mutator] language, @racket[get-root-set]
  returns a list consisting only of the roots it created,
  not all of the other roots it normally would return.
  Use this function to note specific locations as roots
  and set up better tests for your GC.
  
  @racketblock[
    (test (with-heap (make-vector 4)
                     (define f1 (gc:alloc-flat 1))
                     (define c1 (gc:cons f1 f1))
                     (with-roots (list c1)
                                 (gc:deref
                                  (gc:first
                                   (gc:cons f1 f1)))))
          1)]
  
}

@section{Garbage Collector Exports}

@declare-exporting[#:use-sources (plai/scribblings/fake-collector)]

A garbage collector must define the following functions:

@defproc[(init-allocator) void?]{

@racket[init-allocator] is called before all other procedures by a
mutator. Place any requisite initialization code here.

}

@defproc[(gc:deref (loc location?)) heap-value?]{

Given the location of a flat Scheme value, this procedure should return that
value.  If the location does not hold a flat value, this function should signal
an error.

}

@defproc[(gc:alloc-flat (val heap-value?)) location?]{

This procedure should allocate a flat Scheme value (number, symbol, boolean,
closure or empty list) on the heap, returning its location (a number). The
value should occupy a single heap cell, though you may use additional space to
store a tag, etc. You are also welcome to pre-allocate common constants (e.g.,
the empty list). This procedure may need to perform a garbage-collection. If
there is still insufficient space, it should signal an error.

Note that closures are flat values. The environment of a closure is internally
managed, but contains references to values on the heap. Therefore, during
garbage collection, the environment of reachable closures must be updated. The
language exposes the environment via the @racket[procedure-roots] function.

}

@defproc[(gc:cons (first location?) (rest location?)) location?]{

Given the location of the @racket[_first] and @racket[_rest] values, this
procedure must allocate a cons cell on the heap.  If there is insufficient
space to allocate the cons cell, it should signal an error.

}

@defproc[(gc:first (cons-cell location?)) location?]{

If the given location refers to a cons cell, this should return the first
field. Otherwise, it should signal an error.

}

@defproc[(gc:rest (cons-cell location?)) location?]{

If the given location refers to a cons cell, this should return the rest
field. Otherwise, it should signal an error.

}

@defproc[(gc:set-first! (cons-cell location?) (first-value location?)) void?]{

If @racket[_cons-cell] refers to a cons cell, set the head of the cons cell to
@racket[_first-value].  Otherwise, signal an error.

}

@defproc[(gc:set-rest! (cons-cell location?) (rest-value location?)) void?]{

If @racket[_cons-cell] refers to a cons cell, set the tail of the cons cell to
@racket[_rest-value].  Otherwise, signal an error.

}

@defproc[(gc:cons? (loc location?)) boolean?]{


Returns @racket[true] if @racket[_loc] refers to a cons cell.  This function
should never signal an error.

}

@defproc[(gc:flat? (loc location?)) boolean?]{

Returns @racket[true] if @racket[_loc] refers to a flat value.  This function
should never signal an error.

}
