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
                     (only-in plai/gc2/collector
                              root?
                              heap-size
                              location?
                              heap-value?
                              heap-set! heap-ref with-heap
                              get-root-set read-root set-root! make-root)
                     plai/scribblings/fake-collector2
                     plai/scribblings/fake-mutator2
                     plai/random-mutator
                     (only-in plai/web
                              no-web-browser
                              static-files-path)
                     (only-in plai/gc2/mutator
                              set-first!
                              set-rest!
                              import-primitives
                              test/location=?
                              test/value=?
                              printf)))


@title[#:tag "gc2-collector"]{GC Collector Language, 2}

@defmodulelang[plai/gc2/collector]

@COLLECT-LANG is based on @seclink["plai-scheme"]{PLAI}.  It provides
additional procedures and syntax for writing garbage collectors.

@section{Garbage Collector Interface for GC2}

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
  @racket[(or/c boolean? number? symbol? empty?)].
}


@defproc[(heap-set! (loc location?) (val heap-value?)) void?]{
  Sets the value at @racket[_loc] to @racket[_val].
}

@defproc[(heap-ref (loc location?)) heap-value?]{
  Returns the value at @racket[_loc].
}

@defform[(get-root-set)]{
  Returns the current @racket[root?]s as a list. This returns
  valid roots only when invoked via the mutator language. Otherwise
  it returns only what has been set up with @racket[with-roots].
}

@defproc[(read-root (root root?)) location?]{
  Returns the location of @racket[_root].
}

@defproc[(set-root! (root root?) (loc location?)) void?]{
  Updates @racket[root] to refer to @racket[loc].
}

@defproc[(make-root [name symbol?] [get (-> location?)] [set (-> location? void?)])
         root?]{
  Creates a new root. When @racket[read-root] is called, it invokes
  @racket[get] and when @racket[set-root!] is called, it invokes
  @racket[set].
  
  For example, this creates a root that uses the local variable
  @racket[x] to hold its location:
  @racketblock[(let ([x 1])
                 (make-root 'x
                            (λ () x)
                            (λ (new-x) (set! x new-x))))]
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
                                                     
@defform[(with-roots (root-var ...) expr1 expr2 ...)
         #:contracts ([roots-expr (listof location?)])]{
  Evaluates each of @racket[expr1] and the @racket[expr2]s in
  in a context with additional roots, one for each of
  the @racket[root-var]s. The @racket[get-root-set] function
  returns these additional roots. Calling @racket[read-root] on
  one of the newly created roots returns the value of the 
  corresponding @racket[root-var] and calling @racket[set-root!]
  mutates the corresponding variable.
  
  This form is intended to be used in test suites
  for collectors. Since your test suites are not running
  in the @racketmod[plai/gc2/mutator] language, @racket[get-root-set]
  returns a list consisting only of the roots it created,
  not all of the other roots it normally would return.
  Use @racket[with-roots] to note specific locations as roots
  and set up better tests for your GC.
  
  @racketblock[
    (test (with-heap (make-vector 4)
                     (define f1 (gc:alloc-flat 1))
                     (define r1 (make-root 'f1 
                                           (λ () f1)
                                           (λ (v) (set! f1 v))))
                     (define c1 (gc:cons r1 r1))
                     (with-roots (c1)
                                 (gc:deref
                                  (gc:first
                                   (gc:cons r1 r1)))))
          1)]
  
}

@section{Garbage Collector Exports for GC2}

@declare-exporting[#:use-sources (plai/scribblings/fake-collector2)]

A garbage collector must define the following functions:

@defproc[(init-allocator) void?]{

@racket[init-allocator] is called before all other procedures by a
mutator. Place any requisite initialization code here.

}

@defproc[(gc:deref (loc location?)) heap-value?]{

Given the location of a flat value, this procedure should return that
value.  If the location does not hold a flat value, this function should signal
an error.

}

@defproc[(gc:alloc-flat (val heap-value?)) location?]{

This procedure should allocate a flat value (number, symbol, boolean,
or empty list) on the heap, returning its location (a number). The
value should occupy a single heap cell, though you may use additional space to
store a tag, etc. You are also welcome to pre-allocate common constants (e.g.,
the empty list). This procedure may need to perform a garbage-collection. If
there is still insufficient space, it should signal an error.

}

@defproc[(gc:cons (first root?) (rest root?)) location?]{

Given two roots, one for the @racket[first] and @racket[rest] values, this
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

If @racket[cons-cell] refers to a cons cell, set the head of the cons cell to
@racket[first-value].  Otherwise, signal an error.

}

@defproc[(gc:set-rest! (cons-cell location?) (rest-value location?)) void?]{

If @racket[cons-cell] refers to a cons cell, set the tail of the cons cell to
@racket[rest-value].  Otherwise, signal an error.

}

@defproc[(gc:cons? (loc location?)) boolean?]{


Returns @racket[#true] if @racket[loc] refers to a cons cell.  This function
should never signal an error.

}

@defproc[(gc:flat? (loc location?)) boolean?]{

Returns @racket[#true] if @racket[loc] refers to a flat value.  This function
should never signal an error.

}

@defproc[(gc:closure [code-ptr heap-value?] [free-vars (listof root?)]) 
         location?]{
  Allocates a closure with @racket[code-ptr] and the free variables
  in the list @racket[free-vars].
}
@defproc[(gc:closure-code-ptr [loc location?]) heap-value?]{
 Given a location returned from an earlier allocation
 check to see if it is a closure; if not signal an
 error. if so, return the @racket[_code-ptr] for that closure.
}

@defproc[(gc:closure-env-ref [loc location?] [i exact-nonnegative-integer?])
         location?]{
  Given a location returned from an earlier allocation, check
  to see if it is a closure; if not signal an error. Uf so,
  return the @racket[i]th variable in the closure (counting from 0).
}

@defproc[(gc:closure? [loc location?]) boolean?]{
  Determine if a previously allocated location 
  holds a closure. This function will be called only
  with locations previous returned from an allocating
  function or passed to @racket[set-root!]. It should
  never signal an error.
}
