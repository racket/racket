#lang scribble/doc
@(require scribble/manual
          "rkt-exports.rkt"
          "plai-exports.rkt"
          "lang-names.rkt"
          scribble/decode
          (for-syntax scheme)
          (for-label (only-in racket/base
                              list modulo
                              procedure? path-string? 
                              input-port? string? void?
                              exact-nonnegative-integer?
                              exact-positive-integer?)
                     (only-in racket/contract/base
                              or/c listof any/c)
                     (only-in plai/gc2/collector
                              root?
                              heap-size
                              location?
                              heap-value?
                              heap-set! heap-ref with-heap
                              get-root-set read-root set-root!)
                     plai/scribblings/fake-collector2
                     plai/scribblings/fake-mutator2
                     plai/random-mutator
                     (only-in plai/web
                              no-web-browser
                              static-files-path)
                     plai/gc2/mutator))

@title[#:tag "gc2-mutator"]{GC Mutator Language, 2}

@defmodulelang[plai/gc2/mutator]

The @MUTATE-LANG language is used to test garbage collectors written
with the @secref["gc2-collector"] language.  Since collectors support
a subset of Racket's values, the @MUTATE-LANG language supports a
subset of procedures and syntax.  In addition, many procedures that
can be written in the mutator are omitted as they make good test
cases.  Therefore, the mutator language provides only primitive
procedures, such as @racket[+], @racket[cons], etc.

@section{Building Mutators for GC2}

@declare-exporting[#:use-sources (plai/scribblings/fake-mutator2)]

The first expression of a mutator must be:

@defform/subs[
(allocator-setup collector-module 
                 heap-size)
([heap-size exact-nonnegative-integer])]{

@racket[_collector-module] specifies the path to the garbage collector that the
mutator should use.  The collector must be written in the @COLLECT-LANG
language.
}

The rest of a mutator module is a sequence of definitions, expressions and test
cases. The @MUTATE-LANG language transforms these definitions and statements to
use the collector specified in @racket[allocator-setup].  In particular, many
of the primitive forms, such as @racket[cons] map directly to procedures such
as @racket[gc:cons], written in the collector.

@section{Mutator API for GC2}

The @MUTATE-LANG language supports the following procedures and syntactic
forms:

@(define-syntax-rule (defprocthing id content ...)
   @defthing[id procedure? content ...]) 

@(define-syntax (document/lift stx)
   (syntax-case stx ()
     [(_ defidform a ...)
      (with-syntax ([(doc ...)
                     (for/list ([a (in-list (syntax->list #'(a ...)))])
                       (syntax-case a ()
                         [a
                          (identifier? #'a)
                          (with-syntax ([rkt:a (string->symbol (format "rkt:~a" (syntax-e #'a)))])
                            #'@defidform[a]{Just like Racket's @|rkt:a|.})]
                         [(a stuff)
                          (identifier? #'a)
                          (with-syntax ([rkt:a (string->symbol (format "rkt:~a" (syntax-e #'a)))])
                            #'@defidform[a]{Similar to Racket's @|rkt:a|. @stuff})]))])
        #'(begin
            doc ...))]))

@document/lift[defidform
               if and or cond case define-values let let-values let* 
                  (set! @splice[@list{Unlike Racket's @|rkt:set!|, this @racket[set!] is syntactically allowed only
                                      in positions that discard its result, e.g., at the top-level
                                      or in a @racket[begin] expression (although not as the last expression
                                      in a @racket[begin]).}])
                  quote begin]

@defform[(define (id arg-id ...) body-expression ...+)]{
  Just like Racket's @racket[define], except restricted to the simpler form
  above.
}
@deftogether[(@defform[(lambda (arg-id ...) body-expression ...+)]{}
              @defform[(λ (arg-id ...) body-expression ...+)]{})]{
  Just like Racket's @racket[lambda] and @racket[λ], except restricted to the
  simpler form above.
}

@document/lift[defprocthing
               error add1 sub1 zero? + - * / even? odd? = < > <= >= 
                    symbol? symbol=? number? boolean? empty? eq?]

@defproc[(cons [hd any/c] [tl any/c]) cons?]{
  Constructs a (mutable) pair.
}
@defproc[(cons? [v any/c]) boolean?]{
  Returns @racket[#t] when given a value created by @racket[cons],
          @racket[#f] otherwise.
}
@defproc[(first [c cons?]) any/c]{
  Extracts the first component of @racket[c].
}
@defproc[(rest [c cons?]) any/c]{
  Extracts the rest component of @racket[c].
}

@defproc[(set-first! [c cons?] [v any/c])
         void]{
  Sets the @racket[first] of the cons cell @racket[c].
           
  Calls to this function are allowed only in syntactic 
  positions that would discard its result, e.g., at the
  top-level or inside a @racket[begin] expression (but
  not in the last expression in a @racket[begin]). 
  Also, this function appear only in the function
  position of an application expression.

  So, in order to pass around a version of this function,
  you must write something like this
  @racket[(λ (c v) (begin (set-first! c v) 42))],
  perhaps picking a different value than @racket[42]
  as the result.
}

@defproc[(set-rest! [c cons?] [v any/c])
         void]{
  Sets the @racket[rest] of the cons cell @racket[c],
  with the same syntactic restrictions as
  @racket[set-first!].
}

@defidform[empty]{
  The identifier @racket[empty] is defined to invoke
  @racket[(gc:alloc-flat '())] wherever it is used.
}

@defidform[print-only-errors]{
  Behaves like PLAI's @|plai:print-only-errors|.
}

@defidform[halt-on-errors]{
  Behaves like PLAI's @|plai:halt-on-errors|.
}

Other common procedures are left undefined as they can be defined in
terms of the primitives and may be used to test collectors.

Additional procedures from @racketmodname[scheme] may be imported with:

@defform/subs[(import-primitives id ...)()]{

Imports the procedures @racket[_id ...] from @racketmodname[scheme].  Each
procedure is transformed to correctly interface with the mutator.  That is, its
arguments are dereferenced from the mutator's heap and the result is allocated
on the mutator's heap.  The arguments and result must be @racket[heap-value?]s,
even if the imported procedure accepts or produces structured data.

For example, the @MUTATE-LANG language does not define @racket[modulo]:

@racketblock[

(import-primitives modulo)

(test/value=? (modulo 5 3) 2)
]

}

@section{Testing Mutators with GC2}

@MUTATE-LANG provides two forms for testing mutators:

@defform/subs[(test/location=? mutator-expr1 mutator-expr2)()]{

@racket[test/location=?] succeeds if @racket[_mutator-expr1] and
@racket[_mutator-expr2] reference the same location on the heap.

}

@defform/subs[(test/value=? mutator-expr datum/quoted)()]{

@racket[test/value=?] succeeds if @racket[_mutator-expr] and
@racket[_datum/expr] are structurally equal.
@racket[_datum/quoted] is not allocated on the mutator's
heap. Futhermore, it must either be a quoted value or a literal value.

}

@defform/subs[
(printf format mutator-expr ...)
([format literal-string])]{

In @|MUTATE-LANG|, @racket[printf] is a syntactic form and not a procedure. The
format string, @racket[_format] is not allocated on the mutator's heap.

}

@section{Generating Random Mutators for GC2}

@defmodule[plai/gc2/random-mutator]

This PLAI library provides a facility for generating random mutators,
in order to test your garbage collection implementation.

@defproc[(save-random-mutator
          [file path-string?]
          [collector-name string?]
          [#:heap-values heap-values (cons heap-value? (listof heap-value?))
                                     (list 0 1 -1 'x 'y #f #t '())]
          [#:iterations iterations exact-positive-integer? 200]
          [#:program-size program-size exact-positive-integer? 10]
          [#:heap-size heap-size exact-positive-integer? 100])
         void?]{
Creates a random mutator that uses the collector @racket[collector-name] and
saves it in @racket[file].

The mutator is created by first making a random graph whose nodes either have
no outgoing edges, two outgoing edges, or some random number of outgoing edges
and then picking a random path in the graph that ends at one of the nodes with
no edges.

This graph and path are then turned into a PLAI program by creating a
@racket[let] expression that binds one variable per node in the graph.  If the
node has no outgoing edges, it is bound to a @racket[heap-value?].  If the node
has two outgoing edges, it is bound to a pair and the two edges are put into
the first and rest fields. Otherwise, the node is represented as a procedure
that accepts an integer index and returns the destination node of the
corresponding edge.

Once the @racket[let] expression has been created, the program creates a bunch
of garbage and then traverses the graph, according to the randomly created
path. If the result of the path is the expected heap value, the program does
this again, up to @racket[iterations] times. If the result of the path is not
the expected heap value, the program terminates with an error.

The keyword arguments control some aspects of the generation
of random mutators:
@itemize[
@item{Elements from the @racket[heap-values] argument are used as the base
  values when creating nodes with no outgoing edges.  See also
  @racket[find-heap-values].}
@item{The @racket[iterations] argument controls how many times the graph is
  created (and traversed).}
@item{The @racket[program-size] argument is a bound on how big the program it
  is; it limits the number of nodes, the maximum number of edges, and the
  length of the path in the graph.}
@item{The @racket[heap-size] argument controls the size of the heap in the
  generated mutator.}]

}

Example:
@racketblock[(save-random-mutator "tmp.rkt" "mygc.rkt" #:gc2? #t)]
will write to @filepath{tmp.rkt} with a program like this one:
@codeblock{
#lang plai/gc2/mutator
(allocator-setup "mygc.rkt" 200)
(define (build-one)
  (let* ((x0 1)
         (x1 (cons #f #f))
         (x2
          (lambda (x)
            (if (= x 0)
              x0
              (if (= x 1) x0 (if (= x 2) x1 (if (= x 3) x1 x0))))))
         (x3 1)
         (x4 (cons x3 x3))
         (x5 (lambda (x) (if (= x 0) x4 (if (= x 1) x1 x2)))))
    (set-first! x1 x2)
    (set-rest! x1 x3)
    x5))
(define (traverse-one x5) (= 1 (first (x5 0))))
(define (trigger-gc n)
  (if (zero? n) 0 (begin (cons n n) (trigger-gc (- n 1)))))
(define (loop i)
  (if (zero? i)
    'passed
    (let ((obj (build-one)))
      (trigger-gc 200)
      (if (traverse-one obj) (loop (- i 1)) 'failed))))
(loop 200)
}


@defproc[(find-heap-values [input (or/c path-string? input-port?)])
         (listof heap-value?)]{
  Processes @racket[input] looking for occurrences of @racket[heap-value?]s in
  the source of the program and returns them. This makes a good start for the
  @racket[_heap-values] argument to @racket[save-random-mutator].

  If @racket[input] is a port, its contents are assumed to be a well-formed
  PLAI program. If @racket[input] is a file, the contents of the file are used.
}
