#lang scribble/doc
@(require scribble/manual
          "rkt-exports.rkt"
          "plai-exports.rkt"
          "lang-names.rkt"
          (for-syntax scheme)
          (for-label (only-in racket/base
                              list modulo
                              procedure? path-string? 
                              input-port? string? void?
                              exact-nonnegative-integer?
                              exact-positive-integer?)
                     (only-in racket/contract/base
                              or/c listof any/c)
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
                     plai/mutator))

@title[#:tag "mutator"]{@MUTATE-LANG}

@defmodulelang[plai/mutator]

The @MUTATE-LANG language is used to test garbage collectors written with the
@secref["collector"] language.  Since collectors support a subset of Scheme's
values, the @MUTATE-LANG language supports a subset of procedures and syntax.
In addition, many procedures that can be written in the mutator are omitted as
they make good test cases.  Therefore, the mutator language provides only
primitive procedures, such as @racket[+], @racket[cons], etc.

@section{Building Mutators}

@declare-exporting[#:use-sources (plai/scribblings/fake-mutator)]

The first expression of a mutator must be:

@defform/subs[
(allocator-setup collector-module 
                 heap-size)
([heap-size exact-nonnegative-integer])]{

The @racket[_collector-module] form specifies the path to the garbage collector that the
mutator should use.  The collector must be written in the @COLLECT-LANG
language.
}

The rest of a mutator module is a sequence of definitions, expressions and test
cases. The @MUTATE-LANG language transforms these definitions and statements to
use the collector specified in @racket[allocator-setup].  In particular, many
of the primitive forms, such as @racket[cons] map directly to procedures such
as @racket[gc:cons], written in the collector.

@section{Mutator API}

The @MUTATE-LANG language supports the following procedures and syntactic
forms:

@(define-syntax-rule (defprocthing id content ...)
   @defthing[id procedure? content ...]) 

@(define-syntax (document/lift stx)
   (syntax-case stx ()
     [(_ defidform a ...)
      (with-syntax ([(doc ...)
                     (map (λ (a)
                            (with-syntax ([a a]
                                          [rkt:a (string->symbol (format "rkt:~a" (syntax-e a)))]) 
                              #'@defidform[a]{Just like Racket's @|rkt:a|.}))
                          (syntax->list #'(a ...)))])

      #'(begin
          doc ...))]))

@document/lift[defidform
               if and or cond case define-values let let-values let* set! quote begin]

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
               error
               add1 sub1 zero? + - * / even? odd? = < > <= >= 
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
         void?]{
  Sets the @racket[first] of the cons cell @racket[c].
}

@defproc[(set-rest! [c cons?] [v any/c])
         void?]{
  Sets the @racket[rest] of the cons cell @racket[c].
}

@defidform[empty]{
  The identifier @racket[empty] is defined to invoke
  @racket[(gc:alloc-flat '())] wherever it is used.
}

@defprocthing[print-only-errors]{
  Behaves like PLAI's @|plai:print-only-errors|.
}

@defprocthing[halt-on-errors]{
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

@section{Testing Mutators}

@MUTATE-LANG provides two forms for testing mutators:

@defform/subs[(test/location=? mutator-expr1 mutator-expr2)()]{

@racket[test/location=?] succeeds if @racket[_mutator-expr1] and
@racket[_mutator-expr2] reference the same location on the heap.

}

@defform/subs[(test/value=? mutator-expr scheme-datum/quoted)()]{

@racket[test/value=?] succeeds if @racket[_mutator-expr] and
@racket[_scheme-datum/expr] are structurally equal.
@racket[_scheme-datum/quoted] is not allocated on the mutator's
heap. Futhermore, it must either be a quoted value or a literal value.

}

@defform/subs[
(printf format mutator-expr ...)
([format literal-string])]{

In @|MUTATE-LANG|, @racket[printf] is a syntactic form and not a procedure. The
format string, @racket[_format] is not allocated on the mutator's heap.

}

@section{Generating Random Mutators}

@defmodule[plai/random-mutator]

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

@defproc[(find-heap-values [input (or/c path-string? input-port?)])
         (listof heap-value?)]{
  Processes @racket[input] looking for occurrences of @racket[heap-value?]s in
  the source of the program and returns them. This makes a good start for the
  @racket[_heap-values] argument to @racket[save-random-mutator].

  If @racket[input] is a port, its contents are assumed to be a well-formed
  PLAI program. If @racket[input] is a file, the contents of the file are used.
}
