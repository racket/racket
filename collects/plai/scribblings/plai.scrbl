#lang scribble/doc
@(require scribble/manual
          "rkt-exports.rkt"
          "plai-exports.rkt"
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

@(define PLAI-LANG "PLAI Scheme")
@(define COLLECT-LANG "GC Collector Scheme")
@(define MUTATE-LANG "GC Mutator Scheme")
@(define WEB-LANG "Web Application Scheme")

@title{@italic{Programming Languages: Application and Interpretation}}

This is the documentation for the software accompanying the textbook @bold{Programming Languages: Application and
Interpretation} (PLAI). The full book can be found on the Web at:

@(link "http://www.cs.brown.edu/~sk/Publications/Books/ProgLangs/"
       "http://www.cs.brown.edu/~sk/Publications/Books/ProgLangs/")

This package contains the following languages:

@local-table-of-contents[#:style 'immediate-only]

@section[#:tag "plai-scheme"]{@PLAI-LANG} 

@defmodulelang[plai]

@(define scheme-guide '(lib "scribblings/reference/reference.scrbl"))

@PLAI-LANG is derived from the @racketmodname[scheme] language.  In addition,
it includes the @racket[define-type] and @racket[type-case] forms and testing
support.

@subsection[#:tag "define-type"]{Defining Types: @racket[define-type]}

@defform/subs[(define-type type-id variant ...)
              ([variant (variant-id (field-id contract-expr) ...)])]{

Defines the datatype @racket[_type-id].  A constructor @racket[_variant-id] is
defined for each variant.  Each constructor takes an argument for each field of
its variant.

The value of each field is checked by its associated @racket[_contract-expr].
A @racket[_contract-expr] may be an arbitrary predicate or a contract.

In addition to the contructors, a @racket[define-type] expression also defines:

@itemize[

  @item{a predicate @racket[_type-id?] that returns @racket[true] for instances
    of the datatype, and @racket[false] for any other value,}

  @item{for each variant, a predicate @racket[_variant-id?] that returns
    @racket[true] when applied to a value of the same variant and
    @racket[false] for any other value,}

  @item{for each field of each variant, an accessor
    @racket[_variant-id-field-id] that returns the value of the field, and}

  @item{for each field of each variant, a mutator
    @racket[_set-variant-id-field-id!] that set the value of the field.}
]}

@subsection[#:tag "type-case"]{Deconstructing Data Structures: @racket[type-case]}

@defform/subs[(type-case datatype-id expr
                 branch ...)

              ([branch (variant-id (field-id ...) result-expr ...)
                       (else result-expr ...)])]{

Branches on the datatype instance produced by @racket[_expr], which must be an
instance of @racket[_datatype-id] (previously defined with
@racket[define-type]) Each @racket[_branch] extracts the values of the fields,
and binds them to @racket[_field-id ...].

If a branch is not specified for each variant, you may use an @racket[else]
branch to create a catch-all branch.  An @racket[else] branch must be the last
branch in the sequence of branches.  @racket[type-case] signals a compile-time
error if all variants are not covered and the @racket[else] branch is missing.
Similarly, @racket[type-case] signals a compile-time error if an @racket[else]
branch is unreachable because a branch exists for all variants.

}

@subsection[#:tag "testing"]{Testing Infrastructure}

PLAI Scheme provides the following syntactic forms for testing.

@defform/subs[(test result-expr expected-expr)()]{

If @racket[_result-expr] and @racket[_expected-expr] evaluate to the same
value, @racket[_result-value], the test prints

@racketresultfont{(good result-expr result-value expected-value location)}.

If they do not evaluate to the same value, the test prints

@racketresultfont{(bad result-expr result-value expected-value location)}.

If evaluating @racket[_result-expr] signals an error, the test prints

@racketresultfont{(exception result-expr exception-message <no-expected-value> location)}

If evaluating @racket[_expected-expr] signals an error, the test prints

@racketresultfont{(pred-exception result-expr exception-message <no-expected-value> location)}

}

@defform/subs[(test/pred result-expr pred?)()]{

Similar to @racket[test], but instead of supplying an expected value, the
predicate @racket[_pred?] is applied to @racket[_result-expr].

If evaluating @racket[_pred?] signals an error, the test prints

@racketresultfont{(pred-exception result-expr exception-message <no-expected-value> location)}

The syntax of @racket[_pred?] is considered @racket[_expected-value] for the
purposes of test reporting.
}

@defthing[error procedure?]{
 Like @racketmodname[scheme]'s @racket[scheme:error],
 but generates exceptions that are caught by @racket[test/exn].
}

@defform/subs[(test/exn result-expr error-message)()]{

This test succeeds if the expression evaluates to a call to
@racket[error]. Moreover, the error message contained in the exception must
contain the string @racket[_error-message]. Note that @racket[test/exn] only
succeeds if the exception was explicitly raised by the user.

For example, the following test succeeds:

@racketblock[(test/exn (error "/: division by zero") "by zero")]

The error message is @racket["/: division by zero"], and @racket["by zero"] is
a substring of the error message. However, the following test fails:

@racketblock[(test/exn (/ 25 0) "by zero")]

Although the expression raises an exception and the error string contains
@racket["by zero"], since the error was not explicitly raised by user-written
code, the test fails.

The evaluation of @racket[_error-message] is considered
@racket[_expected-value] for the purposes of test reporting.
}

@defform/subs[(test/regexp result-expr error-message-regexp)()]{

This test is similar to @racket[test/exn],but the error message is matched
against a regular expression instead.

The evaluation of @racket[_error-message-regexp] is considered
@racket[_expected-value] for the purposes of test reporting.
}

@subsubsection{Test Flags}

@defproc[(abridged-test-output (abridge? boolean? false)) void?]{

When this flag is set to @racket[true], the test forms never prints
@racket[_result-expr] or @racket[_location].

}

@defproc[(plai-catch-test-exn (catch? boolean? true)) void?]{

When this flag is set to @racket[true], exceptions from tests will be caught.
By default, exceptions are caught.

}


@defproc[(halt-on-errors (halt? boolean? true)) void?]{

This flag determines whether the program immediately halts when a test fails.
By default, programs do not halt on failures.
}

@defproc[(print-only-errors (print? boolean? true)) void?]{

When this flag is set to @racket[true], only tests that fail will be printed.
By default, the results of all tests are printed.

}

@defproc[(test-inexact-epsilon (epsilon number?)) void?]{

When testing inexact values for equality, @racket[test] permits them to differ
by @racket[_epsilon].  The default value of @racket[_epsilon] is @racket[0.01].

}

@defproc[(plai-ignore-exn-strings (ignore? boolean?)) void?]{

If this flag is set to @racket[true], when testing for exceptions with
@racket[test/exn] and @racket[test/regexp], the message of the exception is
ignored.  By default, @racket[test/exn] and @racket[test/regexp] only succeed
when the message of the exception matches the supplied string or regular
expression.

}

@defidform[plai-all-test-results]{

This variable is the list of all tests that have been run so far, with the most
recent test at the head.

}

@section[#:tag "collector"]{@COLLECT-LANG}

@defmodulelang[plai/collector]

@COLLECT-LANG is based on @seclink["plai-scheme"]{PLAI Scheme}.  It provides
additional procedures and syntax for writing garbage collectors.

@subsection{Garbage Collector Interface}

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

@defform[(with-heap heap expr ...)
         #:contracts ([heap (vectorof heap-value?)])]{
 Evaluates @racket[(begin expr ...)] in the context of @racket[heap]. Useful in
 tests:
 @racketblock[
  (test (with-heap (make-vector 20) 
          (init-allocator)
          (gc:deref (gc:alloc-flat 2)))
        2)
  ]}

@subsection{Garbage Collector Exports}

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

@section[#:tag "mutator"]{@MUTATE-LANG}

@defmodulelang[plai/mutator]

The @MUTATE-LANG language is used to test garbage collectors written with the
@secref["collector"] language.  Since collectors support a subset of Scheme's
values, the @MUTATE-LANG language supports a subset of procedures and syntax.
In addition, many procedures that can be written in the mutator are omitted as
they make good test cases.  Therefore, the mutator language provides only
primitive procedures, such as @racket[+], @racket[cons], etc.

@subsection{Building Mutators}

@declare-exporting[#:use-sources (plai/scribblings/fake-mutator)]

The first expression of a mutator must be:

@defform/subs[
(allocator-setup collector-module 
                 heap-size)
([heap-size exact-nonnegative-integer?])]{

@racket[_collector-module] specifies the path to the garbage collector that the
mutator should use.  The collector must be written in the @COLLECT-LANG
language.
}

The rest of a mutator module is a sequence of definitions, expressions and test
cases. The @MUTATE-LANG language transforms these definitions and statements to
use the collector specified in @racket[allocator-setup].  In particular, many
of the primitive forms, such as @racket[cons] map directly to procedures such
as @racket[gc:cons], written in the collector.

@subsection{Mutator API}

The @MUTATE-LANG language supports the following procedures and syntactic
forms:

@(define-syntax (document/lift stx)
   (syntax-case stx ()
     [(_ a ...)
      (with-syntax ([(doc ...)
                     (map (λ (a)
                            (with-syntax ([a a]
                                          [rkt:a (string->symbol (format "rkt:~a" (syntax-e a)))]) 
                              #'@defidform[a]{Just like Racket's @|rkt:a|.}))
                          (syntax->list #'(a ...)))])

      #'(begin
          doc ...))]))

@document/lift[if and or cond case define-values let let-values let* set! quote error begin]

@defform[(define (id arg-id ...) body-expression ...+)]{
  Just like Racket's @racket[define], except restricted to the simpler form
  above.
}
@deftogether[(@defform[(lambda (arg-id ...) body-expression ...+)]{}
              @defform[(λ (arg-id ...) body-expression ...+)]{})]{
  Just like Racket's @racket[lambda] and @racket[λ], except restricted to the
  simpler form above.
}

@document/lift[add1 sub1 zero? + - * / even? odd? = < > <= >= 
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
}

@defproc[(set-rest! [c cons?] [v any/c])
         void]{
  Sets the @racket[rest] of the cons cell @racket[c].
}

@defidform[empty]{
  The identifier @racket[empty] is defined to invoke
  @racket[(gc:alloc-flat empty)] wherever it is used.
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

@subsection{Testing Mutators}

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

@subsection{Generating Random Mutators}

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
  @racket[heap-values] argument to @racket[save-random-mutator].

  If @racket[input] is a port, its contents are assumed to be a well-formed
  PLAI program. If @racket[input] is a file, the contents of the file are used.
}

@section[#:tag "web"]{@WEB-LANG}

@defmodulelang[plai/web]

The @WEB-LANG language allows you to write server-side Web applications for the
PLT Web Server.

For more information about writing Web applications, see:
@other-manual['(lib "web-server/scribblings/web-server.scrbl")].

When you click on the @onscreen{Run} button in DrRacket, your Web application
is launched in the Web server.

The application is available at
@italic{http://localhost:8000/servlets/standalone.rkt}.

The @WEB-LANG language will automatically load this URL in your Web browser.

You may use @racket[no-web-browser] to prevent the browser from being launched
and @racket[static-files-path] to serve additional static files.

@subsection{Web Application Exports}

@declare-exporting[#:use-sources (plai/scribblings/fake-web)]

A Web application must define a procedure @racket[start]:

@defproc[(start (initial-request request?)) response?]{

The initial request to a Web application is serviced by this procedure.

}
