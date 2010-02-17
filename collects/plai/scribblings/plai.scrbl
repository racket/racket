#lang scribble/doc
@(require scribble/manual
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
                              halt-on-errors print-only-errors test-inexact-epsilon plai-ignore-exn-strings plai-all-test-results)
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

@(define-syntax-rule (schememodlang lang)
   (scheme #,(hash-lang) lang))

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

@PLAI-LANG is derived from the @schememodname[scheme] langauge.  In addition, it includes
the @scheme[define-type] and @scheme[type-case] forms and testing
support.

@subsection[#:tag "define-type"]{Defining Types: @scheme[define-type]}

@defform/subs[(define-type type-id variant ...)
              ([variant (variant-id (field-id contract-expr) ...)])]{

Defines the datatype @scheme[_type-id].  A constructor @scheme[_variant-id] is defined for each variant.
Each constructor takes an argument for each field of its variant.

The value of each field is checked by its associated @scheme[_contract-expr].  A @scheme[_contract-expr] may be an
arbitrary predicate or a contract.

In addition to the contructors, a @scheme[define-type] expression also defines:

@itemize{

  @item{a predicate @scheme[_type-id?] that returns @scheme[true] for instances of the
        datatype, and @scheme[false] for any other value,}

  @item{for each variant, a predicate @scheme[_variant-id?] that returns @scheme[true] when applied to a value of the
        same variant and @scheme[false] for any other value,}
        
  @item{for each field of each variant, an accessor @scheme[_variant-id-field-id] that returns the value of the
         field, and}
 
  @item{for each field of each variant, a mutator @scheme[_set-variant-id-field-id!] that set the value of the
        field.}
}
}

@subsection[#:tag "type-case"]{Deconstructing Data Structures: @scheme[type-case]}

@defform/subs[(type-case datatype-id expr 
                 branch ...)
                 
              ([branch (variant-id (field-id ...) result-expr ...)
                       (else result-expr ...)])]{
                       
Branches on the datatype instance produced by @scheme[_expr], which must be an instance of @scheme[_datatype-id] 
(previously defined with @scheme[define-type])  Each @scheme[_branch] extracts the values of the fields, and binds them
to @scheme[_field-id ...].

If a branch is not specified for each variant, you may use an @scheme[else] branch to create a catch-all branch.  An
@scheme[else] branch must be the last branch in the sequence of branches.
@scheme[type-case] signals a compile-time error if all variants are not covered and the @scheme[else] branch is
missing.  Similarly, @scheme[type-case] signals a compile-time error if an @scheme[else] branch is unreachable because
a branch exists for all variants.

}
         
@subsection[#:tag "testing"]{Testing Infrastructure}

PLAI Scheme provides the following syntactic forms for testing.

@defform/subs[(test result-expr expected-expr)()]{

If @scheme[_result-expr] and @scheme[_expected-expr] evaluate to the same value, @scheme[_result-value], the test prints

@schemeresultfont{(good result-expr result-value expected-value location)}.

If they do not evaluate to the same value, the test prints

@schemeresultfont{(bad result-expr result-value expected-value location)}.

If evaluating @scheme[_result-expr] signals an error, the test prints

@schemeresultfont{(exception result-expr exception-message <no-expected-value> location)}

If evaluating @scheme[_expected-expr] signals an error, the test prints

@schemeresultfont{(pred-exception result-expr exception-message <no-expected-value> location)}

}

@defform/subs[(test/pred result-expr pred?)()]{

Similar to @scheme[test], but instead of supplying an expected value, the predicate @scheme[_pred?] is applied to
@scheme[_result-expr].

If evaluating @scheme[_pred?] signals an error, the test prints

@schemeresultfont{(pred-exception result-expr exception-message <no-expected-value> location)}

The syntax of @scheme[_pred?] is considered @scheme[_expected-value] for the purposes of test reporting.
}

@defthing[error procedure?]{
 Like @schememodname[scheme]'s @scheme[scheme:error],
 but generates exceptions that are caught by @scheme[test/exn].
}

@defform/subs[(test/exn result-expr error-message)()]{

This test succeeds if the expression evaluates to a call to @scheme[error]. Moreover, the error message contained in the
exception must contain the string @scheme[_error-message]. Note that @scheme[test/exn] only suceeds if the exception was
explicitly raised by the user.

For example, the following test succeeds:

@(schemeblock (test/exn (error "/: division by zero") "by zero")) 
 
The error message is @scheme{/: division by zero}, and @scheme{by zero} is a substring of the error message. However,
the following test fails:

@(schemeblock (test/exn (/ 25 0) "by zero"))

Although the expression raises an exception and the error string contains @scheme{by zero}, since the error was not
explicitly raised by user-written code, the test fails.

The evaluation of @scheme[_error-message] is considered @scheme[_expected-value] for the purposes of test reporting.
}

@defform/subs[(test/regexp result-expr error-message-regexp)()]{

This test is similar to @scheme[test/exn],but the error message is matched against a regular expression instead.
                        
The evaluation of @scheme[_error-message-regexp] is considered @scheme[_expected-value] for the purposes of test reporting.
}

@subsubsection{Test Flags}

@defproc[(abridged-test-output (abridge? boolean? false)) void?]{

When this flag is set to @scheme[true], the test forms never prints @scheme[_result-expr] or @scheme[_location].
     
}

@defproc[(plai-catch-test-exn (catch? boolean? true)) void?]{

When this flag is set to @scheme[true], exceptions from tests will be caught.
By default, exceptions are caught.

}


@defproc[(halt-on-errors (halt? boolean? true)) void?]{

This flag determines whether the program immediately halts when a test fails.  By default, programs do not halt on
failures.
}

@defproc[(print-only-errors (print? boolean? true)) void?]{

When this flag is set to @scheme[true], only tests that fail will be printed.
By default, the results of all tests are printed.

}

@defproc[(test-inexact-epsilon (epsilon number?)) void?]{

When testing inexact values for equality, @scheme[test] permits them to differ by @scheme[_epsilon].  The
default value of @scheme[_epsilon] is @scheme[0.01].

}

@defproc[(plai-ignore-exn-strings (ignore? boolean?)) void?]{

If this flag is set to @scheme[true], when testing for exceptions with @scheme[test/exn] and @scheme[test/regexp],
the message of the exception is ignored.  By default, @scheme[test/exn] and @scheme[test/regexp] only succeed when the
message of the exception matches the supplied string or regular expression.

}

@defidform[plai-all-test-results]{

This variable is the list of all tests that have been run so far, with the most recent test at the head.

}
      
@section[#:tag "collector"]{@COLLECT-LANG} 

@defmodulelang[plai/collector]

@COLLECT-LANG is based on @seclink["plai-scheme"]{PLAI Scheme}.  It provides additional procedures and
syntax for writing garbage collectors.

@subsection{Garbage Collector Interface}

The @COLLECT-LANG language provides the following functions that provide access to the heap and root set:

@defproc[(heap-size) exact-nonnegative-integer?]{
Returns the size of the heap.  The size of the heap is specified by the mutator that uses the garbage collector.
See @scheme[allocator-setup] for more information.
}

@defproc[(location? [v any/c])
         boolean?]{
Determines if @scheme[v] is an integer between @scheme[0] and @scheme[(- (heap-size) 1)] inclusive.
}

@defproc[(root? [v any/c])
         boolean?]{
Determines if @scheme[v] is a root.
}

@defproc[(heap-value? [v any/c]) boolean?]{
  A value that may be stored on the heap. Roughly corresponds to the contract @scheme[(or/c boolean? number? procedure? symbol? empty?)].
}


@defproc[(heap-set! (loc location?) (val heap-value?)) void?]{
  Sets the value at @scheme[_loc] to @scheme[_val].
}

@defproc[(heap-ref (loc location?)) heap-value?]{
  Returns the value at @scheme[_loc].
}

@defform/subs[(get-root-set id ...)()]{
  Returns the current roots as a list.  Local roots are created for the identifiers @scheme[_id] as well. 
}

@defproc[(read-root (root root?)) location?]{
  Returns the location of @scheme[_root].
}

@defproc[(set-root! (root root?) (loc location?)) void?]{
  Updates the root to reference the given location.
}

@defproc[(procedure-roots (proc procedure?)) (listof root?)]{
  Given a closure stored on the heap, returns a list of the roots reachable from the closure's environment.  If 
  @scheme[_proc] is not reachable, the empty list is returned.
}

@defform[(with-heap heap expr ...)
         #:contracts ([heap (vectorof heap-value?)])]{
 Evaluates @scheme[(begin expr ...)] in the context of @scheme[heap]. Useful in tests:
 @schemeblock[
  (test (with-heap (make-vector 20) 
          (init-allocator)
          (gc:deref (gc:alloc-flat 2)))
        2)
  ]}

@subsection{Garbage Collector Exports}

@declare-exporting[#:use-sources (plai/scribblings/fake-collector)]

A garbage collector must define the following functions:

@defproc[(init-allocator) void?]{

@scheme[init-allocator] is called before all other procedures by a mutator. Place any requisite initialization code
here.
  
}

@defproc[(gc:deref (loc location?)) heap-value?]{

Given the location of a flat Scheme value, this procedure should return that value.  If the location does not hold
a flat value, this function should signal an error.

}

@defproc[(gc:alloc-flat (val heap-value?)) location?]{

This procedure should allocate a flat Scheme value (number, symbol, boolean, closure or empty list) on the heap,
returning its location (a number). The value should occupy a single heap cell, though you may use additional space to
store a tag, etc. You are also welcome to pre-allocate common constants (e.g., the empty list). This procedure may need
to perform a garbage-collection. If there is still insufficient space, it should signal an error.

Note that closures are flat values. The environment of a closure is internally managed, but contains
references to values on the heap. Therefore, during garbage collection, the environment of reachable closures must be
updated. The language exposes the environment via the @scheme[procedure-roots] function.

}

@defproc[(gc:cons (first location?) (rest location?)) location?]{

Given the location of the @scheme[_first] and @scheme[_rest] values, this procedure must allocate a cons cell on the 
heap.  If there is insufficient space to allocate the cons cell, it should signal an error.

}

@defproc[(gc:first (cons-cell location?)) location?]{

If the given location refers to a cons cell, this should return the first field. Otherwise, it should signal an error.

}

@defproc[(gc:rest (cons-cell location?)) location?]{

If the given location refers to a cons cell, this should return the rest field. Otherwise, it should signal an error.

}

@defproc[(gc:set-first! (cons-cell location?) (first-value location?)) void?]{

If @scheme[_cons-cell] refers to a cons cell, set the head of the cons cell to
@scheme[_first-value].  Otherwise, signal an error.

}

@defproc[(gc:set-rest! (cons-cell location?) (rest-value location?)) void?]{

If @scheme[_cons-cell] refers to a cons cell, set the tail of the cons cell to
@scheme[_rest-value].  Otherwise, signal an error.

}

@defproc[(gc:cons? (loc location?)) boolean?]{


Returns @scheme[true] if @scheme[_loc] refers to a cons cell.  This function should never signal an error.

}

@defproc[(gc:flat? (loc location?)) boolean?]{

Returns @scheme[true] if @scheme[_loc] refers to a flat value.  This function should never signal an error.

}

@section[#:tag "mutator"]{@MUTATE-LANG}

@defmodulelang[plai/mutator]

The @MUTATE-LANG language is used to test garbage collectors written with the 
@secref["collector"] language.  Since collectors support a subset of Scheme's values, the @MUTATE-LANG language supports a subset of procedures and syntax.
In addition, many procedures that can be written in the mutator are omitted as they make good test cases.  Therefore,
the mutator language provides only primitive procedures, such as @scheme[+], @scheme[cons], etc.

@subsection{Building Mutators}

@declare-exporting[#:use-sources (plai/scribblings/fake-mutator)]

The first expression of a mutator must be:

@defform/subs[
(allocator-setup collector-module 
                 heap-size)
([heap-size exact-nonnegative-integer?])]{

@scheme[_collector-module] specifies the path to the garbage collector that the mutator should use.  The collector
must be written in the @COLLECT-LANG language.
}

The rest of a mutator module is a sequence of definitions, expressions and test cases. The @MUTATE-LANG language
transforms these definitions and statements to use the collector specified in @scheme[allocator-setup].  In particular,
many of the primitive forms, such as @scheme[cons] map directly to procedures such as @scheme[gc:cons], written in the
collector.

@subsection{Mutator API}

The @MUTATE-LANG language supports the following syntactic forms:

@schemeblock[if and or cond case define define-values let let-values let* set! lambda λ quote error begin]

The language also defines the following procedures:

@schemeblock[add1 sub1 zero? + - * / even? odd? = < > <= >= cons first rest
             set-first! set-rest! cons? symbol? symbol=? number? boolean? empty? eq?]

@defproc[(set-first! [c cons?] [v any/c])
         void]{
 Sets the @scheme[first] of the cons cell @scheme[c].
}

@defproc[(set-rest! [c cons?] [v any/c])
         void]{
 Sets the @scheme[rest] of the cons cell @scheme[c].
}

The identifier @scheme[empty] is defined to invoke @scheme[(gc:alloc-flat empty)] wherever it is used.

Other common procedures are left undefined as they can be defined in
terms of the primitives and may be used to test collectors.

Additional procedures from @schememodname[scheme] may be imported with:

@defform/subs[(import-primitives id ...)()]{

Imports the procedures @scheme[_id ...] from @schememodname[scheme].  Each
procedure is transformed to correctly interface with the mutator.  That is, its
arguments are dereferenced from the mutator's heap and the result is allocated
on the mutator's heap.  The arguments and result must be @scheme[heap-value?]s,
even if the imported procedure accepts or produces structured data.

For example, the @MUTATE-LANG language does not define @scheme[modulo]:

@schemeblock[

(import-primitives modulo)

(test/value=? (modulo 5 3) 2)
]

}

@subsection{Testing Mutators}

@MUTATE-LANG provides two forms for testing mutators:

@defform/subs[(test/location=? mutator-expr1 mutator-expr2)()]{

@scheme[test/location=?] succeeds if @scheme[_mutator-expr1] and @scheme[_mutator-expr2] reference the same location
on the heap.
 
}

@defform/subs[(test/value=? mutator-expr scheme-datum/quoted)()]{

@scheme[test/value=?] succeeds if @scheme[_mutator-expr] and @scheme[_scheme-datum/expr] are structurally equal.
@scheme[_scheme-datum/quoted] is not allocated on the mutator's heap. Futhermore, it must either be a quoted value or a
literal value.

}

@defform/subs[
(printf format mutator-expr ...)
([format literal-string])]{

In @|MUTATE-LANG|, @scheme[printf] is a syntactic form and not a procedure. The format string,
@scheme[_format] is not allocated on the mutator's heap.

}
                          
@subsection{Generating Random Mutators}

@defmodule[plai/random-mutator]

This PLAI library provides a facility for generating random mutators,
in order to test your garbage collection implementation.

@defproc[(save-random-mutator 
          [file path-string?]
          [collector-name string?]
          [#:heap-values heap-values (cons heap-value? (listof heap-value?)) (list 0 1 -1 'x 'y #f #t '())]
          [#:iterations iterations exact-positive-integer? 200] 
          [#:program-size program-size exact-positive-integer? 10]
          [#:heap-size heap-size exact-positive-integer? 100])
         void?]{
Creates a random mutator that uses the collector @scheme[collector-name] and
saves it in @scheme[file].

The mutator is created by first making a random graph whose nodes either
have no outgoing edges, two outgoing edges, or some random number of 
outgoing edges and then picking
a random path in the graph that ends at one of the nodes with no edges.

This graph and path are then turned into a PLAI program by creating
a @scheme[let] expression that binds one variable per node in the graph.
If the node has no outgoing edges, it is bound to a @scheme[heap-value?].
If the node has two outgoing edges, it is bound to a pair and the two
edges are put into the first and rest fields. Otherwise, the node
is represented as a procedure that accepts an integer index and 
returns the destination node of the corresponding edge.

Once the @scheme[let] expression has been created, the program
creates a bunch of garbage and then traverses the graph, 
according to the randomly created path. If the result of the path
is the expected heap value, the program does this again, up
to @scheme[iterations] times. If the result of the path
is not the expected heap value, the program terminates
with an error.

The keyword arguments control some aspects of the generation
of random mutators:
@itemize[@item{Elements from the @scheme[heap-values] argument are used
               as the base values when creating nodes with no outgoing edges.
               See also @scheme[find-heap-values].}
          @item{The @scheme[iterations] argument controls how many times
                the graph is created (and traversed).}
          @item{The @scheme[program-size] argument is a bound on how
                big the program it is; it limits the number of nodes,
                the maximum number of edges, and the length of the 
                path in the graph.}
          @item{The @scheme[heap-size] argument controls the size of the
                heap in the generated mutator.}]

}
               

@defproc[(find-heap-values [input (or/c path-string? input-port?)]) (listof heap-value?)]{
  Processes @scheme[input] looking for occurrences of @scheme[heap-value?]s
  in the source of the program and returns them. This makes a good start
  for the @scheme[heap-values] argument to @scheme[save-random-mutator].
  
  If @scheme[input] is a port, its contents are assumed to be a well-formed 
  PLAI program. If @scheme[input] is a file, the contents of the file are 
  used.
}

@section[#:tag "web"]{@WEB-LANG}

@defmodulelang[plai/web]

The @WEB-LANG language allows you to write server-side Web applications for the PLT Web Server.

For more information about writing Web applications, see:
@other-manual['(lib "web-server/scribblings/web-server.scrbl")].

When you click on the @onscreen{Run} button in DrScheme, your Web application is launched in the Web server.

The application is available at @italic{http://localhost:8000/servlets/standalone.ss}. 

The @WEB-LANG language will automatically load this URL in your Web browser.

You may use @scheme[no-web-browser] to prevent the browser from being launched and @scheme[static-files-path]
to serve additional static files.

@subsection{Web Application Exports}

@declare-exporting[#:use-sources (plai/scribblings/fake-web)]

A Web application must define a procedure @scheme[start]: 

@defproc[(start (initial-request request?)) response?]{

The initial request to a Web application is serviced by this procedure.

}