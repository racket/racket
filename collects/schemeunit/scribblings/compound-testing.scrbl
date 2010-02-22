#lang scribble/doc
@(require "base.ss")

@title{Compound Testing Forms}

@section{Test Cases}

As programs increase in complexity the unit of testing
grows beyond a single check. For example, it may be the case
that if one check fails it doesn't make sense to run
another.  To solve this problem compound testing forms can
be used to group expressions.  If any expression in a group
fails (by raising an exception) the remaining expressions
will not be evaluated.

@defform[(test-begin expr ...)]{

A @scheme[test-begin] form groups the @scheme[expr]s into a
single unit.  If any @scheme[expr] fails the following ones
are not evaluated.  }

For example, in the following code the world is not
destroyed as the preceding check fails:

@schemeblock[
(test-begin
  (check-eq? 'a 'b)
  (code:comment "This line won't be run")
  (destroy-the-world))
]

@defform[(test-case name expr ...)]{

Like a @scheme[test-begin] except a name is associated with
the group of @scheme[expr]s.  The name will be reported if
the test fails.  }

Here's the above example rewritten to use @scheme[test-case]
so the test can be named.

@schemeblock[
(test-case
  "Example test"
  (check-eq? 'a 'b)
  (code:comment "This line won't be run")
  (destroy-the-world))
]


@defproc[(test-case? (obj any)) boolean?]{
 True if @scheme[obj] is a test case, and false otherwise
}



@section{Test Suites}

Test cases can themselves be grouped into test suites.  A
test suite can contain both test cases and test suites.
Unlike a check or test case, a test suite is not immediately
run.  Instead use one of the functions described in
@secref["ui"] or @secref["running"].

@defform/subs[(test-suite name-expr maybe-before maybe-after test ...)
              ([maybe-before (code:line)
                             (code:line #:before before-thunk)]
               [maybe-after (code:line)
                            (code:line #:after after-thunk)])
              #:contracts ([name-expr string?])]{

Constructs a test suite with the given name and tests.  The
tests may be test cases, constructed using @scheme[test-begin] or
@scheme[test-case], or other test suites.

The @scheme[before-thunk] and @scheme[after-thunk] are
optional thunks (functions with no argument).  They are run
before and after the tests are run, respectively. 

Unlike a check or test case, a test suite is not immediately
run.  Instead use one of the functions described in
@secref["ui"] or @secref["running"].}

For example, here is a test suite that displays @tt{Before}
before any tests are run, and @tt{After} when the tests have
finished.

@schemeblock[
(test-suite
  "An example suite"
  #:before (lambda () (display "Before"))
  #:after  (lambda () (display "After"))
  (test-case
    "An example test"
    (check-eq? 1 1))
  (test-suite "A nested test suite"
    (test-case "Another test"
      (check-< 1 2))))
]

@defproc[(make-test-suite [name string?]
                          [tests (listof (or/c test-case? test-suite?))]
                          [#:before before-thunk (-> any) void]
                          [#:after after-thunk (-> any) void])
         test-suite?]{

Constructs a test suite with the given @scheme[name] containing the
given @scheme[tests]. Unlike the @scheme[test-suite] form, the tests
are represented as a list of test values.
}

@defproc[(test-suite? (obj any)) boolean?]{ True if
@scheme[obj] is a test suite, and false otherwise}



@subsection{Utilities for Defining Test Suites}

There are some macros that simplify the common cases of
defining test suites:

@defform[(define-test-suite name test ...)]{ The
@scheme[define-test-suite] form creates a test suite with
the given name (converted to a string) and tests, and binds
it to the same name.}

For example, this code creates a binding for the name
@scheme[example-suite] as well as creating a test suite with
the name @scheme["example-suite"]:

@schemeblock[
(define-test-suite example-suite
  (check = 1 1))
]

@defform[(define/provide-test-suite name test ...)]{ This
for is just like @scheme[define-test-suite], and in addition
it @scheme[provide]s the test suite.}

@;{
Finally, there is the @scheme[test-suite*] macro, which
defines a test suite and test cases using a shorthand
syntax:

@defform[(test-suite* name (test-case-name test-case-body
...) ...)]{ Defines a test suite with the given name, and
creates test cases within the suite, with the given names and
body expressions.

As far I know no-one uses this macro, so it might disappear
in future versions of SchemeUnit.}
}


@section{Compound Testing Evaluation Context}

Just like with checks, there are several parameters that
control the semantics of compound testing forms.

@defparam[current-test-name name (or/c string? false/c)]{

This parameter stores the name of the current test case.  A
value of @scheme[#f] indicates a test case with no name,
such as one constructed by @scheme[test-begin].  }

@defparam[current-test-case-around handler (-> (-> any/c) any/c)]{

This parameter handles evaluation of test cases.  The value
of the parameter is a function that is passed a thunk (a
function of no arguments). The function, when applied,
evaluates the expressions within a test case.  The default
value of the @scheme[current-test-case-around] parameters
evaluates the thunk in a context that catches exceptions and
prints an appropriate message indicating test case failure.}

@defproc[(test-suite-test-case-around [thunk (-> any/c)]) any/c]{

The @scheme[current-test-case-around] parameter is
parameterized to this value within the scope of a
@scheme[test-suite].  This function creates a test case
structure instead of immediately evaluating the thunk.}

@defproc[(test-suite-check-around [thunk (-> any/c)]) any/c]{

The @scheme[current-check-around] parameter is parameterized
to this value within the scope of a @scheme[test-suite].
This function creates a test case structure instead of
immediately evaluating a check.}
