#lang scribble/doc
@(require "base.rkt")

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

A @racket[test-begin] form groups the @racket[expr]s into a
single unit.  If any @racket[expr] fails the following ones
are not evaluated.  }

For example, in the following code the world is not
destroyed as the preceding check fails:

@racketblock[
(test-begin
  (check-eq? 'a 'b)
  (code:comment "This line won't be run")
  (destroy-the-world))
]

@defform[(test-case name expr ...)]{

Like a @racket[test-begin] except a name is associated with
the group of @racket[expr]s.  The name will be reported if
the test fails.  }

Here's the above example rewritten to use @racket[test-case]
so the test can be named.

@racketblock[
(test-case
  "Example test"
  (check-eq? 'a 'b)
  (code:comment "This line won't be run")
  (destroy-the-world))
]


@defproc[(test-case? (obj any)) boolean?]{
 True if @racket[obj] is a test case, and false otherwise.
}

@subsection{Shortcuts for Defining Test Cases}

@defproc*[([(test-check [name string?]
                        [operator (-> any/c any/c any/c)]
                        [v1 any/c]
                        [v2 any/c])
            void?]
           [(test-pred [name string?]
                       [pred (-> any/c any/c)]
                       [v any/c])
            void?]
           [(test-equal? [name string?] [v1 any/c] [v2 any/c]) (void?)]
           [(test-eq? [name string?] [v1 any/c] [v2 any/c]) void?]
           [(test-eqv? [name string?] [v1 any/c] [v2 any/c]) void?]
           [(test-= [name string?] [v1 real?] [v2 real?] [epsilon real?]) void?]
           [(test-true [name string?] [v any/c]) void?]
           [(test-false [name string?] [v any/c]) void?]
           [(test-not-false [name string?] [v any/c]) void?]
           [(test-exn [name string?] [pred (-> exn? any/c)] [thunk (-> any)]) void?]
           [(test-not-exn [name string?] [thunk (-> any)]) void?])]{

Creates a test case with the given @racket[name] that performs the
corresponding check. For example,

@racketblock[(test-equal? "Fruit test" "apple" "pear")]
is equivalent to
@racketblock[(test-case "Fruit test" (check-equal? "apple" "pear"))]
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
tests may be test cases, constructed using @racket[test-begin] or
@racket[test-case], or other test suites.

The @racket[before-thunk] and @racket[after-thunk] are
optional thunks (functions with no argument).  They are run
before and after the tests are run, respectively. 

Unlike a check or test case, a test suite is not immediately
run.  Instead use one of the functions described in
@secref["ui"] or @secref["running"].}

For example, here is a test suite that displays @tt{Before}
before any tests are run, and @tt{After} when the tests have
finished.

@racketblock[
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

Constructs a test suite with the given @racket[name] containing the
given @racket[tests]. Unlike the @racket[test-suite] form, the tests
are represented as a list of test values.
}

@defproc[(test-suite? (obj any)) boolean?]{ True if
@racket[obj] is a test suite, and false otherwise}



@subsection{Utilities for Defining Test Suites}

There are some macros that simplify the common cases of
defining test suites:

@defform[(define-test-suite name test ...)]{ The
@racket[define-test-suite] form creates a test suite with
the given name (converted to a string) and tests, and binds
it to the same name.}

For example, this code creates a binding for the name
@racket[example-suite] as well as creating a test suite with
the name @racket["example-suite"]:

@racketblock[
(define-test-suite example-suite
  (check = 1 1))
]

@defform[(define/provide-test-suite name test ...)]{ This
form is just like @racket[define-test-suite], and in addition
it @racket[provide]s the test suite.}

@;{
Finally, there is the @racket[test-suite*] macro, which
defines a test suite and test cases using a shorthand
syntax:

@defform[(test-suite* name (test-case-name test-case-body
...) ...)]{ Defines a test suite with the given name, and
creates test cases within the suite, with the given names and
body expressions.

As far I know no-one uses this macro, so it might disappear
in future versions of RackUnit.}
}
