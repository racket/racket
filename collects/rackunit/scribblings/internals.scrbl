#lang scribble/doc
@(require "base.rkt")

@declare-exporting[rackunit #:use-sources (rackunit)]

@title[#:tag "internals"]{RackUnit Internals and Extension API}

This section describes RackUnit's facilities for customizing the
behavior of checks and tests and for creating new kinds of test
runners.

@section{Customizing Check Evaluation}

The semantics of checks are determined by the parameters
@racket[current-check-around] and
@racket[current-check-handler].  Other testing form such as
@racket[test-begin] and @racket[test-suite] change the value
of these parameters.

@defparam[current-check-handler handler (-> any/c any)]{

Parameter containing the function that handles exceptions
raised by check failures.  The default value is @racket[raise].
}

@defparam[current-check-around check (-> (-> any) any)]{

Parameter containing the function that handles the execution
of checks.  The default value wraps the evaluation of
@racket[thunk] in a @racket[with-handlers] call that calls
@racket[current-check-handler] if an exception is raised and then
(when an exception is not raised) discards the result, returning
@racket[(void)]. 
}

@section{Customizing Test Evaluation}

Just like with checks, there are several parameters that
control the semantics of compound testing forms.

@defparam[current-test-name name (or/c string? false/c)]{

This parameter stores the name of the current test case.  A
value of @racket[#f] indicates a test case with no name,
such as one constructed by @racket[test-begin].
}

@defparam[current-test-case-around handler (-> (-> any) any)]{

This parameter handles evaluation of test cases.  The value
of the parameter is a function that is passed a thunk (a
function of no arguments). The function, when applied,
evaluates the expressions within a test case.  The default
value of the @racket[current-test-case-around] parameters
evaluates the thunk in a context that catches exceptions and
prints an appropriate message indicating test case failure.
}

@defproc[(test-suite-test-case-around [thunk (-> any)]) any]{

The @racket[current-test-case-around] parameter is
parameterized to this value within the scope of a
@racket[test-suite].  This function creates a test case
structure instead of immediately evaluating the thunk.
}

@defproc[(test-suite-check-around [thunk (-> any/c)]) any/c]{

The @racket[current-check-around] parameter is parameterized
to this value within the scope of a @racket[test-suite].
This function creates a test case structure instead of
immediately evaluating a check.
}

@;{--------}

@section[#:tag "running"]{Programmatically Running Tests and Inspecting Results}

RackUnit provides an API for running tests, from which
custom UIs can be created.

@subsection{Result Types}

@defstruct[(exn:test exn:fail) ()]{

The base structure for RackUnit exceptions.  You should
never catch instances of this type, only the subtypes
documented below.}

@defstruct[(exn:test:check exn:test) ([stack (listof check-info)])]{

A @racket[exn:test:check] is raised when an check fails, and
contains the contents of the check-info stack at the
time of failure.}

@defstruct[test-result ([test-case-name (or/c string #f)])]{

A test-result is the result of running the test with
the given name (with @racket[#f] indicating no name is available).}

@defstruct[(test-failure test-result) ([result any])]{

Subtype of test-result representing a test failure.}

@defstruct[(test-error test-result) ([result exn])]{

Subtype of test-result representing a test error.}

@defstruct[(test-success test-result) ([result any])]{

Subtype of test-result representing a test success.}


@subsection{Functions to Run Tests}

@defproc[(run-test-case (name (or/c string #f)) (action (-> any)))
         test-result]{

Runs the given test case, returning a result representing success,
failure, or error.
}


@defproc[(run-test (test (or/c test-case? test-suite?)))
         (flat-murec-contract ([R (listof (or/c test-result? R))]) R)]{

Runs the given test (test case or test suite) returning a
tree (list of lists) of results}

Example:

@racketblock[
  (run-test
     (test-suite
      "Dummy"
      (test-case "Dummy" (check-equal? 1 2))))
]

@defproc[(fold-test-results [result-fn ('b 'c ... 'a . -> . 'a)]
                            [seed 'a]
			    [test (or/c test-case? test-suite?)]
			    [#:run run (string (() -> any) . -> . 'b 'c ...)]
 			    [#:fdown fdown (string 'a . -> . 'a)] 
			    [#:fup fup (string 'a . -> . 'a)])
          'a]{

Fold @racket[result-fn] pre-order left-to-right depth-first
over the results of @racket[run].  By default @racket[run]
is @racket[run-test-case] and @racket[fdown] and
@racket[fup] just return the seed, so @racket[result-fn] is
folded over the test results.

This function is useful for writing custom folds (and hence UIs) over
test results without you having to take care of all the expected setup
and teardown.  For example, @racket[fold-test-results] will run test
suite before and after actions for you.  However it is still flexible
enough, via its keyword arguments, to do almost anything that
@racket[foldts-test-suite] can.  Hence it should be used in preference to @racket[foldts-test-suite].

The @racket[result-fn] argument is a function from the results of
@racket[run] (defaults to a @racket[test-result]) and the seed to a
new seed.

The @racket[seed] argument is any value.

The @racket[test] argument is a test case or test suite.

The @racket[run] argument is a function from a test case name (string)
and action (thunk) to any values. The values produced by @racket[run]
are fed into the @racket[result-fn].

The @racket[fdown] argument is a function from a test suite name
(string) and the seed, to a new seed.

The @racket[fup] argument is a function from a test suite name
(string) and the seed, to a new seed.
}

Examples:

The following code counts the number of successes:

@racketblock[
(define (count-successes test)
  (fold-test-results
   (lambda (result seed)
     (if (test-success? result)
         (add1 seed)
         seed))
   0
   test))]

The following code returns the symbol @racket['burp] instead
of running test cases.  Note how the @racket[result-fn] receives the
value of @racket[run].

@racketblock[
(define (burp test)
  (fold-test-results
   (lambda (result seed) (cons result seed))
   null
   test
   #:run (lambda (name action) 'burp)))]


@defproc[(foldts-test-suite [fdown (test-suite string thunk thunk 'a -> 'a)]
		 [fup (test-suite string thunk thunk 'a 'a -> 'a)]
		 [fhere(test-case string thunk 'a -> 'a)]
		 [seed 'a]
		 [test (or/c test-case? test-suite?)])
    		 'a]{

The @racket[foldts-test-suite] function is a nifty tree fold (created by Oleg
Kiselyov) that folds over a test in a useful way
(@racket[fold-test-results] isn't that useful as you can't specify
actions around test cases).

The @racket[fdown] argument is a function of test suite, test suite
name, before action, after action, and the seed.  It is run when a
test suite is encountered on the way down the tree (pre-order).

The @racket[fup] argument is a function of test suite, test suite
name, before action, after action, the seed at the current level, and
the seed returned by the children.  It is run on the way up the tree
(post-order).

The @racket[fhere] argument is a function of the test case, test case
name, the test case action, and the seed. (Note that this might change
in the near future to just the test case.  This change would be to
allow @racket[fhere] to discriminate subtypes of test-case, which in
turn would allow test cases that are, for example, ignored).
}

Example:

Here's the implementation of @racket[fold-test-results] in terms of
@racket[foldts-test-suite]:

@racketblock[
(define (fold-test-results suite-fn case-fn seed test)
  (foldts-test-suite
   (lambda (suite name before after seed)
     (before)
     (suite-fn name seed))
   (lambda (suite name before after seed kid-seed)
     (after)
     kid-seed)
   (lambda (case name action seed)
     (case-fn
       (run-test-case name action)
       seed))
   seed
   test))
]

If you're used to folds you'll probably be a bit surprised that the
functions you pass to @racket[foldts-test-suite] receive both the structure they
operate on, and the contents of that structure.  This is indeed
unusual.  It is done to allow subtypes of test-case and test-suite to
be run in customised ways.  For example, you might define subtypes of
test case that are ignored (not run), or have their execution time
recorded, and so on.  To do so the functions that run the test cases
need to know what type the test case has, and hence is is necessary to
provide this information.

If you've made it this far you truly are a master RackUnit hacker.  As
a bonus prize we'll just mention that the code in
@racketfont{hash-monad.rkt} and @racketfont{monad.rkt} might be of
interest for constructing user interfaces.  The API is still in flux,
so isn't documented here.  However, do look at the implementation of
@racket[run-tests] for examples of use.
