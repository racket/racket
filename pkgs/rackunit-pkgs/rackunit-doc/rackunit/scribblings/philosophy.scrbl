#lang scribble/doc
@(require "base.rkt")

@title[#:tag "philosophy"]{The Philosophy of RackUnit}

RackUnit is designed to allow tests to evolve in step with
the evolution of the program under testing.  RackUnit
scales from the unstructured checks suitable for simple
programs to the complex structure necessary for large
projects.

Simple programs, such as those in How to Design Programs,
are generally purely functional with no setup required to
obtain a context in which the function may operate.
Therefore the tests for these programs are extremely simple:
the test expressions are single checks, usually for
equality, and there are no dependencies between expressions.
For example, a HtDP student may be writing simple list
functions such as length, and the properties they are
checking are of the form:

@racketblock[
  (equal? (length null) 0)
  (equal? (length '(a)) 1)
  (equal? (length '(a b)) 2)
]

RackUnit directly supports this style of testing.  A check
on its own is a valid test.  So the above examples may be
written in RackUnit as:

@racketblock[
  (check-equal? (length null) 0)
  (check-equal? (length '(a)) 1)
  (check-equal? (length '(a b)) 2)
]

Simple programs now get all the benefits of RackUnit with
very little overhead.

There are limitations to this style of testing that more
complex programs will expose.  For example, there might be
dependencies between expressions, caused by state, so that
it does not make sense to evaluate some expressions if
earlier ones have failed.  This type of program needs a way
to group expressions so that a failure in one group causes
evaluation of that group to stop and immediately proceed to
the next group.  In RackUnit all that is required is to
wrap a @racket[test-begin] expression around a group of
expressions:

@racketblock[
  (test-begin
    (setup-some-state!)
    (check-equal? (foo! 1) 'expected-value-1)
    (check-equal? (foo! 2) 'expected-value-2))
]

Now if any expression within the @racket[test-begin]
expression fails no further expressions in that group will
be evaluated.

Notice that all the previous tests written in the simple
style are still valid.  Introducing grouping is a local
change only.  This is a key feature of RackUnit's support
for the evolution of the program.

The programmer may wish to name a group of tests.  This is
done using the @racket[test-case] expression, a simple
variant on test-begin:

@racketblock[
  (test-case
    "The name"
    ... test expressions ...)
]

Most programs will stick with this style. However,
programmers writing very complex programs may wish to
maintain separate groups of tests for different parts of the
program, or run their tests in different ways to the normal
RackUnit manner (for example, test results may be logged
for the purpose of improving software quality, or they may
be displayed on a website to indicate service quality). For
these programmers it is necessary to delay the execution of
tests so they can be processed in the programmer's chosen
manner. To do this, the programmer simply wraps a test-suite
around their tests:

@racketblock[
  (test-suite
    "Suite name"
    (check ...)
    (test-begin ...)
    (test-case ...))
]

The tests now change from expressions that are immediately
evaluated to objects that may be programmatically
manipulated. Note again this is a local change. Tests
outside the suite continue to evaluate as before.  
  

@section{Historical Context}

Most testing frameworks, including earlier versions of
RackUnit, support only the final form of testing. This is
likely due to the influence of the SUnit testing framework,
which is the ancestor of RackUnit and the most widely used
frameworks in Java, .Net, Python, and Ruby, and many other
languages. That this is insufficient for all users is
apparent if one considers the proliferation of ``simpler''
testing frameworks in Scheme such as SRFI-78, or the
practice of beginner programmers. Unfortunately these
simpler methods are inadequate for testing larger
systems. To the best of my knowledge RackUnit is the only
testing framework that makes a conscious effort to support
the testing style of all levels of programmer.
