#lang scribble/doc
@(require "base.rkt")

@title{Test Control Flow}

The @racket[before], @racket[after], and @racket[around]
macros allow you to specify code that is always run before,
after, or around expressions in a test case.

@defform[(before before-expr expr1 expr2 ...)]{

Whenever control enters the scope execute the @racket[before-expr]
before executing @racket[expr-1], and @racket[expr-2 ...]}

@defform[(after expr-1 expr-2 ... after-expr)]{

Whenever control exits the scope execute the @racket[after-expr]
after executing @racket[expr-1], and @racket[expr-2 ...]  The @racket[after-expr] is
executed even if control exits via an exception or other means.}

@defform[(around before-expr expr-1 expr-2 ... after-expr)]{

Whenever control enters the scope execute the
@racket[before-expr] before executing @racket[expr-1 expr-2
...], and execute @racket[after-expr] whenever control
leaves the scope.}

Example:

The test below checks that the file @tt{test.dat} contains
the string @tt{"foo"}.  The before action writes to this
file.  The after action deletes it.

@racketblock[
 (around
   (with-output-to-file "test.dat"
      (lambda ()
        (write "foo")))
   (with-input-from-file "test.dat"
     (lambda ()
       (check-equal? "foo" (read))))
   (delete-file "test.dat"))
]


@defform[(delay-test test1 test2 ...)]{

This somewhat curious macro evaluates the given tests in a
context where @racket[current-test-case-around] is
parameterized to @racket[test-suite-test-case-around].  This
has been useful in testing RackUnit.  It might be useful
for you if you create test cases that create test cases.}
