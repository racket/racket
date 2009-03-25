#lang scribble/doc
@(require "base.ss")

@title{Test Control Flow}

The @scheme[before], @scheme[after], and @scheme[around]
macros allow you to specify code that is always run before,
after, or around expressions in a test case.

@defform[(before before-expr expr1 expr2 ...)]{

Whenever control enters the scope execute the @scheme[before-expr]
before executing @scheme[expr-1], and @scheme[expr-2 ...]}

@defform[(after expr-1 expr-2 ... after-expr)]{

Whenever control exits the scope execute the @scheme[after-expr]
after executing @scheme[expr-1], and @scheme[expr-2 ...]  The @scheme[after-expr] is
executed even if control exits via an exception or other means.}

@defform[(around before-expr expr-1 expr-2 ... after-expr)]{

Whenever control enters the scope execute the
@scheme[before-expr] before executing @scheme[expr-1 expr-2
...], and execute @scheme[after-expr] whenever control
leaves the scope.}

Example:

The test below checks that the file @tt{test.dat} contains
the string @tt{"foo"}.  The before action writes to this
file.  The after action deletes it.

@schemeblock[
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
context where @scheme[current-test-case-around] is
parameterized to @scheme[test-suite-test-case-around].  This
has been useful in testing SchemeUnit.  It might be useful
for you if you create test cases that create test cases.}
