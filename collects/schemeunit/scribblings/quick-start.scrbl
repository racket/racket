#lang scribble/doc
@(require "base.ss")

@title[#:tag "quick-start"]{Quick Start Guide for SchemeUnit}

Suppose we have code contained in @tt{file.scm}, which
implements buggy versions of @scheme[+] and @scheme[-]
called @scheme[my-+] and @scheme[my--]:

@schememod[
scheme/base

(define (my-+ a b)
  (if (zero? a)
      b
      (my-+ (sub1 a) (add1 b))))

(define (my-* a b)
  (if (zero? a)
      b
      (my-* (sub1 a) (my-+ b b))))

(provide my-+
         my-*)
]

We want to test this code with SchemeUnit.  We start by
creating a file called @tt{file-test.scm} to contain our
tests.  At the top of @tt{file-test.scm} we import
SchemeUnit and @tt{file.scm}:

@schememod[
scheme/base

(require schemeunit
         "file.scm")
]

Now we add some tests to check our library:

@schemeblock[
(check-equal? (my-+ 1 1) 2 "Simple addition")
(check-equal? (my-* 1 2) 2 "Simple multiplication")
]

This is all it takes to define tests in SchemeUnit.  Now
evaluate this file and see if the library is correct.
Here's the result I get:

@verbatim{
#t
--------------------
FAILURE
name:       check-equal?
location:   (file-test.scm 7 0 117 27)
expression: (check-equal? (my-* 1 2) 2)
params:     (4 2)
actual:     4
expected:   2

--------------------}

The first @scheme[#t] indicates the first test passed.  The
second test failed, as shown by the message.

Requiring SchemeUnit and writing checks is all you need to
get started testing, but let's take a little bit more time
to look at some features beyond the essentials.

Let's say we want to check that a number of properties hold.
How do we do this?  So far we've only seen checks of a
single expression.  In SchemeUnit a check is always a single
expression, but we can group checks into units called test
cases.  Here's a simple test case written using the
@scheme[test-begin] form:

@schemeblock[
(test-begin
 (let ((lst (list 2 4 6 9)))
   (check = (length lst) 4)
   (for-each
    (lambda (elt)
      (check-pred even? elt))
    lst)))
]

Evalute this and you should see an error message like:

@verbatim{
--------------------
A test 
... has a FAILURE
name:       check-pred
location:   (#<path:/Users/noel/programming/schematics/schemeunit/branches/v3/doc/file-test.scm> 14 6 252 22)
expression: (check-pred even? elt)
params:     (#<procedure:even?> 9)
--------------------
}

This tells us that the expression @scheme[(check-pred even?
elt)] failed.  The arguments of this check were
@scheme[even?] and @scheme[9], and as 9 is not even the
check failed.  A test case fails as soon as any check within
it fails, and no further checks are evaluated once this
takes place.

Naming our test cases if useful as it helps remind us what
we're testing.  We can give a test case a name with the
@scheme[test-case] form:

@schemeblock[
(test-case
 "List has length 4 and all elements even"
 (let ((lst (list 2 4 6 9)))
   (check = (length lst) 4)
   (for-each
    (lambda (elt)
      (check-pred even? elt))
    lst)))
]

Now if we want to structure our tests are bit more we can
group them into a test suite:

@schemeblock[
  (define file-tests
    (test-suite
     "Tests for file.scm"

     (check-equal? (my-+ 1 1) 2 "Simple addition")

     (check-equal? (my-* 1 2) 2 "Simple multiplication")

     (test-case
      "List has length 4 and all elements even"
      (let ((lst (list 2 4 6 9)))
        (check = (length lst) 4)
        (for-each
          (lambda (elt)
            (check-pred even? elt))
        lst)))))
]

Evaluate the module now and you'll see the tests no longer
run.  This is because test suites delay execution of their
tests, allowing you to choose how you run your tests.  You
might, for example, print the results to the screen or log
them to a file.

Let's run our tests, using SchemeUnit's simple textual user
interface (there are fancier interfaces available but this
will do for our example).  In @tt{file-test.scm} add the
following lines:

@schemeblock[
  (require schemeunit/text-ui)

  (run-tests file-tests)
]

Now evaluate the file and you should see similar output
again.

These are the basics of SchemeUnit.  Refer to the
documentation below for more advanced topics, such as
defining your own checks.  Have fun!
