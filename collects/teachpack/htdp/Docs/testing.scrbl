#lang scribble/doc

@begin[(require
	 (lib "manual.ss" "scribble"))
       
       (require-for-label 
	 (lib "lang.ss" "big")
	 "../testing.ss")]

@title[#:tag "testing"]{Testing: testing.ss}

The @scheme[testing.ss] teachpack provides forms for formulating test cases
and a primitive for reporting on test cases. 

@defform[(check-expect test-expression expected-value)]{
          where @scheme[test-expression] and
          @scheme[expected-value] are both expressions, though the latter
          should ideally be just a value. 

          The form evaluates @scheme[test-expression] and then
          verifies that its value is the same as
          @scheme[expected-value].}

@defform[(check-within test-expression expected-value delta)]{
          where @scheme[test-expression], @scheme[expected-value], and
          @scheme[delta] are expressions. 

The form evaluates @scheme[test-expression] and verifies that all numbers
within the value are equal to numbers in analogous positions in
@scheme[expected-value], plus or minus @scheme[delta].

Thus,
@schemeblock[(check-within (make-posn 10 .1) (make-posn 10 .109) .01)]
succeeds while
@schemeblock[(check-within (make-posn 10 .1) (make-posn 10 .19) .01)]
fails. 
}

@defform[(check-error test-expression message-string)]{
 where @scheme[test-expression] and @scheme[expected-message] are expressions. 

The form evaluates @scheme[test-expression] and verifies that it signals an
error with @scheme[message-string] as the error report. If
@scheme[test-expression] doesn't signal an error, the test case fails. 
}

@defproc[(generate-report) true]{
 Displays statistics of running tests created with  @scheme[check-expect],
 @scheme[check-within], and @scheme[check-error].

 Place it at the end of the program or run the expression from the
 Interactions window after clicking RUN.
}

@section{Style}

While you are developing functions, you should place test cases below the
relevant function definitions in the Definitions Window of
drscheme. Eventually though, you should collect all these tests at the bottom of
the window.
