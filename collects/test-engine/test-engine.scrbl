#lang scribble/doc
@(require scribble/manual
	  (for-label scheme/base
                     #;test-engine/scheme-tests))

@title{Test Support}

@author["Kathryn Gray"]

@table-of-contents[]

@; ----------------------------------------------------------------------

@section{Using check forms}

@defmodule[test-engine/scheme-tests]
@defmodule[test-engine/scheme-gui]

These modules provide test forms for use in Scheme programs, as well as parameters to configure the behavior of test reports. 

The gui module requires MrEd and produces an independent window when displaying test results. Both modules provide an identical 
set of check forms.

Each check form may only occur at the top-level or within the definitions of a local declaration; 
results are collected and reported by the test function.

 @defproc[(check-expect (test any/c) (expected any/c)) void?]


 Accepts two value-producing expressions and structurally compares the resulting values.
         
 It is an error to produce a function value or an inexact number.
 
 
@defproc[(check-within (test any/c) (expected any/c) (delta number?)) void?]

Like @scheme[check-expect], but with an extra expression that produces a number delta. Every number in the first expression
     must be within delta of the cooresponding number in the second expression.
     
     It is an error to produce a function value.
     
     
@defproc[(check-error (test any/c) (msg string?)) void?] 
Checks that evaluating the first expression signals an error, where the error message matches the string.
       

@defproc[(test) void?] 
                  Runs all of the tests specified by check forms in the current module and reports the results.
                  When using the gui module, the results are provided in a separate window, otherwise the results
                  are printed to the current output port. 

@defproc[(test-format) (-> any/c string?)] 
   A parameter that stores the formatting function for the values tested by the check forms. 

@defproc[(test-silence) bool?] 
                                A parameter that stores a boolean, defaults to #f, that can be used to suppress the printed
                                  summary from test.

@defproc[(test-execute) bool?] A parameter that stores a boolean, defaults to #t, that can be used to suppress evaluation of test expressions.


@section{Integrating languages with Test Engine}
To be written.