#lang scheme/base
(provide all-tests)
(define all-tests (map symbol->string '(
#|

This directory contains code for testing DrScheme. To run the tests,
load run-test.ss. It will return a function that accepts the names of
tests. Those names must be listed here. If no arguments are passed to
the function, all tests will be run.

   stepper-test.ss 

   runs the stepper on the sample solutions and
   checks the results.
   (this test suite is not being maintained)  
 
|# io.ss #|

   This tests the drscheme's io implementation.
   
|# repl-test.ss #|

   This tests various interactions between parameters in the 
   implementation of drscheme.

|# language-test.ss #|   

   This tests that all of the individual settings in the language dialog 
   take effect in the repl.

|# module-lang-test.ss #|

   This tests the code involved in implementing the new module language.
   
   graphics.ss

   This tests the various graphic elements that can appear
   in programs.

  launcher.ss

   This tests the launcher feature of drscheme.

|# sample-solutions-one-window.ss #|

   This tests the sample solutions in HtDP,
   but reuses the same drscheme window.
   There is a race condition in this test,
   so it is commented out here, for now.
   
|# teachpack.ss #|

   Tests the teachpacks

|#)))
