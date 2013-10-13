#lang racket/base

#|

This file will fail to compile if
drracket/tool-lib and the contents of
drracket:tool^ get out of sync.

Running it will open up a DrRacket (not
something we need to do for this test).

|#


;; comment out this test case as it makes 'raco setup'
;; complain in its currently-broken state
;; try again after PR 14093 is fixed.
#;
(module just-compile-me racket/base
  (require drracket/tool
           drracket/private/drsig
           drracket/tool-lib
           racket/unit)
  
  (define-values/invoke-unit 
    (unit
      (import drracket:tool^)
      (export drracket:tool-exports^)
      (define phase1 void)
      (define phase2 void))
    (import drracket:tool^)
    (export drracket:tool-exports^)))
