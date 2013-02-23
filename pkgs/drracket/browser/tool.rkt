#lang racket
(require (only-in "external.rkt" install-help-browser-preference-panel)
         racket/unit
         drracket/tool)
(provide tool@)

;; to add a preference pannel to drracket that sets the browser preference
(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)
  
    (define (phase1) (void))
    (define (phase2) (void))
    
    (install-help-browser-preference-panel)))