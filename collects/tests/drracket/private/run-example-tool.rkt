#lang racket/base
(require "drracket-test-util.rkt"
         setup/setup-unit
         setup/option-sig
         setup/option-unit
         launcher/launcher-unit
         launcher/launcher-sig
         compiler/sig
         compiler/compiler-unit
         compiler/option-unit
         dynext/compile-sig
         dynext/compile-unit
         dynext/link-sig
         dynext/link-unit
         dynext/file-sig
         dynext/file-unit
         racket/unit
         racket/gui/base
         racket/class
         framework/test
         mrlib/switchable-button)

(provide go)

(define (go)
  
  (define init-options@
    (unit (import setup-option^)
          (export)
          (make-zo #f)
          (make-launchers #f)
          (make-docs #f)
          (call-install #f)
          (call-post-install #f)
          (setup-program-name "raco setup")
          (specific-collections '(("coll")))))
  
  (let ([c (make-custodian)])
    (parameterize ([current-custodian c]
                   [exit-handler 
                    (λ (x)
                      (custodian-shutdown-all c))])
      (invoke-unit  
       (compound-unit
         (import)
         (export)
         (link 
          [((OPTIONS : setup-option^)) setup:option@]
          [() init-options@ OPTIONS]
          [((LAUNCHER : launcher^)) launcher@]
          [((COMPILER-OPTION : compiler:option^)) compiler:option@]
          [((DYNEXT-COMPILE : dynext:compile^)) dynext:compile@]
          [((DYNEXT-FILE : dynext:file^)) dynext:file@]
          [((DYNEXT-LINK : dynext:link^)) dynext:link@]
          [((COMPILER : compiler^)) compiler@ COMPILER-OPTION DYNEXT-FILE DYNEXT-COMPILE DYNEXT-LINK]
          [() setup@ LAUNCHER OPTIONS COMPILER-OPTION COMPILER DYNEXT-FILE])))))
  
  (fire-up-drracket-and-run-tests
   (λ () 
     (define drs (wait-for-drracket-frame))
     (queue-callback/res (λ () (send (send drs get-definitions-canvas) focus)))
     (for ([x (in-string "egg\r1\r2\r3")])  
       ;; need #\r to actually get newlines in the editor
       ;; see test:keystroke docs
       (test:keystroke x))
     
     (queue-callback/res
      (λ ()
        (define btn
          (for/or ([x (in-list (send (send drs get-button-panel) get-children))])
            (and (is-a? x switchable-button%)
                 (equal? (send x get-button-label) "Reverse Definitions")
                 x)))
        (send btn command)))
     
     (define content 
       (queue-callback/res (λ () (send (send drs get-definitions-text) get-text))))
     (define expected (apply string (reverse (string->list "easter egg\n1\n2\n3"))))
     (unless (equal? content expected)
       (eprintf "example-tool.rkt: test failed;\nexpected ~s\n but got ~s"
                expected
                content)))))
