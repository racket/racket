#lang racket

(require tests/drracket/private/drracket-test-util
         drracket/private/local-member-names
         racket/gui/base
         framework
         rackunit
         string-constants)

(fire-up-drracket-and-run-tests 
 (λ ()
   (define drr (wait-for-drracket-frame))
   (queue-callback/res (λ () (send (send drr get-definitions-canvas) focus)))
   (test:menu-select "Insert" "Insert Large Letters...")
   (define insert-frame (wait-for-new-frame drr))
   (for ([c (in-string "TR Rulez!")])
     (test:keystroke c))
   (test:button-push "OK")
   (wait-for-new-frame insert-frame)
   (define defs-content
     (queue-callback/res
      (λ () (send (send drr get-definitions-text) get-text))))
   (define semis (for/sum ([i (in-string defs-content)])
                   (if (equal? i #\;)
                       1
                       0)))
   (define spaces (for/sum ([i (in-string defs-content)])
                    (if (equal? i #\space)
                        1
                        0)))
   (unless (and (< 20 spaces)
                (< 20 semis))
     (error 'insert-large-letters-test.rkt 
            "expected more semis or spaces; definitions content was:\n~a"
            defs-content))))

