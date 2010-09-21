#lang racket/base
(require "drracket-test-util.rkt"
         racket/class)

(fire-up-drscheme-and-run-tests 
 (λ ()
   (define drs (wait-for-drscheme-frame))
   (define defs (send drs get-definitions-text))
   (define rep (send drs get-interactions-text))
   (set-language-level! (list #rx"How to Design Programs" #rx"Beginning Student$"))
   (send defs load-file (collection-file-path "hangman1.rkt" "htdp" "tests"))
   (do-execute drs)
   (insert-in-interactions drs "(hangman make-word reveal symbol?)")
   (alt-return-in-interactions drs)
   (define hangman-frame (wait-for-new-frame drs (list (send rep get-user-eventspace))))
   (cond
     [(equal? (send hangman-frame get-label) "Hangman")
      (printf "Hangman test passed.\n")]
     [else
      (error 'hangman.rkt "expected a hangman frame to appear, but got one with the label ~s" 
             (send hangman-frame get-label))])))
