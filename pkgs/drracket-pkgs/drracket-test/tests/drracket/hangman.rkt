#lang racket/base
(require "private/drracket-test-util.rkt"
         racket/class
         racket/gui/base)

(fire-up-drracket-and-run-tests 
 (λ ()
   (define drs (wait-for-drracket-frame))
   (define defs (send drs get-definitions-text))
   (define rep (send drs get-interactions-text))
   (set-language-level! (list #rx"Beginning Student$"))
   (run-one/sync
    (lambda ()
       (send defs load-file (collection-file-path "hangman1.rkt" "htdp" "tests"))))
   (do-execute drs)
   (insert-in-interactions drs "(hangman make-word reveal symbol?)")
   (alt-return-in-interactions drs)
   (define (user-hangman-frame?)
     (define windows (parameterize ([current-eventspace (send rep get-user-eventspace)])
                       (get-top-level-windows)))
     (define labels (map (λ (x) (send x get-label)) windows))
     (member "Hangman" labels))
   (poll-until user-hangman-frame?)))
