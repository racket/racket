#lang racket/base
#|

Adds the incremental-keybindings.rkt file (also shown in the docs)
to DrRacket and then tries out the keystrokes.

|#

(require "private/drracket-test-util.rkt"
         framework/test
         racket/class)

(fire-up-drracket-and-run-tests 
 (λ ()
   (define drs-frame (wait-for-drracket-frame))
   (use-get/put-dialog 
    (λ ()
      (test:menu-select "Edit" "Keybindings" "Add User-defined Keybindings..."))
    (collection-file-path "incremental-keybindings.rkt" 
                          "scribblings"
                          "drracket"))
   (insert-in-definitions drs-frame "#lang racket/base\n")
   (do-execute drs-frame)
   
   (insert-in-definitions drs-frame "(+ 1 (+ 2 3))")
   (queue-callback/res
    (λ () 
      (define defs (send drs-frame get-definitions-text))
      (send defs set-position (+ (send defs paragraph-start-position 1) 5))))
   (test:keystroke #\c '(control))
   (test:keystroke #\e '(control))
   (wait-for-computation drs-frame)
   (test:keystroke 'right '(alt shift))
   (test:keystroke #\c '(control))
   (test:keystroke #\r '(control))
   (wait-for-computation drs-frame)
   (define got
     (queue-callback/res
      (λ () 
        (define ints (send drs-frame get-interactions-text))
        (send ints get-text
              (send ints paragraph-start-position 2)
              (send ints last-position)))))
   
   (unless (equal? got "> (+ 1 (+ 2 3))\n6\n> (+ 2 3)\n5\n> ")
     (error 'incrementalkeybindings-test.rkt "failed-test; got ~s" got))))

