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
   (define defs (queue-callback/res (λ () (send drs-frame get-definitions-text))))
   (define (get-repl-contents)
     (queue-callback/res
      (λ () 
        (define ints (send drs-frame get-interactions-text))
        (send ints get-text
              (send ints paragraph-start-position 2)
              (send ints last-position)))))
   
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
      (send defs set-position (+ (send defs paragraph-start-position 1) 5))))
   (test:keystroke #\c '(control))
   (test:keystroke #\e '(control))
   (wait-for-computation drs-frame)
   (test:keystroke 'right '(alt shift))
   (test:keystroke #\c '(control))
   (test:keystroke #\r '(control))
   (wait-for-computation drs-frame)
   (define got (get-repl-contents))
   
   (unless (equal? got "> (+ 1 (+ 2 3))\n6\n> (+ 2 3)\n5\n> ")
     (error 'incremental-keybindings-test.rkt "failed-test.1; got ~s" got))
   
   
   ;; test c:c;c:e when the insertion
   ;; point is at the end of the editor
   (do-execute drs-frame)
   (queue-callback/res (λ () 
                         (send (send defs get-canvas) focus)
                         (send defs set-position (send defs last-position) (send defs last-position))))
   (test:keystroke #\c '(control))
   (test:keystroke #\e '(control))
   (queue-callback/res void) 
   (sync (system-idle-evt))
   (wait-for-computation drs-frame)
   (define got2 (get-repl-contents))
   (unless (equal? got2 "6\n> (+ 1 (+ 2 3))\n6\n> ")
     (error 'incremental-keybindings-test.rkt "failed-test.2; got ~s" got2))))
