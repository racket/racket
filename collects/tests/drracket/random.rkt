#lang racket

(require "private/drracket-test-util.rkt"
         tests/utils/gui
         mred
         framework
         (prefix-in fw: framework))

(define (run-test)
  (define dr (wait-for-drscheme-frame))
  
  (clear-definitions dr)
  (insert-in-definitions dr (file->string "/home/mflatt/svn/mflatt/text/expmodel/mm-defs.rkt"))

  (let loop ()
    (define n (random 100))
    (cond
     [(n . < . 10)
      (test:keystroke #\A)
      (test:keystroke #\backspace)]
     [(n . < . 50)
      (test:keystroke 'left)]
     [(n . < . 60)
      (test:keystroke 'up)]
     [(n . < . 70)
      (test:keystroke 'down)]
     [(n . < . 80)
      (test:keystroke 'right)]
     [(n . < . 82)
      (sleep 10)]
     [(n . < . 83)
      (test:keystroke 'f5)
      (test:keystroke #\x '(control))
      (test:keystroke #\o)]
     [else
      (test:keystroke 'left)
      (test:keystroke 'right)
      (test:keystroke #\backspace)
      (test:keystroke #\z '(meta))])
    (sleep 0.25)
    (loop)))

(dynamic-require 'drscheme #f)
      
(fw:test:use-focus-table #t)

(thread (λ () 
           (let ([orig-display-handler (error-display-handler)])
             (uncaught-exception-handler
              (λ (x)
                 (if (exn? x)
                     (orig-display-handler (exn-message x) x)
                     (fprintf (current-error-port) "uncaught exception ~s\n" x))
                 (exit 1))))
           (run-test)
           (exit)))
(yield (make-semaphore 0))


