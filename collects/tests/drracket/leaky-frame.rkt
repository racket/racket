#lang racket
(require "drracket-test-util.rkt"
         framework)

(parameterize ([current-command-line-arguments '#()])
  (fire-up-drscheme-and-run-tests 
   (λ ()
     (define drs-frame1 (wait-for-drscheme-frame))
     (sync (system-idle-evt))
     (test:menu-select "File" "New")
     (define drs-frame2b (make-weak-box (wait-for-new-frame drs-frame1)))
     (sync (system-idle-evt))
     (test:menu-select "File" "Close")
     (sync (system-idle-evt))
     (let loop ([n 30])
       (cond
         [(zero? n) 
          (when (weak-box-value drs-frame2b)
            (fprintf (current-error-port) "leak!\n"))]
         [else
          (collect-garbage)
          (when (weak-box-value drs-frame2b)
            (loop (- n 1)))])))))
