#lang racket/base
(require "private/drracket-test-util.rkt"
         racket/class
         racket/gui/base
         framework)

(let ([raw-autosave-contents
       (if (file-exists? autosave:toc-path)
           (call-with-input-file autosave:toc-path read)
           '())])
  (define autosave-contents 
    (filter (λ (x) (file-exists? (bytes->path (list-ref x 1))))
            raw-autosave-contents))
  (unless (null? autosave-contents)
    (error 'autosave-recovery-gui.rkt 
           "there was a non-empty autosave toc when starting the test, so the test won't work right; contents:\n  ~s"
           autosave-contents)))
   
(fire-up-drracket-and-run-tests 
 (λ ()
   (define drs (wait-for-drracket-frame))
   (define defs (send drs get-definitions-text))
   
   
   (queue-callback/res (λ () 
                         ;; insert something so the save file is out of date
                         (send defs insert "\n1")
                         
                         ;; set the autosave delay to 1 second
                         (preferences:set 'framework:autosave-delay 1)))
   ;; wait for 2 seconds
   (sleep 2)
   (queue-callback
    (λ () 
      ((dynamic-require 'framework 'autosave:restore-autosave-files/gui))))
   
   (define new-frame (wait-for-new-frame drs))
   (define new-label (send new-frame get-label))
   (queue-callback/res (λ () (send defs undo)))
   (unless (equal? "Recover Autosaved Files" new-label)
     (error 'autosave-recovery-gui.rkt
            "didn't get autosave file frame, title was ~s" new-label))))
