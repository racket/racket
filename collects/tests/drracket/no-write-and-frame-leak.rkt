#lang racket
(require "private/drracket-test-util.rkt"
         framework)

(parameterize ([current-security-guard
                (make-security-guard
                 (current-security-guard)
                 (λ (who pth what)
                   (when (member 'write what)
                     (error who "Writing to the file system is not allowed"))
                   (when (member 'delete what)
                     (error who "Deleting files is not allowed")))
                 void
                 void)])
  (fire-up-drracket-and-run-tests 
   (λ ()
     (define drs-frame1 (wait-for-drracket-frame))
     (sync (system-idle-evt))
     
     (test:menu-select "File" "New Tab")
     (sync (system-idle-evt))
     
     (define drs-tabb (make-weak-box (send drs-frame1 get-current-tab)))
     (define tab-nsb (make-weak-box (send (send (send drs-frame1 get-current-tab) get-ints) get-user-namespace)))
     
     (test:menu-select "File" (if (eq? (system-type) 'unix) "Close" "Close Tab"))
     (sync (system-idle-evt))
     
     (test:menu-select "File" "New")
     (sync (system-idle-evt))
     
     (define drs-frame2b (make-weak-box (wait-for-new-frame drs-frame1)))
     (define frame2-nsb (make-weak-box (send (send (send (weak-box-value drs-frame2b) get-current-tab) get-ints) get-user-namespace)))
     
     (test:menu-select "File" (if (eq? (system-type) 'unix) "Close" "Close Window"))
     (sync (system-idle-evt))
     
     (let loop ([n 30])
       (cond
         [(zero? n) 
          (when (weak-box-value drs-tabb)
            (fprintf (current-error-port) "frame leak!\n"))
          (when (weak-box-value drs-frame2b)
            (fprintf (current-error-port) "tab leak!\n"))
          (when (weak-box-value tab-nsb)
            (fprintf (current-error-port) "tab namespace leak!\n"))
          (when (weak-box-value frame2-nsb)
            (fprintf (current-error-port) "frame namespace leak!\n"))]
         [else
          (collect-garbage)
          (when (ormap weak-box-value 
                       (list drs-tabb
                             tab-nsb
                             drs-frame2b
                             frame2-nsb))
            (loop (- n 1)))])))))
