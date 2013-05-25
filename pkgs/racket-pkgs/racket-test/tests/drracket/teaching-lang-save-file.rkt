#lang scheme/base
(require scheme/file
         scheme/class
         scheme/port
         framework/test
         framework/preferences)

(require tests/drracket/private/drracket-test-util)

(fire-up-drracket-and-run-tests
 (λ ()
   (let* ([drr-frame (wait-for-drracket-frame)]
          [fn (make-temporary-file "save-teaching-lang-test~a")])
     (test:menu-select "File" "New Tab")
     
     (let ([definitions-text (send drr-frame get-definitions-text)]
           [interactions-text (send drr-frame get-interactions-text)])
       
       (set-language-level! (list #rx"Beginning Student$"))
       (clear-definitions drr-frame)
       (queue-callback/res
        (λ () 
          (send definitions-text set-filename fn)
          (send definitions-text insert "(define (f x) x)\n(f 1)\n")))
       (test:menu-select "File" "Save Definitions")
       (unless (call-with-input-file fn
                 (λ (p) (regexp-match #rx";;[^\n]*metadata" p)))
         
         (eprintf "---- saved file, cut here ----\n")
         (call-with-input-file fn (λ (p) (copy-port p (current-error-port))))
         (eprintf "---- saved file, cut here ----\n")
         (error 'save-teaching-lang-file.rkt
                "expected the saved file to contain the word 'metadata' in a comment"))
       (do-execute drr-frame)
       (test:menu-select "File" (if (eq? (system-type) 'unix) "Close" "Close Tab"))
       (use-get/put-dialog 
        (λ () 
          (test:menu-select "File" "Open..."))
        fn)
       (do-execute drr-frame)
       (let ([result (fetch-output
                      drr-frame
                      (send interactions-text paragraph-start-position 2)
                      (send interactions-text last-position))])
         (test:menu-select "File" (if (eq? (system-type) 'unix) "Close" "Close Tab"))
         (delete-file fn)
         (unless (equal? result "1\n> ")
           (error 'save-teaching-lang-file.rkt "expected the program to produce 1 (followed by the prompt), got ~s" result)))))))
