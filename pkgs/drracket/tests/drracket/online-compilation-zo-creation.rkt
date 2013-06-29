#lang racket/base
(require "private/drracket-test-util.rkt"
         racket/file
         drracket/private/local-member-names
         racket/class
         racket/path
         (submod drracket/private/module-language oc-status-structs))
(fire-up-drracket-and-run-tests 
 (λ ()
   (define tmp-dir (make-temporary-file "online-compilation-zo-creation~a" 'directory))
   (define x.rkt (build-path tmp-dir "x.rkt"))
   (define y.rkt (build-path tmp-dir "y.rkt"))
   (call-with-output-file x.rkt
     (λ (port)
       (fprintf port "#lang racket/base\n")
       (fprintf port "~s\n" `(require "y.rkt")))
     #:exists 'truncate)
   (call-with-output-file y.rkt
     (λ (port)
       (fprintf port "#lang racket/base\n"))
     #:exists 'truncate)
   (define drs-frame (wait-for-drracket-frame))
   (queue-callback/res
    (λ ()
      (send (send drs-frame get-definitions-text) load-file x.rkt)))
   (poll-until
    (λ ()
      (queue-callback/res
       (λ ()
         (clean? (send (send drs-frame get-current-tab) get-oc-status))))))
   
   (define compiled-dir-files
     (cond
       [(directory-exists? (build-path tmp-dir "compiled"))
        (for/list ([file (in-directory (build-path tmp-dir "compiled"))])
          (path->string (find-relative-path tmp-dir file)))]
       [else
        '()]))
   (define expected-file "compiled/drracket/errortrace/y_rkt.zo")
   (unless (member expected-file compiled-dir-files)
     (eprintf "expected to find ~s in compiled dir but it contained ~s\n"
              expected-file compiled-dir-files))
   
   (delete-directory/files tmp-dir)))
