#lang racket/base
(require racket/system
         racket/format
         racket/promise
         "print.rkt")

(provide git)

(define git-exe (delay (find-executable-path
                        (if (eq? (system-type) 'windows)
                            "git.exe"
                            "git"))))

(define (git #:status [status void]
             #:quiet-stderr? [quiet-stderr? #t] ; suppress stderr unless error
             #:fail-mode [fail-mode 'error]
             . args)
  (define exe (force git-exe))
  (unless exe
    (pkg-error (~a "could not find `git' executable\n"
                   "  intended command: git ~a")
               (apply ~a #:separator " " args)))
  (status (apply ~a #:separator " " "git" args))
  (define stderr (if quiet-stderr?
                     (open-output-bytes)
                     (current-error-port)))
  (define r ((parameterize ([current-error-port stderr])
               (with-handlers ([values (lambda (exn)
                                         ;; re-raise after restoring stderr:
                                         (lambda () (raise exn)))])
                 (define r (apply system* exe args))
                 (lambda () r)))))
  (cond
   [r #t]
   [else
    (when quiet-stderr?
      (write-bytes (get-output-bytes stderr) (current-error-port)))
    (case fail-mode
      [(status) #f]
      [else
       (pkg-error "Git command failed")])]))
