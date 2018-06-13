#lang racket/base
(require racket/file)

(provide rename-directory)

(define (rename-directory old-path new-path)
  (cond
    [(eq? 'windows (system-type))
     (with-handlers* ([(lambda (exn)
                         (and (exn:fail:filesystem:errno? exn)
                              (let ([errno (exn:fail:filesystem:errno-errno exn)])
                                (and (eq? 'windows (cdr errno))
                                     (eqv? (car errno) 5))))) ; ERROR_ACCESS_DENIED
                       (lambda (exn)
                         ;; ERROR_ACCESS_DENIED can mean that a file within the
                         ;; directory is open. We can't just rename the directory
                         ;; in that case, but we can copy it.
                         (copy-directory/files old-path new-path
                                               #:keep-modify-seconds? #t
                                               #:preserve-links? #t)
                         (delete-directory/files old-path))])
       (rename-file-or-directory old-path new-path))]
    [else
     (rename-file-or-directory old-path new-path)]))
