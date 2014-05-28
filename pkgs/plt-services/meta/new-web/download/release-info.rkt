#lang racket/base
(require racket/port)

(provide get-version-tag-info)

(define (get-version-tag-info vers)
  (with-handlers ([exn:fail? (lambda (exn)
                               (log-error "error getting v~a announcement: ~a"
                                          vers
                                          (exn-message exn))
                               #f)])
    (define p (collection-file-path (format "v~a.txt" vers)
                                    "meta/announcements"))
    (call-with-input-file p
      (lambda (i)
        (define s (read i))
        (unless (number? s) (error "release info does not start with a number of seconds"))
        (regexp-match #px"^[\\s]*" i) ; discard whitespace
        (list (seconds->date s)
              (port->string i))))))
