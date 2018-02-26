#lang racket/base
(require '#%foreign
         "../common/check.rkt"
         "../file/host.rkt"
         "path.rkt")

(provide _path)

(define/who _path
  (make-ctype _bytes
              (lambda (p)
                (check who path-string? #:or-false p)
                (and p (bytes-append (->host p #f '()) #"\0")))
              (lambda (bstr) (and bstr (path (bytes->immutable-bytes bstr)
                                             (system-path-convention-type))))))
