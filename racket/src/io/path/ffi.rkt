#lang racket/base
(require '#%foreign
         "../common/check.rkt"
         "path.rkt")

(provide _path)

(define/who _path
  (make-ctype _bytes
              (lambda (p)
                (check who path-string? #:or-false p)
                ;; Don't use `->host`, because it converts relative paths
                ;; to absolute paths:
                (and p (bytes-append (path-bytes (->path p)) #"\0")))
              (lambda (bstr) (and bstr (path (bytes->immutable-bytes bstr)
                                             (system-path-convention-type))))))
