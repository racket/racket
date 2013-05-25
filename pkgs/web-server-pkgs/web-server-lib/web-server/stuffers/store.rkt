#lang racket/base
(require racket/contract)

(define-struct store (write read))

(define (dir-store home)
  (make-store
   (lambda (key value)
     (with-output-to-file
         (build-path home (bytes->string/utf-8 key))
       (lambda ()
         (write value))
       #:exists 'replace))
   (lambda (key)
     (with-input-from-file
         (build-path home (bytes->string/utf-8 key))
       (lambda () (read))))))

(provide/contract
 [struct store ([write (bytes? bytes? . -> . void)]
                [read (bytes? . -> . bytes?)])]
 [dir-store (path-string? . -> . store?)])
