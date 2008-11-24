#lang scheme
(require file/md5)

(provide/contract
 [md5-home (parameter/c path-string?)]
 [md5-store (bytes? . -> . bytes?)]
 [md5-lookup (bytes? . -> . bytes?)])

(define md5-home (make-parameter (build-path (find-system-path 'home-dir) ".urls")))

(define (md5-store bs)
  (define hash (md5 bs))
  (with-output-to-file
      (build-path (md5-home) (format "~a" hash))
    (lambda ()
      (write bs))
    #:exists 'replace)
  hash)
(define (md5-lookup hash)
  (with-input-from-file
      (build-path (md5-home) (format "~a" hash))
    (lambda () (read))))
