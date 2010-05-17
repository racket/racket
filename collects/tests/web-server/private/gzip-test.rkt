#lang racket
(require rackunit
         web-server/private/gzip)
(provide gzip-tests)

(define gzip-tests
  (test-suite
   "GZip"
   
   #;(test-case 
    "gzip/bytes"
    (check-equal? (gzip/bytes #"1234567890")
                  #"\37\213\b\0\334k\220I\0\0033426153\267\2604\0\0\345\256\35&\n\0\0\0"))
   
   (test-case 
    "gunzip/bytes"
    (check-equal? (gunzip/bytes #"\37\213\b\0.k\220I\0\0033426153\267\2604\0\0\345\256\35&\n\0\0\0")
                  #"1234567890"))
   
   (test-case
    "identity"
    (let* ([k (random 1000)]
           [b (random 255)]
           [bs (make-bytes k b)])
      (check-equal? (gunzip/bytes (gzip/bytes bs))
                    bs)))
   
   (test-case
    "bad header"
    (check-exn exn?
                (lambda ()
                  (gunzip/bytes #"1234567890"))))))
