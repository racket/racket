#lang racket
(require (planet jaymccarthy/dbm)
         "wrap-dict.rkt")

(define (read-string s)
  (with-input-from-string s read))
(define (write-string v)
  (with-output-to-string (lambda () (write v))))

(define make-read/write-dict
  (make-wrapped-dict
   write-string read-string
   write-string read-string))

(define (call-with-vdbm pth f)
  (define a-dbm #f)
  (dynamic-wind
   (lambda ()
     (set! a-dbm (dbm-open pth)))
   (lambda ()
     (f (make-read/write-dict a-dbm)))
   (lambda ()
     (dbm-close! a-dbm))))

(provide/contract
 [call-with-vdbm (path-string? (dict? . -> . any) . -> . any)])

;; Test

(call-with-vdbm 
 "test"
 (lambda (a-dict)
   (list
    (dict-set! a-dict (cons 1 2) 50)
    (dict-ref a-dict (cons 1 2))
    
    (for/list ([k (in-dict-keys a-dict)])
      k))))

