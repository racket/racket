#lang racket/base
(require rackunit
         mzlib/list
         web-server/http
         "../util.rkt")
(provide test-add-two-numbers
         test-double-counters
         url0
         url0s)

(define url0 "http://test.com/servlets/example.rkt")
(define url0s (list (build-path "servlets") (build-path "example.rkt")))

(define (test-add-two-numbers mkd t p)
  (let* ([x (random 500)]
         [xs (string->bytes/utf-8 (number->string x))]
         [y (random 500)]
         [ys (string->bytes/utf-8 (number->string y))])
    (test-equal? 
     t
     (let* ([d (mkd p)]
            [r0 (call d url0 empty)]
            [k0 (simple-xpath* '(form #:action) r0)]
            [i0 (simple-xpath* '(form input #:name) r0)]
            [r1 (call d (format "~a?~a=~a" k0 i0 xs)
                      (list (make-binding:form (string->bytes/utf-8 i0) xs)))]
            [k1 (simple-xpath* '(form #:action) r1)]
            [i1 (simple-xpath* '(form input #:name) r1)]
            [r2 (call d (format "~a?~a=~a" k1 i1 ys)
                      (list (make-binding:form (string->bytes/utf-8 i1) ys)))]
            [n (simple-xpath* '(p) r2)])
       n)
     (format "The answer is ~a" (+ x y)))))

(define (test-double-counters mkd t p)
  (define d (mkd p))
  (define (invoke u)
    (define sx (call d u empty))
    (define ks (simple-xpath*/list '(div div a #:href) sx))
    (values (simple-xpath*/list '(div div h3) sx)
            (first ks)
            (second ks)))
  (test-equal? t
               (let*-values ([(v0.0 0.0+1 0.0+2) (invoke url0)]
                             ; One add
                             [(v1.0 1.0+1 1.0+2) (invoke 0.0+1)] ; XXX infinite loop after this
                             [(v0.1 0.1+1 0.1+2) (invoke 0.0+2)]
                             ; Two adds
                             [(v2.0 2.0+1 2.0+2) (invoke 1.0+1)]
                             [(v1.1 1.1+1 1.1+2) (invoke 0.1+1)]
                             [(_v1.1 _1.1+1 _1.1+2) (invoke 1.0+2)]
                             [(v0.2 0.2+1 0.2+2) (invoke 0.1+2)])
                 (list v0.0
                       v1.0 v0.1
                       v2.0 v1.1 _v1.1 v0.2))
               (list (list "0" "0")
                     (list "1" "0") (list "0" "1")
                     (list "2" "0") (list "1" "1") (list "1" "1") (list "0" "2"))))
