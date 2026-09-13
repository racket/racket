#lang racket/base
(require "../host/place-local.rkt")

(provide address-bytes-cache-bytes-ref
         address-bytes-cache-ref
         address-bytes-cache-set!)

;; list of (cons addr-bytes (cons address-string port-number))
(define-place-local cache (box null))

(define (address-bytes-cache-bytes-ref address-bstr)
  (for/or ([p (in-list (unbox cache))])
    (and (equal? (car p) address-bstr)
         (cdr p))))

(define (address-bytes-cache-ref hostname port-no)
  (for/or ([p (in-list (unbox cache))])
    (and (equal? (cadr p) hostname)
         (equal? (cddr p) port-no)
         (car p))))

(define (address-bytes-cache-set! address-bstr address+pos)
  (let* ([old-l (unbox cache)]
         [new-l (cons (cons address-bstr address+pos)
                      (if ((length old-l) . > . 5)
                          (let loop ([old-l old-l])
                            (if (null? (cdr old-l))
                                null
                                (cons (car old-l) (loop (cdr old-l)))))
                          old-l))])
    (or (box-cas! cache old-l new-l)
        (address-bytes-cache-set! address-bstr address+pos))))
