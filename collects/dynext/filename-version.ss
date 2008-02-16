#lang scheme/base

;; This module provides the string that should replace xxxxxxx's in
;; file names.  The version number is combined to a single integer,
;; and converted to a string in base 36.  The version is X.Y.Z.W, with
;; the general restrictions that Y<100 and Z<1000.  This code further
;; assumes X<200, W<1000 and will throw an error if not -- if that
;; happens it's time to change the naming scheme or add more x's.

(provide filename-version-part)
(require version/utils)

(define (num->str n digits)
  (let ([radix (string-length digits)])
    (let loop ([n n] [r '()])
      (if (<= n 0)
        (list->string r)
        (loop (quotient n radix)
              (cons (string-ref digits (modulo n radix)) r))))))

(define filename-version-part
  (let loop ([n 0]
             [l (version->list (version))]
             [radix '(200 100 1000 1000)])
    (cond [(null? l)
           (let ([s (num->str n "0123456789abcdefghijklmnopqrstuvwxyz")])
             (string-append (make-string (- 7 (string-length s)) #\_) s))]
          [(not (< -1 (car l) (car radix)))
           (error 'version "internal error, see dynext/filename-version.ss")]
          [else (loop (+ (car l) (* (car radix) n)) (cdr l) (cdr radix))])))
