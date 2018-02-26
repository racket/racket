#lang racket/base
(require "windows.rkt")

(provide protect-path-element)

(define (protect-path-element bstr convention)
  (cond
    [(eq? convention 'windows)
     (if (needs-protect? bstr)
         (bytes-append #"\\\\?\\REL\\\\" bstr)
         bstr)]
    [else
     bstr]))

(define (needs-protect? bstr)
  (define len (bytes-length bstr))
  (cond
    [(and (eqv? len 1)
          (eqv? (bytes-ref bstr 0) (char->integer #\.)))
     ;; would also be covered by loop below
     #t]
    [(and (eqv? len 2)
          (eqv? (bytes-ref bstr 0) (char->integer #\.))
          (eqv? (bytes-ref bstr 1) (char->integer #\.)))
     ;; would also be covered by loop below
     #t]
    [(special-filename? bstr)
     #t]
    [else
     (let loop ([i+1 len] [at-end? #t])
       (cond
         [(zero? i+1) #f]
         [else
          (define i (sub1 i+1))
          (define b (bytes-ref bstr i))
          (cond
            [(and at-end?
                  (or (eqv? b (char->integer #\.))
                      (eqv? b (char->integer #\space))))
             #t]
            [(or (eqv? b (char->integer #\/))
                 (eqv? b (char->integer #\"))
                 (eqv? b (char->integer #\|))
                 (eqv? b (char->integer #\:))
                 (eqv? b (char->integer #\<))
                 (eqv? b (char->integer #\>)))
             #t]
            [else (loop i #f)])]))]))
