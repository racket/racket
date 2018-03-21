#lang racket/base
(require "../common/check.rkt"
         "path.rkt"
         "check-path.rkt"
         "sep.rkt"
         "windows.rkt")

(provide directory-path?
         path->directory-path
         path->path-without-trailing-separator)

(define/who (path->directory-path p-in)
  (check-path-argument who p-in)
  (define p (->path p-in))
  (cond
   [(directory-path? p #:require-sep? #t) p]
   [else
    (case (path-convention p)
      [(unix)
       (path (bytes-append (path-bytes p) #"/") 'unix)]
      [(windows)
       (path (bytes-append (path-bytes p) #"\\") 'windows)])]))

(define (directory-path? p #:require-sep? [require-sep? #f])
  (define bstr (path-bytes p))
  (define len (bytes-length bstr))
  (define convention (path-convention p))
  (define (unixish-path-directory-path?)
    (or (is-sep? (bytes-ref bstr (sub1 len)) convention)
        (and (not require-sep?)
             (or (and (len . >= . 2)
                      (eq? (bytes-ref bstr (sub1 len)) (char->integer #\.))
                      (eq? (bytes-ref bstr (- len 2)) (char->integer #\.))
                      (or (len . = . 2)
                          (is-sep? (bytes-ref bstr (- len 3)) convention)))
                 (and (len . >= . 1)
                      (eq? (bytes-ref bstr (sub1 len)) (char->integer #\.))
                      (or (len . = . 1)
                          (is-sep? (bytes-ref bstr (- len 2)) convention)))))))
    
  (case convention
    [(unix) (unixish-path-directory-path?)]
    [(windows)
     (cond
       [(backslash-backslash-questionmark? bstr)
        ;; Dots are literal in a ".." path, except as a sequence at
        ;; the start of a \\?\REL\.. path (with a single backslash)
        (or (eqv? (bytes-ref bstr (sub1 len)) (char->integer #\\))
            (and (not require-sep?)
                 (eq? 'rel (backslash-backslash-questionmark-kind bstr))
                 (eqv? len
                       (let-values ([(dots-end literal-start) (backslash-backslash-questionmark-dot-ups-end bstr len)])
                         dots-end))))]
       [else (unixish-path-directory-path?)])]))

(define (path->path-without-trailing-separator p)
  (define bstr (path-bytes p))
  (define orig-len (bytes-length bstr))
  (cond
    [(= orig-len 1) p]
    [(and (eq? (path-convention p) 'windows)
          (backslash-backslash-questionmark? bstr))
     ;; \\?\ is more complicated. Do we need to do anything,
     ;; considering that the use for this function is `resolve-path`?
     p]
    [else
     (define len
       (let loop ([len orig-len])
         (cond
           [(zero? len) 0]
           [else
            (define c (bytes-ref bstr (sub1 len)))
            (if (is-sep? c (path-convention p))
                (loop (sub1 len))
                len)])))
     (cond
       [(< len orig-len) (path (subbytes bstr 0 len) (path-convention p))]
       [else p])]))
