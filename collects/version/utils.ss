#lang scheme/base

(provide valid-version? version->list version<? version<=? alpha-version?
         version->integer)

(define rx:version
  ;; (this restricts the last component to be below 999 too, which is
  ;; not really proper according to the spec in schvers.h)
  (pregexp (string-append "^(0|[1-9][0-9]*)[.]"
                          "(0|(0|[1-9][0-9]{0,1})([.](0|[1-9][0-9]{0,2})){0,2}"
                          "(?<![.]0))$")))

(define (valid-version? s)
  (and (string? s) (regexp-match? rx:version s)))

;; the following functions assume valid version string inputs

;; returns a list of 4 integers (see src/mzscheme/src/schvers.h)
(define (version->list str)
  (let ([ver (map string->number (regexp-split #rx"[.]" str))])
    (case (length ver)
      [(2) (append ver '(0 0))]
      [(3) (append ver '(0))]
      [(4) ver]
      [else (error 'version->list "bad version: ~e" str)])))

(define (version<? a b)
  (let loop ([a (version->list a)]
             [b (version->list b)])
    (cond [(null? a) #f]
          [(< (car a) (car b)) #t]
          [(> (car a) (car b)) #f]
          [else (loop (cdr a) (cdr b))])))

(define (version<=? a b)
  (or (equal? a b) (version<? a b)))

(define (alpha-version? v)
  (let ([l (version->list v)])
    (or ((list-ref l 1) . >= . 90) ((list-ref l 2) . >= . 900))))

;; returns an integer representing the version (XXYYZZZWWW) or #f if invalid
;; works for pre v4 versions too
(define (version->integer v)
  (cond
    [(regexp-match-positions #rx"^(?:0|[1-9][0-9]*)" v) ; takes all digits
     => (lambda (m)
          (let* (;; translate to a new-style version
                 [n (string->number (substring v 0 (cdar m)))]
                 [v (if (< n 49)
                      v
                      (let-values ([(q r) (quotient/remainder n 100)])
                        ;; put numbers and possible .N leftover
                        (format "~a.~a~a" q r (substring v (cdar m)))))])
            (and (valid-version? v)
                 (let ([l (version->list v)])
                   (let loop ([v (car l)]
                              [l (cdr l)]
                              [f '(100 1000 1000)])
                     (if (null? l)
                       v
                       (loop (+ (* v (car f)) (car l)) (cdr l) (cdr f))))))))]
    [else #f]))

;; general sanity check, performed once when loaded
(unless (and (< (string->number (car (regexp-match #rx"^[0-9]+" (version)))) 49)
             (integer? (version->integer (version))))
  ;; When this happens, we got to numbers that can be confused with old version
  ;; numbers, and the above code should be modified.  With the current rate of
  ;; changes, this should happen in more 150 years.  Either programming is
  ;; probably done with a direct brain link, or this software has nobody to fix
  ;; it because everybody went back to the trees.
  (error 'version/utils.ss "this file should be updated"))
