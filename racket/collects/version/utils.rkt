#lang racket/base

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

;; returns a list of 4 integers (see src/racket/src/schvers.h)
(define (version->list str)
  (define ver (map string->number (regexp-split #rx"[.]" str)))
  (case (length ver)
    [(2) (append ver '(0 0))]
    [(3) (append ver '(0))]
    [(4) ver]
    [else (error 'version->list "bad version: ~e" str)]))

(define (version<? a b)
  (let loop ([a (version->list a)] [b (version->list b)])
    (cond [(null? a) #f]
          [(< (car a) (car b)) #t]
          [(> (car a) (car b)) #f]
          [else (loop (cdr a) (cdr b))])))

(define (version<=? a b)
  (or (equal? a b) (version<? a b)))

(define (alpha-version? v)
  (define l (version->list v))
  (or ((list-ref l 1) . >= . 90)
      ((list-ref l 2) . >= . 900)
      ((list-ref l 3) . >= . 900)))

;; returns an integer representing the version (XXYYZZZWWW) or #f if invalid
;; works for pre v4 versions too
(define (version->integer ver)
  (define m
    (regexp-match-positions #rx"^(?:0|[1-9][0-9]*)" ver)) ; takes all digits
  ;; translate old versions to new-style versions
  (define n (and m (string->number (substring ver 0 (cdar m)))))
  (define v
    (cond [(not n) #f]
          ;; new versions
          [(< n 49) ver]
          ;; old versions (earliest useful is 49, changed at 3.99)
          [(<= 49 n 379)
           (define-values [q r] (quotient/remainder n 100))
           (define sfx (let ([sfx (substring ver (cdar m))])
                         (cond [(equal? sfx "") ""]
                               ;; NNNpN -> N.NN.N
                               [(regexp-match? #rx"^p[0-9]" sfx)
                                (string-append "." (substring sfx 1))]
                               ;; NNN.N -> N.NN.0.N (not a release version)
                               [(regexp-match? #rx"^[.]" sfx)
                                (string-append ".0" sfx)]
                               [else #f])))
           (and sfx (format "~a.~a~a" q r sfx))]
          ;; bad strings
          [else #f]))
  (and v (valid-version? v)
       (foldl (λ (ver mul acc) (+ ver (* mul acc))) 0
              (version->list v) '(0 100 1000 1000))))
