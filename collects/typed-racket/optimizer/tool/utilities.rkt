#lang racket/base

;; TODO all this stuff should make it into unstable/X

(provide (all-defined-out))

(define (regexp-filter r log)
  (for/list ([l (in-list log)] #:when (regexp-match r l))
    l))

;; (y y -> bool) (listof x) #:key (x -> y) -> (listof (listof x))
;; groups together elements that are considered equal
;; =? should be reflexive, transitive and commutative
(define (group-by =? l #:key [key values])
  (for/fold ([res '()]) ; list of lists
      ([elt (in-list l)])
    (let loop ([classes     res] ; "zipper" of the equivalence classes
               [rev-classes '()])
      (cond [(null? classes)
             ;; did not find an equivalence class, create a new one
             (cons (list elt) res)]
            [(=? (key elt) (key (car (car classes))))
             ;; found the equivalence class
             (append rev-classes ; we keep what we skipped
                     ;; we extend the current class
                     (list (cons elt (car classes)))
                     (cdr classes))] ; and add the rest
            [else ; keep going
             (loop (cdr classes)
                   (cons (car classes) rev-classes))]))))
;; TODO add to unstable/list, and add tests. here's one
;; -> (group-by = '(1 2 1 2 54 2 5 43 7 2 643 1 2 0))
;; '((0) (2 2 2 2 2) (7) (43) (5) (54) (643) (1 1 1))
