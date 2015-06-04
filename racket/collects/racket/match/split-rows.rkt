#lang racket/base

(require "patterns.rkt")

(provide split-rows)

;; split-rows : Listof[Row] -> Listof[Listof[Row]]
;; takes a matrix, and returns a list of matrices
;; each returned matrix does not require the mixture rule to do compilation of
;; the first column.
(define (split-rows rows [acc null])
  (define (loop/var matched-rows prev-mats rows)
    (if (null? rows)
      (reverse (cons (reverse matched-rows) prev-mats))
      (let* ([r (car rows)]
             [p (Row-first-pat r)]
             [rs (cdr rows)])
        (cond [(Row-unmatch r)
               (split-rows rows (cons (reverse matched-rows) prev-mats))]
              [(Var? p)
               (loop/var (cons r matched-rows) prev-mats rs)]
              [else
               (split-rows rows (cons (reverse matched-rows) prev-mats))]))))

  (define (loop/pred matched-rows prev-mats rows orig)
    (if (null? rows)
      (reverse (cons (reverse matched-rows) prev-mats))
      (let* ([r (car rows)]
             [p (Row-first-pat r)]
             [rs (cdr rows)])
        (cond [(Row-unmatch r)
               (split-rows rows (cons (reverse matched-rows) prev-mats))]
              [(and (Pred? p) (equal? p orig))
               ;; use the custom equality on Pred structs
               (loop/pred (cons r matched-rows) prev-mats rs orig)]
              [else
               (split-rows rows (cons (reverse matched-rows) prev-mats))]))))

  (define (loop/con matched-rows prev-mats struct-key rows)
    (if (null? rows)
      (reverse (cons (reverse matched-rows) prev-mats))
      (let* ([r (car rows)]
             [p (Row-first-pat r)]
             [rs (cdr rows)])
        (cond [(Row-unmatch r)
               (split-rows rows (cons (reverse matched-rows) prev-mats))]
              [(and (Struct? p) struct-key (eq? (pat-key p) struct-key))
               ;; (printf "struct-keys were equal: ~a\n" struct-key)
               (loop/con (cons r matched-rows) prev-mats struct-key rs)]
              [(and (Struct? p) (not struct-key))
               ;; (printf "no struct-key so far: ~a\n" struct-key)
               (loop/con (cons r matched-rows) prev-mats (pat-key p) rs)]
              [(and (CPat? p) (not (Struct? p)))
               ;; (printf "wasn't a struct: ~a\n" p)
               (loop/con (cons r matched-rows) prev-mats struct-key rs)]
              [else (split-rows rows (cons (reverse matched-rows)
                                           prev-mats))]))))
  (define (loop/exact matched-rows prev-mats rows)
    (if (null? rows)
      (reverse (cons (reverse matched-rows) prev-mats))
      (let* ([r (car rows)]
             [p (Row-first-pat r)]
             [rs (cdr rows)])
        (cond
          [(Row-unmatch r)
           (split-rows rows (cons (reverse matched-rows) prev-mats))]
          [(Exact? p)
           (loop/exact (cons r matched-rows) prev-mats rs)]
          [else (split-rows rows (cons (reverse matched-rows) prev-mats))]))))
  (if (null? rows)
    (reverse acc)
    (let* ([r (car rows)]
           [p (Row-first-pat r)]
           [rs (cdr rows)])
      (cond [(Row-unmatch r)
             (split-rows rs (cons (list r) acc))]
            [(Var? p)
             (loop/var (list r) acc rs)]
            [(Exact? p)
             (loop/exact (list r) acc rs)]
            [(Pred? p)
             (loop/pred (list r) acc rs p)]
            [(CPat? p)
             (if (Struct? p)
               (begin
                 ;; (printf "found a struct: ~a\n" (pat-key r))
                 (loop/con (list r) acc (pat-key p) rs))
               (loop/con (list r) acc #f rs))]
            [else (split-rows rs (cons (list r) acc))]))))

;; (require mzlib/trace)
;; (trace split-rows)

;; EXAMPLES:
#|
(define mat1 (list r1 r2 r3))
(define mat2 (list r1 r3 r2 r1))
|#
