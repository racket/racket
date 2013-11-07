#lang typed/racket/base

(require racket/match
         racket/set)

(provide 2Setof
         (rename-out [mk-2set 2set]
                     [2set-set1 2set-ones]
                     [2set-set2 2set-manys])
         2set-add
         2set-union)

(define-type (2Setof A) (2set A))
(struct: (A) 2set ([set1 : (Setof A)]
                   [set2 : (Setof A)]))

(: mk-2set : (All (a) (a * -> (2Setof a))))
(define (mk-2set . xs)
  (for/fold ([acc (2set (ann (set) (Setof a))
                        (ann (set) (Setof a)))])
            ([x (in-list xs)])
    (2set-add acc x)))

(: 2set-add : (All (a) ((2Setof a) a * -> (2Setof a))))
(define (2set-add ts . xs)
  (for/fold ([acc ts])
            ([x (in-list xs)])
    (match-define (2set ones manys) acc)
    (cond [(set-member? ones x)
           (2set ones (set-add manys x))]
          [else (2set (set-add ones x) manys)])))

(: 2set-union : (All (a) ((2Setof a) (2Setof a) -> (2Setof a))))
(define/match (2set-union ts1 ts2)
  [((2set s11 s12) (2set s21 s22))
   (define common (set-intersect s11 s21))
   (2set (set-union s11 s21)
         (set-union s12 s22 common))])
