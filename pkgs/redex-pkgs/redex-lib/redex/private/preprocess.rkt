#lang racket/base

(require racket/match
         racket/set
         
         "match-a-pattern.rkt")

(provide preprocess)

;; A set that knows if an element has been added more than once
(struct 2set (set1 set2))

(define 2set-empty (2set (set) (set)))

(define (2set-add ts . xs)
  (foldr (λ (x ts)
            (match ts
              [(2set s1 s2)
               (if (set-member? s1 x)
                   (2set s1 (set-add s2 x))
                   (2set (set-add s1 x) s2))]))
         ts
         xs))

(define/match (2set-union ts1 ts2)
  [((2set s11 s12) (2set s21 s22))
   (define common (set-intersect s11 s21))
   (2set (set-union s11 s21)
         (set-union s12 s22 common))])

(define (preprocess pat)
  (remove-names pat))

(define (remove-names pat)
  (match-define (2set names 2names) (find-names pat))
  (define badnames (set-subtract names 2names))
  (define (strip-named name subpat con)
    (define sub-stripped (strip subpat))
    (if (set-member? badnames name)
        sub-stripped
        (con name sub-stripped)))
  (define (keep-if-good name)
    (and (not (set-member? badnames name))
         name))
  (define (strip pat)
    (match pat
      [`(name ,n ,subpat)
       (strip-named n subpat (λ (n s) `(name ,n ,s)))]
      [`(mismatch-name ,n ,subpat)
       (strip-named n subpat (λ (n s) `(mismatch-name ,n ,s)))]
      [`(in-hole ,p1 ,p2)
       `(in-hole ,(strip p1)
                 ,(strip p2))]
      [`(hide-hole ,p)
       `(hide-hole ,(strip p))]
      [`(list ,sub-pats ...)
       (cons 'list
             (map (match-lambda
                   [`(repeat ,p ,n ,m)
                    (define sub (strip p))
                    (define s-n (keep-if-good n))
                    (define s-m (keep-if-good m))
                    `(repeat ,sub ,s-n ,s-m)]
                   [sub-pat (strip sub-pat)])
                  sub-pats))]
      [else pat]))
  (strip pat))

(define (find-names pat)
  (match pat
    [(or `(name ,n ,subpat)
         `(mismatch ,n ,subpat))
     (2set-add (find-names subpat)
               n)]
    [`(in-hole ,p1 ,p2)
     (2set-union (find-names p1)
                 (find-names p2))]
    [`(hide-hole ,p)
     (find-names p)]
    [`(list ,sub-pats ...)
     (foldr 2set-union
            2set-empty
            (map (match-lambda
                  [`(repeat ,p ,n ,m)
                   (2set-add (find-names p) n m)]
                  [sub-pat (find-names sub-pat)])
                 sub-pats))]
    [else 2set-empty]))
