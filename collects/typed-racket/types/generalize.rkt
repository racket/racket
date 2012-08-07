#lang racket/base

(require "../utils/utils.rkt"
         (rep type-rep filter-rep object-rep rep-utils)
         (utils tc-utils)
         "abbrev.rkt" "subtype.rkt" "substitute.rkt" "union.rkt"
         "numeric-tower.rkt"
         racket/match
         (for-syntax syntax/parse racket/base)
         syntax/id-table racket/dict)

(provide generalize)

;; used to produce a more general type for loop variables, vectors, etc.
;; generalize : Type -> Type
(define (generalize t)
  (let/ec exit
    (let loop ([t* t])
      (match t*
        [(Value: '()) (-lst Univ)]
        [(Value: 0) -Int]
        [(List: ts) (-lst (apply Un ts))]
        [(? (lambda (t) (subtype t -Int))) -Int]
        [(? (lambda (t) (subtype t -Rat))) -Rat]
        [(? (lambda (t) (subtype t -Flonum))) -Flonum]
        [(? (lambda (t) (subtype t -SingleFlonum))) -SingleFlonum]
        [(? (lambda (t) (subtype t -InexactReal))) -InexactReal]
        [(? (lambda (t) (subtype t -Real))) -Real]
        [(? (lambda (t) (subtype t -ExactNumber))) -ExactNumber]
        [(? (lambda (t) (subtype t -FloatComplex))) -FloatComplex]
        [(? (lambda (t) (subtype t -SingleFlonumComplex))) -SingleFlonumComplex]
        [(? (lambda (t) (subtype t -Number))) -Number]
        [(Mu: var (Union: (list (Value: '()) (Pair: _ (F: var))))) t*]
        [(Pair: t1 (Value: '())) (-lst t1)]
        [(MPair: t1 (Value: '())) (-mlst t1)]
        [(or (Pair: t1 t2) (MPair: t1 t2))
         (let ([t-new (loop t2)])
           (if (type-equal? ((match t*
                               [(Pair: _ _) -lst]
                               [(MPair: _ _) -mlst])
                             t1)
                            t-new)
               t-new
               (exit t)))]
        [(ListDots: t bound) (-lst (substitute Univ bound t))]
        [(? (lambda (t) (subtype t -Symbol))) -Symbol]
        [(Value: #t) -Boolean]
        [_ (exit t)]))))
