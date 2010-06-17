#lang scheme/base  
(require "../utils/utils.rkt"
         (rep type-rep filter-rep object-rep rep-utils)
         (utils tc-utils)
         "abbrev.rkt" (only-in scheme/contract current-blame-format)
	 (types comparison printer union subtype utils substitute)
         scheme/list scheme/match scheme/promise
         (for-syntax syntax/parse scheme/base)
         unstable/debug syntax/id-table scheme/dict
         scheme/trace
         (for-template scheme/base))

(provide (all-defined-out)
         (all-from-out "abbrev.rkt")
         ;; these should all eventually go away
         make-Name make-ValuesDots make-Function
         (rep-out filter-rep object-rep))

(define (one-of/c . args)
  (apply Un (map -val args)))

(define (Un/eff . args)
  (apply Un (map tc-result-t args)))


;; used to produce a more general type for loop variables
;; generalize : Type -> Type
(define (generalize t)
  (let/ec exit
    (let loop ([t* t])
      (match t*
        [(Value: '()) (-lst Univ)]
	[(Value: 0) -Nat]
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
        [_ (exit t)]))))


(define (-opt t) (Un (-val #f) t))

(define In-Syntax
  (-mu e
       (Un (-val null) -Boolean -Symbol -String -Keyword -Char -Number 
           (make-Vector (-Syntax e))
           (make-Box (-Syntax e))
           (-lst (-Syntax e))
           (-pair (-Syntax e) (-Syntax e)))))

(define Any-Syntax (-Syntax In-Syntax))

(define (-Sexpof t)
  (-mu sexp
       (Un (-val '())
           -Number -Boolean -Symbol -String -Keyword -Char           
           (-pair sexp sexp)
           (make-Vector sexp)
           (make-Box sexp)
           t)))

(define -Sexp (-Sexpof (Un)))

(define Syntax-Sexp (-Sexpof Any-Syntax))

(define Ident (-Syntax -Symbol))

