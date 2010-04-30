#lang scheme/base  
(require "../utils/utils.ss"
         (rep type-rep filter-rep object-rep rep-utils)
         (utils tc-utils)
         "abbrev.ss" (only-in scheme/contract current-blame-format)
	 (types comparison printer union subtype utils)
         scheme/list scheme/match scheme/promise
         (for-syntax syntax/parse scheme/base)
         unstable/debug syntax/id-table scheme/dict
         scheme/trace
         (for-template scheme/base))

(provide (all-defined-out)
         (all-from-out "abbrev.ss")
         ;; these should all eventually go away
         make-Name make-ValuesDots make-Function
         (rep-out filter-rep object-rep))

(define (one-of/c . args)
  (apply Un (map -val args)))

(define (Un/eff . args)
  (apply Un (map tc-result-t args)))


;; if t is of the form (Pair t* (Pair t* ... (Listof t*)))
;; return t*
;; otherwise, return t
;; generalize : Type -> Type
(define (generalize t)
  (let/ec exit
    (let loop ([t* t])
      (match t*
        [(Value: '()) (-lst Univ)]
	[(Value: 0) -Nat]
        [(Mu: var (Union: (list (Value: '()) (Pair: _ (F: var))))) t*]
        [(Pair: t1 (Value: '())) (-lst t1)]
        [(Pair: t1 t2)
         (let ([t-new (loop t2)])
           (if (type-equal? (-lst t1) t-new)
               t-new
               (exit t)))]
        [_ (exit t)]))))


;; DO NOT USE if t contains #f
(define (-opt t) (Un (-val #f) t))

(define In-Syntax
  (-mu e
       (*Un (-val null) -Boolean -Symbol -String -Keyword -Char -Number 
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

