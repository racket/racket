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

(define (atomic-filter? e)
  (or (TypeFilter? e) (NotTypeFilter? e)))

(define (opposite? f1 f2)
  (match* (f1 f2)
          [((TypeFilter: t1 p1 i1)
            (NotTypeFilter: t2 p1 i2))
           (and (free-identifier=? i1 i2)
                (subtype t1 t2))]
          [((NotTypeFilter: t2 p1 i2)
            (TypeFilter: t1 p1 i1))
           (and (free-identifier=? i1 i2)
                (subtype t1 t2))]
          [(_ _) #f]))

;; is f1 implied by f2?
(define (implied-atomic? f1 f2)
  (if (filter-equal? f1 f2)
      #t
      (match* (f1 f2)
              [((TypeFilter: t1 p1 i1)
                (TypeFilter: t2 p1 i2))
               (and (free-identifier=? i1 i2)
                    (subtype t1 t2))]
              [((NotTypeFilter: t2 p1 i2)
                (NotTypeFilter: t1 p1 i1))
               (and (free-identifier=? i1 i2)
                    (subtype t1 t2))]
              [(_ _) #f])))

(define (compact props)
  (define tf-map (make-hash))
  (define ntf-map (make-hash))
  (let loop ([props props] [others null])
    (if (null? props)
        (append others
                (for/list ([v (in-dict-values tf-map)]) v)
                (for/list ([v (in-dict-values ntf-map)]) v))
        (match (car props)
          [(and p (TypeFilter: t1 f1 x))
           (hash-update! tf-map
                         (list f1 (hash-id x))
                         (match-lambda [(TypeFilter: t2 _ _) (make-TypeFilter (Un t1 t2) f1 x)]
                                       [p (int-err "got something that isn't a typefilter ~a" p)])
                         p)
           (loop (cdr props) others)]
          #;
          [(and p (NotTypeFilter: t1 f1 x))
           (hash-update! ntf-map
                         (list f1 (hash-id x))
                         (match-lambda [(NotTypeFilter: t2 _ _) (make-NotTypeFilter (restrict t1 t2) f1 x)]
                                       [p (int-err "got something that isn't a nottypefilter ~a" p)])
                         p)
           (loop (cdr props) others)]
          [p (loop (cdr props) (cons p others))]))))

(define (-or . args)
  (define mk
    (case-lambda [() -bot]
                 [(f) f]
                 [fs (make-OrFilter fs)]))
  (define (distribute args)
    (define-values (ands others) (partition AndFilter? args))
    (if (null? ands)
        (apply mk others)
        (match-let ([(AndFilter: elems) (car ands)])
          (apply -and (for/list ([a (in-list elems)])
                        (apply -or a (append (cdr ands) others)))))))  
  (let loop ([fs args] [result null])
    (if (null? fs)
        (match result
          [(list) -bot]
          [(list f) f]
          [_ (distribute (compact result))])
        (match (car fs)
          [(and t (Top:)) t]
          [(OrFilter: fs*) (loop (append fs* (cdr fs)) result)]
          [(Bot:) (loop (cdr fs) result)]
          [t 
           (cond [(for/or ([f (in-list (append (cdr fs) result))])
                    (opposite? f t))
                  -top]
                 [(for/or ([f (in-list result)]) (or (filter-equal? f t) (implied-atomic? t f)))
                  (loop (cdr fs) result)]
                 [else
                  (loop (cdr fs) (cons t result))])]))))

(define (-and . args) 
  (let loop ([fs (remove-duplicates args filter-equal?)] [result null])
    (if (null? fs)
        (match result
          [(list) -top]
          [(list f) f]
          ;; don't think this is useful here
          [(list f1 f2) (if (opposite? f1 f2)
                            -bot
                            (if (filter-equal? f1 f2)
                                f1
                                (make-AndFilter (list f1 f2))))]
          [_ (make-AndFilter result)])
        (match (car fs)
          [(and t (Bot:)) t]
          [(AndFilter: fs*) (loop (cdr fs) (append fs* result))]
          [(Top:) (loop (cdr fs) result)]
          [t (loop (cdr fs) (cons t result))]))))
