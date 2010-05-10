#lang scheme/base

(require "../utils/utils.rkt"
         (rep type-rep filter-rep object-rep rep-utils)
         (utils tc-utils) (only-in (infer infer) restrict)
         "abbrev.rkt" (only-in scheme/contract current-blame-format [-> -->] listof)
	 (types comparison printer union subtype utils remove-intersect)
         scheme/list scheme/match scheme/promise
         (for-syntax syntax/parse scheme/base)
         unstable/debug syntax/id-table scheme/dict
         scheme/trace
         (for-template scheme/base))

(provide (all-defined-out))

(define (atomic-filter? e)
  (or (TypeFilter? e) (NotTypeFilter? e)))

(define (opposite? f1 f2)
  (match* (f1 f2)
          [((TypeFilter: t1 p1 i1)
            (NotTypeFilter: t2 p1 i2))
           (and (name-ref=? i1 i2)
                (subtype t1 t2))]
          [((NotTypeFilter: t2 p1 i2)
            (TypeFilter: t1 p1 i1))
           (and (name-ref=? i1 i2)
                (subtype t1 t2))]
          [(_ _) #f]))


(define (name-ref=? a b)
  (or (eq? a b)
      (and (identifier? a)
           (identifier? b)
           (free-identifier=? a b))))

;; is f1 implied by f2?
(define (implied-atomic? f1 f2)
  (if (filter-equal? f1 f2)
      #t
      (match* (f1 f2)
              [((TypeFilter: t1 p1 i1)
                (TypeFilter: t2 p1 i2))
               (and (name-ref=? i1 i2)
                    (subtype t2 t1))]
              [((NotTypeFilter: t2 p1 i2)
                (NotTypeFilter: t1 p1 i1))
               (and (name-ref=? i1 i2)
                    (subtype t2 t1))]
              [(_ _) #f])))

(define (hash-name-ref i)
  (if (identifier? i) (hash-id i) i))

;; compact : (Listof prop) bool -> (Listof prop)
;; props : propositions to compress
;; or? : is this an OrFilter (alternative is AndFilter)
(d/c (compact props or?)
     ((listof Filter/c) boolean? . --> . (listof Filter/c))
  (define tf-map (make-hash))
  (define ntf-map (make-hash))
  (let loop ([props props] [others null])
    (if (null? props)
        (append others
                (for/list ([v (in-dict-values tf-map)]) v)
                (for/list ([v (in-dict-values ntf-map)]) v))
        (match (car props)
          [(and p (TypeFilter: t1 f1 x) (? (lambda _ or?)))
           (hash-update! tf-map
                         (list f1 (hash-name-ref x))
                         (match-lambda [(TypeFilter: t2 _ _) (-filter (Un t1 t2) x f1)]
                                       [p (int-err "got something that isn't a typefilter ~a" p)])
                         p)
           (loop (cdr props) others)]
          [(and p (TypeFilter: t1 f1 x) (? (lambda _ (not or?))))
           (match (hash-ref tf-map (list f1 (hash-name-ref x)) #f)
             [(TypeFilter: (? (lambda (t2) (not (overlap t1 t2)))) _ _)
              ;; we're in an And, and we got two types for the same path that do not overlap
              (list -bot)]
             [(TypeFilter: t2 _ _)
              (hash-set! tf-map (list f1 (hash-name-ref x))
                         (-filter (restrict t1 t2) x f1))
              (loop (cdr props) others)]
             [#f
              (hash-set! tf-map (list f1 (hash-name-ref x))
                         (-filter t1 x f1))
              (loop (cdr props) others)])]
          [(and p (NotTypeFilter: t1 f1 x) (? (lambda _ (not or?))))
           (hash-update! ntf-map
                         (list f1 (hash-name-ref x))
                         (match-lambda [(NotTypeFilter: t2 _ _) 
                                        (-not-filter (Un t1 t2) x f1)]
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
          [_ (distribute (compact result #t))])
        (match (car fs)
          [(and t (Top:)) t]
          [(OrFilter: fs*) (loop (append fs* (cdr fs)) result)]
          [(Bot:) (loop (cdr fs) result)]
          [t 
           (cond [(for/or ([f (in-list (append (cdr fs) result))])
                    (opposite? f t))
                  -top]
                 [(for/or ([f (in-list result)]) (or (filter-equal? f t) (implied-atomic? f t)))
                  (loop (cdr fs) result)]
                 [else
                  (loop (cdr fs) (cons t result))])]))))

(define (-and . args)
  (define mk
    (case-lambda [() -top]
                 [(f) f]
                 [fs (make-AndFilter fs)]))
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
                                (apply mk (compact (list f1 f2) #f))))]
          [_ (apply mk (compact result #f))])
        (match (car fs)
          [(and t (Bot:)) t]
          [(AndFilter: fs*) (loop (cdr fs) (append fs* result))]
          [(Top:) (loop (cdr fs) result)]
          [t (cond [(for/or ([f (in-list (append (cdr fs) result))])
                      (opposite? f t))
                    -bot]
                   [(for/or ([f (in-list result)]) (or (filter-equal? f t) (implied-atomic? t f)))
                    (loop (cdr fs) result)]
                   [else
                    (loop (cdr fs) (cons t result))])]))))
