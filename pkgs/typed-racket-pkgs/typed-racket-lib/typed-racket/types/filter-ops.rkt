#lang racket/base

(require "../utils/utils.rkt"
         racket/list racket/match
         racket/dict
         (prefix-in c: (contract-req))
         (rep type-rep filter-rep object-rep rep-utils)
         (utils tc-utils)
         (only-in (infer infer) restrict)
         (types union subtype remove-intersect abbrev tc-result))

(provide/cond-contract
  [-and (c:->* () #:rest (c:listof Filter/c) Filter/c)]
  [-or (c:->* () #:rest (c:listof Filter/c) Filter/c)]
  [-imp (c:-> Filter/c Filter/c Filter/c)]
  [implied-atomic? (c:-> Filter/c Filter/c boolean?)]
  [complementary? (c:-> Filter/c Filter/c boolean?)]
  [contradictory? (c:-> Filter/c Filter/c boolean?)]
  [add-unconditional-filter-all-args (c:-> Function? Type/c Function?)]
  [add-unconditional-prop (c:-> tc-results/c Filter/c tc-results/c)]
  [erase-filter (c:-> tc-results/c tc-results/c)]
  [name-ref=? (c:-> name-ref/c name-ref/c boolean?)])

(define (atomic-filter? p)
  (or (TypeFilter? p) (NotTypeFilter? p)
      (Top? p) (Bot? p)))

;; contradictory: Filter/c Filter/c -> boolean?
;; Returns true if the AND of the two filters is equivalent to Bot
(define (contradictory? f1 f2)
  (match* (f1 f2)
    [((TypeFilter: t1 p) (NotTypeFilter: t2 p))
     (subtype t1 t2)]
    [((NotTypeFilter: t2 p) (TypeFilter: t1 p))
     (subtype t1 t2)]
    [((Bot:) _) #t]
    [(_ (Bot:)) #t]
    [(_ _) #f]))

;; complementary: Filter/c Filter/c -> boolean?
;; Returns true if the OR of the two filters is equivalent to Top
(define (complementary? f1 f2)
  (match* (f1 f2)
    [((TypeFilter: t1 p) (NotTypeFilter: t2 p))
     (subtype t2 t1)]
    [((NotTypeFilter: t2 p) (TypeFilter: t1 p))
     (subtype t2 t1)]
    [((Top:) (Top:)) #t]
    [(_ _) #f]))

(define (name-ref=? a b)
  (or (equal? a b)
      (and (identifier? a)
           (identifier? b)
           (free-identifier=? a b))))

;; is f1 implied by f2?
(define (implied-atomic? f1 f2)
  (match* (f1 f2)
    [(f f) #t]
    [((Top:) _) #t]
    [(_ (Bot:)) #t]
    [((OrFilter: fs) f2)
     (and (memf (lambda (f) (filter-equal? f f2)) fs) #t)]
    [(f1 (AndFilter: fs))
     (and (memf (lambda (f) (filter-equal? f f1)) fs) #t)]
    [((TypeFilter: t1 p) (TypeFilter: t2 p))
     (subtype t2 t1)]
    [((NotTypeFilter: t2 p) (NotTypeFilter: t1 p))
     (subtype t2 t1)]
    [((NotTypeFilter: t1 p) (TypeFilter: t2 p))
     (not (overlap t1 t2))]
    [(_ _) #f]))

(define (hash-name-ref i)
  (if (identifier? i) (hash-id i) i))

;; compact : (Listof prop) bool -> (Listof prop)
;; props : propositions to compress
;; or? : is this an OrFilter (alternative is AndFilter)
;;
;; This combines all the TypeFilters at the same path into one TypeFilter. If it is an OrFilter the
;; combination is done using Un, otherwise, restrict. The reverse is done for NotTypeFilters. If it is
;; an OrFilter this simplifies to -top if any of the atomic filters simplified to -top, and removes
;; any -bot values. The reverse is done if this is an AndFilter.
;;
(define/cond-contract (compact props or?)
     ((c:listof Filter/c) boolean? . c:-> . (c:listof Filter/c))
  (define tf-map (make-hash))
  (define ntf-map (make-hash))
  (define (restrict-update dict t1 p)
    (hash-update! dict p (λ (t2) (restrict t1 t2)) Univ))
  (define (union-update dict t1 p)
    (hash-update! dict p (λ (t2) (Un t1 t2)) -Bottom))

  (define-values (atomics others) (partition atomic-filter? props))
  (for ([prop (in-list atomics)])
    (match prop
      [(TypeFilter: t1 p)
       ((if or? union-update restrict-update) tf-map t1 p) ]
      [(NotTypeFilter: t1 p)
       ((if or? restrict-update union-update) ntf-map t1 p) ]))
  (define raw-results
    (append others
            (for/list ([(k v) (in-hash tf-map)]) (-filter v k))
            (for/list ([(k v) (in-hash ntf-map)]) (-not-filter v k))))
  (if or?
      (if (member -top raw-results)
          (list -top)
          (filter-not Bot? raw-results))
      (if (member -bot raw-results)
          (list -bot)
          (filter-not Top? raw-results))))



;; invert-filter: Filter/c -> Filter/c
;; Logically inverts a filter.
(define (invert-filter p)
  (match p
    [(Bot:) -top]
    [(Top:) -bot]
    [(TypeFilter: t p) (-not-filter t p)]
    [(NotTypeFilter: t p) (-filter t p)]
    [(AndFilter: fs) (apply -or (map invert-filter fs))]
    [(OrFilter: fs) (apply -and (map invert-filter fs))]
    [(ImpFilter: f1 f2) (-and f1 (invert-filter f2))]))

;; -imp: Filter/c Filter/c -> Filter/c
;; Smart constructor for make-ImpFilter
(define (-imp p1 p2)
  (match* (p1 p2)
    [(t t) -top]
    [((Bot:) _) -top]
    [(_ (Top:)) -top]
    [((Top:) _) p2]
    [(_ (Bot:)) (invert-filter p1)]
    [(_ _) (make-ImpFilter p1 p2)]))

(define (-or . args)
  (define mk
    (case-lambda [() -bot]
                 [(f) f]
                 [fs (make-OrFilter (sort fs filter<?))]))
  (define (distribute args)
    (define-values (ands others) (partition AndFilter? args))
    (if (null? ands)
        (apply mk others)
        (match-let ([(AndFilter: elems) (car ands)])
          (apply -and (for/list ([a (in-list elems)])
                        (apply -or a (append (cdr ands) others)))))))
  (let loop ([fs args] [result null])
    (if (null? fs)
        (distribute (compact result #t))
        (match (car fs)
          [(and t (Top:)) t]
          [(OrFilter: fs*) (loop (append fs* (cdr fs)) result)]
          [(Bot:) (loop (cdr fs) result)]
          [t
           (cond [(for/or ([f (in-list (append (cdr fs) result))])
                    (complementary? f t))
                  -top]
                 [(let ([t-seq (Rep-seq t)])
                    (for/or ([f (in-list result)])
                      (or (= (Rep-seq f) t-seq) (implied-atomic? f t))))
                  (loop (cdr fs) result)]
                 [else
                  (loop (cdr fs) (cons t result))])]))))

(define (-and . args)
  (define mk
    (case-lambda [() -top]
                 [(f) f]
                 [fs (make-AndFilter (sort fs filter<?))]))
  (define (flatten-ands fs)
    (let loop ([fs fs] [results null])
      (match fs
        [(list) results]
        [(cons (AndFilter: fs*) fs) (loop fs (append fs* results))]
        [(cons f fs) (loop fs (cons f results))])))
  ;; Move all the type filters up front as they are the stronger props
  (define-values (filters other-args)
    (partition (λ (f) (or (TypeFilter? f) (NotTypeFilter? f)))
               (flatten-ands (remove-duplicates args eq? #:key Rep-seq))))
  (define-values (type-filters not-type-filters)
    (partition TypeFilter? filters))
  (let loop ([fs (append type-filters not-type-filters other-args)] [result null])
    (if (null? fs)
        (apply mk (compact result #f))
        (match (car fs)
          [(and t (Bot:)) t]
          [(Top:) (loop (cdr fs) result)]
          [t (cond [(for/or ([f (in-list (append (cdr fs) result))])
                      (contradictory? f t))
                    -bot]
                   [(let ([t-seq (Rep-seq t)])
                      (for/or ([f (in-list result)])
                        (or (= (Rep-seq f) t-seq)
                            (implied-atomic? t f))))
                    (loop (cdr fs) result)]
                   [else
                    (loop (cdr fs) (cons t result))])]))))

;; add-unconditional-prop: tc-results? Filter/c? -> tc-results?
;; Ands the given proposition to the filters in the tc-results.
;; Useful to express properties of the form: if this expressions returns at all, we learn this
(define (add-unconditional-prop results prop)
  (match results
    [(tc-any-results: f) (tc-any-results (-and prop f))]
    [(tc-results: ts (list (FilterSet: fs+ fs-) ...) os)
     (ret ts
          (for/list ([f+ fs+] [f- fs-])
            (-FS (-and prop f+) (-and prop f-)))
          os)]
    [(tc-results: ts (list (FilterSet: fs+ fs-) ...) os dty dbound)
     (ret ts
          (for/list ([f+ fs+] [f- fs-])
            (-FS (-and prop f+) (-and prop f-)))
          os)]))


;; ands the given type filter to both sides of the given arr for each argument
;; useful to express properties of the form: if this function returns at all,
;; we learn this about its arguments (like fx primitives, or car/cdr, etc.)
(define (add-unconditional-filter-all-args arr type)
  (match arr
    [(Function: (list (arr: dom rng rest drest kws)))
     (match rng
       [(Values: (list (Result: tp (FilterSet: -true-filter -false-filter) op)))
        (let ([new-filters (apply -and (build-list (length dom)
                                                   (lambda (i)
                                                     (-filter type i))))])
          (make-Function
           (list (make-arr
                  dom
                  (make-Values
                   (list (-result tp
                                  (-FS (-and -true-filter new-filters)
                                       (-and -false-filter new-filters))
                                  op)))
                  rest drest kws))))])]))

;; tc-results/c -> tc-results/c
(define (erase-filter tc)
  (match tc
    [(tc-any-results: _) (tc-any-results -no-filter)]
    [(tc-results: ts _ _)
     (ret ts
          (for/list ([f (in-list ts)]) -no-filter)
          (for/list ([f (in-list ts)]) -no-obj))]
    [(tc-results: ts _ _ dty dbound)
     (ret ts
          (for/list ([f (in-list ts)]) -no-filter)
          (for/list ([f (in-list ts)]) -no-obj)
          dty dbound)]))
