(module norm-arity '#%kernel
  (#%require "for.ss" "define.ss" "small-scheme.ss" "sort.ss")
  (#%provide norm:procedure-arity 
             norm:raise-arity-error
             normalize-arity) ;; for test suites
  (define norm:procedure-arity
    (let ([procedure-arity
           (λ (p)
             (normalize-arity (procedure-arity p)))])
      procedure-arity))
  (define norm:raise-arity-error 
    (let ([raise-arity-error
           (λ (name arity-v . arg-vs)
             (if (or (exact-nonnegative-integer? arity-v)
                     (arity-at-least? arity-v)
                     (and (list? arity-v)
                          (andmap (λ (x) (or (exact-nonnegative-integer? x)
                                             (arity-at-least? x)))
                                  arity-v)))
                 (apply raise-arity-error name (normalize-arity arity-v) arg-vs)
                 
                 ;; here we let raise-arity-error signal an error
                 (apply raise-arity-error name arity-v arg-vs)))])
      raise-arity-error))
  
  ;; normalize-arity : (or/c arity (listof arity))
  ;;                -> (or/c null
  ;;                         arity
  ;;                         non-empty-non-singleton-sorted-list-of-nat
  ;;                         (append non-empty-sorted-list-of-nat
  ;;                                 (list (make-arity-at-least nat))))
  ;;
  ;;  where arity = nat | (make-arity-at-least nat)
  ;;
  ;;  result is normalized in the following sense:
  ;;   - no duplicate entries
  ;;   - nats are sorted
  ;;   - at most one arity-at-least, always at the beginning
  (define (normalize-arity a)
    (if (pair? a)
        (let-values ([(min-at-least) #f])
          
          (for ((a (in-list a)))
            (when (arity-at-least? a)
              (when (or (not min-at-least)
                        (< (arity-at-least-value a)
                           (arity-at-least-value min-at-least)))
                (set! min-at-least a))))
          
          (if-one-then-no-list
           (cond
             [min-at-least
              (append (sort
                       (uniq
                        (filter (λ (x) (and (number? x)
                                            (< x (arity-at-least-value
                                                  min-at-least))))
                                a))
                       <)
                      (list min-at-least))]
             [else
              (sort (uniq a) <)])))
        a))
  
  ;; have my own version of this to avoid a circular dependency
  (define (filter p l)
    (cond
      [(null? l) l]
      [else
       (let ([x (car l)])
         (if (p x)
             (cons x (filter p (cdr l)))
             (filter p (cdr l))))]))
  
  (define (if-one-then-no-list lst)
    (cond
      [(and (pair? lst) (null? (cdr lst)))
       (car lst)]
      [else lst]))
  
  (define (uniq lst)
    (let ([ht (make-hash)])
      (for-each (λ (i) (hash-set! ht i #f)) lst)
      (hash-map ht (λ (x y) x)))))