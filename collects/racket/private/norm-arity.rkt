(module norm-arity '#%kernel
  (#%require "define.rkt" "small-scheme.rkt" "sort.rkt")
  (#%provide norm:procedure-arity
             norm:raise-arity-error
             normalize-arity) ;; for test suites
  (define norm:procedure-arity
    (let ([procedure-arity (λ (p) (normalize-arity (procedure-arity p)))])
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
                 (apply raise-arity-error name
                        (normalize-arity arity-v) arg-vs)
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
  ;;   - at most one arity-at-least, always at the end
  ;;   - if there is only one possibility, it is returned by itself (ie,
  ;;     not in a list)
  (define (normalize-arity arity)
    (if (pair? arity)
      (let loop ([min-at-least #f] [min-num 0] [as arity] [numbers '()])
        ;; (1) find the minimal arity-at-least if any, find only numbers
        (if (pair? as)
          (let ([a (car as)] [as (cdr as)])
            (if (arity-at-least? a)
              (if (and min-at-least (<= min-num (arity-at-least-value a)))
                (loop min-at-least min-num as numbers)
                (loop a (arity-at-least-value a) as numbers))
              (loop min-at-least min-num as (cons a numbers))))
          ;; (2) remove redundant numbers and sort
          (let loop ([numbers (sort (if min-at-least
                                      (filter-below min-num numbers)
                                      numbers)
                                    >)] ; reversed in the loop below
                     [result (if min-at-least (list min-at-least) '())])
            ;; (3) throw out duplicates (while reversing the list)
            (cond [(pair? numbers)
                   (loop (cdr numbers)
                         (if (and (pair? result)
                                  (eq? (car numbers) (car result)))
                           result
                           (cons (car numbers) result)))]
                  ;; result is never null (otherwise the input would be null)
                  [(null? (cdr result)) (car result)]
                  [else result]))))
      arity))

  ;; have my own version of this to avoid a circular dependency
  (define (filter-below max l)
    (cond [(null? l) l]
          [else (let ([x (car l)])
                  (if (< x max)
                    (cons x (filter-below max (cdr l)))
                    (filter-below max (cdr l))))])))
