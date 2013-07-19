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
    (unless (procedure-arity? arity)
            (raise-argument-error 'normalize-arity "procedure-arity?" arity))
    (if (pair? arity)
        (let* ([reversed (reverse-sort-arity arity)]
               [normalized (normalize-reversed-arity reversed '())]
               [simplified (normalize-singleton-arity normalized)])
          simplified)
        arity))

  (define (normalize-singleton-arity arity)
    (if (and (pair? arity) (null? (cdr arity)))
        (car arity)
        arity))

  (define (normalize-reversed-arity arity tail)
    (if (pair? arity)
        (normalize-reversed-arity (cdr arity) (arity-insert (car arity) tail))
        tail))

  (define (arity-insert elem arity)
    (if (pair? arity)
        (let ([next (car arity)])
          (if (arity-at-least? next)
              (let ([next-value (arity-at-least-value next)])
                (if (arity-at-least? elem)
                    ;; arity-at-least + arity-at-least
                    (let ([elem-value (arity-at-least-value elem)])
                      (if (< elem-value next-value)
                          (cons elem (cdr arity))
                          arity))
                    ;; number + arity-at-least
                    (if (< elem (- next-value 1))
                        (cons elem arity)
                        (if (= elem (- next-value 1))
                            (cons (arity-at-least elem) (cdr arity))
                            arity))))
              ;; number + number
              (if (< elem next)
                  (cons elem arity)
                  arity)))
        (cons elem arity)))

  (define (reverse-sort-arity arity)
    (sort arity arity>?))

  (define (arity>? a b)
    (if (arity-at-least? a)
        (if (arity-at-least? b)
            (> (arity-at-least-value a) (arity-at-least-value b))
            #t)
        (if (arity-at-least? b)
            #f
            (> a b)))))
