#lang racket/base

(require (for-syntax racket/base syntax/name) racket/private/norm-arity)

(provide identity const thunk thunk* negate curry curryr
         normalize-arity normalized-arity? arity=? arity-includes?)

(define (identity x) x)

(define (const c)
  (define (const . _) c)
  (make-keyword-procedure const const))

(define-syntax (thunk stx)
  (syntax-case stx ()
    [(_ body0 body ...) (syntax/loc stx (lambda () body0 body ...))]))

(define-syntax (thunk* stx)
  (syntax-case stx ()
    [(_ body0 body ...)
     (with-syntax ([proc (syntax-property
                          (syntax/loc stx
                            ;; optimize 0- and 1-argument cases
                            (case-lambda [() body0 body ...]
                                         [(x) (th)] [xs (th)]))
                          'inferred-name (syntax-local-infer-name stx))])
       (syntax/loc stx
         (letrec ([th proc])
           (make-keyword-procedure (lambda (_1 _2 . _3) (th)) proc))))]))

(define (negate f)
  (unless (procedure? f) (raise-argument-error 'negate "procedure?" f))
  (let-values ([(arity) (procedure-arity f)] [(_ kwds) (procedure-keywords f)])
    (case (and (null? kwds) arity) ; optimize some simple cases
      [(0) (lambda () (not (f)))]
      [(1) (lambda (x) (not (f x)))]
      [(2) (lambda (x y) (not (f x y)))]
      [else (compose1 not f)]))) ; keyworded or more args => just compose

(define (make-curry right?)
  ;; The real code is here
  (define (curry* f args kws kvs)
    (unless (procedure? f)
      (raise-argument-error (if right? 'curryr 'curry) "procedure?" f))
    (let* ([arity (procedure-arity f)]
           [max-arity (cond [(integer? arity) arity]
                            [(arity-at-least? arity) #f]
                            [(ormap arity-at-least? arity) #f]
                            [else (apply max arity)])]
           [n (length args)])
      (define (loop args n)
        (cond
          [(procedure-arity-includes? f n)
           (if (null? kws) (apply f args) (keyword-apply f kws kvs args))]
          [(and max-arity (n . > . max-arity))
           (apply raise-arity-error f arity args)]
          [else
           (letrec [(curried
                     (case-lambda
                       [() curried] ; return itself on zero arguments
                       [more (loop (if right?
                                     (append more args) (append args more))
                                   (+ n (length more)))]))]
             curried)]))
      ;; take at least one step if we can continue (there is a higher arity)
      (if (equal? n max-arity)
        (if (null? kws) (apply f args) (keyword-apply f kws kvs args))
        (letrec ([curried
                  (lambda more
                    (let ([args (if right?
                                  (append more args) (append args more))])
                      (loop args (+ n (length more)))))])
          curried))))
  ;; curry is itself curried -- if we get args then they're the first step
  (define curry
    (case-lambda [(f) (define (curried . args) (curry* f args '() '()))
                      curried]
                 [(f . args) (curry* f args '() '())]))
  (make-keyword-procedure (lambda (kws kvs f . args) (curry* f args kws kvs))
                          curry))

(define curry  (make-curry #f))
(define curryr (make-curry #t))

(define (normalized-arity? a)
  (or (null? a)
      (arity? a)
      (and (list? a)
           ((length a) . >= . 2)
           (andmap arity? a)
           (if (ormap arity-at-least? a)
               (non-empty-non-singleton-sorted-list-ending-with-arity? a)
               (non-singleton-non-empty-sorted-list? a)))))

(define (arity? a)
  (or (exact-nonnegative-integer? a)
      (and (arity-at-least? a)
           (exact-nonnegative-integer? (arity-at-least-value a)))))

;; non-empty-non-singleton-sorted-list-ending-with-arity? : xx -> boolean
;; know that 'a' is a list of at least 2 elements
(define (non-empty-non-singleton-sorted-list-ending-with-arity? a)
  (let loop ([bound (car a)]
             [lst (cdr a)])
    (cond
      [(null? (cdr lst))
       (and (arity-at-least? (car lst))
            (> (arity-at-least-value (car lst)) (+ 1 bound)))]
      [else
       (and (exact-nonnegative-integer? (car lst))
            ((car lst) . > . bound)
            (loop (car lst)
                  (cdr lst)))])))

(define (non-empty-sorted-list? a)
  (and (pair? a)
       (sorted-list? a)))

(define (non-singleton-non-empty-sorted-list? a)
  (and (pair? a)
       (pair? (cdr a))
       (sorted-list? a)))

(define (sorted-list? a)
  (or (null? a)
      (sorted/bounded-list? (cdr a) (car a))))

(define (sorted/bounded-list? a bound)
  (or (null? a)
      (and (number? (car a))
           (< bound (car a))
           (sorted/bounded-list? (cdr a) (car a)))))

(define (arity-supports-number? arity n)
  (cond
    [(exact-nonnegative-integer? arity) (= arity n)]
    [(arity-at-least? arity) (<= (arity-at-least-value arity) n)]
    [(list? arity)
     (for/or {[elem (in-list arity)]}
       (arity-supports-number? elem n))]))

(define (arity-supports-at-least? arity n)
  (cond
    [(exact-nonnegative-integer? arity) #f]
    [(arity-at-least? arity) (<= (arity-at-least-value arity) n)]
    [(list? arity)
     (define min-at-least
       (for/fold {[min-at-least #f]} {[elem (in-list arity)]}
         (cond
           [(exact-nonnegative-integer? elem) min-at-least]
           [(arity-at-least? elem)
            (cond
              [(not min-at-least) (arity-at-least-value elem)]
              [else (min min-at-least (arity-at-least-value elem))])])))
     (cond
       [(not min-at-least) #f]
       [else
        (for/and {[i (in-range n min-at-least)]}
          (arity-supports-number? arity i))])]))

(define (unchecked-arity-includes? one two)
  (cond
    [(exact-nonnegative-integer? two)
     (arity-supports-number? one two)]
    [(arity-at-least? two)
     (arity-supports-at-least? one (arity-at-least-value two))]
    [(list? two)
     (for/and {[elem (in-list two)]}
       (unchecked-arity-includes? one elem))]))

(define (arity-includes? one two)
  (unless (procedure-arity? one)
    (raise-argument-error 'arity-includes? "procedure-arity?" 0 one two))
  (unless (procedure-arity? two)
    (raise-argument-error 'arity-includes? "procedure-arity?" 1 one two))
  (unchecked-arity-includes? one two))

(define (arity=? one two)
  (unless (procedure-arity? one)
    (raise-argument-error 'arity=? "procedure-arity?" 0 one two))
  (unless (procedure-arity? two)
    (raise-argument-error 'arity=? "procedure-arity?" 1 one two))
  (and
    (unchecked-arity-includes? one two)
    (unchecked-arity-includes? two one)))
