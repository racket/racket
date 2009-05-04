#lang scheme/base

(provide const negate curry curryr)

(define (const c)
  (define (const . _) c)
  (make-keyword-procedure const const))

(define (negate f)
  (unless (procedure? f) (raise-type-error 'negate "procedure" f))
  (let-values ([(arity) (procedure-arity f)]
               [(required-kws accepted-kws) (procedure-keywords f)])
    (define negated ; simple version, optimize some cases
      (case arity
        [(0) (lambda () (not (f)))]
        [(1) (lambda (x) (not (f x)))]
        [(2) (lambda (x y) (not (f x y)))]
        [else (lambda xs (not (apply f xs)))]))
    (if (and (null? required-kws) (null? accepted-kws))
      negated
      ;; keyworded function
      (make-keyword-procedure (lambda (kws kvs . args)
                                (not (keyword-apply f kws kvs args)))
                              negated))))

(define (make-curry right?)
  ;; The real code is here
  (define (curry* f args kws kvs)
    (unless (procedure? f)
      (raise-type-error (if right? 'curryr 'curry) "procedure" f))
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
