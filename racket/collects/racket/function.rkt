#lang racket/base

(require (for-syntax racket/base racket/list syntax/name)
         racket/match racket/private/arity)

(provide identity const thunk thunk* negate curry curryr
         (all-from-out racket/private/arity)
         conjoin disjoin)

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


;; Originally from `unstable/function`.
;; Originally written by Carl Eastlund

;; ryanc: adjusted limit of inner cases from 8 to 2
;; All uses so far seem to be predicates, so more cases seem
;; unnecessary. Also, all uses so far are first-order, so
;; outer case-lambda* might be better replaced with macro.

(define conjoin
  (case-lambda*
   [(f ... 8)
    (begin
      (for ([f* (in-list (list f ...))])
        (unless (procedure? f*)
          (raise-argument-error 'conjoin "procedure?" f*)))
      (make-intermediate-procedure
       'conjoined
       [(x (... ...) 2) (and (f x (... ...)) ...)]
       [xs (and (apply f xs) ...)]
       #:keyword
       [(keys vals . args)
        (and (keyword-apply f keys vals args) ...)]))]
   [fs
    (begin
      (for ([f* (in-list fs)])
        (unless (procedure? f*)
          (raise-argument-error 'conjoin "procedure?" f*)))
      (make-intermediate-procedure
       'conjoined
       [(x ... 2) (andmap (lambda (f) (f x ...)) fs)]
       [xs (andmap (lambda (f) (apply f xs)) fs)]
       #:keyword
       [(keys vals . args)
        (andmap (lambda (f) (keyword-apply f keys vals args)) fs)]))]))

(define disjoin
  (case-lambda*
   [(f ... 8)
    (begin
      (for ([f* (in-list (list f ...))])
        (unless (procedure? f*)
          (raise-argument-error 'conjoin "procedure?" f*)))
      (make-intermediate-procedure
       'disjoined
       [(x (... ...) 2) (or (f x (... ...)) ...)]
       [xs (or (apply f xs) ...)]
       #:keyword
       [(keys vals . args)
        (or (keyword-apply f keys vals args) ...)]))]
   [fs
    (begin
      (for ([f* (in-list fs)])
        (unless (procedure? f*)
          (raise-argument-error 'conjoin "procedure?" f*)))
      (make-intermediate-procedure
       'disjoined
       [(x ... 2) (ormap (lambda (f) (f x ...)) fs)]
       [xs (ormap (lambda (f) (apply f xs)) fs)]
       #:keyword
       [(keys vals . args)
        (ormap (lambda (f) (keyword-apply f keys vals args)) fs)]))]))

(define-syntax (make-intermediate-procedure stx)
  (syntax-case stx [quote]
    [(_ (quote name) positional-clause ... #:keyword keyword-clause)
     (syntax/loc stx
       (make-keyword-procedure
        (let* ([name (case-lambda keyword-clause)]) name)
        (let* ([name (case-lambda* positional-clause ...)]) name)))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Automatic case-lambda repetition
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-for-syntax (split-syntax-at orig stx id)
  (let loop ([found #f]
             [seen null]
             [stx stx])
    (syntax-case stx []
      [(head . tail)
       (and (identifier? #'head)
            (free-identifier=? #'head id))
       (if found
           (raise-syntax-error
            #f
            (format "duplicate occurrence of ~a" (syntax-e id))
            orig
            #'head)
           (loop (list (reverse seen) #'head #'tail)
                 (cons #'head seen)
                 #'tail))]
      [(head . tail) (loop found (cons #'head seen) #'tail)]
      [_ found])))

(define-for-syntax (expand-ellipsis-clause stx pattern expr)
  (cond
   [(split-syntax-at stx pattern #'(... ...))
    =>
    (lambda (found)
      (syntax-case found [...]
        [([pre ... repeat] (... ...) [count post ... . tail])
         (and (identifier? #'repeat)
              (exact-nonnegative-integer? (syntax-e #'count)))
         (build-list
          (add1 (syntax-e #'count))
          (lambda (i)
            (with-syntax ([(var ...)
                           (generate-temporaries
                            (build-list i (lambda (j) #'repeat)))]
                          [body expr])
              (list
               (syntax/loc pattern (pre ... var ... post ... . tail))
               (syntax/loc expr
                 (let-syntax ([the-body
                               (lambda _
                                 (with-syntax ([(repeat (... ...)) #'(var ...)])
                                   #'body))])
                   the-body))))))]
        [(pre mid post)
         (raise-syntax-error
          #f
          "expected ellipsis between identifier and natural number literal"
          stx
          #'mid)]))]
   [else (list (list pattern expr))]))

(define-syntax (case-lambda* stx)
  (syntax-case stx []
    [(_ [pattern body] ...)
     (with-syntax ([([pattern body] ...)
                    (append-map
                     (lambda (p e) (expand-ellipsis-clause stx p e))
                     (syntax->list #'(pattern ...))
                     (syntax->list #'(body ...)))])
       (syntax/loc stx
         (case-lambda [pattern body] ...)))]))
