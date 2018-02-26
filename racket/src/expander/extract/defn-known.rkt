#lang racket/base
(require racket/match
         "../run/status.rkt"
         "../compile/side-effect.rkt"
         "../compile/known.rkt")
(provide add-defn-known!)

(struct struct-shape (num-fields num-parent-fields op-types))

(define (add-defn-known! seen-defns syms rhs)
  (for ([s (in-list syms)])
    (unless (hash-ref seen-defns s #f)
      (hash-set! seen-defns s (known-defined))))
  (cond 
   ;; Recognize known-arity `lambda` and `case-lambda`
   [(and (= 1 (length syms)) (lambda-arity rhs))
    =>
    (lambda (arity)
      (hash-set! seen-defns
                 (car syms)
                 (known-function arity
                                 (pure-lambda? (car syms)
                                               rhs
                                               seen-defns))))]
   ;; Recognize structure declarations
   [(expr-struct-shape rhs seen-defns)
    =>
    (lambda (shape)
      (when (= (length syms) (length (struct-shape-op-types shape)))
        (for ([sym (in-list syms)]
              [op-type (in-list (struct-shape-op-types shape))])
          (hash-set! seen-defns sym 
                     (known-struct-op op-type
                                      (case op-type
                                        [(general-accessor general-mutator)
                                         (- (struct-shape-num-fields shape)
                                            (struct-shape-num-parent-fields shape))]
                                        [else (struct-shape-num-fields shape)]))))))]
   ;; Recognize structure-property declaration
   [(and (= 3 (length syms)) (simple-property? rhs))
    (hash-set! seen-defns (list-ref syms 0) (known-property))
    (hash-set! seen-defns (list-ref syms 1) (known-function 1 #t))
    (hash-set! seen-defns (list-ref syms 2) (known-function 1 #t))]))

(define (lambda-arity e)
  (match e
    [`(lambda (,args ...) ,_) (length args)]
    [`(case-lambda [(,argss ...) ,_] ...) (map length argss)]
    [_ #f]))

(define (pure-lambda? self-id e seen-defns)
  (match e
    [`(lambda (,args ...) ,body)
     (pure-body? self-id null args body seen-defns)]
    [`(case-lambda [(,argss ...) ,bodys] ...)
     (define arity (map length argss))
     (for/and ([args (in-list argss)]
               [body (in-list bodys)])
       (pure-body? self-id arity args body seen-defns))]
    [_ #f]))

(define (pure-body? self-id self-arity args orig-body seen-defns)
  (define locals
    (for/hash ([arg (in-list args)])
      (values arg (known-defined))))
  (define body
    ;; Strip away a `begin` that's there to record a function name:
    (match orig-body
      [`(begin (quote ,_) ,e) e]
      [else orig-body]))
  (cond
   [(and (pair? body)
         (eq? (car body) self-id)
         ((sub1 (length body)) . > . (length args)))
    ;; Allow a self-call as pure, as long as the number of arguments
    ;; grows. We'll only conclude that the function is pure overall if
    ;; that assumption now as justified, but we require the number of
    ;; arguments to grow to disallow an infinite loop as pure.
    (define num-args (length args))
    (not (any-side-effects? body 1
                            #:known-defns seen-defns
                            #:known-locals (hash-set locals
                                                     self-id
                                                     (known-function
                                                      (for/list ([a (in-list self-arity)]
                                                                 #:when (a . > . num-args))
                                                        a)
                                                      #t))))]
   [else
    (not (any-side-effects? body 1
                            #:known-defns seen-defns
                            #:known-locals locals))]))

(define struct-general-op-types
  '(struct-type constructor predicate general-accessor general-mutator))

(define (expr-struct-shape e defns)
  (let loop ([e e])
    (match e
      [`(let-values () ,e) (loop e)]
      [`(make-struct-type ,_ #f ,n 0 #f . ,_)
       (and (exact-nonnegative-integer? n)
            (struct-shape n 0 struct-general-op-types))]
      [`(make-struct-type ,_ ,s ,n 0 #f . ,_)
       (define h (hash-ref defns s #f))
       (and (known-struct-op? h)
            (exact-nonnegative-integer? n)
            (eq? (known-struct-op-type h) 'struct-type)
            (struct-shape (+ n (known-struct-op-field-count h))
                          (known-struct-op-field-count h)
                          struct-general-op-types))]
      [`(let-values (((,ty ,mk ,pred ,ref ,mut) ,mst))
         (values ,ty ,mk ,pred
                 (,make-struct-field-xs ,refs ,is ,_) ...))
       (define shape (expr-struct-shape mst defns))
       (and shape
            (equal? (struct-shape-op-types shape) struct-general-op-types)
            (let ([num-immediate-fields (- (struct-shape-num-fields shape)
                                           (struct-shape-num-parent-fields shape))])
              (for/and ([make-struct-field-x (in-list make-struct-field-xs)]
                        [r (in-list refs)]
                        [i (in-list is)])
                (and (< i num-immediate-fields)
                     (if (eq? make-struct-field-x 'make-struct-field-accessor)
                         (eq? r ref)
                         (eq? r mut)))))
            (struct-shape (struct-shape-num-fields shape)
                          (struct-shape-num-parent-fields shape)
                          (append '(struct-type constructor predicate)
                                  (for/list ([make-struct-field-x (in-list make-struct-field-xs)])
                                    (if (eq? make-struct-field-x 'make-struct-field-accessor)
                                        'accessor
                                        'mutator)))))]
      [_ #f])))

;; checks for properties without guards
(define (simple-property? e)
  (match e
    [`(make-struct-type-property ,_) #t]
    [_ #f]))
