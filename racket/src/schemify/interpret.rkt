#lang racket/base
(require racket/unsafe/undefined
         racket/unsafe/ops
         "match.rkt"
         "wrap.rkt"
         "interp-match.rkt")

;; Interpreter for the output of "jitify". This little interpreter is
;; useful to avoid going through a more heavyweight `eval` or
;; `interpret`, mainly because we don't need to go through a macro
;; expander. Also, because it's tailored to the shape of a linklet
;; outer layer, it can implement that layer more efficiently and
;; compactly.

(provide interpretable-jitified-linklet
         interpret-linklet)

(struct indirect (stack element))
(struct indirect-checked indirect ())

(define (interpretable-jitified-linklet linklet-e strip-annotations)
  ;; Return a compiled linklet in two parts: a vector expression for
  ;; constants to be run once, and a expression for the linklet body.
  ;; A compiled expression uses a list as a stack for local variables,
  ;; where the coldest element is is a vector of constants, and the
  ;; 1th slot is a vector of linklet arguments for imports and
  ;; exports, and the 2nd slot is a vector for top-level variables. We
  ;; don't have to worry about continuations, because linklet bodies
  ;; are constrained.
  ;;
  ;; Bindings in the environment are represented as positions that
  ;; count from the coldest end of the stack; that position relative
  ;; to the hottest end can be computed from the current stack depth.

  (define (stack->pos stack-depth i)
    (- stack-depth i 1))
  
  (define (start linklet-e)
    (match linklet-e
      [`(lambda . ,_)
       ;; No constants:
       (define-values (compiled-body num-body-vars)
         (compile-linklet-body linklet-e '#hasheq() 0))
       (vector #f
               num-body-vars
               compiled-body)]
      [`(let* ,bindings ,body)
       (let loop ([bindings bindings] [pos 0] [env '#hasheq()] [accum '()])
         (cond
           [(null? bindings)
            (define-values (compiled-body num-body-vars)
              (compile-linklet-body body env 1))
            (vector (list->vector (reverse accum))
                    num-body-vars
                    compiled-body)]
           [else
            (let ([binding (car bindings)])
              (loop (cdr bindings)
                    (add1 pos)
                    (hash-set env (car binding) (indirect 0 pos))
                    (cons (compile-expr (cadr binding) env 1)
                          accum)))]))]))

  (define (compile-linklet-body v env stack-depth)
    (match v
      [`(lambda ,args . ,body)
       (define args-env
         (for/fold ([env env]) ([arg (in-list args)]
                                [i (in-naturals)])
           (hash-set env arg (indirect stack-depth i))))
       (define body-vars-index (add1 stack-depth))
       (define-values (body-env num-body-vars)
         (for/fold ([env args-env] [num-body-vars 0]) ([e (in-wrap-list body)])
           (let loop ([e e] [env env] [num-body-vars num-body-vars])
             (match e
               [`(define ,id . ,_)
                (values (hash-set env (unwrap id) (indirect body-vars-index num-body-vars))
                        (add1 num-body-vars))]
               [`(define-values ,ids . ,_)
                (for/fold ([env env] [num-body-vars num-body-vars]) ([id (in-wrap-list ids)])
                  (values (hash-set env (unwrap id) (indirect body-vars-index num-body-vars))
                          (add1 num-body-vars)))]
               [`(begin . ,body)
                (for/fold ([env env] [num-body-vars num-body-vars]) ([e (in-wrap-list body)])
                  (loop e env num-body-vars))]
               [`,_ (values env num-body-vars)]))))
       (values (compile-top-body body body-env (+ 2 stack-depth))
               num-body-vars)]))

  ;; Like `compile-body`, but flatten top-level `begin`s
  (define (compile-top-body body env stack-depth)
    (define bs (let loop ([body body])
                 (match body
                   [`() '()]
                   [`((begin ,subs ...) . ,rest)
                    (loop (append subs rest))]
                   [`(,e . ,rest)
                    (cons (compile-expr e env stack-depth)
                          (loop rest))])))
    (cond
      [(null? bs) '#(void)]
      [(and (pair? bs) (null? (cdr bs)))
       (car bs)]
      [else
       (list->vector (cons 'begin bs))]))

  (define (compile-body body env stack-depth)
    (match body
      [`(,e) (compile-expr e env stack-depth)]
      [`,_
       (list->vector
        (cons 'begin
              (for/list ([e (in-wrap-list body)])
                (compile-expr e env stack-depth))))]))

  (define (compile-expr e env stack-depth)
    (match e
      [`(lambda ,ids . ,body)
       (define-values (body-env count rest?)
         (args->env ids env stack-depth))
       (vector 'lambda (count->mask count rest?) (compile-body body body-env (+ stack-depth count)))]
      [`(case-lambda [,idss . ,bodys] ...)
       (define lams (for/list ([ids (in-list idss)]
                               [body (in-list bodys)])
                      (compile-expr `(lambda ,ids . ,body) env stack-depth)))
       (define mask (for/fold ([mask 0]) ([lam (in-list lams)])
                      (bitwise-ior mask (interp-match lam [#(lambda ,mask) mask]))))
       (list->vector (list* 'case-lambda mask lams))]
      [`(let ([,ids ,rhss] ...) . ,body)
       (define len (length ids))
       (define body-env
         (for/fold ([env env]) ([id (in-list ids)]
                                [i (in-naturals)])
           (hash-set env (unwrap id) (+ stack-depth i))))
       (vector 'let
               (for/vector #:length len ([rhs (in-list rhss)])
                 (compile-expr rhs env stack-depth))
               (compile-body body body-env (+ stack-depth len)))]
      [`(letrec . ,_) (compile-letrec e env stack-depth)]
      [`(letrec* . ,_) (compile-letrec e env stack-depth)]
      [`(begin . ,vs)
       (compile-body vs env stack-depth)]
      [`(begin0 ,e . ,vs)
       (vector 'begin0 (compile-expr e env stack-depth) (compile-body vs env stack-depth))]
      [`(pariah ,e)
       (compile-expr e env stack-depth)]
      [`(if ,tst ,thn ,els)
       (vector 'if
               (compile-expr tst env stack-depth)
               (compile-expr thn env stack-depth)
               (compile-expr els env stack-depth))]
      [`(with-continuation-mark ,key ,val ,body)
       (vector 'wcm
               (compile-expr key env stack-depth)
               (compile-expr val env stack-depth)
               (compile-expr body env stack-depth))]
      [`(quote ,v)
       (let ([v (strip-annotations v)])
         ;; Protect with `quote` any value that looks like an
         ;; interpreter instruction:
         (if (or (vector? v)
                 (pair? v)
                 (symbol? v)
                 (number? v))
             (vector 'quote v)
             v))]
      [`(set! ,id ,rhs)
       (compile-assignment id rhs env stack-depth)]
      [`(define ,id ,rhs)
       (compile-assignment id rhs env stack-depth)]
      [`(define-values ,ids ,rhs)
       (define gen-ids (for/list ([id (in-list ids)])
                         (gensym id)))
       (compile-expr `(call-with-values (lambda () ,rhs)
                        (lambda ,gen-ids
                          ,@(if (null? ids)
                                '((void))
                                (for/list ([id (in-list ids)]
                                           [gen-id (in-list gen-ids)])
                                  `(set! ,id ,gen-id)))))
                     env
                     stack-depth)]
      [`(call-with-values ,proc1 (lambda ,ids . ,body))
       (compile-expr `(call-with-values ,proc1 (case-lambda
                                                 [,ids . ,body]))
                     env
                     stack-depth)]
      [`(call-with-values (lambda () . ,body) (case-lambda [,idss . ,bodys] ...))
       (vector 'cwv
               (compile-body body env stack-depth)
               (for/list ([ids (in-list idss)]
                          [body (in-list bodys)])
                 (define-values (new-env count rest?)
                   (args->env ids env stack-depth))
                 (vector (count->mask count rest?)
                         (compile-body body new-env (+ stack-depth count)))))]
      [`(variable-set! ,dest-id ,e ',constance)
       (define dest-var (hash-ref env (unwrap dest-id)))
       (vector 'set-variable!
               (stack->pos stack-depth (indirect-stack dest-var)) (indirect-element dest-var)
               (compile-expr e env stack-depth)
               constance)]
      [`(variable-ref ,id)
       (define var (hash-ref env (unwrap id)))
       (vector 'ref-variable/checked (stack->pos stack-depth (indirect-stack var)) (indirect-element var))]
      [`(variable-ref/no-check ,id)
       (define var (hash-ref env (unwrap id)))
       (vector 'ref-variable (stack->pos stack-depth (indirect-stack var)) (indirect-element var))]
      [`(#%app ,_ ...) (compile-apply (wrap-cdr e) env stack-depth)]
      [`(,rator ,_ ...)  (compile-apply e env stack-depth)]
      [`,id
       (define u (unwrap id))
       (define var (hash-ref env u #f))
       (cond
         [(not var)
          (if (number? u)
              (vector 'quote u)
              u)]
         [(indirect? var)
          (define pos (stack->pos stack-depth (indirect-stack var)))
          (define elem (indirect-element var))
          (if (indirect-checked? var)
              (vector 'ref-indirect/checked pos elem u)
              (cons pos elem))]
         [else
          (stack->pos stack-depth var)])]))

  (define (compile-letrec e env stack-depth)
    (match e
      [`(,_ ([,ids ,rhss] ...) . ,body)
       (define (make-env indirect)
         (for/fold ([env env]) ([id (in-list ids)]
                                [i (in-naturals)])
           (hash-set env (unwrap id) (indirect stack-depth i))))
       (define rhs-env (make-env indirect-checked))
       (define body-env (make-env indirect))
       (define body-stack-depth (add1 stack-depth))
       (vector 'letrec
               (for/vector #:length (length ids) ([rhs (in-list rhss)])
                 (compile-expr rhs rhs-env body-stack-depth))
               (compile-body body body-env body-stack-depth))]))

  (define (compile-apply es env stack-depth)
    (list->vector (cons 'app
                        (for/list ([e (in-wrap-list es)])
                          (compile-expr e env stack-depth)))))
  
  (define (compile-assignment id rhs env stack-depth)
    (define compiled-rhs (compile-expr rhs env stack-depth))
    (define u (unwrap id))
    (define var (hash-ref env u))
    (cond
      [(indirect? var)
       (define s (stack->pos stack-depth (indirect-stack var)))
       (define e (indirect-element var))
       (if (indirect-checked? var)
           (vector 'set!-indirect/checked s e compiled-rhs u)
           (vector 'set!-indirect s e compiled-rhs))]
      [else (error 'compile "unexpected set!")]))

  (define (args->env ids env stack-depth)
    (let loop ([ids ids] [env env] [count 0])
      (cond
        [(wrap-null? ids) (values env count #f)]
        [(wrap-pair? ids) (loop (wrap-cdr ids)
                                (hash-set env (unwrap (wrap-car ids)) (+ stack-depth count))
                                (add1 count))]
        [else
         (values (hash-set env (unwrap ids) (+ stack-depth count))
                 (add1 count)
                 #t)])))

  (start linklet-e))

;; ----------------------------------------

(define (interpret-linklet b primitives variable-ref variable-ref/no-check variable-set!
                           make-arity-wrapper-procedure)
  (interp-match
   b
   [#(,consts ,num-body-vars ,b)
    (let ([consts (and consts
                       (let ([vec (make-vector (vector-length consts))])
                         (define stack (list vec))
                         (for ([b (in-vector consts)]
                               [i (in-naturals)])
                           (vector-set! vec i (interpret-expr b stack primitives void void void void))
                           vec)
                         vec))])
      (lambda args
        (define body-vec (make-vector num-body-vars unsafe-undefined))
        (define base-stack (if consts (list consts) null))
        (define stack (list* body-vec (list->vector args) base-stack))
        (interpret-expr b stack primitives variable-ref variable-ref/no-check variable-set!
                         make-arity-wrapper-procedure)))]))

(define (interpret-expr b stack primitives variable-ref variable-ref/no-check variable-set!
                        make-arity-wrapper-procedure)
  (define (interpret b stack)
    (cond
      [(integer? b) (list-ref stack b)]
      [(pair? b) (vector-ref (list-ref stack (car b)) (cdr b))]
      [(symbol? b) (hash-ref primitives b)]
      [(vector? b)
       (interp-match
        b
        [#(app ,rator-b)
         (define len (vector-length b))
         (define rator (interpret rator-b stack))
         (cond
           [(eq? len 2)
            (rator)]
           [(eq? len 3)
            (rator
             (interpret (unsafe-vector*-ref b 2) stack))]
           [(eq? len 4)
            (rator
             (interpret (unsafe-vector*-ref b 2) stack)
             (interpret (unsafe-vector*-ref b 3) stack))]
           [else
            (apply (interpret rator-b stack)
                   (for/list ([b (in-vector b 2)])
                     (interpret b stack)))])]
        [#(quote ,v) v]
        [#(ref-indirect/checked ,s ,e ,name)
         (define v (vector-ref (list-ref stack s) e))
         (check-not-unsafe-undefined v name)]
        [#(ref-variable ,s ,e)
         (variable-ref/no-check (vector-ref (list-ref stack s) e))]
        [#(ref-variable/checked ,s ,e)
         (variable-ref (vector-ref (list-ref stack s) e))]
        [#(let ,rhss ,b)
         (define len (vector-length rhss))
         (let loop ([i 0] [new-stack stack])
           (if (= i len)
               (interpret b new-stack)
               (loop (add1 i) (cons (interpret (unsafe-vector*-ref rhss i) stack)
                                    new-stack))))]
        [#(letrec ,rhss ,b)
         (define len (vector-length rhss))
         (define frame-vec (make-vector len unsafe-undefined))
         (define new-stack (cons frame-vec stack))
         (let loop ([i 0])
           (if (= i len)
               (interpret b new-stack)
               (begin
                 (vector-set! frame-vec i (interpret (vector-ref rhss i) new-stack))
                 (loop (add1 i)))))]
        [#(begin)
         (define last (sub1 (vector-length b)))
         (let loop ([i 1])
           (if (= i last)
               (interpret (unsafe-vector*-ref b i) stack)
               (begin
                 (interpret (unsafe-vector*-ref b i) stack)
                 (loop (add1 i)))))]
        [#(begin0 ,b0)
         (define last (sub1 (unsafe-vector-length b)))
         (begin0
           (interpret b0 stack)
           (let loop ([i 2])
             (interpret (unsafe-vector*-ref b i) stack)
             (unless (= i last)
               (loop (add1 i)))))]
        [#(if ,tst ,thn ,els)
         (if (interpret tst stack)
             (interpret thn stack)
             (interpret els stack))]
        [#(wcm ,key ,val ,body)
         (with-continuation-mark
          (interpret key stack)
          (interpret val stack)
          (interpret body stack))]
        [#(cwv ,b ,clauses)
         (define vs (call-with-values (lambda () (interpret b stack)) list))
         (define len (length vs))
         (let loop ([clauses clauses])
           (cond
             [(null? clauses) (error 'call-with-values "arity error")]
             [else
              (interp-match
               (car clauses)
               [#(,mask ,b)
                (if (matching-argument-count? mask len)
                    (interpret b (push-stack stack vs mask))
                    (loop (cdr clauses)))])]))]
        [#(lambda ,mask ,b)
         (make-arity-wrapper-procedure
          (lambda args
            (if (matching-argument-count? mask (length args))
                (interpret b (push-stack stack args mask))
                (error "arity error")))
          mask
          #f)]
        [#(case-lambda ,mask)
         (define n (vector-length b))
         (make-arity-wrapper-procedure
          (lambda args
            (define len (length args))
            (let loop ([i 2])
              (cond
                [(= i n) (error "arity error")]
                [else
                 (interp-match
                  (unsafe-vector*-ref b i)
                  [#(lambda ,mask ,b)
                   (if (matching-argument-count? mask len)
                       (interpret b (push-stack stack args mask))
                       (loop (add1 i)))])])))
          mask
          #f)]
        [#(set-variable! ,s ,e ,b ,c)
         (variable-set! (vector-ref (list-ref stack s) e)
                        (interpret b stack)
                        c)]
        [#(set!-indirect ,s ,e ,b)
         (unsafe-vector*-set! (list-ref stack s) e (interpret b stack))]
        [#(set!-indirect/checked ,s ,e ,b ,name)
         (define v (interpret b stack))
         (define vec (list-ref stack s))
         (check-not-unsafe-undefined/assign (unsafe-vector*-ref vec e) name)
         (unsafe-vector*-set! vec e v)])]
      [else b]))

  (define (matching-argument-count? mask len)
    (bitwise-bit-set? mask len))

  (interpret b stack))

;; mask has a single bit set or all bits above some bit
(define (push-stack stack vals mask)
  (define rest? (negative? mask))
  (define count (if rest?
                    (integer-length mask)
                    (sub1 (integer-length mask))))
  (let loop ([stack stack] [vals vals] [count (if rest? (sub1 count) count)])
    (cond
      [(zero? count)
       (if rest? (cons vals stack) stack)]
      [else
       (loop (cons (car vals) stack) (cdr vals) (sub1 count))])))

(define (count->mask count rest?)
  (arithmetic-shift (if rest? -1 1) count))

;; ----------------------------------------

(module+ main
  (define primitives (hash 'list list
                           'vector vector
                           'add1 add1
                           'values values
                           'continuation-mark-set-first continuation-mark-set-first))
  (define b
    (interpretable-jitified-linklet '(let* ([s "string"])
                                       (lambda (x two-box)
                                         (define other 5)
                                         (begin
                                           (define f (lambda (y)
                                                       (vector x y)))
                                           (define g (case-lambda
                                                       [() no]
                                                       [ys
                                                        (vector x ys)])))
                                         (define-values (one two) (values 100 200))
                                         (variable-set! two-box two 'constant)
                                         (letrec ([ok 'ok])
                                           (set! other (call-with-values (lambda () (values 71 (begin0 88 ok)))
                                                         (lambda (v q) (list q v))))
                                           (with-continuation-mark
                                            'x 'cm/x
                                            (list (if s s #f) x ok other
                                                  (f 'vec) (g 'also-vec 'more)
                                                  one two (variable-ref two-box)
                                                  (continuation-mark-set-first #f 'x 'no))))))
                                    values))
  (define l (interpret-linklet b primitives unbox unbox (lambda (b v c)
                                                          (set-box! b v))
                               (lambda (proc mask name) proc)))
  (l 'the-x (box #f)))
