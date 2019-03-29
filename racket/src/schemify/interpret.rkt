#lang racket/base
(require racket/unsafe/undefined
         racket/fixnum
         "match.rkt"
         "wrap.rkt"
         "path-for-srcloc.rkt"
         "to-fasl.rkt"
         "interp-match.rkt"
         "interp-stack.rkt")

;; Interpreter for the output of "jitify". This little interpreter is
;; useful to avoid going through a more heavyweight `eval` or
;; `interpret`, mainly because we don't need to go through a macro
;; expander. Also, because it's tailored to the shape of a linklet
;; outer layer, it can implement that layer more efficiently and
;; compactly.

;; The interpreter operates on its own "bytecode" format, so
;; `interpretable-jitified-linklet` compiles to that format, and
;; `interpret-linklet` runs it.

;; The interpreter is safe-for-space. It uses flat closures, a
;; persistent mapping from indices to values for the environment, and
;; explicit operations to remove mappings from the environment as
;; needed to implement space safety.

(provide interpretable-jitified-linklet
         interpret-linklet)

(struct indirect (pos element))
(struct boxed (pos))
(struct boxed/check boxed ())

(define (interpretable-jitified-linklet linklet-e strip-annotations)
  ;; Return a compiled linklet in two parts: a vector expression for
  ;; constants to be run once, and a expression for the linklet body.
  
  ;; Conceptually, the run-time environment is implemented as a list,
  ;; and identifiers are mapped to positions in that list, where 0
  ;; corresponds to the last element of the list and more deeply
  ;; nested bindings are pushed on to the front. The `stack-depth` at
  ;; compile time corresponds to the length of that list. The
  ;; compile-time environment maps names to those coordinates. But
  ;; those coodinates are shifted for closure capture, where negative
  ;; positions are used to access elements of the closure.

  ;; At run time, instead of a list, the "stack" is implemented as a
  ;; persistent map, but the position keys for that mapping are still
  ;; contiguous integers shifted from the compile-time coordinates. A
  ;; `stack-info` record at compile time manages the translation from
  ;; environment coordinates to run-time positions.

  ;; The compilation pass is responsible not only for turning names
  ;; into run-time positions, but also for tracking the last use of a
  ;; variable, so its mapping can be removed at runtime to preserve
  ;; space safety. To compute last use, the compiler must always work
  ;; from the end expressions toward starting expressions. That's why
  ;; `compile-list` compiles later expressions before earlier ones in
  ;; the list, for example.

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
       (define bindings-stk-i (stack-info #f #hasheq() #f #f 0))
       (let loop ([bindings bindings] [elem 0] [env '#hasheq()] [accum '()])
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
                    (fx+ elem 1)
                    (hash-set env (car binding) (indirect 0 elem))
                    (let ([rhs (cadr binding)])
                      (cons (cond
                              [(or (path? rhs)
                                   (path-for-srcloc? rhs)
                                   (to-fasl? rhs))
                               ;; The caller must extract all the paths from the bindings
                               ;; and pass them back in at interp time; assume '#%path is
                               ;; not a primitive
                               '#%path]
                              [else
                               (compile-expr rhs env 1 bindings-stk-i #t)])
                            accum))))]))]))

  (define (compile-linklet-body v env stack-depth)
    (match v
      [`(lambda ,args . ,body)
       ;; The `args` here are linklet import and export variables
       (define num-args (length args))
       (define args-env
         (for/fold ([env env]) ([arg (in-list args)]
                                [i (in-naturals)])
           (hash-set env arg (+ stack-depth i))))
       (define body-vars-index (+ num-args stack-depth))
       ;; Gather all the names that have `define`s, and build up the
       ;; enviornment that has them consceptually pushed after the
       ;; import and export variables.
       (define-values (body-env num-body-vars)
         (for/fold ([env args-env] [num-body-vars 0]) ([e (in-wrap-list body)])
           (let loop ([e e] [env env] [num-body-vars num-body-vars])
             (match e
               [`(define ,id . ,_)
                (values (hash-set env (unwrap id) (boxed (+ body-vars-index num-body-vars)))
                        (add1 num-body-vars))]
               [`(define-values ,ids . ,_)
                (for/fold ([env env] [num-body-vars num-body-vars]) ([id (in-wrap-list ids)])
                  (values (hash-set env (unwrap id) (boxed (+ body-vars-index num-body-vars)))
                          (add1 num-body-vars)))]
               [`(begin . ,body)
                (for/fold ([env env] [num-body-vars num-body-vars]) ([e (in-wrap-list body)])
                  (loop e env num-body-vars))]
               [`,_ (values env num-body-vars)]))))
       (define body-stack-depth (+ num-body-vars num-args stack-depth))
       ;; This `stack-info` is mutated as expressiones are compiled,
       ;; because that's more convenient than threading it through as
       ;; both an argument and a result
       (define stk-i (stack-info #f #hasheq() #f #f 0))
       (define new-body
         (compile-top-body body body-env body-stack-depth stk-i))
       (values new-body
               num-body-vars)]))

  ;; Like `compile-body`, but flatten top-level `begin`s
  (define (compile-top-body body env stack-depth stk-i)
    (define bs (let loop ([body body])
                 (match body
                   [`() '()]
                   [`((begin ,subs ...) . ,rest)
                    (loop (append subs rest))]
                   [`(,e . ,rest)
                    (define new-rest (loop rest))
                    (cons (compile-expr e env stack-depth stk-i #t)
                          new-rest)])))
    (cond
      [(null? bs) '#(void)]
      [(and (pair? bs) (null? (cdr bs)))
       (car bs)]
      [else
       (list->vector (cons 'begin bs))]))

  (define (compile-body body env stack-depth stk-i tail?)
    (match body
      [`(,e) (compile-expr e env stack-depth stk-i tail?)]
      [`,_
       (list->vector
        (cons 'begin (compile-list body env stack-depth stk-i tail?)))]))

  (define (compile-list body env stack-depth stk-i tail?)
    (let loop ([body body])
      (cond
        [(null? body) '()]
        [else
         (define rest-body (wrap-cdr body))
         (define new-rest (loop rest-body))
         (cons (compile-expr (wrap-car body) env stack-depth stk-i (and tail? rest-body))
               new-rest)])))

  (define (compile-expr e env stack-depth stk-i tail?)
    (match e
      [`(lambda ,ids . ,body)
       (define-values (body-env count rest?)
         (args->env ids env stack-depth))
       (define cmap (make-hasheq))
       (define body-stack-depth (+ stack-depth count))
       ;; A fresh `stack-info` reflects how a flat closure shifts the
       ;; coordinates of the variables that it captures; captured
       ;; variables are added to `cmap` as they are discovered
       (define body-stk-i (stack-info stack-depth cmap #hasheq() #f 0))
       (define new-body (compile-body body body-env body-stack-depth body-stk-i #t))
       (define rev-cmap (for/hasheq ([(i pos) (in-hash cmap)]) (values (- -1 pos) i)))
       (vector 'lambda
               (count->mask count rest?)
               (wrap-property e 'inferred-name)
               (for/vector #:length (hash-count cmap) ([i (in-range (hash-count cmap))])
                 (stack->pos (hash-ref rev-cmap i) stk-i))
               new-body)]
      [`(case-lambda [,idss . ,bodys] ...)
       (define lams (for/list ([ids (in-list idss)]
                               [body (in-list bodys)])
                      (compile-expr `(lambda ,ids . ,body) env stack-depth stk-i tail?)))
       (define mask (for/fold ([mask 0]) ([lam (in-list lams)])
                      (bitwise-ior mask (interp-match lam [#(lambda ,mask) mask]))))
       (define name (wrap-property e 'inferred-name))
       (list->vector (list* 'case-lambda mask name lams))]
      [`(let ([,ids ,rhss] ...) . ,body)
       (define len (length ids))
       (define body-env
         (for/fold ([env env]) ([id (in-list ids)]
                                [i (in-naturals)])
           (hash-set env (unwrap id) (+ stack-depth i))))
       (define body-stack-depth (+ stack-depth len))
       (define new-body (compile-body body body-env body-stack-depth stk-i tail?))
       (define pos (stack->pos stack-depth stk-i #:nonuse? #t))
       (stack-info-forget! stk-i stack-depth pos len)
       (define new-rhss (list->vector
                         (compile-list rhss env stack-depth stk-i #f)))
       (or
        ;; Merge nested `let`s into a `let*` to reduce vector nesting
        (cond
          [(vector? new-body)
           (interp-match
            new-body
            [#(let ,pos2 ,rhss2 ,b)
             (vector 'let* (list pos pos2) (list new-rhss rhss2) b)]
            [#(let* ,poss ,rhsss ,b)
             (vector 'let* (cons pos poss) (cons new-rhss rhsss) b)]
            [#() #f])]
          [else #f])
        (vector 'let pos new-rhss new-body))]
      [`(letrec . ,_) (compile-letrec e env stack-depth stk-i tail?)]
      [`(letrec* . ,_) (compile-letrec e env stack-depth stk-i tail?)]
      [`(begin . ,vs)
       (compile-body vs env stack-depth stk-i tail?)]
      [`(begin0 ,e)
       (compile-expr e env stack-depth stk-i tail?)]
      [`(begin0 ,e . ,vs)
       (define new-body (compile-body vs env stack-depth stk-i #f))
       (vector 'begin0
               (compile-expr e env stack-depth stk-i #f)
               new-body)]
      [`($value ,e)
       (vector '$value (compile-expr e env stack-depth stk-i #f))]
      [`(if ,tst ,thn ,els)
       (define then-stk-i (stack-info-branch stk-i))
       (define else-stk-i (stack-info-branch stk-i))
       (define new-then (compile-expr thn env stack-depth then-stk-i tail?))
       (define new-else (compile-expr els env stack-depth else-stk-i tail?))
       (define all-clear (stack-info-merge! stk-i (list then-stk-i else-stk-i)))
       (vector 'if
               (compile-expr tst env stack-depth stk-i #f)
               (add-clears new-then then-stk-i all-clear)
               (add-clears new-else else-stk-i all-clear))]
      [`(with-continuation-mark ,key ,val ,body)
       (define new-body (compile-expr body env stack-depth stk-i tail?))
       (define new-val (compile-expr val env stack-depth stk-i #f))
       (vector 'wcm
               (compile-expr key env stack-depth stk-i #f)
               new-val
               new-body)]
      [`(quote ,v)
       (let ([v (strip-annotations v)])
         ;; Protect with `quote` any value that looks like an
         ;; interpreter instruction:
         (if (or (vector? v)
                 (pair? v)
                 (symbol? v)
                 (number? v)
                 (box? v))
             (vector 'quote v)
             v))]
      [`(set! ,id ,rhs)
       (compile-assignment id rhs env stack-depth stk-i)]
      [`(define ,id ,rhs)
       (compile-assignment id rhs env stack-depth stk-i)]
      [`(define-values ,ids ,rhs)
       (define gen-ids (for/list ([id (in-list ids)])
                         (gensym (unwrap id))))
       (compile-expr `(call-with-values (lambda () ,rhs)
                        (lambda ,gen-ids
                          ,@(if (null? ids)
                                '((void))
                                (for/list ([id (in-list ids)]
                                           [gen-id (in-list gen-ids)])
                                  `(set! ,id ,gen-id)))))
                     env
                     stack-depth
                     stk-i
                     tail?)]
      [`(call-with-values ,proc1 (lambda ,ids . ,body))
       (compile-expr `(call-with-values ,proc1 (case-lambda
                                                 [,ids . ,body]))
                     env
                     stack-depth
                     stk-i
                     tail?)]
      [`(call-with-values (lambda () . ,body) (case-lambda [,idss . ,bodys] ...))
       (define body-stk-is (for/list ([body (in-list bodys)])
                             (stack-info-branch stk-i)))
       (define initial-new-clauses
         (for/list ([ids (in-list idss)]
                    [body (in-list bodys)]
                    [body-stk-i (in-list body-stk-is)])
           (define-values (new-env count rest?)
             (args->env ids env stack-depth))
           (define new-stack-depth (+ stack-depth count))
           (define new-body (compile-body body new-env new-stack-depth body-stk-i tail?))
           (define pos (stack->pos stack-depth body-stk-i #:nonuse? #t))
           (stack-info-forget! body-stk-i stack-depth pos count)
           (vector (count->mask count rest?)
                   new-body)))
       (define all-clear (stack-info-merge! stk-i body-stk-is))
       (vector 'cwv
               (compile-body body env stack-depth stk-i #f)
               (stack->pos stack-depth stk-i #:nonuse? #t)
               (match e
                 [`(,_ ,_ ,receiver) (wrap-property receiver 'inferred-name)])
               (for/list ([initial-new-clause (in-list initial-new-clauses)]
                          [body-stk-i (in-list body-stk-is)])
                 (define body (vector-ref initial-new-clause 1))
                 (vector (vector-ref initial-new-clause 0)
                         (add-clears body body-stk-i all-clear))))]
      [`(call-with-module-prompt (lambda () . ,body))
       (vector 'cwmp0 (compile-body body env stack-depth stk-i tail?))]
      [`(call-with-module-prompt (lambda () . ,body) ',ids ',constances ,vars ...)
       (vector 'cwmp
               (compile-body body env stack-depth stk-i tail?)
               ids
               constances
               (compile-list vars env stack-depth stk-i #f))]
      [`(variable-set! ,dest-id ,e ',constance)
       (define dest-var (hash-ref env (unwrap dest-id)))
       (define new-expr (compile-expr e env stack-depth stk-i #f))
       (vector 'set-variable!
               (stack->pos dest-var stk-i)
               new-expr
               constance)]
      [`(variable-ref ,id)
       (define var (hash-ref env (unwrap id)))
       (vector 'ref-variable/checked (stack->pos var stk-i))]
      [`(variable-ref/no-check ,id)
       (define var (hash-ref env (unwrap id)))
       (vector 'ref-variable (stack->pos var stk-i))]
      [`(#%app ,_ ...) (compile-apply (wrap-cdr e) env stack-depth stk-i tail?)]
      [`(,rator ,_ ...)  (compile-apply e env stack-depth stk-i tail?)]
      [`,id
       (define u (unwrap id))
       (define var (hash-ref env u #f))
       (cond
         [(not var)
          (if (number? u)
              (vector 'quote u)
              u)]
         [(indirect? var)
          (define pos (stack->pos (indirect-pos var) stk-i))
          (define elem (indirect-element var))
          (cons pos elem)]
         [(boxed? var)
          (define pos (stack->pos (boxed-pos var) stk-i))
          (if (boxed/check? var)
              (vector 'unbox/checked pos u)
              (vector 'unbox pos))]
         [else
          (stack->pos var stk-i)])]))

  (define (compile-letrec e env stack-depth stk-i tail?)
    (match e
      [`(,_ ([,ids ,rhss] ...) . ,body)
       (define count (length ids))
       (define (make-env boxed)
         (for/fold ([env env]) ([id (in-list ids)]
                                [i (in-naturals)])
           (hash-set env (unwrap id) (boxed (+ (- count i 1) stack-depth)))))
       (define rhs-env (make-env boxed/check))
       (define body-env (make-env boxed))
       (define body-stack-depth (+ stack-depth count))
       (define new-body (compile-body body body-env body-stack-depth stk-i tail?))
       (define new-rhss (list->vector
                         (compile-list rhss rhs-env body-stack-depth stk-i #F)))
       (define pos (stack->pos stack-depth stk-i #:nonuse? #t))
       (stack-info-forget! stk-i stack-depth pos count)
       (vector 'letrec pos new-rhss new-body)]))

  (define (compile-apply es env stack-depth stk-i tail?)
    (define new-es (compile-list es env stack-depth stk-i #f))
    (unless tail?
      (stack-info-non-tail! stk-i stack-depth))
    (list->vector (cons 'app new-es)))

  (define (compile-assignment id rhs env stack-depth stk-i)
    (define compiled-rhs (compile-expr rhs env stack-depth stk-i #f))
    (define u (unwrap id))
    (define var (hash-ref env u))
    (cond
      [(indirect? var)
       (define s (stack->pos (indirect-pos var) stk-i))
       (define e (indirect-element var))
       (vector 'set!-indirect s e compiled-rhs)]
      [(boxed? var)
       (define s (stack->pos (boxed-pos var) stk-i))
       (if (boxed/check? var)
           (vector 'set!-boxed/checked s compiled-rhs u)
           (vector 'set!-boxed s compiled-rhs u))]
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

  (define (add-clears e stk-i all-clear)
    (define local-use-map (stack-info-local-use-map stk-i))
    (define clears
      (for/list ([pos (in-hash-keys all-clear)]
                 #:unless (hash-ref local-use-map pos #f))
        pos))
    (cond
      [(null? clears) e]
      [else (vector 'clear clears e)]))

  (start linklet-e))

;; ----------------------------------------

(define (interpret-linklet b            ; compiled form
                           paths        ; unmarshaled paths
                           primitives   ; hash of symbol -> value
                           ;; the implementation of variables:
                           variable-ref variable-ref/no-check variable-set!
                           ;; to create a procedure with a specific arity mask:
                           make-arity-wrapper-procedure)
  (interp-match
   b
   [#(,consts ,num-body-vars ,b)
    (let ([consts (and consts
                       (let ([vec (make-vector (vector*-length consts))])
                         (define stack (stack-set empty-stack 0 vec))
                         (for/fold ([paths paths]) ([b (in-vector consts)]
                                                    [i (in-naturals)])
                           (cond
                             [(eq? b '#%path)
                              (vector-set! vec i (car paths))
                              (cdr paths)]
                             [else
                              (vector-set! vec i (interpret-expr b stack primitives void void void void))
                              paths]))
                         vec))])
      (lambda args
        (define start-stack (if consts
                                (stack-set empty-stack 0 consts)
                                empty-stack))
        (define args-stack (for/fold ([stack start-stack]) ([arg (in-list args)]
                                                            [i (in-naturals (if consts 1 0))])
                             (stack-set stack i arg)))
        (define post-args-pos (stack-count args-stack))
        (define stack (for/fold ([stack args-stack]) ([i (in-range num-body-vars)])
                        (stack-set stack (+ i post-args-pos) (box unsafe-undefined))))
        (interpret-expr b stack primitives variable-ref variable-ref/no-check variable-set!
                        make-arity-wrapper-procedure)))]))

(define (interpret-expr b stack primitives variable-ref variable-ref/no-check variable-set!
                        make-arity-wrapper-procedure)

  ;; Returns `result ...` when `tail?` is #t, and
  ;; returns `(values stack result ...)` when `tail?` is #f.
  ;; An updated "stack" is returned because bindings are
  ;; removed from the stack on their last uses (where there is
  ;; a non-tail call after the last use)
  (define (interpret b stack [tail? #f])
    (cond
      [(integer? b) (stack-ref stack b tail?)]
      [(box? b) (stack-ref stack b tail?)]
      [(pair? b)
       (define-values (new-stack vec) (stack-ref stack (car b)))
       (define val (vector*-ref vec (cdr b)))
       (if tail?
           val
           (values new-stack val))]
      [(symbol? b)
       (define val (hash-ref primitives b))
       (if tail?
           val
           (values stack val))]
      [(vector? b)
       (interp-match
        b
        [#(app ,rator-b)
         (define len (vector*-length b))
         (define-values (rand-stack rator) (interpret rator-b stack))
         (define-syntax-rule (add-value stack app)
           (call-with-values
            (lambda () app)
            (case-lambda
              [(v) (values stack v)]
              [vs (apply values stack vs)])))
         (cond
           [(eq? len 2)
            (if tail?
                (rator)
                (add-value stack (rator)))]
           [(eq? len 3)
            (define-values (stack rand) (interpret (vector*-ref b 2) rand-stack))
            (if tail?
                (rator rand)
                (add-value stack (rator rand)))]
           [(eq? len 4)
            (define-values (stack1 rand1) (interpret (vector*-ref b 2) rand-stack))
            (define-values (stack2 rand2) (interpret (vector*-ref b 3) stack1))
            (if tail?
                (rator rand1 rand2)
                (add-value stack (rator rand1 rand2)))]
           [else
            (define-values (stack rev-rands)
              (for/fold ([stack rand-stack] [rev-rands null]) ([b (in-vector b 2)])
                (define-values (new-stack v) (interpret b stack))
                (values new-stack (cons v rev-rands))))
            (define rands (reverse rev-rands))
            (if tail?
                (apply rator rands)
                (add-value stack (apply rator rands)))])]
        [#(quote ,v)
         (if tail?
             v
             (values stack v))]
        [#(unbox ,s)
         (define-values (new-stack bx) (stack-ref stack s))
         (define val (unbox* bx))
         (if tail?
             val
             (values new-stack val))]
        [#(unbox/checked ,s ,name)
         (define-values (new-stack bx) (stack-ref stack s))
         (define v (unbox* bx))
         (define val (check-not-unsafe-undefined v name))
         (if tail?
             val
             (values new-stack val))]
        [#(ref-variable ,s)
         (define-values (new-stack var) (stack-ref stack s))
         (define val (variable-ref/no-check var))
         (if tail?
             val
             (values new-stack val))]
        [#(ref-variable/checked ,s)
         (define-values (new-stack var) (stack-ref stack s))
         (define val (variable-ref var))
         (if tail?
             val
             (values new-stack val))]
        [#(let ,pos ,rhss ,b)
         (define len (vector*-length rhss))
         (define body-stack
           (let loop ([i 0] [stack stack])
             (cond
               [(fx= i len) stack]
               [else
                (define-values (new-stack val) (interpret (vector*-ref rhss i) stack))
                (stack-set (loop (fx+ i 1) new-stack) (fx+ i pos) val)])))
         (interpret b body-stack tail?)]
        [#(let* ,poss ,rhsss ,b)
         (define body-stack
           (for/fold ([stack stack]) ([pos (in-list poss)]
                                      [rhss (in-list rhsss)])
             (define len (vector*-length rhss))
             (let loop ([i 0] [stack stack])
               (cond
                 [(fx= i len) stack]
                 [else
                  (define-values (new-stack val) (interpret (vector*-ref rhss i) stack))
                  (stack-set (loop (fx+ i 1) new-stack) (fx+ i pos) val)]))))
         (interpret b body-stack tail?)]
        [#(letrec ,pos ,rhss ,b)
         (define len (vector*-length rhss))
         (define-values (body-stack boxes)
           (for/fold ([stack stack] [boxes null]) ([i (in-range len)])
             (define bx (box unsafe-undefined))
             (values (stack-set stack (fx+ (fx- len i 1) pos) bx)
                     (cons bx boxes))))
         (let loop ([i 0] [stack body-stack] [boxes boxes])
           (cond
             [(fx= i len)
              (interpret b stack tail?)]
             [else
              (define-values (new-stack val) (interpret (vector*-ref rhss i) stack))
              (set-box! (car boxes) val)
              (loop (fx+ i 1) new-stack (cdr boxes))]))]
        [#(begin)
         (define last (fx- (vector*-length b) 1))
         (let loop ([i 1] [stack stack])
           (cond
             [(fx= i last)
              (interpret (vector*-ref b i) stack tail?)]
             [else
              (call-with-values
               (lambda () (interpret (vector*-ref b i) stack))
               (case-lambda
                 [(new-stack val) (loop (fx+ i 1) new-stack)]
                 [(new-stack . vals) (loop (fx+ i 1) new-stack)]))]))]
        [#(begin0 ,b0)
         (define last (fx- (vector*-length b) 1))
         (call-with-values
          (lambda () (interpret b0 stack))
          (lambda (stack . vals)
            (let loop ([i 2] [stack stack])
              (define new-stack
                (call-with-values
                 (lambda () (interpret (vector*-ref b i) stack))
                 (case-lambda
                   [(new-stack val) new-stack]
                   [(new-stack . vals) new-stack])))
              (if (fx= i last)
                  (if tail?
                      (apply values vals)
                      (apply values new-stack vals))
                  (loop (fx+ i 1) new-stack)))))]
        [#($value ,e)
         (let ([v (interpret e stack)])
           v)]
        [#(clear ,clears ,e)
         (let loop ([clears clears] [stack stack])
           (cond
             [(null? clears)
              (interpret e stack tail?)]
             [else
              (loop (cdr clears) (stack-remove stack (car clears)))]))]
        [#(if ,tst ,thn ,els)
         (define-values (new-stack val) (interpret tst stack))
         (if val
             (interpret thn new-stack tail?)
             (interpret els new-stack tail?))]
        [#(wcm ,key ,val ,body)
         (define-values (k-stack k-val) (interpret key stack))
         (define-values (v-stack v-val) (interpret val k-stack))
         (with-continuation-mark
          k-val
          v-val
          (interpret body v-stack tail?))]
        [#(cwv ,b ,pos ,name ,clauses)
         (define-values (new-stack vs)
           (call-with-values
            (lambda () (interpret b stack))
            (lambda (stack . vals) (values stack vals))))
         (define len (length vs))
         (let loop ([clauses clauses] [full-mask 0])
           (cond
             [(null? clauses)(apply raise-arity-mask-error (or name '|#<procedure>|) full-mask vs)]
             [else
              (interp-match
               (car clauses)
               [#(,mask ,b)
                (if (matching-argument-count? mask len)
                    (interpret b (push-stack new-stack pos vs mask) tail?)
                    (loop (cdr clauses) (fxior mask full-mask)))])]))]
        [#(cwmp0 ,b)
         (unless tail? (error 'interpret "expect call-with-module-prompt in tail position"))
         ((hash-ref primitives 'call-with-module-prompt)
          (lambda () (interpret b stack #t)))]
        [#(cwmp ,b ,ids ,constances ,var-es)
         (unless tail? (error 'interpret "expect call-with-module-prompt in tail position"))
         (apply (hash-ref primitives 'call-with-module-prompt)
                (lambda () (interpret b stack #t))
                ids
                constances
                (for/list ([e (in-list var-es)])
                  (interpret e stack #t)))]
        [#(lambda ,mask ,name ,close-vec ,_)
         (define-values (new-stack captured) (capture-closure close-vec stack))
         (define val
           (make-arity-wrapper-procedure
            (lambda args
              (cond
                [(matching-argument-count? mask (length args))
                 (apply-function b captured args)]
                [else
                 (apply raise-arity-mask-error (or name '|#<procedure>|) mask args)]))
            mask
            name))
         (if tail?
             val
             (values new-stack val))]
        [#(case-lambda ,mask ,name)
         (define n (vector*-length b))
         (define-values (new-stack captureds)
           (let loop ([i 3] [stack stack])
             (cond
               [(fx= i n) (values stack '())]
               [else
                (define-values (rest-stack rest-captureds) (loop (fx+ i 1) stack))
                (define-values (new-stack captured)
                  (interp-match
                   (vector*-ref b i)
                   [#(lambda ,mask ,name ,close-vec) (capture-closure close-vec rest-stack)]))
                (values new-stack (cons captured rest-captureds))])))
         (define val
           (make-arity-wrapper-procedure
            (lambda args
              (define len (length args))
              (let loop ([i 3] [captureds captureds] [full-mask 0])
                (cond
                  [(fx= i n)
                   (apply raise-arity-mask-error (or name '|#<procedure>|) full-mask args)]
                  [else
                   (define one-b (vector*-ref b i))
                   (interp-match
                    one-b
                    [#(lambda ,mask)
                     (if (matching-argument-count? mask len)
                         (apply-function one-b (car captureds) args)
                         (loop (fx+ i 1) (cdr captureds) (fxior full-mask mask)))])])))
            mask
            #f))
         (if tail?
             val
             (values new-stack val))]
        [#(set-variable! ,s ,b ,c)
         (define-values (var-stack var) (stack-ref stack s))
         (define-values (val-stack val) (interpret b var-stack))
         (variable-set! var val c)
         (if tail?
             (void)
             (values val-stack (void)))]
        [#(set!-indirect ,s ,e ,b)
         (define-values (vec-stack vec) (stack-ref stack s))
         (define-values (val-stack val) (interpret b vec-stack))
         (vector*-set! vec e val)
         (if tail?
             (void)
             (values val-stack (void)))]
        [#(set!-boxed ,s ,b ,name)
         (define-values (bx-stack bx) (stack-ref stack s))
         (define-values (v-stack v) (interpret b bx-stack))
         (set-box*! bx v)
         (if tail?
             (void)
             (values v-stack (void)))]
        [#(set!-boxed/checked ,s ,b ,name)
         (define-values (bx-stack bx) (stack-ref stack s))
         (define-values (v-stack v) (interpret b bx-stack))
         (check-not-unsafe-undefined/assign (unbox* bx) name)
         (set-box*! bx v)
         (if tail?
             (void)
             (values v-stack (void)))])]
      [else (if tail?
                b
                (values stack b))]))

  (define (capture-closure close-vec stack)
    (define len (vector*-length close-vec))
    (let loop ([i 0] [stack stack] [captured empty-stack])
      (cond
        [(= i len) (values stack captured)]
        [else
         (define-values (val-stack val) (stack-ref stack (vector*-ref close-vec i)))
         (loop (add1 i)
               val-stack
               (stack-set captured (- -1 i) val))])))

  (define (apply-function b captured args)
    (interp-match
     b
     [#(lambda ,mask ,name ,close-vec ,b)
      (interpret b (push-stack captured 0 args mask) #t)]))

  (cond
    [(vector? b)
     (interp-match
      b
      [#(begin)
       (define last (sub1 (vector*-length b)))
       (let loop ([i 1])
         (define e (vector*-ref b i))
         (cond
           [(= i last)
            (interpret e stack #t)]
           [else
            (interpret e stack #t)
            (loop (add1 i))]))]
      [#()
       (interpret b stack #t)])]
    [else
     (interpret b stack #t)]))

;; ----------------------------------------

(define (count->mask count rest?)
  (if rest?
      (bitwise-xor -1 (sub1 (arithmetic-shift 1 (sub1 count))))
      (arithmetic-shift 1 count)))

(define (matching-argument-count? mask len)
  (bitwise-bit-set? mask len))

;; ----------------------------------------

(module+ main
  (require racket/pretty)
  (define primitives (hash 'list list
                           'vector vector
                           'add1 add1
                           'values values
                           'continuation-mark-set-first continuation-mark-set-first))
  (struct var ([val #:mutable]) #:transparent)
  (define b
    (interpretable-jitified-linklet '(let* ([s "string"])
                                       (lambda (x two-box)
                                         (define other 5)
                                         (begin
                                           (define f (lambda (y)
                                                       (let ([z y])
                                                         (vector x z))))
                                           (define g (case-lambda
                                                       [() no]
                                                       [ys
                                                        (vector x ys)])))
                                         (define h (lambda (t x y a b)
                                                     (list (if t (list x a) (list y b))
                                                           (list a b))))
                                         (define h2 (lambda (t x)
                                                      (if t
                                                          x
                                                          (let ([y 10])
                                                            y))))
                                         (define h3 (lambda (t x)
                                                      (let ([y (let ([z 0])
                                                                 z)])
                                                        (list x y (let ([z 2])
                                                                    z)))))
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
  (pretty-print b)
  (define l (interpret-linklet b primitives var-val var-val (lambda (b v c)
                                                              (set-var-val! b v))
                               (lambda (proc mask name) proc)))
  (l 'the-x (var #f)))
