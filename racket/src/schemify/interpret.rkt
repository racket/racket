#lang racket/base
(require racket/unsafe/undefined
         racket/fixnum
         racket/symbol
         "match.rkt"
         "wrap.rkt"
         "interp-match.rkt"
         "interp-stack.rkt"
         "gensym.rkt")

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

(provide interpreter-link!
         interpretable-jitified-linklet
         interpret-linklet)

(struct indirect (pos element))
(struct boxed (pos))
(struct boxed/check boxed ())

(define primitives '#hasheq())
(define strip-annotations (lambda (e) e))
(define variable-ref (lambda (var) (unbox var)))
(define variable-ref/no-check (lambda (var) (unbox var)))
(define variable-set! (lambda (var v) (set-box! var v)))
(define variable-set!/define (lambda (var v) (set-box! var v)))
(define make-interp-procedure* (lambda (proc mask name) proc))

(define (interpreter-link! prims
                           strip
                           var-ref var-ref/no-check
                           var-set! var-set!/def
                           make-proc)
  (set! primitives prims)
  (set! strip-annotations strip)
  (set! variable-ref var-ref)
  (set! variable-ref/no-check var-ref/no-check)
  (set! variable-set! var-set!)
  (set! variable-set!/define var-set!/def)
  (set! make-interp-procedure* make-proc))

(define (interpretable-jitified-linklet linklet-e serializable?)
  ;; Return a compiled linklet as an expression for the linklet body.
  
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
    (define-values (compiled-body num-body-vars)
      (compile-linklet-body linklet-e '#hasheq() 0))
    (vector num-body-vars
            compiled-body))

  (define (compile-linklet-body v env stack-depth)
    (match v
      [`(lambda ,args . ,body)
       ;; Gather all `set!`ed variables, since they'll need to be boxed
       ;; if they're not top-level `define`s
       (define mutated (extract-list-mutated body '#hasheq()))
       ;; The `args` here are linklet import and export variables
       (define num-args (length args))
       (define args-env
         (for/fold ([env env]) ([arg (in-list args)]
                                [i (in-naturals)])
           (hash-set env arg (+ stack-depth i))))
       (define body-vars-index (+ num-args stack-depth))
       ;; Gather all the names that have `define`s, and build up the
       ;; environment that has them consceptually pushed after the
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
       ;; This `stack-info` is mutated as expressions are compiled,
       ;; because that's more convenient than threading it through as
       ;; both an argument and a result
       (define stk-i (make-stack-info #:track-use? #t))
       (define new-body
         (compile-top-body body body-env body-stack-depth stk-i mutated))
       (values new-body
               num-body-vars)]))

  ;; Like `compile-body`, but flatten top-level `begin`s
  (define (compile-top-body body env stack-depth stk-i mutated)
    (define bs (let loop ([body body])
                 (match body
                   [`() '()]
                   [`((begin ,subs ...) . ,rest)
                    (loop (append subs rest))]
                   [`(,e . ,rest)
                    (define new-rest (loop rest))
                    (cons (compile-expr e env stack-depth stk-i #t mutated)
                          new-rest)])))
    (cond
      [(null? bs) '#(void)]
      [(and (pair? bs) (null? (cdr bs)))
       (car bs)]
      [else
       (list->vector (cons 'begin bs))]))

  (define (compile-body body env stack-depth stk-i tail? mutated)
    (match body
      [`(,e) (compile-expr e env stack-depth stk-i tail? mutated)]
      [`,_
       (list->vector
        (cons 'begin (compile-list body env stack-depth stk-i tail? mutated)))]))

  (define (compile-list body env stack-depth stk-i tail? mutated)
    (let loop ([body body])
      (cond
        [(null? body) '()]
        [else
         (define rest-body (wrap-cdr body))
         (define new-rest (loop rest-body))
         (cons (compile-expr (wrap-car body) env stack-depth stk-i (and tail? (null? rest-body)) mutated)
               new-rest)])))

  (define (compile-expr e env stack-depth stk-i tail? mutated)
    (match e
      [`(lambda ,ids . ,body)
       (define-values (body-env count rest?)
         (args->env ids env stack-depth mutated))
       (define cmap (make-hasheq))
       (define body-stack-depth (+ stack-depth count))
       ;; A fresh `stack-info` reflects how a flat closure shifts the
       ;; coordinates of the variables that it captures; captured
       ;; variables are added to `cmap` as they are discovered
       (define body-stk-i (make-stack-info #:capture-depth stack-depth
                                           #:closure-map cmap
                                           #:track-use? #t))
       (define new-body (compile-body body body-env body-stack-depth body-stk-i #t mutated))
       (define rev-cmap (for/hasheq ([(i pos) (in-hash cmap)]) (values (- -1 pos) i)))
       (vector 'lambda
               (count->mask count rest?)
               (extract-procedure-wrap-data e)
               (for/vector #:length (hash-count cmap) ([i (in-range (hash-count cmap))])
                 (stack->pos (hash-ref rev-cmap i) stk-i))
               (add-boxes/remove-unused new-body ids mutated body-env body-stk-i))]
      [`(case-lambda [,idss . ,bodys] ...)
       (define lams (for/list ([ids (in-list idss)]
                               [body (in-list bodys)])
                      (compile-expr `(lambda ,ids . ,body) env stack-depth stk-i tail? mutated)))
       (define mask (for/fold ([mask 0]) ([lam (in-list lams)])
                      (bitwise-ior mask (interp-match lam [#(lambda ,mask) mask]))))
       (list->vector (list* 'case-lambda mask (extract-procedure-wrap-data e) lams))]
      [`(let ([,ids ,rhss] ...) . ,body)
       (define len (length ids))
       (define body-env
         (for/fold ([env env]) ([id (in-list ids)]
                                [i (in-naturals)])
           (define u (unwrap id))
           (define pos (+ stack-depth i))
           (hash-set env u (if (hash-ref mutated u #f) (boxed pos) pos))))
       (define body-stack-depth (+ stack-depth len))
       (define c-body (compile-body body body-env body-stack-depth stk-i tail? mutated))
       (define new-body (add-boxes/remove-unused c-body ids mutated body-env stk-i))
       (define pos (stack->pos stack-depth stk-i #:nonuse? #t))
       (stack-info-forget! stk-i stack-depth pos len)
       (define new-rhss (compile-list rhss env stack-depth stk-i #f mutated))
       (or
        ;; Merge nested `let`s into a `let*` to reduce vector nesting
        (cond
          [(null? new-rhss) new-body]
          [(vector? new-body)
           (interp-match
            new-body
            [#(let ,pos2 ,rhss2 ,b)
             (vector 'let* (list pos pos2) (list (list->vector new-rhss) rhss2) b)]
            [#(let* ,poss ,rhsss ,b)
             (vector 'let* (cons pos poss) (cons (list->vector new-rhss) rhsss) b)]
            [#(clear ,poss ,e)
             ;; Check check the `let`-bounding bindings are immediately cleared,
             ;; in which case they're unused
             (let loop ([pos pos] [poss poss] [rhss new-rhss])
               (cond
                 [(null? rhss)
                  ;; bindings are unused
                  (let ([e (if (null? poss)
                               e
                               (vector 'clear poss e))])
                    ;; Use `beginl` instead of `begin` to encourage further collapsing
                    (vector 'beginl (append new-rhss (begins->list e))))]
                 [(null? poss) #f]
                 [(eqv? pos (car poss))
                  (loop (add1 pos) (cdr poss) (cdr rhss))]
                 [else #f]))]
            [#() #f])]
          [else #f])
        (vector 'let pos (list->vector new-rhss) new-body))]
      [`(letrec . ,_) (compile-letrec e env stack-depth stk-i tail? mutated)]
      [`(letrec* . ,_) (compile-letrec e env stack-depth stk-i tail? mutated)]
      [`(begin . ,vs)
       (compile-body vs env stack-depth stk-i tail? mutated)]
      [`(begin-unsafe . ,vs)
       (compile-body vs env stack-depth stk-i tail? mutated)]
      [`(begin0 ,e)
       (compile-expr e env stack-depth stk-i tail? mutated)]
      [`(begin0 ,e . ,vs)
       (define new-body (compile-body vs env stack-depth stk-i #f mutated))
       (vector 'begin0
               (compile-expr e env stack-depth stk-i #f mutated)
               new-body)]
      [`($value ,e)
       (vector '$value (compile-expr e env stack-depth stk-i #f mutated))]
      [`(if ,tst ,thn ,els)
       (define then-stk-i (stack-info-branch stk-i))
       (define else-stk-i (stack-info-branch stk-i))
       (define new-then (compile-expr thn env stack-depth then-stk-i tail? mutated))
       (define new-else (compile-expr els env stack-depth else-stk-i tail? mutated))
       (define all-clear (stack-info-merge! stk-i (list then-stk-i else-stk-i)))
       (vector 'if
               (compile-expr tst env stack-depth stk-i #f mutated)
               (add-clears new-then then-stk-i all-clear)
               (add-clears new-else else-stk-i all-clear))]
      [`(with-continuation-mark* ,mode ,key ,val ,body)
       (define new-body (compile-expr body env stack-depth stk-i tail? mutated))
       (define new-val (compile-expr val env stack-depth stk-i #f mutated))
       (vector 'wcm
               (compile-expr key env stack-depth stk-i #f mutated)
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
       (compile-assignment id rhs env stack-depth stk-i mutated)]
      [`(define ,id ,rhs)
       (compile-assignment id rhs env stack-depth stk-i mutated)]
      [`(define-values ,ids ,rhs)
       (define gen-ids (for/list ([id (in-list ids)])
                         (deterministic-gensym (unwrap id))))
       (compile-expr `(call-with-values (lambda () ,rhs)
                        (lambda ,gen-ids
                          ,@(if (null? ids)
                                (list (void))
                                (for/list ([id (in-list ids)]
                                           [gen-id (in-list gen-ids)])
                                  `(set! ,id ,gen-id)))))
                     env
                     stack-depth
                     stk-i
                     tail?
                     mutated)]
      [`(call-with-values ,proc1 (lambda ,ids . ,body))
       (compile-expr `(call-with-values ,proc1 (case-lambda
                                                 [,ids . ,body]))
                     env
                     stack-depth
                     stk-i
                     tail?
                     mutated)]
      [`(call-with-values (lambda () . ,body) (case-lambda [,idss . ,bodys] ...))
       (define body-stk-is (for/list ([body (in-list bodys)])
                             (stack-info-branch stk-i)))
       (define initial-new-clauses
         (for/list ([ids (in-list idss)]
                    [body (in-list bodys)]
                    [body-stk-i (in-list body-stk-is)])
           (define-values (new-env count rest?)
             (args->env ids env stack-depth mutated))
           (define new-stack-depth (+ stack-depth count))
           (define c-body (compile-body body new-env new-stack-depth body-stk-i tail? mutated))
           (define new-body (add-boxes/remove-unused c-body ids mutated new-env body-stk-i))
           (define pos (stack->pos stack-depth body-stk-i #:nonuse? #t))
           (stack-info-forget! body-stk-i stack-depth pos count)
           (vector (count->mask count rest?)
                   new-body)))
       (define all-clear (stack-info-merge! stk-i body-stk-is))
       (vector 'cwv
               (compile-body body env stack-depth stk-i #f mutated)
               (stack->pos stack-depth stk-i #:nonuse? #t)
               (match e
                 [`(,_ ,_ ,receiver) (wrap-property receiver 'inferred-name)])
               (for/list ([initial-new-clause (in-list initial-new-clauses)]
                          [body-stk-i (in-list body-stk-is)])
                 (define body (vector-ref initial-new-clause 1))
                 (vector (vector-ref initial-new-clause 0)
                         (add-clears body body-stk-i all-clear))))]
      [`(call-with-module-prompt (lambda () . ,body))
       (vector 'cwmp0 (compile-body body env stack-depth stk-i tail? mutated))]
      [`(call-with-module-prompt (lambda () . ,body) ',ids ',constances ,vars ...)
       (vector 'cwmp
               (compile-body body env stack-depth stk-i tail? mutated)
               ids
               constances
               (compile-list vars env stack-depth stk-i #f mutated))]
      [`(variable-set! ,dest-id ,e)
       (define dest-var (hash-ref env (unwrap dest-id)))
       (define new-expr (compile-expr e env stack-depth stk-i #f mutated))
       (vector 'set-variable!
               (stack->pos dest-var stk-i)
               new-expr
               #f
               #f)]
      [`(variable-set!/define ,dest-id ,e ',constance)
       (define dest-var (hash-ref env (unwrap dest-id)))
       (define new-expr (compile-expr e env stack-depth stk-i #f mutated))
       (vector 'set-variable!
               (stack->pos dest-var stk-i)
               new-expr
               constance
               #t)]
      [`(variable-ref ,id)
       (define var (hash-ref env (unwrap id)))
       (vector 'ref-variable/checked (stack->pos var stk-i))]
      [`(variable-ref/no-check ,id)
       (define var (hash-ref env (unwrap id)))
       (vector 'ref-variable (stack->pos var stk-i))]
      [`(#%app ,_ ...) (compile-apply (wrap-cdr e) env stack-depth stk-i tail? mutated)]
      [`(#%app/value ,_ ...) (compile-apply (wrap-cdr e) env stack-depth stk-i tail? mutated)]
      [`(#%app/no-return ,_ ...) (compile-apply (wrap-cdr e) env stack-depth stk-i tail? mutated)]
      [`(,rator ,_ ...)  (compile-apply e env stack-depth stk-i tail? mutated)]
      [`,id
       (define u (unwrap id))
       (define var (hash-ref env u #f))
       (cond
         [(not var)
          (cond
            [(number? u) (vector 'quote u)]
            [(and (symbol? u) (not serializable?) (hash-ref primitives u #f))
             => (lambda (v)
                  (cond
                    [(procedure? v) v]
                    [else (vector 'quote v)]))]
            [else u])]
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

  (define (compile-letrec e env stack-depth stk-i tail? mutated)
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
       (define c-body (compile-body body body-env body-stack-depth stk-i tail? mutated))
       (define new-rhss (list->vector
                         (compile-list rhss rhs-env body-stack-depth stk-i #f mutated)))
       (define new-body (add-boxes/remove-unused c-body ids #hasheq() body-env stk-i))
       (define pos (stack->pos stack-depth stk-i #:nonuse? #t))
       (stack-info-forget! stk-i stack-depth pos count)
       (vector 'letrec pos new-rhss new-body)]))

  (define (compile-apply es env stack-depth stk-i tail? mutated)
    (unless tail?
      (stack-info-non-tail! stk-i stack-depth))
    (define new-es (compile-list es env stack-depth stk-i #f mutated))
    (list->vector (cons 'app new-es)))

  (define (compile-assignment id rhs env stack-depth stk-i mutated)
    (define compiled-rhs (compile-expr rhs env stack-depth stk-i #f mutated))
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
      [else (error 'compile "unexpected set! ~s -> ~v" u var)]))

  (define (extract-expr-mutated e mutated)
    (match e
      [`(lambda ,ids . ,body)
       (extract-list-mutated body mutated)]
      [`(case-lambda [,idss . ,bodys] ...)
       (for/fold ([mutated mutated]) ([body (in-list bodys)])
         (extract-list-mutated body mutated))]
      [`(let ([,ids ,rhss] ...) . ,body)
       (extract-list-mutated body (extract-list-mutated rhss mutated))]
      [`(letrec ([,ids ,rhss] ...) . ,body)
       (extract-list-mutated body (extract-list-mutated rhss mutated))]
      [`(letrec* ([,ids ,rhss] ...) . ,body)
       (extract-list-mutated body (extract-list-mutated rhss mutated))]
      [`(begin . ,vs)
       (extract-list-mutated vs mutated)]
      [`(begin0 ,vs)
       (extract-list-mutated vs mutated)]
      [`(begin-unsafe ,vs)
       (extract-list-mutated vs mutated)]
      [`($value ,e)
       (extract-expr-mutated e mutated)]
      [`(if ,tst ,thn ,els)
       (define tst-mutated (extract-expr-mutated tst mutated))
       (define thn-mutated (extract-expr-mutated thn tst-mutated))
       (extract-expr-mutated els thn-mutated)]
      [`(with-continuation-mark* ,mode ,key ,val ,body)
       (define key-mutated (extract-expr-mutated key mutated))
       (define val-mutated (extract-expr-mutated val key-mutated))
       (extract-expr-mutated body val-mutated)]
      [`(quote ,v)
       mutated]
      [`(set! ,id ,rhs)
       (define new-mutated (hash-set mutated (unwrap id) #t))
       (extract-expr-mutated rhs new-mutated)]
      [`(define ,id ,rhs)
       (extract-expr-mutated rhs mutated)]
      [`(define-values ,ids ,rhs)
       (extract-expr-mutated rhs mutated)]
      [`(variable-set! ,dest-id ,e)
       (extract-expr-mutated e mutated)]
      [`(variable-set!/define ,dest-id ,e ',constance)
       (extract-expr-mutated e mutated)]
      [`(variable-ref ,id)
       mutated]
      [`(variable-ref/no-check ,id)
       mutated]
      [`(#%app ,es ...)
       (extract-list-mutated es mutated)]
      [`(#%app/value ,es ...)
       (extract-list-mutated es mutated)]
      [`(#%app/no-return ,es ...)
       (extract-list-mutated es mutated)]
      [`(,es ...)
       (extract-list-mutated es mutated)]
      [`,id
       mutated]))

  (define (extract-list-mutated body mutated)
    (let loop ([body body] [mutated mutated])
      (cond
        [(null? body) mutated]
        [else
         (loop (wrap-cdr body)
               (extract-expr-mutated (wrap-car body) mutated))])))

  (define (args->env ids env stack-depth mutated)
    (let loop ([ids ids] [env env] [count 0])
      (cond
        [(wrap-null? ids) (values env count #f)]
        [(wrap-pair? ids) (loop (wrap-cdr ids)
                                (env-set env (unwrap (wrap-car ids)) (+ stack-depth count) mutated)
                                (add1 count))]
        [else
         (values (env-set env (unwrap ids) (+ stack-depth count) mutated)
                 (add1 count)
                 #t)])))

  (define (env-set env u pos mutated)
    (hash-set env u (if (hash-ref mutated u #f)
                        (boxed pos)
                        pos)))

  (define (add-clears e stk-i all-clear)
    (cond
      [(stack-info-branch-need-clears? stk-i)
       (define local-use-map (stack-info-local-use-map stk-i))
       (define clears
         (for/list ([pos (in-hash-keys all-clear)]
                    #:unless (hash-ref local-use-map pos #f))
           pos))
       (cond
         [(null? clears) e]
         [else (vector 'clear (sort clears <) e)])]
      [else e]))

  (define (add-boxes/remove-unused e ids mutated env stk-i)
    (cond
      [(null? ids) e]
      [(pair? ids)
       (add-boxes/remove-unused (add-boxes/remove-unused e (car ids) mutated env stk-i)
                                (cdr ids)
                                mutated
                                env
                                stk-i)]
      [else
       (define u (unwrap ids))
       (define var (hash-ref env u #f))
       (define pos (stack->pos (if (boxed? var) (boxed-pos var) var) stk-i)) ; box result means unused
       (cond
         [(box? pos)
          (cond
            [(and (vector? e) (eq? 'clear (vector-ref e 0)))
             (vector 'clear (cons (unbox pos) (vector-ref e 1)) (vector-ref e 2))]
            [else
             (vector 'clear (list (unbox pos)) e)])]
         [(not (hash-ref mutated u #f))
          e]
         [else
          (vector 'enbox pos e)])]))

  (define (extract-procedure-wrap-data e)
    ;; Get name and method-arity information
    (define encoded-name (wrap-property e 'inferred-name))
    (define name
      (cond
        [(eq? encoded-name '|[|) #f]
        [(symbol? encoded-name)
         (define s (symbol->immutable-string encoded-name))
         (cond
           [(fx= 0 (string-length s)) encoded-name]
           [else
            (define ch (string-ref s 0))
            (cond
              [(or (char=? #\[ ch)
                   (char=? #\] ch))
               (string->symbol (substring s 1 (string-length s)))]
              [else encoded-name])])]
        [else encoded-name]))
    (if (wrap-property e 'method-arity-error)
        (box name)
        name))

  (define (begins->list e)
    ;; Convert an expression to a list of expressions, trying to
    ;; flatten `begin`s.
    (cond
      [(vector? e)
       (interp-match
        e
        [#(beginl ,es) es]
        [#(begin)
         (define len (sub1 (vector*-length e)))
         (cond
           [(len . < . 4)
            (let loop ([i 1])
              (cond
                [(= i len)
                 (begins->list (vector*-ref e i))]
                [else (cons (vector*-ref e i)
                            (loop (add1 i)))]))]
           [else (list e)])]
        [#() (list e)])]
      [else (list e)]))

  (with-deterministic-gensym
    (start linklet-e)))

;; ----------------------------------------

(define (interpret-linklet b)
  (interp-match
   b
   [#(,num-body-vars ,b)
    (lambda args
      (define start-stack empty-stack)
      (define args-stack (for/fold ([stack start-stack]) ([arg (in-list args)]
                                                          [i (in-naturals 0)])
                           (stack-set stack i arg)))
      (define post-args-pos (stack-count args-stack))
      (define stack (for/fold ([stack args-stack]) ([i (in-range num-body-vars)])
                      (stack-set stack (+ i post-args-pos) (box unsafe-undefined))))
      (interpret-expr b stack))]))

(define (interpret-expr b stack)

  ;; An updated "stack" must be returned when bindings are removed
  ;; from the stack on their last uses (where there is a non-tail call
  ;; after the last use). But that stack is not needed by a caller if
  ;; we're in tail position with respect to the start of interpreting.
  ;; That case is when `return-mode` is #f. Meanwhile, if we're in a
  ;; `with-continuation-mark`, we need a little trampoline to make a
  ;; call to an unknown function (that might use marks) while also
  ;; keeping track of the latest stack; that case is when
  ;; `return-mode` is a hash table, and returning 'trampoline
  ;; for the stack triggers the trampoline to ship marks to be
  ;; around the call within `values` to return the actual stack.
  (define (interpret b stack [return-mode 'values])
    (cond
      [(integer? b) (stack-ref stack b (not return-mode))]
      [(box? b) (stack-ref stack b (not return-mode))]
      [(pair? b)
       (define-values (new-stack vec) (stack-ref stack (car b)))
       (define val (vector*-ref vec (cdr b)))
       (if return-mode
           (values new-stack val)
           val)]
      [(symbol? b)
       (define val (hash-ref primitives b))
       (if return-mode
           (values stack val)
           val)]
      [(vector? b)
       (interp-match
        b
        [#(app ,rator-b)
         (define len (vector*-length b))
         (define-values (rand-stack rator) (interpret rator-b stack))
         (define-syntax-rule (return-stack stack app)
           (if (eq? return-mode 'values)
               (call-with-values
                (lambda () app)
                (case-lambda
                  [(v) (values stack v)]
                  [vs (apply values stack vs)]))
               (let ([marks return-mode])
                 (values
                  'trampoline
                  (lambda ()
                    (call-with-values
                     (lambda ()
                       (let loop ([i (hash-iterate-first marks)])
                         (cond
                           [(not i) app]
                           [else
                            (define-values (k v) (hash-iterate-key+value marks i))
                            (with-continuation-mark
                             k v
                             (loop (hash-iterate-next marks i)))])))
                     (case-lambda
                       [(v) (values stack v)]
                       [vs (apply values stack vs)])))))))
         (cond
           [(eq? len 2)
            (if return-mode
                (return-stack rand-stack (rator))
                (rator))]
           [(eq? len 3)
            (define-values (stack rand) (interpret (vector*-ref b 2) rand-stack))
            (if return-mode
                (return-stack stack (rator rand))
                (rator rand))]
           [(eq? len 4)
            (define-values (stack1 rand1) (interpret (vector*-ref b 2) rand-stack))
            (define-values (stack2 rand2) (interpret (vector*-ref b 3) stack1))
            (if return-mode
                (return-stack stack2 (rator rand1 rand2))
                (rator rand1 rand2))]
           [else
            (define-values (stack rev-rands)
              (for/fold ([stack rand-stack] [rev-rands null]) ([b (in-vector b 2)])
                (define-values (new-stack v) (interpret b stack))
                (values new-stack (cons v rev-rands))))
            (define rands (reverse rev-rands))
            (if return-mode
                (return-stack stack (apply rator rands))
                (apply rator rands))])]
        [#(quote ,v)
         (if return-mode
             (values stack v)
             v)]
        [#(unbox ,s)
         (define-values (new-stack bx) (stack-ref stack s))
         (define val (unbox* bx))
         (if return-mode
             (values new-stack val)
             val)]
        [#(unbox/checked ,s ,name)
         (define-values (new-stack bx) (stack-ref stack s))
         (define v (unbox* bx))
         (define val (check-not-unsafe-undefined v name))
         (if return-mode
             (values new-stack val)
             val)]
        [#(ref-variable ,s)
         (define-values (new-stack var) (stack-ref stack s))
         (define val (variable-ref/no-check var))
         (if return-mode
             (values new-stack val)
             val)]
        [#(ref-variable/checked ,s)
         (define-values (new-stack var) (stack-ref stack s))
         (define val (variable-ref var))
         (if return-mode
             (values new-stack val)
             val)]
        [#(let ,pos ,rhss ,b)
         (define len (vector*-length rhss))
         (define body-stack
           (let loop ([i 0] [stack stack])
             (cond
               [(fx= i len) stack]
               [else
                (define-values (new-stack val) (interpret (vector*-ref rhss i) stack))
                (stack-set (loop (fx+ i 1) new-stack) (fx+ i pos) val)])))
         (interpret b body-stack return-mode)]
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
                  (loop (fx+ i 1) (stack-set new-stack (fx+ i pos) val))]))))
         (interpret b body-stack return-mode)]
        [#(letrec ,pos ,rhss ,b)
         (define len (vector*-length rhss))
         (define-values (body-stack boxes)
           (let loop ([stack stack] [i 0])
             (cond
               [(= i len)
                (values stack null)]
               [else
                (define bx (box unsafe-undefined))
                (define-values (new-stack boxes)
                  (loop (stack-set stack (fx+ (fx- len i 1) pos) bx)
                        (add1 i)))
                (values new-stack (cons bx boxes))])))
         (let loop ([i 0] [stack body-stack] [boxes boxes])
           (cond
             [(fx= i len)
              (interpret b stack return-mode)]
             [else
              (define-values (new-stack val) (interpret (vector*-ref rhss i) stack))
              (set-box! (car boxes) val)
              (loop (fx+ i 1) new-stack (cdr boxes))]))]
        [#(begin)
         (define last (fx- (vector*-length b) 1))
         (let loop ([i 1] [stack stack])
           (cond
             [(fx= i last)
              (interpret (vector*-ref b i) stack return-mode)]
             [else
              (call-with-values
               (lambda () (interpret (vector*-ref b i) stack))
               (case-lambda
                 [(new-stack val) (loop (fx+ i 1) new-stack)]
                 [(new-stack . vals) (loop (fx+ i 1) new-stack)]))]))]
        [#(beginl ,bs)
         (let loop ([bs bs] [stack stack])
           (cond
             [(null? (cdr bs))
              (interpret (car bs) stack return-mode)]
             [else
              (call-with-values
               (lambda () (interpret (car bs) stack))
               (case-lambda
                 [(new-stack val) (loop (cdr bs) new-stack)]
                 [(new-stack . vals) (loop (cdr bs) new-stack)]))]))]
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
                  (if return-mode
                      (apply values new-stack vals)
                      (apply values vals))
                  (loop (fx+ i 1) new-stack)))))]
        [#($value ,e)
         (let-values ([(new-stack v) (interpret e stack)])
           (if return-mode
               (values new-stack v)
               v))]
        [#(clear ,clears ,e)
         (let loop ([clears clears] [stack stack])
           (cond
             [(null? clears)
              (interpret e stack return-mode)]
             [else
              (loop (cdr clears) (stack-remove stack (car clears)))]))]
        [#(enbox ,pos ,e)
         (define new-stack (stack-set stack pos (box (stack-ref stack pos #t))))
         (interpret e new-stack return-mode)]
        [#(if ,tst ,thn ,els)
         (define-values (new-stack val) (interpret tst stack))
         (if val
             (interpret thn new-stack return-mode)
             (interpret els new-stack return-mode))]
        [#(wcm ,key ,val ,body)
         (define-values (k-stack k-val) (interpret key stack))
         (define-values (v-stack v-val) (interpret val k-stack))
         (cond
           [(not return-mode)
            ;; In tail position, so we can just use
            ;; with-continuation-mark` directly:
            (with-continuation-mark
             k-val
             v-val
             (interpret body v-stack #f))]
           [(eq? return-mode 'values)
            ;; Not in tail position with respect to a `with-continuation-mark`.
            ;; Build a trampoline so that we can get an updated stack, but a function
            ;; can be called in tail position with respect to marks
            ((call-with-values
              (lambda ()
                (with-continuation-mark
                 k-val v-val
                 (interpret body v-stack (hasheq k-val v-val))))
              (case-lambda
                [(stack v) (if (eq? stack 'trampoline)
                               ;; trampoline return:
                               v
                               ;; normal return:
                               (lambda () (values stack v)))]
                [(stack . vs) (lambda () (apply values stack vs))])))]
           [else
            ;; In tail position with respect to a `with-continuation-mark`,
            ;; so take advantage of its `return-mode` trampoline:
            (with-continuation-mark
             k-val v-val
             (interpret body v-stack (hash-set return-mode k-val v-val)))])]
        [#(cwv ,b ,pos ,name ,clauses)
         (define-values (new-stack vs)
           (call-with-values
            (lambda () (interpret b stack))
            (lambda (stack . vals) (values stack vals))))
         (define len (length vs))
         (let loop ([clauses clauses] [full-mask 0])
           (cond
             [(null? clauses) (apply raise-arity-mask-error (or name '|#<procedure>|) full-mask vs)]
             [else
              (interp-match
               (car clauses)
               [#(,mask ,b)
                (if (matching-argument-count? mask len)
                    (interpret b (push-stack new-stack pos vs mask) return-mode)
                    (loop (cdr clauses) (fxior mask full-mask)))])]))]
        [#(cwmp0 ,b)
         (when return-mode (error 'interpret "expect call-with-module-prompt in tail position"))
         ((hash-ref primitives 'call-with-module-prompt)
          (lambda () (interpret b stack #f)))]
        [#(cwmp ,b ,ids ,constances ,var-es)
         (when return-mode (error 'interpret "expect call-with-module-prompt in tail position"))
         (apply (hash-ref primitives 'call-with-module-prompt)
                (lambda () (interpret b stack #f))
                ids
                constances
                (for/list ([e (in-list var-es)])
                  (interpret e stack #f)))]
        [#(lambda ,mask ,wrap-data ,close-vec ,_)
         (define-values (new-stack captured) (capture-closure close-vec stack))
         (define val
           (make-interp-procedure*
            (lambda args
              (apply-function b captured args))
            mask
            wrap-data))
         (if return-mode
             (values new-stack val)
             val)]
        [#(case-lambda ,mask ,wrap-data)
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
                   [#(lambda ,mask ,_ ,close-vec) (capture-closure close-vec rest-stack)]))
                (values new-stack (cons captured rest-captureds))])))
         (define val
           (make-interp-procedure*
            (lambda args
              (define len (length args))
              (let loop ([i 3] [captureds captureds] [full-mask 0])
                (cond
                  [(fx= i n)
                   ;; We shouldn't get here, because the wrapper should enforce arity,
                   ;; but just in case:
                   (apply raise-arity-mask-error '|#<procedure>| full-mask args)]
                  [else
                   (define one-b (vector*-ref b i))
                   (interp-match
                    one-b
                    [#(lambda ,mask)
                     (if (matching-argument-count? mask len)
                         (apply-function one-b (car captureds) args)
                         (loop (fx+ i 1) (cdr captureds) (fxior full-mask mask)))])])))
            mask
            wrap-data))
         (if return-mode
             (values new-stack val)
             val)]
        [#(set-variable! ,s ,b ,c ,defn?)
         (define-values (var-stack var) (stack-ref stack s))
         (define-values (val-stack val) (interpret b var-stack))
         (if defn?
             (variable-set!/define var val c)
             (variable-set! var val))
         (if return-mode
             (values val-stack (void))
             (void))]
        [#(set!-indirect ,s ,e ,b)
         (define-values (vec-stack vec) (stack-ref stack s))
         (define-values (val-stack val) (interpret b vec-stack))
         (vector*-set! vec e val)
         (if return-mode
             (values val-stack (void))
             (void))]
        [#(set!-boxed ,s ,b ,name)
         (define-values (bx-stack bx) (stack-ref stack s))
         (define-values (v-stack v) (interpret b bx-stack))
         (set-box*! bx v)
         (if return-mode
             (values v-stack (void))
             (void))]
        [#(set!-boxed/checked ,s ,b ,name)
         (define-values (bx-stack bx) (stack-ref stack s))
         (define-values (v-stack v) (interpret b bx-stack))
         (check-not-unsafe-undefined/assign (unbox* bx) name)
         (set-box*! bx v)
         (if return-mode
             (values v-stack (void))
             (void))])]
      [else (if return-mode
                (values stack b)
                b)]))

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
      (interpret b (push-stack captured 0 args mask) #f)]))

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
            (interpret e stack #f)]
           [else
            (interpret e stack #f)
            (loop (add1 i))]))]
      [#()
       (interpret b stack #f)])]
    [else
     (interpret b stack #f)]))

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
                           'continuation-mark-set-first continuation-mark-set-first
                           'gensym gensym
                           'apply apply
                           'make-weak-box make-weak-box
                           'void void))
  (struct var ([val #:mutable]) #:transparent)
  (interpreter-link! primitives
                     values
                     var-val var-val
                     (lambda (b v) (set-var-val! b v)) (lambda (b v c) (set-var-val! b v))
                     (lambda (proc mask name) proc))
  (define b
    (interpretable-jitified-linklet '(let* ([s "string"])
                                       (lambda (x two-box)
                                         (define other 5)
                                         (begin
                                           (define f (lambda (y)
                                                       (let ([z y])
                                                         (vector x z))))
                                           (define g (case-lambda
                                                       [() (let ([unused (g)])
                                                             (let ([also-unused (g)])
                                                               (begin
                                                                 (list (g no)))))]
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
                                         (variable-set!/define two-box two 'constant)
                                         (letrec ([ok 'ok])
                                           (set! other (call-with-values (lambda () (values 71 (begin0 88 ok)))
                                                         (lambda (v q) (list q v))))
                                           (with-continuation-mark*
                                            general
                                            'x 'cm/x
                                            (list (if s s #f) x ok other
                                                  (f 'vec) (g 'also-vec 'more)
                                                  one two (variable-ref two-box)
                                                  (continuation-mark-set-first #f 'x 'no))))))
                                    #f))
  (pretty-print b)
  (define l (interpret-linklet b null))
  (l 'the-x (var #f)))
