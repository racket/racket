#lang racket/base
(require racket/match
         racket/set
         compiler/faslable-correlated
         "merged.rkt")

(provide simplify-linklet)

;; Simplifying is an optimizaiton pass that is aimed at enabling
;; definition pruning. In particular, `(variable-reference-constant?
;; (#%variable-reference id))` is resolved to a boolean when `id`
;; refers to a ready defined variable, and that tends to enable
;; removal of unused keyword-function layers. Along similar lines,
;; loop-tying function calls are inlined.

;; Making sure that a defined variable is ready is the tricky part. To
;; handle the case that a keyword-argument function is defined below
;; its use, we have to do the usual recognition of struct definitions
;; and abstract delayed function-call flows.

(struct assigner (lhss))

(define (simplify-linklet phase-merged)
  (for/hasheqv ([(root-phase mgd) (in-hash phase-merged)])
    (define body (merged-body mgd))
    (define defined-names (merged-defined-names mgd))

    (define defined-ready (make-hasheq)) ; sym -> 'ready, 'constructor, or procedure
    (define mutated (make-hasheq))

    (define (unwrap rhs)
      (if (faslable-correlated? rhs)
          (faslable-correlated-e rhs)
          rhs))

    ;; Traversal to determine potentially mutated definitions
    ;; ------------------------------------------------------
    
    (define (mutation-traversal b)
      (let loop ([b b])
        (cond
          [(faslable-correlated? b)
           (loop (faslable-correlated-e b))]
          [else
           (match b
             [`(define-values ,ids ,rhs)
              (loop rhs)
              (for ([id (in-list ids)])
                (unless (eq? 'constructor (hash-ref defined-ready id #f))
                  (hash-set! defined-ready id #t)))]
             [`(lambda ,args ,body)
              (loop body)]
             [`(case-lambda [,argss ,bodys] ...)
              (for ([body (in-list bodys)])
                (loop body))]
             [`(let-values ([,idss ,rhss] ...) ,body)
              (for ([rhs (in-list rhss)])
                (loop rhs))
              (loop body)]
             [`(letrec-values ([,idss ,rhss] ...) ,body)
              (for ([rhs (in-list rhss)])
                (loop rhs))
              (loop body)]
             [`(if ,tst ,thn ,els)
              (loop tst)
              (loop thn)
              (loop els)]
             [`(begin . ,body)
              (for-each loop body)]
             [`(begin-unsafe . ,body)
              (for-each loop body)]
             [`(begin0 ,e . ,body)
              (loop e)
              (for-each loop body)]
             [`(set! ,id ,rhs)
              ;; we could get some cross pollution from local names that
              ;; are in distinct scopes, but it's unlikely enough that we
              ;; don't bother keeping track
              (hash-set! mutated id #t)
              (loop rhs)]
             [`(quote . ,_) (void)]
             [`(#%foreign-inline . ,_) (void)]
             [`(with-continuation-mark ,key ,val ,body)
              (loop key)
              (loop val)
              (loop body)]
             [`(#%variable-reference ,id)
              (loop id)]
             [`(#%variable-reference . ,_)
              (void)]
             [`(,rator ,rands ...)
              (loop rator)
              (for-each loop rands)]
             [_
              (cond
                [(and (symbol? b)
                      (hash-ref defined-names b #f))
                 (define r (hash-ref defined-ready b #f))
                 (unless r
                   (hash-set! mutated b #t))
                 (when (procedure? r)
                   (hash-set! defined-ready b #t)
                   (r))]
                [else
                 (void)])])])))

    (define checking #f)
    
    (define (immediate? rhs)
      (let ([rhs (unwrap rhs)])
        (match rhs
          [`(quote ,_) #t]
          [`(#%foreign-inline ,_ ,mode) (memq mode '(copy pure))]
          [`(lambda . ,_) #t]
          [`(case-lambda ., _) #t]
          [`(let-values ([,ids ,rhs] ...)
              ,body)
           (and (for/and ([rhs (in-list rhs)])
                  (immediate? rhs))
                (immediate? body))]
          [`(,rator ,args ...)
           (or
            (and (or (memq rator
                           ;; primitives that don't immediately call any
                           ;; function that they are given:
                           '(unsafe-make-struct-type-property/guard-calls-no-arguments
                             make-struct-type
                             make-struct-field-accessor
                             make-struct-field-mutator
                             values
                             list
                             cons
                             current-inspector
                             check-inspector))
                     (and (eq? rator 'make-struct-type-property)
                          (or ((length args) . <= . 1)
                              (and (not (cadr args))
                                   ((length args) . <= . 2))))
                     (eq? (hash-ref defined-ready rator #f) 'constructor))
                 (for/and ([arg (in-list args)])
                   (immediate? arg)))
            #f)]
          [_ (or (not (symbol? rhs))
                 (not (hash-ref defined-names rhs #f))
                 (hash-ref defined-ready rhs #f))])))

    (for ([b (in-list body)])
       (let loop ([b b])
         (cond
           [(faslable-correlated? b)
            (loop (faslable-correlated-e b))]
           [(match b
              [`(define-values ,ids ,rhs)
               (set! checking ids)
               (immediate? rhs)]
              [_ #f])
            ;; definition where we can treat the identifers as ready early
            (match b
              [`(define-values ,ids ,_)
               (define traversed? #f)
               (define (traverse!)
                 (unless traversed?
                   (set! traversed? #t)
                   (mutation-traversal b)))
               (for ([id (in-list ids)])
                 (hash-set! defined-ready id traverse!))
               ;; recognize that a constructor doesn't call its arguments:
               (match b
                 [`(define-values (,struct: ,make ,pred ,ref ,set) (make-struct-type ,args ...))
                  (when (or ((length args) . < . 10)
                            (not (list-ref args 9))) ; no guard procedure
                    (hash-set! defined-ready make 'constructor))]
                 [_ (void)])])]
           [else
            (mutation-traversal b)])))

    ;; Detect loop-tying assigners
    ;; ---------------------------
    
    ;; Detect functions that look like loop-tying functions,
    ;; because they take N arguments and assignment them to N
    ;; defined variables. We'll inline these.

    (define assigners (make-hasheq))
    (for ([b (in-list body)])
      (match b
        [`(define-values (,id) ,rhs)
         (let ([rhs (unwrap rhs)])
           (match rhs
             [`(lambda (,arg ...)
                 (begin
                   (set! ,lhs ,rhs)
                   ...))
              (define mapping
                (for/fold ([mapping #hasheq()]) ([lhs (in-list lhs)]
                                                 [rhs (in-list rhs)])
                  (and mapping
                       (memq rhs arg)
                       (hash-ref defined-names lhs #f)
                       (hash-set mapping rhs lhs))))
              (when (and mapping
                         (= (hash-count mapping) (length rhs)))
                (hash-set! assigners id (assigner (for/list ([arg (in-list arg)])
                                                    (hash-ref mapping arg #f)))))]
             [_ (void)]))]
        [_ (void)]))

    ;; Update linklet body based on gathers information
    ;; ------------------------------------------------
    
    (define new-body
      (for/list ([b (in-list body)])
        (let env-loop ([b b] [env #hasheq()])
          (define (loop b) (env-loop b env))
          (cond
            [(faslable-correlated? b)
             (struct-copy faslable-correlated b
                          [e (loop (faslable-correlated-e b))])]
            [else
             (match b
               [`(define-values ,ids ,rhs)
                `(define-values ,ids ,(loop rhs))]
               [`(lambda ,args ,body)
                `(lambda ,args ,(loop body))]
               [`(case-lambda [,argss ,bodys] ...)
                `(case-lambda ,@(for/list ([args (in-list argss)]
                                           [body (in-list bodys)])
                                  `[,args ,(loop body)]))]
               [`(let-values ([,idss ,rhss] ...) ,body)
                ;; Sometimes, a name that we'd like to drop is
                ;; referenced in an unreachable branch, but through
                ;; a let-binding indirection. Perform copy propagation
                ;; to push the reference into branches.
                (define (copy-propagate? ids rhs)
                  (and (pair? ids)
                       (null? (cdr ids))
                       (not (hash-ref mutated (car ids) #f))
                       (symbol? rhs)
                       (hash-ref defined-names rhs #f)
                       (not (hash-ref mutated rhs #f))))
                (define new-env
                  (for/fold ([env env]) ([ids (in-list idss)]
                                         [rhs (in-list rhss)])
                    (cond
                      [(copy-propagate? ids rhs)
                       ;; copy propagation:
                       (hash-set env (car ids) rhs)]
                      [else env])))
                `(let-values ,(for/list ([ids (in-list idss)]
                                         [rhs (in-list rhss)]
                                         #:unless (copy-propagate? ids rhs))
                                `[,ids ,(loop rhs)])
                   ,(env-loop body new-env))]
               [`(letrec-values ([,idss ,rhss] ...) ,body)
                `(letrec-values ,(for/list ([ids (in-list idss)]
                                            [rhs (in-list rhss)])
                                   `[,ids ,(loop rhs)])
                   ,(loop body))]
               [`(if ,tst ,thn ,els)
                (define new-tst (loop tst))
                (match new-tst
                  ['#t (loop thn)]
                  ['#f (loop els)]
                  [else
                   `(if ,new-tst ,(loop thn) ,(loop els))])]
               [`(begin . ,body)
                `(begin ,@(map loop body))]
               [`(begin-unsafe . ,body)
                `(begin-unsafe ,@(map loop body))]
               [`(begin0 ,e . ,body)
                `(begin0 ,(loop e) ,@(map loop body))]
               [`(set! ,id ,rhs)
                `(set! ,id ,(loop rhs))]
               [`(quote . ,_) b]
               [`(#%foreign-inline . ,_) b]
               [`(with-continuation-mark ,key ,val ,body)
                `(with-continuation-mark ,(loop key) ,(loop val) ,(loop body))]
               [`(#%variable-reference ,id) b]
               [`(#%variable-reference . ,_) b]
               [`(variable-reference-constant? (#%variable-reference ,id))
                (cond
                  [(and (hash-ref defined-names id #f)
                        (not (hash-ref mutated id #f)))
                   #t]
                  [else
                   `(variable-reference-constant? (#%variable-reference ,(loop id)))])]
               [`(,rator ,rands ...)
                (define a (hash-ref assigners rator #f))
                (cond
                  [(and a (= (length rands) (length (assigner-lhss a))))
                   `(begin
                      ,@(for/list ([rand (in-list rands)]
                                   [lhs (in-list (assigner-lhss a))])
                          (if lhs
                              `(set! ,lhs ,(loop rand))
                              (loop rand)))
                      (void))]
                  [else
                   `(,(loop rator) ,@(map loop rands))])]
               [_
                (cond
                  [(and (symbol? b)
                        (hash-ref env b #f))
                   => (lambda (new-b) new-b)]
                  [else b])])]))))

    (values root-phase
            (struct-copy merged mgd
                         [body new-body]))))
