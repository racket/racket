#lang racket/base
(require racket/match
         racket/set
         compiler/zo-structs
         compiler/faslable-correlated
         "remap.rkt"
         "import.rkt"
         "merged.rkt"
         "name.rkt"
         "remap.rkt"
         "binding-lookup.rkt"
         "path-submod.rkt")

;; Prune unnused definitions,
;;  * soundly, with a simple approximation of `pure?`, by default
;;  * unsoundly, assuming all definitions are pure, optionally

(provide gc-find-uses!
         gc-definitions)

(define (gc-find-uses! used  ; symbol -> 'used or thunk
                       used-externally ; symbol -> #t
                       phase-merged
                       provided-names
                       stx-vec
                       names transformer-names
                       one-mods
                       excluded-module-mpis included-module-phases
                       #:keep-defines? keep-defines?
                       #:prune-definitions? prune-definitions?)

  (define (used-name-at-defined-names! name defined-names)
    (define v (hash-ref used name #f))
    (hash-set! used name 'used)
    (unless (and defined-names (hash-ref defined-names name #f))
      (hash-set! used-externally name #t))
    (when (procedure? v)
      (v)))

  (define (used-name-externally! name)
    (used-name-at-defined-names! name #f))

  (for* ([(phase provided) (in-hash provided-names)]
         [name (in-list provided)])
    (used-name-externally! name))

  ;; This traversal is relatively slow, since we extract a list of interned
  ;; scope symbols and phases and then loop over that list
  (let loop ([stx stx-vec])
    (cond
      [(identifier? stx)
       (for* ([phase (in-list (syntax-bound-phases stx))]
              [space-sym (in-list (cons #f (syntax-bound-interned-scope-symbols stx phase)))])
         (define intro (if space-sym
                           (make-interned-syntax-introducer space-sym)
                           (lambda (s mode) s)))
         (define b (identifier-binding (intro stx 'add) phase))
         (when (list? b)
           (define mpi (car b))
           (define path/submod (resolved-module-path->path/submod (module-path-index-resolve mpi)))
           (define sym (cadr b))
           (define phase (list-ref b 4))
           (define-values (new-name at-phase)
             (binding-lookup path/submod phase sym
                             names transformer-names
                             one-mods
                             excluded-module-mpis included-module-phases))
           (used-name-externally! new-name)))]
      [(syntax? stx) (loop (syntax-e stx))]
      [(pair? stx) (loop (car stx)) (loop (cdr stx))]
      [(vector? stx) (for ([e (in-vector stx)])
                       (loop e))]
      [(hash? stx) (for ([e (in-hash-values stx)])
                     (loop e))]
      [(prefab-struct-key stx) (loop (struct->vector stx))]
      [(box? stx) (loop (unbox stx))]
      [else (void)]))

  (for ([(root-phase mgd) (in-hash phase-merged)])
    (define body (merged-body mgd))
    (define defined-names (merged-defined-names mgd))

    (define (used-name! name)
      (used-name-at-defined-names! name defined-names))

    (define (used! b)
      (cond
        [(faslable-correlated? b)
         (used! (faslable-correlated-e b))]
        [else
         (match b
           [`(lambda ,args . ,body)
            (for-each used! body)]
           [`(case-lambda [,argss . ,bodys] ...)
            (for ([body (in-list bodys)])
              (for-each used! body))]
           [`(let-values ([,idss ,rhss] ...) ,body)
            (for-each used! rhss)
            (used! body)]
           [`(letrec-values ([,idss ,rhss] ...) ,body)
            (for-each used! rhss)
            (used! body)]
           [`(if ,tst ,thn ,els)
            (used! tst)
            (used! thn)
            (used! els)]
           [`(begin . ,body)
            (for-each used! body)]
           [`(begin-unsafe . ,body)
            (for-each used! body)]
           [`(begin0 ,e . ,body)
            (used! e)
            (for-each used! body)]
           [`(set! ,id ,rhs)
            ;; don't count this as a use of `id`; we'll drop the
            ;; assignment if `id` ends up unused; furthermore, if
            ;; `id` is not yet used and rhs is an identifier, then
            ;; attach rhs as delayed, because that enables GC of
            ;; loop-tied definitions
            (define u (hash-ref used id #f))
            (cond
              [(and (procedure? u) (or (symbol? rhs)
                                       (pure? rhs)))
               (hash-set! used id (lambda ()
                                    (u)
                                    (used! rhs)))]
              [else
               (used! rhs)])]
           [`(quote . ,_) (void)]
           [`(with-continuation-mark ,key ,val ,body)
            (used! key)
            (used! val)
            (used! body)]
           [`(#%variable-reference ,id)
            (used-name! id)]
           [`(#%variable-reference . ,_) (void)]
           [`(,rator ,rands ...)
            (used! rator)
            (for-each used! rands)]
           [_
            (when (symbol? b)
              (used-name! b))])]))

    (for ([b (in-list body)])
      (match b
        [`(define-values ,ids ,rhs)
         (define done? #f)
         (define (used-rhs!)
           (unless done?
             (set! done? #t)
             (used! rhs))
           ;; All in group are used together:
           (for-each used! ids))
         (for ([id (in-list ids)])
           (cond
             [(eq? 'used (hash-ref used id #f))
              (used-rhs!)]
             [else
              (hash-set! used id used-rhs!)]))
         (unless (and (not keep-defines?)
                      (or prune-definitions?
                          (pure? rhs)))
           (used-rhs!))]
        [_
         (cond
           [(transformer-definition-name b)
            => (lambda (name)
                 (define (used-trans!) (used! b))
                 (if (hash-ref used name #f)
                     (used-trans!)
                     (hash-set! used name used-trans!)))]
           [(pure? b) (void)]
           [else (used! b)])]))))

(define (gc-definitions used phase-merged)
  ;; Anything not marked as used at this point can be dropped
  (for/hasheqv ([(root-phase mgd) (in-hash phase-merged)])
    (define body (merged-body mgd))

    (define defined-names (merged-defined-names mgd))
    (define new-defined-names (make-hasheq))
    
    (define pruned-body
      ;; Drop unused definitions
      (for/list ([b (in-list body)]
                 #:when (match b
                          [`(define-values ,ids ,rhs)
                           (define keep?
                             (for/or ([id (in-list ids)])
                               (eq? 'used (hash-ref used id #f))))
                            (when keep?
                              (for ([id (in-list ids)])
                                (hash-set! new-defined-names id #t)))
                            keep?]
                          [_
                           (cond
                             [(transformer-definition-name b)
                              => (lambda (name)
                                   (eq? 'used (hash-ref used name #f)))]
                             [else (not (pure? b))])]))
                                 
        b))

    ;; drop assignments to unused definitions
    (define new-body
      (remap-names pruned-body
                   (lambda (id) id)
                   #:set!-keep (lambda (id rhs)
                                 (cond
                                   [(or (not (hash-ref defined-names id #f))
                                        (eq? (hash-ref used id #f) 'used))
                                    'keep]
                                   [else
                                    (if (symbol? rhs) #f 'rhs-only)]))))
    
    (values root-phase (struct-copy merged mgd
                                    [body new-body]
                                    [defined-names new-defined-names]))))

(define (pure? b)
  (match b
    [`(lambda . ,_) #t]
    [`(case-lambda . ,_) #t]
    [`(begin ,b) (pure? b)]
    [`(begin-unsafe ,b) (pure? b)]
    [`(quote . ,_) #t]
    [`(let-values ([,idss ,rhss] ...) ,body)
     (and (andmap pure? rhss)
          (pure? body))]
    [`(#%variable-reference . ,_) #t]
    [`(void) #t]
    [_ (not (or (pair? b)
                (symbol? b)))]))

(define (transformer-definition-name b)
  (match b
    [`(let-values ([(,id) ,rhs])
        (begin
          (.set-transformer! ',name ,id-use)
          (void)))
     (and (eq? id id-use)
          (match rhs
            [`(make-rename-transformer . ,_)
             ;; `identifier-binding` tells us whether to keep the target
             ;; of the rename; we don't know that target here, and we don't
             ;; know whether this name is used directly anyway; it's ok
             ;; to just keep it
             #f]
            [_ #t])
          name)]
    [_ #f]))
