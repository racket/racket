#lang racket/base
(require racket/match
         racket/set
         compiler/zo-structs
         compiler/faslable-correlated
         "remap.rkt"
         "import.rkt"
         "merged.rkt")

;; Prune unnused definitions,
;;  * soundly, with a simple approximation of `pure?`, by default
;;  * unsoundly, assuming all definitions are pure, optionally

(provide gc-find-uses!
         gc-definitions)

(define (gc-find-uses! used  ; symbol -> 'used or thunk
                       used-externally ; symbol -> #t
                       phase-merged
                       provided-names
                       #:keep-defines? keep-defines?
                       #:assume-pure? assume-pure?)
  (for ([(root-phase mgd) (in-hash phase-merged)])
    (define body (merged-body mgd))
    (define defined-names (merged-defined-names mgd))
    
    (define (used-name! name)
      (define v (hash-ref used name #f))
      (hash-set! used name 'used)
      (unless (hash-ref defined-names name #f)
        (hash-set! used-externally name #t))
      (when (procedure? v)
        (v)))

    (for ([name (in-list (hash-ref provided-names root-phase null))])
      (used-name! name)
      (hash-set! used-externally name #t))

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
           [`(begin0 ,e . ,body)
            (used! e)
            (for-each used! body)]
           [`(set! ,id ,rhs)
            (used-name! id)
            (used! rhs)]
           [`(quote . _) (void)]
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
                      (or assume-pure?
                          (pure? rhs)))
           (used-rhs!))]
        [_ (unless (pure? b)
             (used! b))]))))

(define (gc-definitions used phase-merged)
  ;; Anything not marked as used at this point can be dropped
  (for/hasheqv ([(root-phase mgd) (in-hash phase-merged)])
    (define body (merged-body mgd))
     
    (define new-defined-names (make-hasheq))
    
    (define new-body
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
                          [_ (not (pure? b))]))
        b))

    (values root-phase (struct-copy merged mgd
                                    [body new-body]
                                    [defined-names new-defined-names]))))

(define (pure? b)
  (match b
    [`(lambda . ,_) #t]
    [`(case-lambda . ,_) #t]
    [`(quote . ,_) #t]
    [`(let-values ([,idss ,rhss] ...) ,body)
     (and (andmap pure? rhss)
          (pure? body))]
    [`(#%variable-reference . ,_) #t]
    [`(void) #t]
    [_ (not (or (pair? b)
                (symbol? b)))]))


