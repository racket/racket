#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     racket/list
                     (only-in syntax/path-spec resolve-path-spec))
         (for-template racket/base))

(provide rewriter/ids
         (for-syntax apply-to syntax-ids))

(define-syntax (rewriter stx)
  (syntax-case stx ()
    [(_ from to vars contexts once? exactly-once?)
     (with-syntax ([ids (syntax-ids #'from (syntax->list #'vars))])
       #'(rewriter/ids from to ids contexts once? exactly-once?))]))

(define-syntax (rewriter/ids stx)
  (syntax-case stx ()
    [(_ from to ids contexts once? exactly-once?)
     (with-syntax* ([to-rw-id #'to-rw]
                    [body-rw-stx (contextual-rewrite-stx #'from #'to #'ids #'contexts #'once? #'exactly-once?)]
                    [module-body-rw (module-body-context-rw-stx #'to-rw-id #'body-rw-stx)])
       #'(λ (stx)
           (syntax-case stx ()
             [(_ to-rw)
              (let ([to-rw-id #'to-rw])
                module-body-rw)])))]))

(define-for-syntax (contextual-rewrite-stx from to ids contexts once? exactly-once?)
  (with-syntax* ([(ctx ...) contexts]
                 [rewrite-here #'rewrite-here]
                 [(loop-id ...) (generate-temporaries contexts)]
                 [(all-loops ...) (append (syntax->list #'(loop-id ...)) (list #'rewrite-here))]
                 [first-loop-id (car (syntax->list #'(all-loops ...)))]
                 [(next ...) (cdr (syntax->list #'(all-loops ...)))])
    #`(λ (stx)
        (define applied? #f)
        (define (loop-id stx)
          (syntax-case stx (ctx)
            [(ctx rest (... ...))
             (with-syntax ([(-rest (... ...)) (next #'(rest (... ...)))])
               #'(ctx -rest (... ...)))]
            [(a . b)
             (with-syntax ([-a (loop-id #'a)]
                           [-b (loop-id #'b)])
               #'(-a . -b))]
            [_ stx])) ...
        (define (rewrite-here stx)
          (syntax-case stx #,ids
            [#,from
             (begin
               (when (or #,once? #,exactly-once?)
                 (when applied? 
                   (raise-syntax-error 'rewrite (format "~a ==> ~a applied a second time"
                                                         '#,from
                                                         '#,to)
                                       stx))
                 (set! applied? #t))
               #'#,to)]
            [(a . b)
             (with-syntax ([-a (rewrite-here #'a)]
                           [-b (rewrite-here #'b)])
               #'(-a . -b))]
            [_
             stx]))
        (begin0 (first-loop-id stx)
                (when (and #,exactly-once? (not applied?))
                  (raise-syntax-error 'rewrite (format "~a ==> ~a was never applied"
                                                         '#,from
                                                         '#,to)
                                       stx))))))

(define-for-syntax (syntax-ids stx vars)
  (syntax-case stx (...)
    [(a . b)
     (append (syntax-ids #'a vars) (syntax-ids #'b vars))]
    [(... ...)
     '()]
    [()
     '()]
    [id
     (identifier? #'id)
     (if (findf (λ (v) (free-identifier=? #'id v)) vars)
         '()
         (list #'id))]
    [_
     '()]))

(define-syntax (relative-path-rewriter-stx stx)
  (syntax-case stx ()
    [(_ mod-directory-path)
     (with-syntax* ([body-rw-stx (rel-path-body-rw-stx #'mod-directory-path)]
                    [full-stx-id #'full-stx]
                    [module-rw-stx (module-body-context-rw-stx #'full-stx-id #'body-rw-stx)])
       #`(λ (stx)
           (syntax-case stx ()
             [(_ full-stx)
              (let ([full-stx-id #'full-stx])
                module-rw-stx)])))]))

(define-for-syntax (rel-path-body-rw-stx mod-directory-path)
  #`(λ (stx)
      (let loop ([stx stx]
                 [in-require? #f])
        (syntax-case stx (require lib planet submod)
          [(require rqs (... ...))
           (with-syntax ([rqs-rw (loop #'(rqs (... ...)) #t)])
             #'(require . rqs-rw))]
          [(planet . rest)
           in-require?
           stx]
          [(lib . rest)
           in-require?
           stx]
          [(submod p . rest)
           (and in-require?
                (or
                 (equal? "." (syntax-e #'p))
                 (equal? ".." (syntax-e #'p))))
           stx]
          [(a . b)
           (with-syntax ([a-rw (loop #'a in-require?)]
                         [b-rw (loop #'b in-require?)])
             #'(a-rw . b-rw))]
          [relative-path
           (and in-require?
                (string? (syntax-e #'relative-path))
                (relative-path? (syntax-e #'relative-path)))
           (let* ([p-string (syntax-e #'relative-path)]
                  [full-path (path->string
                              (simplify-path 
                               (cleanse-path
                                (build-path #,mod-directory-path
                                            (string->path p-string)))))])
             #`(file #,full-path))]
          [_ stx]))))

(define-syntax (path-spec-rewriter stx)
  (syntax-case stx ()
    [(_ path-spec local-mod)
     (let ([file (resolve-path-spec #'path-spec #'path-spec #'path-spec)])
       (with-syntax* ([body-rw-stx (fp->local-body-rw-stx (path->string file) #'local-mod)]
                      [full-stx-id #'full-stx]
                      [module-rw-stx (module-body-context-rw-stx #'full-stx-id #'body-rw-stx)])
         #`(λ (stx)
             (syntax-case stx ()
               [(_ full-stx)
                (let ([full-stx-id #'full-stx])
                  module-rw-stx)]))))]))

(define-syntax (full-path->local-rewriter stx)
  (syntax-case stx ()
    [(_ full-path local-mod)
     (with-syntax* ([body-rw-stx (fp->local-body-rw-stx #'full-path #'local-mod)]
                    [full-stx-id #'full-stx]
                    [module-rw-stx (module-body-context-rw-stx #'full-stx-id #'body-rw-stx)])
       #`(λ (stx)
           (syntax-case stx ()
             [(_ full-stx)
              (let ([full-stx-id #'full-stx])
                module-rw-stx)])))]))

(define-for-syntax (fp->local-body-rw-stx full-path-candidate local-mod)
  #`(λ (stx)
      (let loop ([stx stx]
                 [in-require? #f])
        (syntax-case stx (require lib planet submod file)
          [(require rqs (... ...))
           (with-syntax ([rqs-rw (loop #'(rqs (... ...)) #t)])
             #'(require . rqs-rw))]
          [(planet . rest)
           in-require?
           stx]
          [(lib . rest)
           in-require?
           stx]
          [(submod p . rest)
           (and in-require?
                (or
                 (equal? "." (syntax-e #'p))
                 (equal? ".." (syntax-e #'p))))
           stx]
          [(submod p rest (... ...))
           in-require?
           (with-syntax ([p-rw (loop #'p)])
             (syntax-case #'p-rw (submod)
               [(submod p-inner rest-inner (... ...))
                #'(submod p-inner rest-inner (... ...) rest (... ...))]
               [_
                #'(submod p-rw rest (... ...))]))]
          [(file full-path)
           (and in-require?
                (string? (syntax-e #'full-path))
                (complete-path? (syntax-e #'full-path)))
           ;; intended to rewrite paths already modified by the relative-path pass
           ;; (everything that pass touches will be a complete path), or 
           ;; pre-existing complete paths (shouldn't see anything else here, right?...)
           (if (equal? #,full-path-candidate
                       (syntax-e #'full-path))
               #'#,local-mod
               stx)]
          [(a . b)
           (with-syntax ([a-rw (loop #'a in-require?)]
                         [b-rw (loop #'b in-require?)])
             #'(a-rw . b-rw))]
          [_ stx]))))

(define-for-syntax (module-body-context-rw-stx full-stx body-rw-stx)
  #`(let loop ([stx #,full-stx])
      (syntax-case stx (module)
        [(module . body)
         (with-syntax ([body-rw (#,body-rw-stx #'body)])
           #'(module . body-rw))]
        [(a . b)
         (with-syntax ([a-rw (loop #'a)]
                       [b-rw (loop #'b)])
           #'(a-rw . b-rw))]
        [_
         stx])))


(define-for-syntax (apply-to ids arg)
  (syntax-case ids (rewriter)
    [((rewriter . stuff) . b)
     (with-syntax ([rest (apply-to #'b arg)]
                   [rw-id (syntax-local-lift-expression #'(rewriter . stuff))])
       #'(rw-id rest))]
    [(a . b)
     (with-syntax ([rest (apply-to #'b arg)])
       #'(a rest))]
    [()
     arg]))