#lang racket/base
(require (for-syntax racket/base)
         "wrap.rkt")

;; One more time, still yet another pattern matching library again...
(provide match)

(define-for-syntax (extract-pattern-variables pattern)
  (syntax-case pattern (unquote ?)
    [(unquote (? pred?))
     null]
    [(unquote bind-id)
     (if (free-identifier=? #'bind-id #'_)
         null
         (list #'bind-id))]
    [(p1 . p2) (append (extract-pattern-variables #'p1)
                       (extract-pattern-variables #'p2))]
    [else null]))

(define-for-syntax (check-one id pattern head-id)
  (define (check-one/expr e pattern)
    (syntax-case pattern (unquote)
      [(unquote bind-id) #`#t]
      [_ #`(let ([a #,e])
             #,(check-one #'a pattern #f))]))
  (syntax-case pattern (unquote ?)
    [(unquote (? pred?))
     #`(pred? #,id)]
    [(unquote bind-id) #`#t]
    [(pat ellipses)
     (and (identifier? #'ellipses)
          (free-identifier=? #'ellipses (quote-syntax ...)))
     (if (syntax-case #'pat (unquote)
           [(unquote bind-id) #t]
           [_ #f])
         #`(wrap-list? #,id)
         #`(and (wrap-list? #,id)
                (for/and ([v (in-wrap-list #,id)])
                  #,(check-one #'v #'pat #f))))]
    [(m-id . p2)
     (and head-id (identifier? #'m-id))
     #`(and (eq? 'm-id #,head-id)
            #,(check-one/expr #`(cdr (unwrap #,id)) #'p2))]
    [(p1 . p2)
     #`(let ([p (unwrap #,id)])
         (and (pair? p)
              #,(check-one/expr #'(car p) #'p1)
              #,(check-one/expr #'(cdr p) #'p2)))]
    [_
     (if (or (identifier? pattern)
             (let ([v (syntax-e pattern)])
               (or (keyword? v)
                   (boolean? v)
                   (null? v))))
         #`(wrap-eq? (quote #,pattern) #,id)
         #`(wrap-equal? (quote #,pattern) #,id))]))

(define-for-syntax (extract-one id pattern)
  (syntax-case pattern (unquote ?)
    [(unquote (? pred?))
     #`(values)]
    [(unquote bind-id)
     (if (free-identifier=? #'bind-id #'_)
         #'(values)
         id)]
    [(pat ellipses)
     (and (identifier? #'ellipses)
          (free-identifier=? #'ellipses (quote-syntax ...)))
     (syntax-case #'pat (unquote)
       [(unquote bind-id)
        (if (free-identifier=? #'bind-id #'_)
            #'(values)
            #`(unwrap-list #,id))]
       [_
        (with-syntax ([pat-ids (extract-pattern-variables #'pat)])
          #`(for/lists pat-ids ([v (in-wrap-list #,id)])
              #,(extract-one #'v #'pat)))])]
    [(p1 . p2)
     (let ([ids1 (extract-pattern-variables #'p1)]
           [ids2 (extract-pattern-variables #'p2)])
       (cond
         [(and (null? ids1) (null? ids2))
          #'(values)]
         [(null? ids1)
          #`(let ([d (cdr (unwrap #,id))])
              #,(extract-one #'d #'p2))]
         [(null? ids2)
          #`(let ([a (car (unwrap #,id))])
              #,(extract-one #'a #'p1))]
         [else
          #`(let ([p (unwrap #,id)])
              (let-values ([#,ids1 (let ([a (car p)])
                                     #,(extract-one #'a #'p1))]
                           [#,ids2 (let ([d (cdr p)])
                                     #,(extract-one #'d #'p2))])
                (values #,@ids1 #,@ids2)))]))]
    [_
     #'(values)]))

(define-for-syntax (extract-guard body)
  (syntax-case body ()
    [(#:guard guard-expr . body)
     #'guard-expr]
    [_ #f]))

(define-for-syntax (remove-guard body)
  (syntax-case body ()
    [(#:guard guard-expr . body)
     #'body]
    [_ body]))

(define-syntax (match stx)
  (syntax-case stx (quasiquote)
    [(_ expr [`pattern body0 body ...] ...)
     #`(let ([v expr])
         #,(let ([patterns (syntax->list #'(pattern ...))])
             (define (build-matches head-id)
               (let loop ([patterns patterns]
                          [bodys (syntax->list #'((body0 body ...) ...))])
                 (cond
                   [(null? patterns)
                    #'(error 'match "failed ~e" v)]
                   [else
                    (define ids (extract-pattern-variables (car patterns)))
                    (define match? (check-one #'v (car patterns) head-id))
                    (define guard (extract-guard (car bodys)))
                    #`(if #,(if guard
                                #`(and #,match? #,guard)
                                match?)
                          (let-values ([#,ids #,(extract-one #'v (car patterns))])
                            . #,(remove-guard (car bodys)))
                          #,(loop (cdr patterns) (cdr bodys)))])))
             ;; If the first pattern is `(<id> ....)`, then
             ;; extract the input head symbol, because we're
             ;; likely to want to check it for many pattern cases
             (syntax-case (and (pair? patterns) (car patterns)) ()
               [(id . _)
                (identifier? #'id)
                #`(let ([hd (let ([p (unwrap v)])
                              (and (pair? p) (unwrap (car p))))])
                    #,(build-matches #'hd))]
               [_ (build-matches #f)])))]))
