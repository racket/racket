(module module-reader scheme/base

(provide (rename-out [provide-module-reader #%module-begin]
                     [wrap wrap-read-all])
         (except-out (all-from-out scheme/base) #%module-begin))

(require (for-syntax scheme/base))

(define-syntax (provide-module-reader stx)
  (syntax-case stx ()
    [(_ lib body ...)
     (let ([-read        #f]
           [-read-syntax #f]
           [-wrapper1    #f]
           [-wrapper2    #f])
       (define -body
         (let loop ([body #'(body ...)])
           (define (err str)
             (raise-syntax-error 'syntax/module-reader str
                                 (car (syntax->list body))))
           (syntax-case body ()
             [(#:read r body ...)
              (if -read
                (err "got two #:read keywords")
                (begin (set! -read #'r) (loop #'(body ...))))]
             [(#:read-syntax r body ...)
              (if -read-syntax
                (err "got two #:read-syntax keywords")
                (begin (set! -read-syntax #'r) (loop #'(body ...))))]
             [(#:wrapper1 w body ...)
              (if -wrapper1
                (err "got two #:wrapper1 keywords")
                (begin (set! -wrapper1 #'w) (loop #'(body ...))))]
             [(#:wrapper2 w body ...)
              (if -wrapper2
                (err "got two #:wrapper2 keywords")
                (begin (set! -wrapper2 #'w) (loop #'(body ...))))]
             [(k . b) (keyword? (syntax-e #'k))
              (err "got an unknown keyword")]
             [_ body])))
       (with-syntax ([-read        (or -read        #'read)]
                     [-read-syntax (or -read-syntax #'read-syntax)]
                     [-wrapper1    (or -wrapper1    #'#f)]
                     [-wrapper2    (or -wrapper2    #'#f)]
                     [(body ...)   -body])
         (syntax/loc stx
           (#%module-begin
            body ...
            (#%provide (rename *read read) (rename *read-syntax read-syntax))
            (define-values (*read *read-syntax)
              (let ([rd  -read]
                    [rds -read-syntax]
                    [w1  -wrapper1]
                    [w2  (let ([w -wrapper2])
                           (cond [(not w) (lambda (in r _) (r in))]
                                 [(procedure-arity-includes? w 3) w]
                                 [else (lambda (in r _) (w in r))]))])
                (values
                 (lambda (in modpath line col pos)
                   (w2 in
                       (lambda (in)
                         (wrap-internal 'lib in rd w1 #f modpath #f
                                        line col pos))
                       #f))
                 (lambda (src in modpath line col pos)
                   (w2 in
                       (lambda (in)
                         (wrap-internal 'lib in (lambda (in) (rds src in))
                                        w1 #t modpath src
                                        line col pos))
                       #t)))))))))]))

(define (wrap-internal lib port read wrapper stx? modpath src line col pos)
  (let* ([body (lambda ()
                 (let loop ([a null])
                   (let ([v (read port)])
                     (if (eof-object? v) (reverse a) (loop (cons v a))))))]
         [body (cond [(not wrapper) (body)]
                     [(procedure-arity-includes? wrapper 2) (wrapper body stx?)]
                     [else (wrapper body)])]
         [p-name (object-name port)]
         [name (if (path? p-name)
                 (let-values ([(base name dir?) (split-path p-name)])
                   (string->symbol
                    (path->string (path-replace-suffix name #""))))
                 'page)]
         [tag-src (lambda (v)
                    (if stx?
                      (datum->syntax
                       #f v (vector src line col pos
                                    (- (or (syntax-position modpath) (add1 pos))
                                       pos)))
                      v))]
         [lib (if stx? (datum->syntax #f lib modpath modpath) lib)]
         [r `(,(tag-src 'module) ,(tag-src name) ,lib . ,body)])
    (if stx? (datum->syntax #f r) r)))

(define (wrap lib port read modpath src line col pos)
  (wrap-internal lib port read #f #f modpath src line col pos))

)
