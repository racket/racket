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
              (let* ([rd  -read]
                     [rds -read-syntax]
                     [w1  -wrapper1]
                     [w1-extra? (and w1 (procedure-arity-includes? w1 2))]
                     [w1r (if w1-extra? (lambda (t) (w1 t #f)) w1)]
                     [w1s (if w1-extra? (lambda (t) (w1 t #t)) w1)]
                     [w2  (or -wrapper2 (lambda (in r) (r in)))])
                (values
                 (lambda (in modpath line col pos)
                   (w2 in (lambda (in)
                            (wrap-internal 'lib in rd w1r modpath #f
                                           line col pos))))
                 (lambda (src in modpath line col pos)
                   (w2 in (lambda (in)
                            (wrap-internal 'lib in (lambda (in) (rds src in))
                                           w1s modpath src
                                           line col pos)))))))))))]))

(define (wrap-internal lib port read wrapper modpath src line col pos)
  (let* ([body (lambda ()
                 (let loop ([a null])
                   (let ([v (read port)])
                     (if (eof-object? v) (reverse a) (loop (cons v a))))))]
         [body (if wrapper (wrapper body) (body))]
         [p-name (object-name port)]
         [name (if (path? p-name)
                 (let-values ([(base name dir?) (split-path p-name)])
                   (string->symbol
                    (path->string (path-replace-suffix name #""))))
                 'page)]
         [tag-src (lambda (v)
                    (if (syntax? modpath)
                      (datum->syntax #f v
                                     (vector src line col pos
                                             (- (or (syntax-position modpath)
                                                    (add1 pos))
                                                pos)))
                      v))]
         [lib-src (lambda (v)
                    (if (syntax? modpath)
                      (datum->syntax #f lib modpath modpath)
                      v))])
    `(,(tag-src 'module) ,(tag-src name) ,(lib-src lib) . ,body)))

(define (wrap lib port read modpath src line col pos)
  (wrap-internal lib port read #f modpath src line col pos))

)
