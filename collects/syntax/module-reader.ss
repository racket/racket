(module module-reader scheme/base

(provide (rename-out [provide-module-reader #%module-begin]
                     [wrap wrap-read-all])
         (except-out (all-from-out scheme/base) #%module-begin))

(require (for-syntax scheme/base))

(define-syntax (provide-module-reader stx)
  (syntax-case stx ()
    [(_ lib body ...)
     (let ([key-args '()])
       (define (err str [sub #f])
         (raise-syntax-error 'syntax/module-reader str sub))
       (define -body
         (let loop ([body (syntax->list #'(body ...))])
           (if (not (and (pair? body)
                         (pair? (cdr body))
                         (keyword? (syntax-e (car body)))))
             (datum->syntax stx body stx)
             (let* ([k (car body)] [k* (syntax-e k)] [v (cadr body)])
               (cond
                 [(assq k* key-args) (err (format "got two ~s keywords" k*) k)]
                 [(not (memq k* '(#:read #:read-syntax #:wrapper1 #:wrapper2
                                  #:whole-body-readers?)))
                  (err "got an unknown keyword" (car body))]
                 [else (set! key-args (cons (cons k* v) key-args))
                       (loop (cddr body))])))))
       (define (get kwd [dflt #f])
         (cond [(assq kwd key-args) => cdr] [else dflt]))
       (unless (equal? (and (assq '#:read key-args) #t)
                       (and (assq '#:read-syntax key-args) #t))
         (err "must specify either both #:read and #:read-syntax, or none"))
       (when (and (assq '#:whole-body-readers? key-args)
                  (not (assq '#:read key-args)))
         (err "got a #:whole-body-readers? without #:read and #:read-syntax"))
       (quasisyntax/loc stx
         (#%module-begin
          #,@-body
          (#%provide (rename *read read) (rename *read-syntax read-syntax))
          (define-values (*read *read-syntax)
            (let* ([rd  #,(get '#:read #'read)]
                   [rds #,(get '#:read-syntax #'read-syntax)]
                   [w1  #,(get '#:wrapper1 #'#f)]
                   [w2  #,(get '#:wrapper2 #'#f)]
                   [w2  (cond [(not w2) (lambda (in r _) (r in))]
                              [(procedure-arity-includes? w2 3) w2]
                              [else (lambda (in r _) (w2 in r))])]
                   [base 'lib]
                   [whole? #,(get '#:whole-body-readers? #'#f)])
              (values
               (lambda (in modpath line col pos)
                 (w2 in
                     (lambda (in)
                       (wrap-internal base in rd whole?
                                      w1 #f modpath #f line col pos))
                     #f))
               (lambda (src in modpath line col pos)
                 (w2 in
                     (lambda (in)
                       (wrap-internal
                        base in (lambda (in) (rds src in)) whole?
                        w1 #t modpath src line col pos))
                     #t))))))))]))

(define (wrap-internal lib port read whole? wrapper stx?
                       modpath src line col pos)
  (let* ([body (lambda ()
                 (if whole?
                   (read port)
                   (let loop ([a null])
                     (let ([v (read port)])
                       (if (eof-object? v) (reverse a) (loop (cons v a)))))))]
         [body (cond [(not wrapper) (body)]
                     [(procedure-arity-includes? wrapper 2) (wrapper body stx?)]
                     [else (wrapper body)])]
         [all-loc (vector src line col pos
                          (let-values ([(l c p) (port-next-location port)])
                            (and p (- p pos))))]
         [body (if (and stx? (not (syntax? body)))
                 (datum->syntax #f body all-loc)
                 body)]
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
    (if stx? (datum->syntax #f r all-loc) r)))

(define (wrap lib port read modpath src line col pos)
  (wrap-internal lib port read #f #f #f modpath src line col pos))

)
