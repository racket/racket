
;; See copy in "expander.sls"
(define-syntax begin0
  (syntax-rules (void)
    [(_ expr0) expr0]
    [(_ expr0 expr ...)
     (call-with-values (lambda ()
                         (call-with-values (lambda () expr0)
                           (case-lambda
                            [(x) (values x #f)]
                            [args (values args #t)])))
       (lambda (l apply?)
         expr ...
         (if apply?
             (#%apply values l)
             l)))]))

