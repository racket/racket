(module persistent-close mzscheme
  (require-for-template mzscheme)
  (require-for-syntax (lib "kerncase.ss" "syntax"))
  (require "file-vector.ss")
  (provide close/file)
  
  (define-for-syntax (index-of id ids)
    (let loop ([idx 0] [ids ids])
      (cond
        [(null? ids) #f]
        [(bound-identifier=? id (car ids)) idx]
        [else (loop (add1 idx) (cdr ids))])))
    
  ;; replace/fvector-refs: id (listof id) expr -> expr
  ;; replace uses of id with appropriate file-vector refs
  (define-for-syntax (replace/fvector-refs fvec-id ids expr)
    (kernel-syntax-case expr #t
      [(lambda formals body-exprs ...)
       #`(lambda formals
           #,@(map
               (lambda (body-expr)
                 (replace/fvector-refs fvec-id ids body-expr))
               (syntax->list #'(body-exprs ...))))]
      [(case-lambda (formals bodiess ...) ...)
       #`(case-lambda
           #,@(map
               (lambda (formal bodies)
                 (with-syntax ([(bodies ...) bodies])
                   #`(formal #,@(map
                                 (lambda (body)
                                   (replace/fvector-refs fvec-id ids body))
                                 (syntax->list #'(bodies ...))))))
               (syntax->list #'(formals ...))
               (syntax->list #'((bodiess ...) ...))))]
      [(if tst csq)
       #`(if #,(replace/fvector-refs fvec-id ids #'tst)
             #,(replace/fvector-refs fvec-id ids #'csq))]
      [(if tst csq alt)
       #`(if #,(replace/fvector-refs fvec-id ids #'tst)
             #,(replace/fvector-refs fvec-id ids #'csq)
             #,(replace/fvector-refs fvec-id ids #'alt))]
      [(begin exprs ...)
       #`(begin #,@(map
                    (lambda (expr)
                      (replace/fvector-refs fvec-id ids expr))
                    (syntax->list #'(exprs ...))))]
      [(begin0 expr0 exprs ...)
       #`(begin0 #,(replace/fvector-refs fvec-id ids #'expr0)
                 #,@(map
                     (lambda (expr)
                       (replace/fvector-refs fvec-id ids expr))
                     (syntax->list #'(exprs ...))))]
      [(let-values (((varss ...) rhss) ...) exprs ...)
       #`(let-values (#,(map
                         (lambda (vars rhs)
                           #`[#,vars #,(replace/fvector-refs fvec-id ids rhs)])
                         (syntax->list #'((varss ...) ...))
                         (syntax->list #'(rhss ...))))
           #,@(map
               (lambda (expr)
                 (replace/fvector-refs fvec-id ids expr)
                 (syntax->list #'(exprs ...)))))]
      [(letrec-values (((varss ...) rhss) ...) exprs ...)
       #`(letrec-values (#,(map
                            (lambda (vars rhs)
                              #`[#,vars #,(replace/fvector-refs fvec-id ids rhs)])
                            (syntax->list #'((varss ...) ...))
                            (syntax->list #'(rhss ...))))
           #,@(map
               (lambda (expr)
                 (replace/fvector-refs fvec-id ids expr))
               (syntax->list #'(exprs ...))))]
      [(set! var rhs)
       (cond
         [(index-of #'var ids)
          => (lambda (idx)
               #`(file-vector-set! #,fvec-id #,idx #,(replace/fvector-refs fvec-id ids #'rhs)))]
         [else
          #`(set! var (replace/fvector-refs fvec-id ids #'rhs))])]
      [(quote datum) expr]
      [(quote-syntax datum) expr]
      [(with-continuation-mark key val body)
       #`(with-continuation-mark #,(replace/fvector-refs fvec-id ids #'key)
           #,(replace/fvector-refs fvec-id ids #'val)
           #,(replace/fvector-refs fvec-id ids #'body))]
      [(#%app exprs ...)
       #`(#%app #,@(map
                    (lambda (expr)
                      (replace/fvector-refs fvec-id ids expr))
                    (syntax->list #'(exprs ...))))]
      [(#%datum . datum) expr]
      [(#%top . variable) expr]
      [var
       (cond
         [(index-of #'var ids)
          => (lambda (idx)
               #`(file-vector-ref #,fvec-id #,idx))]
         [else #'var])]))
  
  ;; (replace vector-expr (identifier ...) body-expr)
  ;; body-expr should be fully expanded.
  (define-syntax (replace stx)
    (syntax-case stx ()
      [(_ fvec-expr (ids ...) body-exprs ...)
       (with-syntax ([fvec (datum->syntax-object #'_ 'fvec)])
         #`(let ([fvec fvec-expr])
             #,@(map
                 (lambda (body-expr)
                   (replace/fvector-refs #'fvec (syntax->list #'(ids ...)) body-expr))
                 (syntax->list #'(body-exprs ...)))))]
      [_else
       (raise-syntax-error #f "replace: bad syntax" stx)]))
  
  (define-syntax (close/file stx)
    (syntax-case stx ()
      [(_ file-tag (ids ...) body-exprs ...)
       (syntax-case (local-expand #'(let-values ([(ids ...) (values ids ...)]) body-exprs ...)
                                  'expression '()) (#%app)
         [(let-values ([(ids ...) (#%app values ref-vals ...)]) new-body-exprs ...)
          #'(replace (make-file-vector file-tag ref-vals ...) (ids ...) new-body-exprs ...)])]
      [_else
       (raise-syntax-error #f "close/file: bad syntax" stx)])))

