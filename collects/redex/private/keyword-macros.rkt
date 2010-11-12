#lang racket

(require (for-template racket/base racket/contract))

(define (parse-kw-args formals actuals source form-name)
  (let loop ([current (for/hash ([arg formals]) (values (car arg) #f))]
             [rest actuals])
    (syntax-case rest ()
      [() (map (λ (arg) 
                 (match (hash-ref current (car arg))
                   [#f (cadr arg)]
                   [x (match (cdr (cdr arg))
                        ['() x]
                        [`((,ctc ,desc))
                         (apply-contract ctc x desc form-name)])]))
               formals)]
      [(kw . rest)
       (not (keyword? (syntax-e (syntax kw))))
       (raise-syntax-error #f "expected a keyword" source (syntax kw))]
      [(kw arg . rest)
       (keyword? (syntax-e (syntax arg)))
       (raise-syntax-error #f "expected an argument expression" source (syntax arg))]
      [(kw arg . rest)
       (let ([none (gensym)])
         (eq? none (hash-ref current (syntax-e (syntax kw)) none)))
       (raise-syntax-error #f "invalid keyword" source (syntax kw))]
      [(kw arg . rest)
       (hash-ref current (syntax-e (syntax kw)))
       (raise-syntax-error #f "repeated keyword" source (syntax kw))]
      [(kw)
       (raise-syntax-error #f "missing argument expression after keyword" source (syntax kw))]
      [(kw arg . rest)
       (loop (hash-set current (syntax-e (syntax kw)) (syntax arg))
             (syntax rest))]
      [else (raise-syntax-error #f "bad keyword argument syntax" source rest)])))

(define (apply-contract ctc expr desc form)
  #`(contract #,ctc #,expr
              #,(let ([m (syntax-source-module expr)])
                  (cond [(module-path-index? m)
                         (format "~a" (module-path-index-resolve m))]
                        [(or (symbol? m) (path? m))
                         (format "~a" m)]
                        [else (format "~s client" form)]))
              '#,form #,desc
              #(#,(syntax-source expr)
                #,(syntax-line expr) 
                #,(syntax-column expr) 
                #,(syntax-position expr)
                #,(syntax-span expr))))

(provide parse-kw-args apply-contract)
