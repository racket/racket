
(module stacktrace mzscheme
  (require (lib "unitsig.ss")
           (lib "kerncase.ss" "syntax")
           (lib "stx.ss" "syntax"))
  
  (provide stacktrace@ stacktrace^ stacktrace-imports^)
  
  (define-signature stacktrace-imports^ (calltrace-key print-call-trace))
  (define-signature stacktrace^ (annotate))
  
  (define o (current-output-port))
  (define (oprintf . args) (apply fprintf o args))
  
  (define-struct stx-protector (stx))
  
  (define stacktrace@
    (unit/sig stacktrace^
      (import stacktrace-imports^)
  
      ;; TEMPLATE FUNCTIONS:
      ;;  these functions' definitions follow the data definitions presented in the Syntax
      ;;  chapter of the MzScheme Manual. 
      
      (define (top-level-expr-iterator stx)
        (kernel-syntax-case stx #f
          [(module identifier name (#%plain-module-begin . module-level-exprs))
           #`(module identifier name
               (#%plain-module-begin 
                #,@(map module-level-expr-iterator (syntax->list #'module-level-exprs))))]
          [else-stx
           (general-top-level-expr-iterator stx)]))
      
      (define (module-level-expr-iterator stx)
        (kernel-syntax-case stx #f
          [(provide . provide-specs)
           stx]
          [else-stx
           (general-top-level-expr-iterator stx)]))
      
      (define (general-top-level-expr-iterator stx)
        (kernel-syntax-case stx #f
          [(define-values (var ...) expr)
           (let ([var-list (syntax->list #'(var ...))])
             (cond [(= (length var-list) 1) #`(define-values (var ...) #,(expr-iterator #'expr (car var-list)))]
                   [else #`(define-values (var ...) #,(expr-iterator #'expr #f))]))]
          [(define-syntaxes (var ...) expr)
           #`(define-syntaxes (var ...) #,(expr-iterator #'expr #f))]
          [(begin . top-level-exprs)
           #`(begin #,@(map top-level-expr-iterator (syntax->list #'top-level-exprs)))]
          [(require . require-specs)
           stx]
          [(require-for-syntax . require-specs)
           stx]
          [else
           (expr-iterator stx #f)]))
  
      (define (expr-iterator stx potential-name)
        (let* ([name-guess (or (syntax-property stx 'inferred-name) potential-name)]
               [recur-tail (lambda (expr) (expr-iterator expr name-guess))]
               [recur-non-tail (lambda (expr) (expr-iterator expr #f))]
               [recur-with-name (lambda (expr name) (expr-iterator expr name))]
               [recur-on-sequence (lambda (exprs) 
                                    (let loop ([remaining exprs])
                                      (cond [(null? remaining) null]
                                            [(null? (cdr remaining)) (list (recur-tail (car remaining)))]
                                            [else (cons (recur-non-tail (car remaining))
                                                        (loop (cdr remaining)))])))]
               [lambda-clause-abstraction
                (lambda (clause)
                  (kernel-syntax-case clause #f
                    [(arglist . bodies)
                     (let-values ([(arglist-proper improper?) (arglist-flatten #'arglist)])
                       (if name-guess
                           #`(arglist (with-continuation-mark 
                                          #,calltrace-key
                                        'unimportant
                                        (begin (let ([call-depth (length (continuation-mark-set->list
                                                                          (current-continuation-marks)
                                                                          #,calltrace-key))])
                                                 (#,print-call-trace 
                                                    (quote-syntax #,name-guess)
                                                    #,(syntax-original? name-guess)
                                                    (#,stx-protector-stx #,(make-stx-protector stx))
                                                    (list #,@arglist-proper)
                                                    #,improper?
                                                    call-depth))
                                               #,@(recur-on-sequence (syntax->list #'bodies)))))
                           #`(arglist #,@(recur-on-sequence (syntax->list #'bodies)))))]
                    [else
                     (error 'expr-syntax-object-iterator 
                            "unexpected (case-)lambda clause: ~a" 
                            (syntax-object->datum stx))]))]
               [let-values-abstraction
                (lambda (stx)
                  (kernel-syntax-case stx #f
                    [(kwd (((variable ...) rhs) ...) . bodies)
                     (let* ([clause-fn 
                             (lambda (vars rhs)
                               (let ([var-list (syntax->list vars)])
                                 (cond [(= (length var-list) 1) 
                                        #`(#,vars #,(recur-with-name rhs (car var-list)))]
                                       [else
                                        #`(#,vars #,(recur-non-tail rhs))])))])
                       (with-syntax ([(new-clause ...)
                                      (map clause-fn
                                           (syntax->list #`((variable ...) ...))
                                           (syntax->list #`(rhs ...)))])
                         #`(kwd (new-clause ...) #,@(recur-on-sequence (syntax->list #'bodies)))))]
                    [else
                     (error 'expr-syntax-object-iterator 
                            "unexpected let(rec) expression: ~a"
                            stx
                            ;(syntax-object->datum stx)
                            )]))]) 
          (kernel-syntax-case stx #f
            [var-stx
             (identifier? (syntax var-stx))
             stx]
            [(lambda . clause)
             #`(lambda #,@(lambda-clause-abstraction #'clause))]
            [(case-lambda . clauses)
             #`(case-lambda #,@(map lambda-clause-abstraction (syntax->list #'clauses)))]
            [(if test then)
             #`(if #,(recur-non-tail #'test) #,(recur-tail #'then))]
            [(if test then else)
             #`(if
                #,(recur-non-tail #'test)
                #,(recur-non-tail #'then)
                #,(recur-non-tail #'else))]
            [(begin . bodies)
             #`(begin #,@(recur-on-sequence (syntax->list #'bodies)))]
            [(begin0 . bodies)
             #`(begin #,@(map recur-non-tail #'bodies))]
            [(let-values . _)
             (let-values-abstraction stx)]
            [(letrec-values . _)
             (let-values-abstraction stx)]
            [(set! var val)               
             #`(set! var #,(recur-with-name #'val #'var))]
            [(quote _)
             stx]
            [(quote-syntax _)
             stx]
            [(with-continuation-mark key mark body)
             #`(with-continuation-mark
                #,(recur-non-tail #'key)
                #,(recur-non-tail #'mark)
                #,(recur-tail #'body))]
            [(#%app . exprs)
             #`(#%app #,@(map recur-non-tail (syntax->list #'exprs)))]
            [(#%datum . _)
             stx]
            [(#%top . var)
             stx]
            [else
             (error 'expr-iterator "unknown expr: ~a" 
                    (syntax-object->datum stx))]))) 
      
      (define (arglist-flatten arglist)
        (let loop ([remaining arglist]
                   [so-far null])
        (syntax-case remaining ()
          [()
           (values (reverse so-far) #f)]
          [var
           (identifier? (syntax var))
           (values (reverse (cons #'var so-far)) #t)]
          [(var . rest)
           (loop #'rest (cons #'var so-far))])))
      
      
      (define (annotate x) (top-level-expr-iterator x)))))
