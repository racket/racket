(module annotator mzscheme

  (require (prefix kernel: (lib "kerncase.ss" "syntax"))
           (lib "class.ss")
           (lib "list.ss")
           (lib "marks.ss" "mztake" "private")
           (lib "mred.ss" "mred")
           (lib "load-annotator.ss" "mztake" "private")
           (prefix srfi: (lib "search.ss" "srfi" "1"))
           )
  (provide annotate-stx annotate-for-single-stepping)

  (define (arglist-bindings arglist-stx)
    (syntax-case arglist-stx ()
      [var
       (identifier? arglist-stx)
       (list arglist-stx)]
      [(var ...)
       (syntax->list arglist-stx)]
      [(var . others)
       (cons #'var (arglist-bindings #'others))]))




  ;; Retreives the binding of a variable from a normal-breakpoint-info.
  ;; Returns a list of pairs `(,variable-name-stx ,variable-value). Each
  ;; item in the list is a shadowed instance of a variable with the given
  ;; name, with the first item being the one in scope.
  #;
  (define (bindings top-mark marks sym)
    (let ([mark-list (cons top-mark (continuation-mark-set->list marks debug-key))])
      (map (lambda (binding) (list (mark-binding-binding binding)
                                   (mark-binding-value binding)))
           (lookup-all-bindings (lambda (id) (eq? (syntax-e id) sym))
                                mark-list))))



  (define (annotate-for-single-stepping stx break? break-before break-after record-bound-id)
    (annotate-stx
     stx
     (lambda (debug-info annotated raw is-tail?)
       (let* ([start (syntax-position raw)]
              [end (+ start (syntax-span raw) -1)])
         (if is-tail?
             #`(let-values ([(value-list) #f])
                 (if (#,break? #,start)
                     (set! value-list (#,break-before
                                       #,debug-info
                                       (current-continuation-marks))))
                 (if (not value-list)
                     #,annotated
                     (apply values value-list)))
             #`(let-values ([(value-list) #f])
                 (if (#,break? #,start)
                     (set! value-list (#,break-before
                                       #,debug-info
                                       (current-continuation-marks))))
                 (if (not value-list)
                     (call-with-values
                         (lambda () #,annotated)
                       (case-lambda
                         [(val) (if (#,break? #,end)
                                    (#,break-after
                                     #,debug-info
                                     (current-continuation-marks) val)
                                    val)]
                         [vals (if (#,break? #,end)
                                   (apply #,break-after
                                          #,debug-info
                                          (current-continuation-marks) vals)
                                   (apply values vals))]))
                     (if (#,break? #,end)
                         (apply #,break-after
                                #,debug-info
                                (current-continuation-marks) value-list)
                         (apply values value-list)))))))
     record-bound-id))


  ; annotate-stx : (syntax? (syntax? . -> . syntax?)
  ;                 (symbol? syntax? syntax? . -> . void?) . -> . syntax?)
  (define (annotate-stx stx break-wrap record-bound-id)

    (define breakpoints (make-hash-table))
    
    (define (top-level-annotate stx)
      (kernel:kernel-syntax-case
       stx #f
       [(module identifier name (#%plain-module-begin . module-level-exprs))
        (quasisyntax/loc stx (module identifier name
                               (#%plain-module-begin 
                                #,@(map module-level-expr-iterator
                                        (syntax->list #'module-level-exprs)))))]
       [else-stx
        (general-top-level-expr-iterator stx)]))
    
    (define (module-level-expr-iterator stx)
      (kernel:kernel-syntax-case
       stx #f
       [(provide . provide-specs)
        stx]
       [else-stx
        (general-top-level-expr-iterator stx)]))
    
    (define (general-top-level-expr-iterator stx)
      (kernel:kernel-syntax-case
       stx #f
       [(define-values (var ...) expr)
        #`(define-values (var ...)
            #,(annotate #`expr (syntax->list #`(var ...)) #t))]
       [(define-syntaxes (var ...) expr)
        stx]
       [(define-values-for-syntax (var ...) expr)
        #`(define-values-for-syntax (var ...) 
            #,(annotate #`expr #`(syntax->list #'(var ...)) #t))]      
       [(begin . top-level-exprs)
        (quasisyntax/loc stx (begin #,@(map (lambda (expr)
                                              (module-level-expr-iterator expr))
                                            (syntax->list #'top-level-exprs))))]
       [(require . require-specs)
        stx]
       [(require-for-syntax . require-specs)
        stx]
       [(require-for-template dot require-specs) stx]
       [else
        (annotate stx '() #f)]))
    
    (define (annotate expr bound-vars is-tail?)
      
      (define annotate-break?
        (let ([pos (syntax-position expr)]
              [src (syntax-source expr)])
          (and src
               ; (is-a? src object%) ; FIX THIS
               pos
               (hash-table-get breakpoints pos (lambda () #t))
               (kernel:kernel-syntax-case
                expr #f
                [(if test then) #t]
                [(if test then else) #t]
                [(begin . bodies) #t]
                [(begin0 . bodies) #t]
                [(let-values . clause) #t]
                [(letrec-values . clause) #t]
                [(set! var val) #t]
                [(with-continuation-mark key mark body) #t]
                [(#%app . exprs) #t]
                [_ #f])
               (begin
                 (hash-table-put! breakpoints pos #f)
                 (when (not is-tail?)
                   (hash-table-put! breakpoints (+ pos (syntax-span expr) -1) #f))
                 #t))))
      
      (define (let/rec-values-annotator letrec?)
        (kernel:kernel-syntax-case
         expr #f
         [(label (((var ...) rhs) ...) . bodies)
          (let* ([new-bindings (apply append
                                      (map syntax->list
                                           (syntax->list #`((var ...) ...))))]
                 [new-rhs (if letrec?
                              (map (lambda (expr)
                                     (annotate expr (append new-bindings bound-vars) #f))
                                   (syntax->list #'(rhs ...)))
                              (map (lambda (expr) (annotate expr bound-vars #f))
                                   (syntax->list #'(rhs ...))))]
                 [last-body (car (reverse (syntax->list #'bodies)))]
                 [all-but-last-body (reverse (cdr (reverse (syntax->list #'bodies))))]
                 [bodies (append (map (lambda (expr)
                                        (annotate expr
                                                  (append new-bindings bound-vars) #f))
                                      all-but-last-body)
                                 (list (annotate
                                        last-body
                                        (append new-bindings bound-vars) is-tail?)))])
            (for-each (lambda (id) (record-bound-id 'bind id id)) new-bindings)
            (with-syntax ([(new-rhs/trans ...) new-rhs])
              (quasisyntax/loc expr
                               (label (((var ...) new-rhs/trans) ...)
                                      #,@bodies))))]))
      
      (define (lambda-clause-annotator clause)
        (kernel:kernel-syntax-case
         clause #f
         [(arg-list . bodies)
          (let* ([new-bound-vars (arglist-bindings #'arg-list)]
                 [all-bound-vars (append new-bound-vars bound-vars)]
                 [new-bodies (let loop ([bodies (syntax->list #'bodies)])
                               (if (equal? '() (cdr bodies))
                                   (list (annotate (car bodies) all-bound-vars #t))
                                   (cons (annotate (car bodies) all-bound-vars #f)
                                         (loop (cdr bodies)))))])
            (for-each (lambda (id) (record-bound-id 'bind id id)) new-bound-vars)
            (quasisyntax/loc clause
                             (arg-list #,@new-bodies)))]))
      
      (define annotated
        (syntax-recertify
         (kernel:kernel-syntax-case
          expr #f
          [var-stx (identifier? (syntax var-stx))
                   (let ([binder (and (syntax-original? expr)
                                      (srfi:member expr bound-vars module-identifier=?))])
                     (when binder
                       (let ([f (first binder)])
                         (record-bound-id 'ref expr f)))
                     expr)]
          
          [(lambda . clause)
           (quasisyntax/loc expr 
                            (lambda #,@(lambda-clause-annotator #'clause)))]
          
          [(case-lambda . clauses)
           (quasisyntax/loc expr
                            (case-lambda #,@(map lambda-clause-annotator (syntax->list #'clauses))))]
          
          [(if test then)
           (quasisyntax/loc expr (if #,(annotate #'test bound-vars #f)
                                     #,(annotate #'then bound-vars is-tail?)))]
          
          [(if test then else)
           (quasisyntax/loc expr (if #,(annotate #'test bound-vars #f)
                                     #,(annotate #'then bound-vars is-tail?)
                                     #,(annotate #'else bound-vars is-tail?)))]
          
          [(begin . bodies)
           (letrec ([traverse
                     (lambda (lst)
                       (if (and (pair? lst) (equal? '() (cdr lst)))
                           `(,(annotate (car lst) bound-vars is-tail?))
                           (cons (annotate (car lst) bound-vars #f)
                                 (traverse (cdr lst)))))])
             (quasisyntax/loc expr (begin #,@(traverse (syntax->list #'bodies)))))]
          
          [(begin0 . bodies)
           (quasisyntax/loc expr (begin0 #,@(map (lambda (expr)
                                                   (annotate expr bound-vars #f))
                                                 (syntax->list #'bodies))))]
          
          [(let-values . clause)
           (let/rec-values-annotator #f)]
          
          [(letrec-values . clause) 
           (let/rec-values-annotator #t)]

          [(set! var val)
           (let ([binder (and (syntax-original? #'var)
                              (srfi:member #'var bound-vars module-identifier=?))])
             (when binder
               (let ([f (first binder)])
                 (record-bound-id 'set expr f)))
             (quasisyntax/loc expr (set! var #,(annotate #`val bound-vars #f))))]
          
          [(quote _) expr]
          
          [(quote-syntax _) expr]
          
          [(with-continuation-mark key mark body)
           (quasisyntax/loc expr (with-continuation-mark key
                                                         #,(annotate #'mark bound-vars #f)
                                                         #,(annotate #'body bound-vars is-tail?)))]
          
          [(#%app . exprs)
           (let ([subexprs (map (lambda (expr) 
                                  (annotate expr bound-vars #f))
                                (syntax->list #'exprs))])
             (if is-tail?
                 (quasisyntax/loc expr #,subexprs)
                 (wcm-wrap (make-debug-info expr bound-vars bound-vars 'normal #f)
                           (quasisyntax/loc expr #,subexprs))))]
          
          [(#%datum . _) expr]
          
          [(#%top . var) expr]
          
          [else (error 'expr-syntax-object-iterator "unknown expr: ~a"
                       (syntax-object->datum expr))])
         expr
         (current-code-inspector)
         #f))
      
      (if annotate-break?
          (break-wrap
           (make-debug-info expr bound-vars bound-vars 'at-break #f)
           annotated
           expr
           is-tail?)
          annotated))
    
    (values (top-level-annotate stx) (hash-table-map breakpoints (lambda (k v) k))))

  #;
  (define (tests)
    (run/single-stepping-annotation
     (current-custodian) "a.ss"
     (map string->path '("/home/gmarceau/projects/mztake/collects/mztake/a.ss"
                         "/home/gmarceau/projects/mztake/collects/mztake/b.ss"))
     (lambda (fn pos)
       (printf "break?: ~a ~a~n" fn pos) #t)
     (lambda (bp-info) (printf "break: ~a~n" bp-info) #f)))  
)                                                                 