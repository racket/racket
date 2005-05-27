(module debugger-annotate mzscheme
  
  (require (prefix kernel: (lib "kerncase.ss" "syntax"))
           "shared.ss"
           "marks.ss"
           (lib "contract.ss"))
  
  (define count 0)
  
  (provide annotate)
  
  (define (arglist-bindings arglist-stx)
    (syntax-case arglist-stx ()
      [var
       (identifier? arglist-stx)
       (list arglist-stx)]
      [(var ...)
       (syntax->list arglist-stx)]
      [(var . others)
       (cons #'var (arglist-bindings #'others))]))
  
  (define (annotate stx breakpoints breakpoint-origin break)
    
    (define (top-level-annotate stx)
      (kernel:kernel-syntax-case stx #f
        [(module identifier name (#%plain-module-begin . module-level-exprs))
         (quasisyntax/loc stx (module identifier name
                                (#%plain-module-begin 
                                 #,@(map module-level-expr-iterator
                                         (syntax->list #'module-level-exprs)))))]
        [else-stx
         (general-top-level-expr-iterator stx)]))
    
    (define (module-level-expr-iterator stx)
      (kernel:kernel-syntax-case stx #f
        [(provide . provide-specs)
         stx]
        [else-stx
         (general-top-level-expr-iterator stx)]))
    
    (define (general-top-level-expr-iterator stx)
      (kernel:kernel-syntax-case stx #f
        [(define-values (var ...) expr)
         #`(define-values (var ...)
             #,(annotate #`expr (syntax->list #`(var ...)) #t))]
        [(define-syntaxes (var ...) expr)
         stx]
        [(begin . top-level-exprs)
         (quasisyntax/loc stx (begin #,@(map (lambda (expr)
                                               (module-level-expr-iterator expr))
                                             (syntax->list #'top-level-exprs))))]
        [(require . require-specs)
         stx]
        [(require-for-syntax . require-specs)
         stx]
        [else
         (annotate stx '() #f)]))
    
    (define (annotate expr bound-vars is-tail?)
      
      (define (let/rec-values-annotator letrec?)
        (kernel:kernel-syntax-case expr #f
          [(label (((var ...) rhs) ...) . bodies)
           (let* ([new-bindings (apply append (map syntax->list (syntax->list #`((var ...) ...))))]
                  [new-rhs (if letrec?
                               (map (lambda (expr) (annotate expr (append new-bindings bound-vars) #f))
                                    (syntax->list #`(rhs ...)))
                               (map (lambda (expr) (annotate expr bound-vars #f))
                                    (syntax->list #`(rhs ...))))]
                  [last-body (car (reverse (syntax->list #`bodies)))]
                  [all-but-last-body (reverse (cdr (reverse (syntax->list #`bodies))))]
                  [bodies (append (map (lambda (expr) (annotate expr (append new-bindings bound-vars) #f))
                                       all-but-last-body)
                                  (list (annotate last-body (append new-bindings bound-vars) is-tail?)))])
             (with-syntax ([(new-rhs/trans ...) new-rhs])
               (quasisyntax/loc expr
                                (label (((var ...) new-rhs/trans) ...)
                                       #,@bodies))))]))
      
      (define (lambda-clause-annotator clause)
        (kernel:kernel-syntax-case clause #f
          [(arg-list . bodies)
           (let* ([new-bound-vars (append (arglist-bindings #`arg-list) bound-vars)]
                  [new-bodies (let loop ([bodies (syntax->list #`bodies)])
                                (if (equal? '() (cdr bodies))
                                    (list (annotate (car bodies) new-bound-vars #t))
                                    (cons (annotate (car bodies) new-bound-vars #f)
                                          (loop (cdr bodies)))))])
             (quasisyntax/loc clause
                              (arg-list #,@new-bodies)))]))
      
      (define (break-wrap debug-info annotated)
        #`(begin
            (#,break (current-continuation-marks) 'debugger-break #,debug-info)
            #,annotated))
      
      (define annotated
        (kernel:kernel-syntax-case expr #f
          [var-stx (identifier? (syntax var-stx)) expr]
        
	  [(lambda . clause)
	   (quasisyntax/loc expr 
			    (lambda #,@(lambda-clause-annotator #`clause)))]
	  
	  [(case-lambda . clauses)
	   (quasisyntax/loc expr
			    (case-lambda #,@(map lambda-clause-annotator (syntax->list #`clauses))))]
	  
	  [(if test then)
	   (quasisyntax/loc expr (if #,(annotate #`test bound-vars #f)
				     #,(annotate #`then bound-vars is-tail?)))]
	  
	  [(if test then else)
	   (quasisyntax/loc expr (if #,(annotate #`test bound-vars #f)
				     #,(annotate #`then bound-vars is-tail?)
				     #,(annotate #`else bound-vars is-tail?)))]
        
	  [(begin . bodies)
	   (letrec ([traverse
		     (lambda (lst)
		       (if (and (pair? lst) (equal? '() (cdr lst)))
			   `(,(annotate (car lst) bound-vars is-tail?))
			   (cons (annotate (car lst) bound-vars #f)
				 (traverse (cdr lst)))))])
	     (quasisyntax/loc expr (begin #,@(traverse (syntax->list #`bodies)))))]
	  
	  [(begin0 . bodies)
	   (quasisyntax/loc expr (begin0 #,@(map (lambda (expr)
						   (annotate expr bound-vars #f))
						 (syntax->list #`bodies))))]
	  
	  [(let-values . clause)
	   (let/rec-values-annotator #f)]
	  
	  [(letrec-values . clause) 
	   (let/rec-values-annotator #t)]
	  
	  [(set! var val) 
	   (quasisyntax/loc expr (set! var #,(annotate #`val bound-vars #f)))]
	  
	  [(quote _) expr]
	  
	  [(quote-syntax _) expr]
	  
	  ;; FIXME: we have to think harder about this
	  [(with-continuation-mark key mark body)
	   (quasisyntax/loc expr (with-continuation-mark key
							 #,(annotate #`mark bound-vars #f)
							 #,(annotate #`body bound-vars is-tail?)))]
	  
	  [(#%app . exprs)
	   (let ([subexprs (map (lambda (expr) 
				  (annotate expr bound-vars #f))
				(syntax->list #`exprs))])
	     (if is-tail?
		 (quasisyntax/loc expr #,subexprs)
		 (wcm-wrap (make-debug-info expr bound-vars bound-vars 'normal #f)
			   (quasisyntax/loc expr #,subexprs))))]
        
        [(#%datum . _) expr]
        
        [(#%top . var) expr]
        
        [else (error 'expr-syntax-object-iterator "unknown expr: ~a"
                     (syntax-object->datum expr))]))

      (set! count (+ count 1))
      (if (= (modulo count 100) 0)
          (fprintf (current-error-port) "syntax-source: ~v\nsyntax-position: ~v\n" (syntax-source expr) (syntax-position expr)))
      
      
      (if (and (eq? (syntax-source expr) breakpoint-origin)
               (memq (- (syntax-position expr) 1) ; syntax positions start at one.
                     breakpoints))
          (break-wrap (make-debug-info expr bound-vars bound-vars 'at-break #f)
                             annotated)
          annotated))
    
    (top-level-annotate stx)))

                