(module debugger-annotate mzscheme
  
  (require (prefix kernel: (lib "kerncase.ss" "syntax"))
           (lib "marks.ss" "mztake" "private")
           (lib "mred.ss" "mred")
           (lib "load-annotator.ss" "mztake" "private")
           (lib "more-useful-code.ss" "mztake" "private")
           (lib "list.ss"))
  
  (provide annotate-stx
           run/incremental-annotation
           bindings)
  
  ;; TARGETS is a list of pairs:
  ;;     `(,module-long-filename (,character-offset ...))

  (define (run/incremental-annotation main-module custodian targets receive-result)
    
    (define ((break target) mark-set kind final-mark)
      (let ([mark-list (continuation-mark-set->list mark-set debug-key)])
        (receive-result (make-normal-breakpoint-info (cons final-mark mark-list) target))))
    
    (define ((err-display-handler source) message exn)
      (thread (lambda () (receive-result (make-error-breakpoint-info (list source exn))))))
    
    (define (annotate-module-with-error-handler stx err-hndlr)
      (syntax-case stx (module #%plain-module-begin)
        [(module name req (#%plain-module-begin body ...))
         #`(module name req (#%plain-module-begin
                             (error-display-handler #,err-hndlr)
                             body ...))]))
    
    (define (path->target path)
      (first (filter (lambda (c) (equal? (first c) path))
                     targets)))
    
    (let* ([all-used-module-paths (map first targets)]
           
           [annotate-module? (lambda (fn m)
                               (memf (lambda (sym) (equal? sym fn))
                                     all-used-module-paths))]
           
           [annotator (lambda (fn m stx)
                        ;;(printf "annotating: ~a~n~n" fn)
                        (let* ([target (path->target fn)]
                               [breakpoints (second target)]
                               [stx (annotate-stx (expand stx) (list fn breakpoints) (break target))])
                          ;; add an error handler so anything that goes wrong points to the correct module
                          (annotate-module-with-error-handler stx (err-display-handler fn))))])
      
      (parameterize ([current-custodian custodian]
                     [current-namespace (make-namespace-with-mred)]
                     [error-display-handler (err-display-handler (format "Loading module ~a..." main-module))])
        (require/annotations `(file ,main-module) annotate-module? annotator))))

  (define (arglist-bindings arglist-stx)
    (syntax-case arglist-stx ()
      [var
       (identifier? arglist-stx)
       (list arglist-stx)]
      [(var ...)
       (syntax->list arglist-stx)]
      [(var . others)
       (cons #'var (arglist-bindings #'others))]))
  
  (define (annotate-break? expr targets)
    (and (eq? (syntax-source expr) (first targets))
         (memq (- (syntax-position expr) 1) ; syntax positions start at one.
               (second targets))))

  (define (annotate-stx stx targets break-fn)
    
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
            (#,break-fn (current-continuation-marks) 'debugger-break #,debug-info)
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

      (if (annotate-break? expr targets)
          (break-wrap (make-debug-info expr bound-vars bound-vars 'at-break #f)
                             annotated)
          annotated))
    
    (top-level-annotate stx))

  ;; Retreives the binding of a variable from a normal-breakpoint-info.
  ;; Returns a list of pairs `(,variable-name-stx ,variable-value). Each
  ;; item in the list is a shadowed instance of a variable with the given
  ;; name, with the first item being the one in scope.
  (define (bindings event sym)
    (let ([mark-list (normal-breakpoint-info-mark-list event)])
      (map (lambda (binding) (list (mark-binding-binding binding)
                                   (mark-binding-value binding)))
           (lookup-all-bindings (lambda (id) (eq? (syntax-e id) sym))
                                mark-list)))))

