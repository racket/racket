
(module kerncase racket/base
  (require (for-syntax racket/base)
           (for-template racket/base))

  (define-syntax kernel-syntax-case-internal
    (lambda (stx)
      (syntax-case stx ()
	[(_ stxv phase rel? (extras ...) kernel-context [pattern . rhs] ...)
         (let ()
           (define kernel-ids (syntax-e
                               (quote-syntax
                                (quote 
                                 quote-syntax #%top
                                 #%plain-lambda case-lambda
                                 let-values letrec-values letrec-syntaxes+values
                                 begin begin0 set!
                                 with-continuation-mark
                                 if #%plain-app #%expression
                                 define-values define-syntaxes begin-for-syntax
                                 module module*
                                 #%plain-module-begin 
                                 #%require #%provide #%declare 
                                 #%variable-reference))))
           (define (replace-same-free-id pat)
             (cond
              [(identifier? pat)
               (or (for/or ([kernel-id (in-list kernel-ids)])
                     (and (free-identifier=? pat kernel-id)
                          (datum->syntax kernel-id (syntax-e kernel-id) pat pat)))
                   pat)]
              [(pair? pat) (cons (replace-same-free-id (car pat))
                                 (replace-same-free-id (cdr pat)))]
              [(vector? pat)
               (list->vector (map replace-same-free-id (vector->list pat)))]
              [(box? pat)
               (box (replace-same-free-id (unbox pat)))]
              [(prefab-struct-key pat)
               => (lambda (key)
                    (apply make-prefab-struct
                           key
                           (map replace-same-free-id (cdr (struct->vector pat)))))]
              [(syntax? pat)
               (datum->syntax pat (replace-same-free-id (syntax-e pat)) pat pat)]
              [else pat]))
           (with-syntax ([(pattern ...)
                          (map (lambda (pat)
                                 (replace-same-free-id pat))
                               (syntax->list #'(pattern ...)))])
             (quasisyntax/loc
                 stx
               (syntax-case* stxv (extras ... #,@kernel-ids)
                             (let ([p phase])
                               (cond
                                [(and #,(syntax-e #'rel?) (= p 0)) 
                                 free-identifier=?]
                                [(and #,(syntax-e #'rel?) (= p 1)) 
                                 free-transformer-identifier=?]
                                [else (lambda (a b)
                                        (free-identifier=? a b p '#,(syntax-local-phase-level)))]))
                 [pattern . rhs] ...))))])))
  
  (define-syntax kernel-syntax-case
    (lambda (stx)
      (syntax-case stx ()
	[(_ stxv trans? clause ...)
         (quasisyntax/loc stx
           (kernel-syntax-case-internal stxv (if trans? 1 0) #t () #,stx clause ...))])))

  (define-syntax kernel-syntax-case*
    (lambda (stx)
      (syntax-case stx ()
	[(_ stxv trans? (extras ...) clause ...)
         (quasisyntax/loc stx
           (kernel-syntax-case-internal stxv (if trans? 1 0) #t (extras ...) #,stx clause ...))])))

  (define-syntax kernel-syntax-case/phase
    (lambda (stx)
      (syntax-case stx ()
	[(_ stxv phase clause ...)
         (quasisyntax/loc stx
           (kernel-syntax-case-internal stxv phase #f () #,stx clause ...))])))

  (define-syntax kernel-syntax-case*/phase
    (lambda (stx)
      (syntax-case stx ()
	[(_ stxv phase (extras ...) clause ...)
         (quasisyntax/loc stx
           (kernel-syntax-case-internal stxv phase #f (extras ...) #,stx clause ...))])))

  (define (kernel-form-identifier-list)
    (syntax-e (quote-syntax
               (begin
                begin0
                define-values
                define-syntaxes
                begin-for-syntax
                set!
                let-values
                letrec-values
                #%plain-lambda
                case-lambda
                if
                quote
                letrec-syntaxes+values
                with-continuation-mark
                #%expression
                #%plain-app
                #%top
                #%datum
                #%variable-reference
                module module* #%provide #%require #%declare))))

  (provide kernel-syntax-case
           kernel-syntax-case*
           kernel-syntax-case/phase
           kernel-syntax-case*/phase
           kernel-form-identifier-list))
