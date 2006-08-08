
(module yacc-ext mzscheme
  
  (require (prefix yacc: (lib "yacc.ss" "parser-tools")))
  (provide parser
           options
           productions)
  
  (define-syntax options
    (lambda (stx)
      (raise-syntax-error #f "options keyword used out of context" stx)))
  
  (define-syntax productions
    (lambda (stx)
      (raise-syntax-error #f "productions keyword used out of context" stx)))
  
  (define-syntax (parser stx)
    (syntax-case stx ()
      [(parser form ...)
       (let* ([stop-list (list #'begin #'options #'productions)]
              [forms (syntax->list #'(form ...))]
              [options+productions
               (let loop ([forms forms] [opts null] [prods null])
                 (if (pair? forms)
                     (let ([eform0 (local-expand (car forms) 'expression stop-list)]
                           [forms (cdr forms)])
                       (syntax-case eform0 (begin options productions)
                         [(begin subform ...)
                          (loop (append (syntax->list #'(subform ...)) forms) opts prods)]
                         [(options subform ...)
                          (loop forms (append (syntax->list #'(subform ...)) opts) prods)]
                         [(productions subform ...)
                          (loop forms opts (append (syntax->list #'(subform ...)) prods))]
                         [else
                          (raise-syntax-error #f "bad parser subform" eform0)]))
                     (cons opts (reverse prods))))]
              [opts (car options+productions)]
              [prods (cdr options+productions)])
         (with-syntax ([(opt ...) opts]
                       [(prod ...) prods])
           #'(yacc:parser opt ... (grammar prod ...))))]))
  
  
  )
