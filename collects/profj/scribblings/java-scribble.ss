(module java-scribble scheme/base
  
  (require scribble/struct scribble/basic scribble/scheme scribble/manual scribble/decode)
  
   (define (*javagrammars style nonterms clauseses)
    (make-table
     `((valignment baseline baseline baseline baseline baseline)
       (alignment right left center left left)
       (style ,style))
     (let ([empty-line (make-flow (list (make-paragraph (list (tt 'nbsp)))))]
           [to-flow (lambda (i) (make-flow (list (make-paragraph (list i)))))])
       (cdr
        (apply append
               (map
                (lambda (nonterm clauses)
                  (list*
                   (list empty-line empty-line empty-line empty-line empty-line)
                   (list (to-flow nonterm)
                         empty-line
                         (to-flow "=")
                         empty-line
                         (make-flow (list (car clauses))))
                   (map (lambda (clause)
                          (list empty-line
                                empty-line
                                (to-flow "|")
                                empty-line
                                (make-flow (list clause))))
                        (cdr clauses))))
                nonterms clauseses))))))
  
  (define (*javagrammar lits s-expr clauseses-thunk)
    (parameterize ([current-variable-list
                    (let loop ([form s-expr])
                      (cond
                        [(symbol? form) (if (memq form lits)
                                            null
                                            (list form))]
                        [(pair? form) (append (loop (car form))
                                              (loop (cdr form)))]
                        [else null]))])
      (let ([l (clauseses-thunk)])
        (*javagrammars #f 
                       (map (lambda (x)
                              (make-element #f
                                            (list (hspace 2)
                                                  (car x))))
                            l)
                       (map cdr l)))))
  
  (define-syntax java
    (syntax-rules ()
      ((_ term) (tt term))))
  
  (define-syntax javablock
    (syntax-rules ()
      [(_ term ...) (make-table #f 
                                (list (list (make-flow (list (make-paragraph (list (java term))))) ...)))]))
  
  (define-syntax javagrammar
    (syntax-rules ()
      [(_ #:literals (lit ...) id (#:tag t term ...) ...) 
       (*javagrammar '(lit ...) 
                     '(id term ... ...)
                     (lambda () (list (list (scheme id)
                                            (make-table #f
                                                        (list (list 
                                                               (make-flow (list 
                                                                           (make-paragraph 
                                                                            (list (elemref t (scheme term ...))))))) ...))))))]
      [(_ #:literals (lit ...) id (term ...) ...)
       (*javagrammar '(lit ...) 
                     '(id term ... ...)
                     (lambda () (list (list (scheme id)
                                            (make-table #f
                                                        (list (list 
                                                               (make-flow (list 
                                                                           (make-paragraph 
                                                                            (list (scheme term ...)))))) ...))))))]
      [(_ #:tag t id (term ...) ...)
       (javagrammar #:literals () id (#:tag t term ...) ...)]
      [(_ #:literals (lit ...) #:tag t id (term ...) ...)
       (javagrammar #:literals (lit ...) id (#:tag t term ...) ...)]
      [(_ id (#:tag t term ...) ...)
       (javagrammar #:literals () id (#:tag t term ...) ...)]
      [(_ id (term ...) ...) (javagrammar #:literals () id (term ...) ...)]))
  
  (define semi (tt ";"))  
  
  (provide java javagrammar javablock semi)
  
  
  )
