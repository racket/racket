(module match-expander mzscheme
  (provide (all-defined))
  (require-for-syntax "match-expander-struct.ss"
                      "match-error.ss")
  

  
  ;; (define-match-expander id transformer-for-plt-match
  ;;                           [transformer-for-match]
  ;;                           [transformer-outside-of-match])
  ;; if only three args, the third is assumed to be the transformer-outside-of-match
  ;; I wish I had keyword macro args
  
  (define-syntax (define-match-expander stx)
    (define (lookup v alist)
      (cond [(assoc v alist) => cadr]
            [else #f]))
    (define (parse args)
      (let loop ([args args]
                 [alist '()])
        (if (null? args) 
            alist
            (let* ([stx-v (car args)]
                   [v (syntax-e stx-v)])
              (cond
                [(not (keyword? v))
                 (match:syntax-err stx-v "Argument must be a keyword")]
                [(not (member v '(#:expression #:plt-match #:match)))
                 (match:syntax-err stx-v "Keyword argument is not a correct keyword")]
                [else                
                 (loop (cddr args)
                       (cons (list v (cadr args))
                             alist))])))))
    (syntax-case stx ()
      [(_ id kw . rest)
       (keyword? (syntax-e #'kw))
       (let* ([args (syntax->list #'(kw . rest))]
              [parsed-args (parse args)])
         (with-syntax
             ([match-xform (lookup #:match parsed-args)]
              [plt-match-xform (lookup #:plt-match parsed-args)]
              [std-xform (or (lookup #:expression parsed-args)
                             #'(lambda (stx)
                                 (match:syntax-err stx "This match expander must be used inside match")))])
           (if (identifier? #'std-xform)
               #`(define-syntax id (make-match-expander plt-match-xform
                                                        match-xform
                                                        (lambda (stx)
                                                          (syntax-case stx (set!)
                                                            #;[(set! id v) #'(set! std-xform v)]
                                                            [(nm args (... ...)) #'(std-xform args (... ...))]
                                                            [nm #'std-xform]))
                                                        (syntax-local-certifier)))
               #'(define-syntax id (make-match-expander plt-match-xform match-xform std-xform (syntax-local-certifier))))))]
      [(_ id plt-match-xform match-xform std-xform)
       (if (identifier? (syntax std-xform))
           #`(define-syntax id (make-match-expander plt-match-xform
						    match-xform
                                                    (lambda (stx)
                                                      (syntax-case stx (set!)
                                                        #;[(set! id v) #'(set! std-xform v)]
                                                        [(nm args (... ...)) #'(std-xform args (... ...))]
                                                        [nm #'std-xform]))
                                                    (syntax-local-certifier)))
           #'(define-syntax id (make-match-expander plt-match-xform match-xform std-xform (syntax-local-certifier))))]
      [(_ id plt-match-xform std-xform)
       (if (identifier? (syntax std-xform))
           #`(define-syntax id (make-match-expander plt-match-xform
                                                    #f
                                                    (lambda (stx)
                                                      (syntax-case stx (set!)
                                                        #;[(set! id v) #'(set! std-xform v)]
                                                        [(nm args (... ...)) #'(std-xform args (... ...))]
                                                        [nm #'std-xform]))
						    (syntax-local-certifier)))
           #'(define-syntax id (make-match-expander plt-match-xform #f std-xform (syntax-local-certifier))))]      
      [(_ id plt-match-xform) 
       #'(define-syntax id 
           (make-match-expander 
            plt-match-xform 
            #f
            (lambda (stx) 
              (match:syntax-err stx "This match expander must be used inside match"))
	    (syntax-local-certifier)))]
      
      [_ (match:syntax-err stx "Invalid use of define-match-expander")]
      ))
  
  )