(module match-expander mzscheme
  (provide (all-defined))
  ;(require "match-expander-struct.ss")
  (require-for-template "match-expander-struct.ss"
                        "match-error.ss")
  (require-for-syntax "match-expander-struct.ss"
                      "match-error.ss")
  

  
  ;; (define-match-expander id transformer-for-plt-match
  ;;                           [transformer-for-match]
  ;;                           [transformer-outside-of-match])
  ;; if only three args, the third is assumed to be the transformer-outside-of-match
  ;; I wish I had keyword macro args
  
  (define-syntax (define-match-expander stx)
    (syntax-case stx ()
      [(_ id plt-match-xform match-xform std-xform)
       (if (identifier? (syntax std-xform))
           #`(define-syntax id (make-match-expander plt-match-xform
                                                    match-xform
                                                    (lambda (stx)
                                                      (syntax-case stx (set!)
                                                        #;[(set! id v) #'(set! std-xform v)]
                                                        [(nm args (... ...)) #'(std-xform args (... ...))]
                                                        [nm #'std-xform]))))
           #'(define-syntax id (make-match-expander plt-match-xform match-xform std-xform)))]
      [(_ id plt-match-xform std-xform)
       (if (identifier? (syntax std-xform))
           #`(define-syntax id (make-match-expander plt-match-xform
                                                    #f
                                                    (lambda (stx)
                                                      (syntax-case stx (set!)
                                                        #;[(set! id v) #'(set! std-xform v)]
                                                        [(nm args (... ...)) #'(std-xform args (... ...))]
                                                        [nm #'std-xform]))))
           #'(define-syntax id (make-match-expander plt-match-xform #f std-xform)))]      
      [(_ id plt-match-xform) 
       #'(define-syntax id 
           (make-match-expander 
            plt-match-xform 
            #f
            (lambda (stx) 
              (match:syntax-err stx "This match expander must be used inside match"))))]
      
      [_ (match:syntax-err stx "Invalid use of define-match-expander")]
      ))
  
  )