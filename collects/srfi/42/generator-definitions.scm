(module generator-definitions mzscheme
  (provide define-generator)
  
  (require-for-syntax "generator-struct.scm")
  (require-for-template mzscheme)
  
  ; syntax 
  ;    (define-generator name proc)
  ;    (define-generator (name arg) body ...)
  ;  The procedure takes a generator clause as input, and returns
  ;  either a loop structure or a syntax-object representing
  ;  a (simpler) generator clause.
  (define-syntax (define-generator stx)
    (syntax-case stx ()
      [(_ (name arg) body ...)
       (begin
         (unless (identifier? #'name)
           (raise-syntax-error 
            'define-generator "expected (define-generator <id> <procedure>), got: " #'name))
         (unless (identifier? #'name)
           (raise-syntax-error 
            'define-generator "expected (define-generator (<id> name) <body> ...), got: " #'name))
         #'(begin
             (define-for-syntax name (make-generator 'name (lambda (arg) body ...)))
             (define-syntax name (make-generator 'name (lambda (arg) body ...)))))]
      [(_ name proc)
       (begin
         (unless (identifier? #'name)
           (raise-syntax-error 
            'define-generator "expected (define-generator <id> <procedure>), got: " #'name))
         #'(begin
             (define-for-syntax name (make-generator 'name proc))
             (define-syntax name (make-generator 'name proc))))]
      [_else
       (raise-syntax-error 
        'define-generator
        "expected either (define-generator <id> <proc>) or (define-generator (<id1> <id2>) <body> ... , got: "
        stx)])))
