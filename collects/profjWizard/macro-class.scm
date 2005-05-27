#cs(module macro-class mzscheme
     (require-for-syntax (file "class.scm") (file "aux-syntax.scm"))
     
     (provide 
      class ;; (class Name Super (Type Name) ...)
      union ;; (union Type [Class (Type Name) ...] ...)
      )
     
     (define-syntax (class stx)
       (syntax-case stx ()
         [(class Name Super (FType FName) ...)
          (printf
           (make-class
            (list (identifier->string (syntax Name))
                  (identifier->string (syntax Super))
                  (map (lambda (x) 
                         (let* ([x (syntax-e x)]
                                [type (identifier->string (car x))]
                                [name (identifier->string (cadr x))])
                           (list type name)))
                       (syntax->list (syntax ((FType FName) ...)))))))
          (syntax (void))]))
     
     (define-syntax (union stx)
       (syntax-case stx (withToString withTemplate)
         [(union Type [Class (FType FName) ...] ... withToString)
          (syntax 10)]
         [(union Type [Class (FType FName) ...] ...)
          (printf
           (make-union
            (list (identifier->string (syntax Type))
                  (map (lambda (x) 
                         (let* ([x (syntax-e x)]
                                [class (identifier->string (car x))]
                                [fields (map (lambda (f)
                                               (let* ([x (syntax->list f)]
                                                      [type (identifier->string (car x))]
                                                      [name (identifier->string (cadr x))])
                                                 `(,type ,name)))
                                             (cdr x))])
                           (cons class fields)))
                       (syntax->list (syntax ((Class (FType FName) ...) ...)))))))
          (syntax (void))]))
     
     
     )
