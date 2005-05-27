(module string-constant-lang mzscheme
  (provide (rename -#%module-begin #%module-begin)
           #%datum)
  
  (define-syntax (-#%module-begin stx)
    (syntax-case stx ()
      [(_ (name str) ...)
       (and (andmap symbol? (syntax-object->datum (syntax (name ...))))
            (andmap string? (syntax-object->datum (syntax (str ...)))))
       (with-syntax ([string-constants (datum->syntax-object stx 'string-constants)])
         (syntax
          (#%plain-module-begin
           (provide string-constants)
           (define string-constants
             '((name str) ...)))))]
      [(_ prs ...)
       (for-each
        (lambda (pr-stx)
          (let ([pr (syntax-object->datum pr-stx)])
            (unless (and (list? pr) 
                         (= 2 (length pr))
                         (symbol? (car pr))
                         (string? (cadr pr)))
              (raise-syntax-error 'string-constant-lang "bad string constant" stx pr-stx))))
        (syntax->list (syntax (prs ...))))])))