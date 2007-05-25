(module language-tester mzscheme
  (provide make-module-eval
           make-eval/mod-path)
  
  (define (go ns)
    (lambda ()
      (parameterize ([current-namespace ns])
        (eval '(run-start 
                (lambda ()
                  (start-interaction 
                      (lambda (k*v)
                        (lambda (k*v)
                          ((car k*v) k*v)))))
                start)))))
  
  (define-syntax (make-module-eval m-expr)
    (syntax-case m-expr (module)
      [(_ (module m-id . rest))
       #'(let ([ns (make-namespace)])
           (parameterize ([current-namespace ns])
             (eval '(require (lib "abort-resume.ss" "web-server" "prototype-web-server")
                             (lib "serialize.ss")))
             (eval '(module m-id . rest))
             (eval '(require m-id)))
           
           (values
            (go ns)
            (lambda (s-expr)
              (parameterize ([current-namespace ns])
                (eval s-expr)))))]
      [else
       (raise-syntax-error #f "make-module-evel: dropped through" m-expr)]))
  
  (define (make-eval/mod-path pth)
    (let ([ns (make-namespace)])
      (parameterize ([current-namespace ns])
        (eval `(require (lib "abort-resume.ss" "web-server" "prototype-web-server")
                        (lib "serialize.ss")
                        (file ,pth))))
      (values (go ns)
              (lambda (expr)
                (parameterize ([current-namespace ns])
                  (eval expr)))))))