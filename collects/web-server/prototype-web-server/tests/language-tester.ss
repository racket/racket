(module language-tester mzscheme
  (provide make-module-eval
           make-eval/mod-path)
  
  (define-syntax (make-module-eval m-expr)
    (syntax-case m-expr (module)
      [(_ (module m-id . rest))
       #'(let ([ns (make-namespace)])
           (parameterize ([current-namespace ns])
             (eval '(require "../abort-resume.ss"
                             (lib "serialize.ss")))
             (eval '(module m-id . rest))
             (eval '(require m-id)))
           
           (values
            (lambda ()
              (parameterize ([current-namespace ns])
                (eval '(abort/cc 
                        (with-continuation-mark safe-call? '(#t start)
                          (start
                           (with-continuation-mark the-cont-key start
                             (start-interaction 
                              (lambda (k*v)
                                (lambda (k*v)
                                  ((car k*v) k*v)))))))))))
            (lambda (s-expr)
              (parameterize ([current-namespace ns])
                (eval s-expr)))))]
      [else
       (raise-syntax-error #f "make-module-evel: dropped through" m-expr)]))
  
  (define (make-eval/mod-path pth)
    (let ([ns (make-namespace)])
      (parameterize ([current-namespace ns])
        (eval `(require (lib "client.ss" "web-server" "prototype-web-server")
                        (lib "serialize.ss")
                        (file ,pth))))
      (lambda (expr)
        (parameterize ([current-namespace ns])
          (eval expr))))))