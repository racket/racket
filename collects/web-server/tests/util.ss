(module util mzscheme
  (require (lib "connection-manager.ss" "web-server" "private")
           (lib "timer.ss" "web-server" "private"))
  (provide make-module-eval
           make-eval/mod-path
           make-mock-connection
           redact)
  
  (define (make-mock-connection ib)
    (define ip (open-input-bytes ib))
    (define op (open-output-bytes))
    (values (make-connection (make-timer never-evt +inf.0 (lambda () (void)))
                             ip op (make-custodian) #f (make-semaphore 1))
            ip
            op))
  
  (define (redact b)
    (regexp-replace 
     #"Date: [a-zA-Z0-9:, ]+ GMT\r\n"
     (regexp-replace
      #"Last-Modified: [a-zA-Z0-9:, ]+ GMT\r\n"
      b
      #"Last-Modified: REDACTED GMT\r\n")
     #"Date: REDACTED GMT\r\n"))
  
  (define-syntax (make-module-eval m-expr)
    (syntax-case m-expr (module)
      [(_ (module m-id . rest))
       #'(let ([ns (make-namespace)])
           (parameterize ([current-namespace ns])
             (eval '(require (lib "abort-resume.ss" "web-server" "lang")
                             (lib "serialize.ss")))
             (eval '(module m-id . rest))
             (eval '(require m-id)))
           
           (lambda (s-expr)
             (parameterize ([current-namespace ns])
               (eval s-expr))))]
      [else
       (raise-syntax-error #f "make-module-evel: dropped through" m-expr)]))
  
  (define (make-eval/mod-path pth)
    (let ([ns (make-namespace)])
      (parameterize ([current-namespace ns])
        (eval `(require (lib "abort-resume.ss" "web-server" "lang")
                        (lib "serialize.ss")
                        ,pth)))
      (lambda (expr)
        (parameterize ([current-namespace ns])
          (eval expr))))))