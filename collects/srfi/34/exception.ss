;; SRFI 34 for PLT Scheme
;; Zhu Chongkai, April 2005
;; <mrmathematica@yahoo.com>
(module exception mzscheme
  
  (provide with-exception-handler
           guard
           raise)
  
  (define-syntax with-exception-handler
    (syntax-rules ()
      ((_ handler thunk)
       (with-handlers (((lambda (exn) #t) handler)) (thunk)))))
  
  (define-syntax guard
    (syntax-rules (else)
      ((_ (var clause ... (else de ...)) e1 e2 ...)
       (with-handlers (((lambda (exn) #t)
                        (lambda (var) (cond clause ...
                                            (else de ...)))))
         e1 e2 ...))
      ((_ (var clause ...) e1 e2 ...)
       (with-handlers (((lambda (exn) #t)
                        (lambda (var) (cond clause ...
                                            (else (raise var))))))
         e1 e2 ...)))))
