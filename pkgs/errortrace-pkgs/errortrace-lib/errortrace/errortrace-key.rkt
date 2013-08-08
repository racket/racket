(module errortrace-key '#%kernel

  ;; this file is badly named; it contains
  ;; all of the code used at runtime by the
  ;; various annotations inserted by this 
  ;; library.
  
  ;; Defining `errortrace-key' as a function is a performance hack:
  ;; the compiler can track function constants, and in particular the
  ;; fact that it's not an impersonated/chaperoned mark key, so that a
  ;; `with-continuation-mark' using this key can be dropped if the
  ;; body expression is simple.
  (define-values (errortrace-key) (lambda () 'anything))
  
  (define-values (test-coverage-info) 
    (make-parameter
     (make-hash)
     (λ (x) 
       (if (hash? x)
           (void)
           (error 'test-coverage-info "expected a hash, got ~e" x))
       x)))
  
  (define-values (test-covered)
    (lambda (key)
      (hash-set! (test-coverage-info) key #t)))

  (define-values (init-test-coverage)
    (lambda (l)
      (hash-set! (test-coverage-info) 'base l)))
    
  (#%provide errortrace-key
             init-test-coverage
             test-covered
             test-coverage-info))
