(module errortrace-key '#%kernel

  ;; this file is badly named; it contains
  ;; all of the code used at runtime by the
  ;; various annotations inserted by this 
  ;; library.
  
  (define-values (errortrace-key) (gensym 'key))
  
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
