(module scheme-gui scheme/base
    
  (require scheme/class)
  (require "test-engine.scm")
    
  (define scheme-test-data (make-parameter (list #f #f))) 
  
  (define scheme-test% 
    (class* test-engine% ()
            
      (super-instantiate ())
      (inherit-field test-info test-display)
      (inherit setup-info)
      
      (field [tests null] 
             [test-objs null])
            
      (define/public (add-test tst)
        (set! tests (cons tst tests)))
      (define/public (get-info) 
        (unless test-info (send this setup-info 'check-require))
        test-info)
           
      (define/augment (run)
        (inner (void) run)
        (for-each (lambda (t) (run-test t)) (reverse tests)))
      
      (define/augment (run-test test) (test)
        (inner (void) run-test test))
      
      ))
    
  (provide scheme-test% scheme-test-data)
  )