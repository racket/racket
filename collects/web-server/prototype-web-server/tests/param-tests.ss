(module param-tests mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2))
           "util.ss")
  (provide param-suite)
  
  (define the-dispatch
    `(lambda (k*v)
       (lambda (k*v)
         ((car k*v) k*v))))
  
  (define param-suite
    (test-suite
     "Test Web Parameters"
     
     ;; ****************************************
     ;; ****************************************
     ;; BASIC TESTS  
     (test-suite
      "Basic Tests"
      
      (test-case
       "web-parameterize does not overwrite with multiple parameters"
       (let-values ([(go meval)
                     (make-module-eval
                      (module m (lib "lang.ss" "web-server" "prototype-web-server")
                        (define first (make-web-parameter #f))
                        (define second (make-web-parameter #f))
                        (provide start)
                        (define (start initial)
                          (web-parameterize ([first 1]
                                             [second 2])
                                            (+ (first) (second))))))])
         (go the-dispatch)
         (check = 3 (meval '(dispatch-start #f)))))
      
      (test-case
       "web-parameterize does not overwrite with multiple parameters across send/suspend"
       
       (let-values ([(go meval)
                     (make-module-eval
                      (module m (lib "lang.ss" "web-server" "prototype-web-server")
                        (provide start)
                        (define first (make-web-parameter #f))
                        (define second (make-web-parameter #f))
                        (define (start ignore)
                          (web-parameterize ([first 1]
                                             [second 2])
                                            (send/suspend (lambda (k) k))
                                            (+ (first) (second))))))])
         (go the-dispatch)
         (let ([first-key (meval '(dispatch-start #f))])
           (check = 3 (meval `(dispatch (list ,first-key #f)))))))))))