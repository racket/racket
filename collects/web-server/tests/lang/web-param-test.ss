(module web-param-test mzscheme
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2))
           "../util.ss")
  (provide web-param-tests)
  
  (define the-dispatch
    `(lambda (k*v)
       (lambda (k*v)
         ((car k*v) k*v))))
  
  (define web-param-tests
    (test-suite
     "Web Parameters"
     
     (test-suite
      "Basic Tests"
      
      (test-case
       "web-parameterize does not overwrite with multiple parameters"
       (let-values ([(meval)
                     (make-module-eval
                      (module m (lib "lang.ss" "web-server")
                        (define first (make-web-parameter #f))
                        (define second (make-web-parameter #f))
                        (provide start)
                        (define (start initial)
                          (web-parameterize ([first 1]
                                             [second 2])
                                            (+ (first) (second))))))])
         (check = 3 (meval '(dispatch-start start #f)))))
      
      (test-case
       "web-parameterize does not overwrite with multiple parameters across send/suspend"
       
       (let-values ([(meval)
                     (make-module-eval
                      (module m (lib "lang.ss" "web-server")
                        (provide start)
                        (define first (make-web-parameter #f))
                        (define second (make-web-parameter #f))
                        (define (start ignore)
                          (web-parameterize ([first 1]
                                             [second 2])
                                            (send/suspend (lambda (k) k))
                                            (+ (first) (second))))))])
         (let ([first-key (meval '(dispatch-start start #f))])
           (check = 3 (meval `(dispatch ,the-dispatch (list ,first-key #f)))))))))))