#lang racket/base
(require rackunit
         "../util.rkt")
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
                    (module m web-server/lang
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
                    (module m web-server/lang
                      (provide start)
                      (define first (make-web-parameter #f))
                      (define second (make-web-parameter #f))
                      (define (start ignore)
                        (web-parameterize ([first 1]
                                           [second 2])
                                          (call-with-serializable-current-continuation (lambda (k) k))
                                          (+ (first) (second))))))])
       (let ([first-key (meval '(dispatch-start start #f))])
         (check = 3 (meval `(dispatch ,the-dispatch (list ,first-key #f))))))))))
