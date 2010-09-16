#lang racket
(require rackunit
         web-server/lang/abort-resume)
(require/expose web-server/lang/abort-resume (web-prompt))
(provide abort-resume-tests)

(define abort-resume-tests
  (test-suite
   "Abort/Resume"
   
   (test-suite
    "with-current-saved-continuation-marks-and"
    
    (test-case
     "Easy"
     (check-equal? 
      (call-with-web-prompt
       (lambda () 
         (with-current-saved-continuation-marks-and
          'k1 'v1
          (lambda ()
            (continuation-mark-set->list
             (current-continuation-marks web-prompt)
             the-save-cm-key)))))
      (list (make-immutable-hash (list (cons 'k1 'v1))))))
    
    (test-case
     "Preserve (beta)"
     (check-equal? 
      (call-with-web-prompt
       (lambda () 
         (with-continuation-mark the-save-cm-key
           (make-immutable-hash (list (cons 'k2 'v2)))
           
           (call-with-immediate-continuation-mark
            the-save-cm-key
            (lambda (old-cms)
              (with-continuation-mark the-save-cm-key
                (hash-set old-cms 'k1 'v1)
                (continuation-mark-set->list
                 (current-continuation-marks web-prompt)
                 the-save-cm-key)))
            (make-immutable-hash empty)))))
      (list (make-immutable-hash 
             (list (cons 'k1 'v1)
                   (cons 'k2 'v2))))))
    
    #;(test-case
       "Preserve"
       (check-equal? 
        (call-with-web-prompt
         (lambda () 
           (with-continuation-mark the-save-cm-key
             (make-immutable-hash (list (cons 'k2 'v2)))
             (with-current-saved-continuation-marks-and
              'k1 'v1
              (lambda ()
                (continuation-mark-set->list
                 (current-continuation-marks web-prompt)
                 the-save-cm-key))))))
        (list (make-immutable-hash 
               (list (cons 'k1 'v1)
                     (cons 'k2 'v2))))))
    
    (test-case
     "Update (beta)"
     (check-equal? 
      (call-with-web-prompt
       (lambda () 
         (with-continuation-mark the-save-cm-key
           (make-immutable-hash (list (cons 'k2 'v2) (cons 'k1 'v3)))
           
           (call-with-immediate-continuation-mark
            the-save-cm-key
            (lambda (old-cms)
              (with-continuation-mark the-save-cm-key
                (hash-set old-cms 'k1 'v1)
                (continuation-mark-set->list
                 (current-continuation-marks web-prompt)
                 the-save-cm-key)))
            (make-immutable-hash empty)))))
      (list (make-immutable-hash 
             (list (cons 'k1 'v1)
                   (cons 'k2 'v2))))))
    
    #;(test-case
       "Update"
       (check-equal? 
        (call-with-web-prompt
         (lambda () 
           (with-continuation-mark the-save-cm-key
             (make-immutable-hash (list (cons 'k2 'v2) (cons 'k1 'v3)))
             (with-current-saved-continuation-marks-and
              'k1 'v1
              (lambda ()
                (continuation-mark-set->list
                 (current-continuation-marks web-prompt)
                 the-save-cm-key))))))
        (list (make-immutable-hash 
               (list (cons 'k1 'v1)
                     (cons 'k2 'v2))))))
    
    (test-case
     "Double (beta)"
     (check-equal? 
      (call-with-web-prompt
       (lambda () 
         (with-continuation-mark the-save-cm-key
           (make-immutable-hash (list (cons 'k3 'v1) (cons 'k4 'v2)))
           ((lambda (x) x)
            (with-continuation-mark the-save-cm-key
              (make-immutable-hash (list (cons 'k2 'v2) (cons 'k1 'v3)))
              
              (call-with-immediate-continuation-mark
               the-save-cm-key
               (lambda (old-cms)
                 (with-continuation-mark the-save-cm-key
                   (hash-set old-cms 'k1 'v1)
                   (continuation-mark-set->list
                    (current-continuation-marks web-prompt)
                    the-save-cm-key)))
               (make-immutable-hash empty)))))))
      (list (make-immutable-hash 
             (list (cons 'k1 'v1)
                   (cons 'k2 'v2)))
            (make-immutable-hash 
             (list (cons 'k3 'v1)
                   (cons 'k4 'v2))))))
    
    #;(test-case
       "Double"
       (check-equal? 
        (call-with-web-prompt
         (lambda () 
           (with-continuation-mark the-save-cm-key
             (make-immutable-hash (list (cons 'k3 'v1) (cons 'k4 'v2)))
             ((lambda (x) x)
              (with-continuation-mark the-save-cm-key
                (make-immutable-hash (list (cons 'k2 'v2) (cons 'k1 'v3)))
                (with-current-saved-continuation-marks-and
                 'k1 'v1
                 (lambda ()
                   (continuation-mark-set->list
                    (current-continuation-marks web-prompt)
                    the-save-cm-key))))))))
        (list (make-immutable-hash 
               (list (cons 'k1 'v1)
                     (cons 'k2 'v2)))
              (make-immutable-hash 
               (list (cons 'k3 'v1)
                     (cons 'k4 'v2)))))))
   
   (test-suite 
    "activation-record-list"
    
    (test-case
     "Not in prompt"
     (check-exn exn? (lambda () (activation-record-list))))
    
    (test-case
     "Easy"
     (check-equal? (call-with-web-prompt
                    (lambda () (activation-record-list)))
                   empty))
    
    (test-case
     "Single"
     (check-equal? (call-with-web-prompt
                    (lambda () 
                      (let/ec esc
                        ('f1 (with-continuation-mark the-cont-key +
                               (esc (reverse (activation-record-list))))))))
                   (list (vector + #f #f))))
    
    (test-case
     "Double"
     (check-equal? (call-with-web-prompt
                    (lambda () 
                      (let/ec esc
                        ('f1 (with-continuation-mark the-cont-key +
                               ('f2 (with-continuation-mark the-cont-key -
                                      (esc (reverse (activation-record-list))))))))))
                   ; Opposite the order of c-c-m
                   (list (vector + #f #f)
                         (vector - #f #f))))
    
    (test-case
     "Unsafe"
     (check-exn
      exn?
      (lambda ()
        (call-with-web-prompt
         (lambda () 
           (with-continuation-mark safe-call? #f
             (activation-record-list))))))))
   
   (test-suite
    "abort"
    
    (test-case
     "Not in prompt"
     (check-exn exn? (lambda () (abort (lambda () 42)))))
    
    (test-case
     "Simple"
     (check-equal? (call-with-web-prompt
                    (lambda () 
                      (abort (lambda () 42))))
                   42)))
   
   (test-suite
    "resume"
    
    (test-case
     "Simple"
     (check-equal? (resume empty (list 42))
                   42))
    
    #;(test-case
     "Empty frame"
     (check-exn exn? (lambda () (resume (reverse (list (vector #f #f #f))) (list 42)))))
    
    (test-case
     "Kont"
     (let ([f (lambda (x) (* x x))])
       (check-equal? (resume (reverse (list (vector f #f #f))) (list 42))
                     (f 42))))
    
    (test-case
     "Kont 2"
     (let ([f (lambda (x) (* x x))]
           [g (lambda (x) (+ x x))])
       (check-equal? (resume (reverse (list (vector f #f #f) (vector g #f #f))) (list 42))
                     (f (g 42)))))
    
    (test-case
     "Cont-key"
     (let ([f (lambda (x) (* x x))]
           [g (lambda (x) (+ x x))]
           [esc-b (box #f)]
           [capture (lambda _ (reverse (activation-record-list)))])
       (check-equal? (call-with-web-prompt
                      (lambda ()
                        (let/ec esc 
                          (set-box! esc-b esc)
                          (resume (reverse
                                   (list (vector f #f #f) (vector g #f #f)
                                         (vector esc #f #f) (vector capture #f #f)))
                                  (list 42)))))
                     (list (vector f #f #f) (vector g #f #f)
                           (vector (unbox esc-b) #f #f)))))
    
    (test-case
     "marks"
     (let ([f (lambda (x) (* x x))]
           [g (lambda (x) (+ x x))])
       (check-equal? (call-with-web-prompt
                      (lambda ()
                        (let/ec esc 
                          (resume (reverse
                                   (list (vector f (make-immutable-hash (list (cons 3 4) (cons 1 2))) #f) 
                                         (vector g (make-immutable-hash (list (cons 5 6))) #f)
                                         (vector esc (make-immutable-hash (list (cons 7 8))) #f)
                                         (vector (lambda _
                                                   (continuation-mark-set->list*
                                                    (current-continuation-marks)
                                                    (list 1 3 5 7)))
                                                 #f
                                                 #f)))
                                  (list 42)))))
                     (list (vector #f #f #f 8)
                           (vector #f #f 6 #f)
                           (vector 2 4 #f #f)))))
    
    (test-case
     "cm-key"
     (let ([f (lambda (x) (* x x))]
           [g (lambda (x) (+ x x))]
           [esc-b (box #f)]
           [capture (lambda _ (activation-record-list))])
       (check-equal? (call-with-web-prompt
                      (lambda ()
                        (let/ec esc 
                          (set-box! esc-b esc)
                          (resume (reverse
                                   (list (vector f (make-immutable-hash (list (cons 3 4) (cons 1 2))) #f) 
                                         (vector g (make-immutable-hash (list (cons 5 6))) #f)
                                         (vector esc (make-immutable-hash (list (cons 7 8))) #f)
                                         (vector capture #f #f)))
                                  (list 42)))))
                     (reverse
                      (list (vector f (make-immutable-hash (list (cons 3 4) (cons 1 2))) #f) 
                            (vector g (make-immutable-hash (list (cons 5 6))) #f)
                            (vector (unbox esc-b) (make-immutable-hash (list (cons 7 8))) #f)))))))
   
   ; XXX test kont   
   
   ; XXX test kont-append-fun
   
   ; XXX test send/suspend
   
   ; XXX test dispatch-start
   
   ; XXX test dispatch
   
   ))
