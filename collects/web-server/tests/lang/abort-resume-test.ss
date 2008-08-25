#lang scheme
(require (planet "test.ss" ("schematics" "schemeunit.plt" 2))
         (planet "util.ss" ("schematics" "schemeunit.plt" 2))
         web-server/lang/abort-resume)
(provide abort-resume-tests)

(define abort-resume-tests
  (test-suite
   "Abort/Resume"
   
   (test-suite
    "current-saved-continuation-marks-and"
    
    (test-case
     "Not in prompt"
     (check-exn exn? (lambda () (current-saved-continuation-marks-and 'k1 'v1))))
    
    (test-case
     "Easy"
     (check-equal? (abort/cc
                    (lambda () (current-saved-continuation-marks-and 'k1 'v1)))
                   (make-immutable-hash (list (cons 'k1 'v1)))))
    
    (test-case
     "Preserve"
     (check-equal? (abort/cc
                    (lambda () 
                      (with-continuation-mark the-save-cm-key (make-immutable-hash (list (cons 'k2 'v2)))
                        (current-saved-continuation-marks-and 'k1 'v1))))
                   (make-immutable-hash 
                    (list (cons 'k1 'v1)
                         (cons 'k2 'v2)))))
    
    (test-case
     "Update"
     (check-equal? (abort/cc
                    (lambda () 
                      (with-continuation-mark the-save-cm-key 
                        (make-immutable-hash (list (cons 'k2 'v2) (cons 'k1 'v3)))
                        (current-saved-continuation-marks-and 'k1 'v1))))
                   (make-immutable-hash 
                    (list (cons 'k1 'v1)
                         (cons 'k2 'v2)))))
    
    (test-case
     "Double"
     (check-equal? (abort/cc
                    (lambda () 
                      (with-continuation-mark the-save-cm-key 
                        (make-immutable-hash (list (cons 'k3 'v1) (cons 'k4 'v0)))
                        ((lambda (x) x)
                         (with-continuation-mark the-save-cm-key 
                           (make-immutable-hash (list (cons 'k2 'v2) (cons 'k1 'v3)))
                           (current-saved-continuation-marks-and 'k1 'v1))))))
                   (make-immutable-hash 
                    (list (cons 'k1 'v1)
                         (cons 'k2 'v2))))))
   
   (test-suite 
    "activation-record-list"
    
    (test-case
     "Not in prompt"
     (check-exn exn? (lambda () (activation-record-list))))
    
    (test-case
     "Easy"
     (check-equal? (abort/cc
                    (lambda () (activation-record-list)))
                   empty))
    
    (test-case
     "Single"
     (check-equal? (abort/cc
                    (lambda () 
                      (let/ec esc
                        ('f1 (with-continuation-mark the-cont-key 'f1
                               (esc (activation-record-list)))))))
                   (list (vector 'f1 #f))))
    
    (test-case
     "Double"
     (check-equal? (abort/cc
                    (lambda () 
                      (let/ec esc
                        ('f1 (with-continuation-mark the-cont-key 'f1
                               ('f2 (with-continuation-mark the-cont-key 'f2
                                      (esc (activation-record-list)))))))))
                   ; Opposite the order of c-c-m
                   (list (vector 'f1 #f)
                         (vector 'f2 #f))))
    
    (test-case
     "Unsafe"
     (check-exn
      exn?
      (lambda ()
        (abort/cc
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
     (check-equal? (abort/cc
                    (lambda () 
                      (abort (lambda () 42))))
                   42)))
   
   (test-suite
    "resume"
    
    (test-case
     "Simple"
     (check-equal? (resume empty (list 42))
                   42))
    
    (test-case
     "Empty frame"
     (check-exn exn? (lambda () (resume (list (vector #f #f)) (list 42)))))
    
    (test-case
     "Kont"
     (let ([f (lambda (x) (* x x))])
       (check-equal? (resume (list (vector f #f)) (list 42))
                     (f 42))))
    
    (test-case
     "Kont 2"
     (let ([f (lambda (x) (* x x))]
           [g (lambda (x) (+ x x))])
       (check-equal? (resume (list (vector f #f) (vector g #f)) (list 42))
                     (f (g 42)))))
    
    (test-case
     "Cont-key"
     (let ([f (lambda (x) (* x x))]
           [g (lambda (x) (+ x x))]
           [esc-b (box #f)]
           [capture (lambda _ (activation-record-list))])
       (check-equal? (abort/cc
                      (lambda ()
                        (let/ec esc 
                          (set-box! esc-b esc)
                          (resume (list (vector f #f) (vector g #f)
                                        (vector esc #f) (vector capture #f))
                                  (list 42)))))
                     (list (vector f #f) (vector g #f)
                           (vector (unbox esc-b) #f)))))
    
    (test-case
     "marks"
     (let ([f (lambda (x) (* x x))]
           [g (lambda (x) (+ x x))])
       (check-equal? (abort/cc
                      (lambda ()
                        (let/ec esc 
                          (resume (list (vector f (make-immutable-hash (list (cons 3 4) (cons 1 2)))) 
                                        (vector g (make-immutable-hash (list (cons 5 6))))
                                        (vector esc (make-immutable-hash (list (cons 7 8))))
                                        (vector (lambda _
                                                  (continuation-mark-set->list*
                                                   (current-continuation-marks)
                                                   (list 1 3 5 7)))
                                                  #f))
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
       (check-equal? (abort/cc
                      (lambda ()
                        (let/ec esc 
                          (set-box! esc-b esc)
                          (resume (list (vector f (make-immutable-hash (list (cons 3 4) (cons 1 2)))) 
                                        (vector g (make-immutable-hash (list (cons 5 6))))
                                        (vector esc (make-immutable-hash (list (cons 7 8))))
                                        (vector capture #f))
                                  (list 42)))))
                     (list (vector f (make-immutable-hash (list (cons 3 4) (cons 1 2)))) 
                           (vector g (make-immutable-hash (list (cons 5 6))))
                           (vector (unbox esc-b) (make-immutable-hash (list (cons 7 8)))))))))
   
   ; XXX test kont   
   
   ; XXX test kont-append-fun
   
   ; XXX test send/suspend
   
   ; XXX test dispatch-start
   
   ; XXX test dispatch
   
   ))