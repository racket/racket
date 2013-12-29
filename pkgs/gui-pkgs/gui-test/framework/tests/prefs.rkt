#lang racket/base
(require "test-suite-utils.rkt")

(module test racket/base)
  
  (define ((check-equal? x) y) (equal? x y))
  (define pref-sym 'plt:not-a-real-preference)
  (define marshalling-pref-sym 'plt:not-a-real-preference-marshalling)
  (define default-test-sym 'plt:not-a-real-preference-default-test)
  
  (shutdown-mred)
  
  (test
   'preference-unbound
   (check-equal? 'passed)
   `(with-handlers ([exn:unknown-preference?
                     (lambda (x)
                       'passed)])
      (preferences:get ',pref-sym)))
  (test 'preference-set-default/get
        (check-equal? 'passed)
        `(begin (preferences:set-default ',pref-sym 'passed symbol?)
                (preferences:get ',pref-sym)))
  (test 'preference-set/get
        (check-equal? 'new-pref)
        `(begin (preferences:set ',pref-sym 'new-pref)
                (preferences:get ',pref-sym)))

  (test 'preference-marshalling
        (check-equal? 'the-answer)
        `(begin (preferences:set-default ',marshalling-pref-sym (lambda () 'the-answer) procedure?)
                (preferences:set-un/marshall ',marshalling-pref-sym
                                             (lambda (f) (f))
                                             (lambda (v) (lambda () v)))
                (begin0 ((preferences:get ',marshalling-pref-sym))
                        (preferences:set ',marshalling-pref-sym (lambda () 2)))))
  (shutdown-mred)
  (test 'preference-marshalling
        (check-equal? 2)
        `(begin (preferences:set-default ',marshalling-pref-sym (lambda () 'the-answer) procedure?)
                (preferences:set-un/marshall ',marshalling-pref-sym
                                             (lambda (f) (f))
                                             (lambda (v) (lambda () v)))
                ((preferences:get ',marshalling-pref-sym))))
  
  (with-handlers ([eof-result? (lambda (x) (void))])
    (send-sexp-to-mred '(begin (preferences:set 'framework:verify-exit #f)
                               (exit:exit)
                               
                               ;; do this yield here so that exit:exit
                               ;; actually exits on this interaction.
                               ;; right now, exit:exit queue's a new event to exit
                               ;; instead of just exiting immediately.
                               (yield (make-semaphore 0)))))
  
  (test 'preference-get-after-restart
        (check-equal? 'new-pref)
        `(begin (preferences:set-default ',pref-sym 'passed symbol?)
                (preferences:get ',pref-sym)))

  (test 'preference-no-set-default-stage1
        (check-equal? 'stage1)
        `(begin (preferences:set-default ',default-test-sym 'default symbol?)
                (preferences:set ',default-test-sym 'new-value)
                'stage1))
  (shutdown-mred)
  (test 'preference-no-set-default-stage2
        (check-equal? 'stage2)
        `(begin 'stage2))
  (shutdown-mred)
  (test 'preference-no-set-default-stage3
        (check-equal? 'new-value)
        `(begin (preferences:set-default ',default-test-sym 'default symbol?)
                (preferences:get ',default-test-sym)))
  
  (test 'preference-add-callback
        (check-equal? 2)
        `(begin 
           (let ([x 1])
             (define remove-it (preferences:add-callback ',default-test-sym (λ (a b) (set! x (+ x 1)))))
             (preferences:set ',default-test-sym 'xyz)
             (remove-it)
             (preferences:set ',default-test-sym 'pdq)
             x)))
  
  (test 'preference-add-weak-callback
        (check-equal? 2)
        `(begin 
           (let ([x 1])
             (define f (λ (a b) (set! x (+ x 1))))
             (define remove-it (preferences:add-callback ',default-test-sym f #t))
             (preferences:set ',default-test-sym 'xyz)
             (remove-it)
             (preferences:set ',default-test-sym 'pdq)
             x)))
  
  (test 'preference-add-weak-callback2
        (check-equal? 3)
        `(begin 
           (let ([x 1])
             (define f (λ (a b) (set! x (+ x 1))))
             (unless (zero? (random 1)) (set! f 'not-a-proc)) ;; try to stop inlining
             (define remove-it (preferences:add-callback ',default-test-sym f #t))
             (collect-garbage) (collect-garbage) (collect-garbage)
             (preferences:set ',default-test-sym 'xyz)
             (remove-it)
             (preferences:set ',default-test-sym 'pdq)
             (f 'a 'b)  ;; make sure safe-for-space doesn't free 'f' earlier
             x)))
  
  (test 'preference-weak-callback-is-weak
        (check-equal? #t)
        `(begin 
           (let ([x 1])
             (define f (λ (a b) (set! x (+ x 1))))
             (define wb (make-weak-box f))
             (define remove-it (preferences:add-callback ',default-test-sym f #t))
             (set! f #f)
             (begin0
               (let loop ([n 10])
                 (cond
                   [(not (weak-box-value wb)) #t]
                   [(zero? n) 'f-still-alive]
                   [else
                    (collect-garbage)
                    (loop (- n 1))]))
               (remove-it)))))
  
  (test 'dialog-appears
        (check-equal? 'passed)
        (lambda ()
          (queue-sexp-to-mred '(begin (send (make-object frame:basic% "frame") show #t)
                                      (preferences:show-dialog)))
          (wait-for-frame "Preferences")
          (queue-sexp-to-mred '(begin (preferences:hide-dialog)
                                      (let ([f (get-top-level-focus-window)])
                                        (if f
                                            (if (string=? "Preferences" (send f get-label))
                                                'failed
                                                'passed)
                                            'passed))))))
