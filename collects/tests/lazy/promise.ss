#lang scheme/base

(require scheme/promise tests/eli-tester (for-syntax scheme/base))

;; check that things are `promise?'s or not
(define (test-types)
  (for ([v (list 1 '(1) (lambda () 1))])
    (test (promise? v) => #f))
  (for ([v (list (delay 1) (lazy 1) (delay (delay 1)) (lazy (lazy 1)))])
    (test (promise? v) => #t)))

;; basic delay/lazy/force tests
(define (basic-promise-tests)
  (define thunk1 (lambda () 1))
  (define promise1 (delay 1))
  (define ? #f)
  ;; test a few different values
  (define-syntax-rule (t (f x ...))
    (begin (set! ? 1)      (test (f x ...) => ?)
           (set! ? '())    (test (f x ...) => ?)
           (set! ? '(1))   (test (f x ...) => ?)
           (set! ? thunk1) (test (f x ...) => ?)))
  (define-syntax-rule (t* (f x ...))
    (begin (t (f x ...)) (set! ? promise1) (test (f x ...) => ?)))
  ;; `force' is identity for non-promises
  (t (force ?))
  ;; basic checks that `delay' works as expected with all kinds of values
  (t* (force (delay ?)))
  (t* (force (force (delay (delay ?)))))
  (t* (force (delay (force (delay ?)))))
  ;; basic checks that `lazy' works as expected with all kinds of values
  (t (force (lazy ?)))
  (t (force (lazy (lazy ?))))
  (t (force (force (lazy (lazy ?)))))
  (t (force (lazy (lazy (lazy (lazy ?))))))
  ;; check that `lazy' combines as expected with `delay' in regards to `force'
  ;;   (generally, each `L*D?' sequence requires a force)
  (t* (force (lazy (delay ?))))
  (t* (force (lazy (lazy (delay ?)))))
  (t* (force (lazy (lazy (lazy (delay ?))))))
  ;; two delays = two forces
  (t* (force (force (lazy (delay (delay ?))))))
  (t* (force (force (delay (lazy (delay ?))))))
  (t* (force (force (lazy (lazy (delay (delay ?)))))))
  (t* (force (force (lazy (delay (lazy (delay ?)))))))
  (t* (force (force (delay (lazy (lazy (delay ?)))))))
  (t* (force (force (lazy (lazy (lazy (delay (delay ?))))))))
  (t* (force (force (lazy (lazy (delay (lazy (delay ?))))))))
  (t* (force (force (lazy (delay (lazy (lazy (delay ?))))))))
  (t* (force (force (delay (lazy (lazy (lazy (delay ?))))))))
  ;; now push the second force inside
  (t* (force (lazy  (force (delay (delay ?))))))
  (t* (force (delay (force (lazy (delay ?))))))
  (t* (force (lazy  (force (lazy (delay (delay ?)))))))
  (t* (force (lazy  (force (delay (lazy (delay ?)))))))
  (t* (force (delay (force (lazy (lazy (delay ?)))))))
  (t* (force (lazy  (force (lazy (lazy (delay (delay ?))))))))
  (t* (force (lazy  (force (lazy (delay (lazy (delay ?))))))))
  (t* (force (lazy  (force (delay (lazy (lazy (delay ?))))))))
  (t* (force (delay (force (lazy (lazy (lazy (delay ?))))))))
  (t* (force (lazy  (delay (force (delay ?))))))
  (t* (force (lazy  (lazy  (force (delay (delay ?)))))))
  (t* (force (lazy  (delay (force (lazy (delay ?)))))))
  (t* (force (lazy  (lazy  (force (lazy (delay (delay ?))))))))
  (t* (force (lazy  (lazy  (force (delay (lazy (delay ?))))))))
  (t* (force (lazy  (delay (force (lazy (lazy (delay ?))))))))
  (t* (force (lazy  (lazy  (delay (force (delay ?)))))))
  (t* (force (lazy  (lazy  (lazy  (force (delay (delay ?))))))))
  (t* (force (lazy  (lazy  (delay (force (lazy (delay ?)))))))))

(define (basic-promise-behavior-tests)
  (define (force+catch p) (with-handlers ([exn? values]) (force p)))
  ;; results are cached
  (let* ([c 0] [p (delay (set! c (add1 c)) c)])
    (test c => 0
          (force p) => 1
          (force p) => 1
          (force p) => 1
          c => 1))
  ;; errors are caught
  (let ([p #f])
    (test (void? (set! p (delay (error "BOOM"))))
          (force p) =error> "BOOM"
          (eq? (force+catch p) (force+catch p)))) ; and cached
  ;; raised values too
  (let ([c 0] [p #f])
    (test (void? (set! p (delay (set! c (add1 c)) (raise c))))
          c => 0
          (force p) => (raise 1)
          (force p) => (raise 1)
          c => 1))
  ;; test the predicates
  (letrec ([forced+running?
            (lambda (p) (list (promise-forced? p) (promise-running? p)))]
           [p (delay (forced+running? p))])
    (test (forced+running? p) => '(#f #f)
          (force p)           => '(#f #t)
          (forced+running? p) => '(#t #f))))

(provide promise-tests)
(define (promise-tests)
  (test do (test-types)
        do (basic-promise-tests)
        do (basic-promise-behavior-tests)))
