#lang racket/base

;; Tests for the various racket promises

(require racket/promise tests/eli-tester (for-syntax racket/base))

;; check that things are `promise?'s or not
(define (test-types)
  (for ([v (list 1 '(1) (lambda () 1))])
    (test (promise? v) => #f))
  (for ([v (list (delay 1) (lazy 1) (delay (delay 1)) (lazy (lazy 1)))])
    (test (promise? v) => #t)))

(define (test-syntax)
  (test (delay) =error> "bad syntax"
        (lazy)  =error> "bad syntax"
        (delay #:foo 1 2) =error> "bad syntax"
        (force (delay/thread #:group #f)) =error> "bad syntax"
        (force (delay/thread #:group #f 1)) => 1
        (force (delay/thread 1 #:group #f 2)) => 2
        (force (delay/thread #:groupie #f 1)) =error> "bad syntax"))

;; basic delay/lazy/force tests
(define (test-basic-promises)
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

(define (test-basic-promise-behavior)
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

(define (test-printout)
  (letrec ([foo (delay (set! s (format "~a" foo)) 3)] [s #f])
    (test (format "~a" foo) => "#<promise:foo>"
          (force foo) => 3
          s => "#<promise:!running!foo>"
          (format "~a" foo) => "#<promise!3>"))
  (let ([foo (delay (values 1 2 3))])
    (test (format "~a" foo) => "#<promise:foo>"
          (force foo) => (values 1 2 3)
          (format "~a" foo) => "#<promise!(values 1 2 3)>"))
  (let ([foo (delay (error "boom"))])
    (test (format "~a" foo) => "#<promise:foo>"
          (force foo) => (error "boom")
          (format "~a" foo) => "#<promise!exn!boom>"
          (format "~s" foo) => "#<promise!exn!\"boom\">"))
  (let ([foo (delay (raise 3))])
    (test (format "~a" foo) => "#<promise:foo>"
          (force foo) => (raise 3)
          (format "~a" foo) => "#<promise!raise!3>")))

(define (test-delay/name)
  (let* ([x 1] [p (delay/name (set! x (add1 x)) x)])
    (test (promise? p)
          x => 1
          (force p) => 2
          x => 2
          (format "~a" p) => "#<promise:p>"
          (force p) => 3
          x => 3)))

(define (test-delay/strict)
  (let* ([x 1] [p (delay/strict (set! x (add1 x)) x)])
    (test (promise? p)
          x => 2
          (force p) => 2
          x => 2
          (force (delay/strict (values 1 2 3))) => (values 1 2 3)
          (promise? (force (delay/strict (delay 1)))))))

(define (test-delay/sync)
  (letrec ([p (delay/sync (force p))])
    (test (force p) =error> "reentrant"))
  (let* ([ch (make-channel)]
         [p (delay/sync (channel-get ch) (channel-get ch) 99)])
    (test (format "~a" p) => "#<promise:p>")
    (thread (lambda () (force p) (channel-get ch)))
    (channel-put ch 'x)
    (test (format "~a" p) => "#<promise:!running!p>")
    (channel-put ch 'x)
    (channel-put ch 'x)
    (test (format "~a" p) => "#<promise!99>"
          (force p) => 99)))

(define (test-delay/thread)
  (define-syntax-rule (t delayer)
    (begin (let* ([ch (make-channel)]
                  [p (delayer (channel-get ch) 99)])
             (thread (lambda () (channel-put ch 'x)))
             (test (force p) => 99))
           (test (force (delayer (+ 1 "2"))) =error> "contract violation")))
  (t delay/sync)
  (t delay/idle)
  (let* ([ch (make-channel)] [p (delay/idle #:wait-for ch 99)])
    (test (format "~a" p) => "#<promise:p>"
          (force p) => 99
          (format "~a" p) => "#<promise!99>"))
  (let* ([ch (make-channel)]
         [p (delay/idle #:wait-for ch (channel-get ch) 99)])
    (channel-put ch 'x)
    (test (format "~a" p) => "#<promise:!running!p>"
          (channel-put ch 'x)
          (force p) => 99
          (format "~a" p) => "#<promise!99>")))

(provide promise-tests)
(module+ main (promise-tests))
(define (promise-tests)
  (test do (test-syntax)
        do (test-types)
        do (test-basic-promises)
        do (test-basic-promise-behavior)
        do (test-printout)
        do (test-delay/name)
        do (test-delay/strict)
        do (test-delay/sync)
        do (test-delay/thread)
        ;; misc tests
        (let ([x (lazy (delay 1))]) (force x) (force x)) => 1
        ))
