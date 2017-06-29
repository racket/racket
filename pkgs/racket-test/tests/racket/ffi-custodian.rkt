#lang racket/base
(require ffi/unsafe/custodian)

(define c (make-custodian))

(define done? #f)

(define val (cons 1 2))

(define (reg)
  (register-custodian-shutdown val
                               (lambda (x)
                                 (when done? (error "duplicate!"))
                                 (set! done? (equal? x '(1 . 2))))
                               c
                               #:at-exit? #t))

(unregister-custodian-shutdown val (reg))
(void (reg))

(custodian-shutdown-all c)

(unless done?
  (error "shutdown didn't work"))

;; ----------------------------------------
;; Check that shutdown procedure is retained strongly

(define (go)
  (define done? #f)

  (define remembered (make-weak-hasheq))
  (define r-count 0)
  (define (remember v)
    (hash-set! remembered v #t)
    (set! r-count (add1 r-count))
    v)

  (define c (make-custodian))
  (let ([c-sub (parameterize ([current-custodian c])
                 (make-custodian))]
        [b (box #t)])
    (parameterize ([current-custodian c-sub])
      (void
       (register-custodian-shutdown (box 0)
                                    (remember
                                     (lambda (x)
                                       (set! done? (unbox b))))))))

  (for ([i 2])
    (collect-garbage))

  (unless (= r-count (hash-count remembered))
    (error 'remembered "gone! ~s" (hash-count remembered)))

  (custodian-shutdown-all c)

  done?)

(for ([i 10])
  (unless (go)
    (error "shutdown failed")))

;; ----------------------------------------
;; Check that already-shutdown custodians are handled

(when (register-custodian-shutdown 88 void c)
  (error "should have been #f due to shutdown"))
(unless (eq? 'cb
             (register-finalizer-and-custodian-shutdown
              88 void c
              #:custodian-unavailable (lambda (proc)
                                        (unless (and (procedure? proc)
                                                     (procedure-arity-includes? proc 0))
                                          (error "should have received a thunk"))
                                        'cb)))
  (error "custodian-shutdown callback wasn't called"))

(unregister-custodian-shutdown 'anything #f)
