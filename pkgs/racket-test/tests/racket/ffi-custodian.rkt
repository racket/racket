#lang racket/base
(require ffi/unsafe/custodian
         ffi/unsafe)

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

;; ----------------------------------------
;; Check unregistration callback after successful register

(let ([c2 (make-custodian)]
      [val (gensym)]
      [ran? #f])
  (define unreg
    (register-finalizer-and-custodian-shutdown
     val (lambda (v) (set! ran? #t)) c2
     #:custodian-available (lambda (unreg) unreg)))
  (unless (and (procedure? unreg)
               (procedure-arity-includes? unreg 1))
    (error "custodian-shutdown unregister is not a suitable procedure"))
  (unreg val)
  (custodian-shutdown-all c2)
  (when ran?
    (error "custodian-shutdown unregister did not work")))

;; check that the unregister function doesn't retain the value:
(let ([c2 (make-custodian)]
      [val (gensym)])
  (define unreg
    (register-finalizer-and-custodian-shutdown
     val void c2
     #:custodian-available (lambda (unreg) unreg)))
  (unless (eq? 'cgc (system-type 'gc))
    (let ([we (make-will-executor)]
          [done? #f])
      (will-register we val (lambda (val)
                              (unreg val)
                              (set! done? #t)))
      (collect-garbage)
      (unless (and (will-try-execute we)
                   done?)
        (error "will wasn't ready")))))

;; ----------------------------------------
;; Check that `#:ordered?` works with  `register-finalizer`,
;; including the case where an intermediate custodian is GCed

(define (check-finalization #:forget-custodian? [forget-custodian? #f])
  (define gone? #f)

  (define c0 (make-custodian))
  (define c1 (make-custodian c0))

  (define x (vector 1 2))
  ; (vector-set! x 1 x) ; prevents finalization in CS

  (parameterize ([current-custodian c1])
    (vector-set! x 0 (register-custodian-shutdown x void #:ordered? #t))
    (register-finalizer x (lambda (x) (set! gone? #t))))

  (when forget-custodian?
    (set! c1 #f)
    (collect-garbage)
    (sync (system-idle-evt)))

  (set! x #f)
  (collect-garbage)
  (sync (system-idle-evt))

  (unless gone? (error "finalizer should have been called")))

(check-finalization)
(check-finalization #:forget-custodian? #t)
