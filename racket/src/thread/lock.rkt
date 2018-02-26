#lang racket/base

(require "internal-error.rkt")

(provide with-lock
         make-lock
         lock-acquire
         lock-release
         own-lock?)

(define-syntax-rule (with-lock (lock caller) expr ...)
  (begin
    (lock-acquire lock caller)
    (begin0
        (let () expr ...)
      (lock-release lock caller))))

(struct future-lock* (box owner))

(define (lock-owner lock)
  (unbox (future-lock*-owner lock)))

(define (make-lock)
  (future-lock* (box 0) (box #f)))

(define (lock-acquire lock caller [block? #t])
  (define box (future-lock*-box lock))
  (let loop ()
    (cond
      [(and (= 0 (unbox box)) (box-cas! box 0 1)) ;; got lock
       (unless (box-cas! (future-lock*-owner lock) #f caller)
         (internal-error "Lock already has owner."))
       #t]
      [block?
       (loop)]
      [else
       #f])))

(define (lock-release lock caller)
  (when (eq? caller (unbox (future-lock*-owner lock)))
    (unless (box-cas! (future-lock*-owner lock) caller #f)
      (internal-error "Failed to reset owner\n"))
    (unless (box-cas! (future-lock*-box lock) 1 0)
      (internal-error "Lock release failed\n"))))

(define (own-lock? lock caller)
  (and (eq? caller (unbox (future-lock*-owner lock)))
       (begin0
           #t
         (unless (= 1 (unbox (future-lock*-box lock)))
           (internal-error "Caller 'owns' lock but lock is free.")))))

