#lang racket/base
(require ffi/unsafe/vm)

;; make sure that unreachable threads don't cause a thread-pool leak

(define $threads
  (if (eq? 'cs (system-type 'gc))
      (vm-eval '(foreign-procedure "(cs)threads" () scheme-object))
      (lambda () null)))

(unless (positive? (length ($threads)))
  (error "bad thread list"))

(define orig-custodian (current-custodian))
(define keep null)

(define (go n
            #:m [m 100]
            #:accum? [accum? #f]
            #:in-same-custodian? [in-same-custodian? #f])
  (for ([i (in-range m)])
    (define c (make-custodian))
    (parameterize ([current-custodian c])
      (define p (if (= n 1) 'own (make-parallel-thread-pool n)))
      (for ([j (in-range n)])
        (parameterize ([current-custodian (if in-same-custodian?
                                              c
                                              orig-custodian)])
          (define t
            (thread #:pool p (lambda () (semaphore-wait (make-semaphore)))))
          (when accum?
            (set! keep (cons t keep))))))
    (sync (system-idle-evt))
    (collect-garbage)
    (custodian-shutdown-all c)))

(when (eq? 'cs (system-type 'gc))
  (go 1)
  (go 2)
  (go 1 #:in-same-custodian? #t)
  (go 2 #:in-same-custodian? #t)
  (unless ((length ($threads)) . < . 5)
    (error "too many pool threads still running"))

  ;; These checks make sure that we were testing something useful above:
  (go 1 #:m 10 #:accum? #t)
  (unless ((length ($threads)) . > . 10)
    (error "not enough pool threads still running" (length ($threads))))
  (go 2 #:m 10 #:accum? #t)
  (unless ((length ($threads)) . > . 30)
    (error "not enough (even more) pool threads still running" (length ($threads)))))
