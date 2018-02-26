(meta-cond
 [(threaded?)
  (define make-pthread-parameter make-thread-parameter)
  (define (fork-pthread thunk)
    (fork-thread (lambda ()
                   (init-virtual-registers)
                   (thunk))))
  (define pthread? thread?)
  ;; make-condition
  ;; condition-wait
  ;; condition-signal
  ;; condition-broadcast
  ;; make-mutex
  ;; mutex-acquire
  ;; mutex-release
  ]
 [else
  (define make-pthread-parameter #%make-parameter)
  (define (fork-pthread) (void))
  (define (pthread?) #f)
  (define (make-condition) (void))
  (define (condition-wait c m) (void))
  (define (condition-signal c) (void))
  (define (condition-broadcast c) (void))
  (define (make-mutex) (void))
  (define mutex-acquire
    (case-lambda
     [(m block?) (void)]
     [(m) (void)]))
  (define (mutex-release m) (void))
  ])

(define (active-pthreads) #%$active-threads)
