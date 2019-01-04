(meta-cond
 [(threaded?)
  (define make-pthread-parameter make-thread-parameter)
  (define (fork-pthread thunk)
    (fork-thread (let ([place-registers (get-place-registers)])
                   (lambda ()
                     (init-virtual-registers)
                     (set-place-registers! place-registers)
                     (thunk)))))
  (define pthread? thread?)
  (define in-original-host-thread?
    (let ([initial-thread-id (get-thread-id)])
      (lambda ()
        (eqv? (get-thread-id) initial-thread-id))))
  (define (get-initial-pthread)
    (get-initial-thread))
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
  (define (get-initial-pthread) #f)
  (define (pthread?) #f)
  (define (in-original-host-thread?) #t)
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
