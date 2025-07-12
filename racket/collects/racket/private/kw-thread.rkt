(module kw-thread "pre-base.rkt"
  (require (prefix-in k: '#%kernel)
           '#%futures)
  
  (provide thread
           make-parallel-thread-pool
           parallel-thread-pool?
           parallel-thread-pool-close)

  (define (thread thunk
                  #:keep-results? [keep-results? #f]
                  #:pool [pool #f])
    (if pool
        (thread/parallel thunk pool keep-results?)
        (k:thread thunk keep-results?))))
