(module kw-thread "pre-base.rkt"
  (#%declare #:require=define)

  (require (prefix-in k: '#%kernel)
           '#%futures)
  
  (provide thread
           make-parallel-thread-pool
           parallel-thread-pool?
           parallel-thread-pool-close)

  (define (thread thunk
                  #:keep [keep #f]
                  #:pool [pool #f])
    (if pool
        (thread/parallel thunk pool keep)
        (k:thread thunk keep))))
