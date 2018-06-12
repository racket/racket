(module kw-syntax-local "pre-base.rkt"
  (require (prefix-in k: '#%kernel))

  (provide local-expand
           local-expand/capture-lifts
           local-transformer-expand
           local-transformer-expand/capture-lifts)

  (define (local-expand s context stop-ids [intdefs '()]
                        #:extend-stop-ids? [extend-stop-ids? #t])
    (k:local-expand s context stop-ids intdefs extend-stop-ids?))

  (define (local-expand/capture-lifts s context stop-ids [intdefs '()] [lift-key (gensym 'lift)]
                                      #:extend-stop-ids? [extend-stop-ids? #t])
    (k:local-expand/capture-lifts s context stop-ids intdefs lift-key extend-stop-ids?))

  (define (local-transformer-expand s context stop-ids [intdefs '()]
                                    #:extend-stop-ids? [extend-stop-ids? #t])
    (k:local-transformer-expand s context stop-ids intdefs extend-stop-ids?))

  (define (local-transformer-expand/capture-lifts
           s context stop-ids [intdefs '()] [lift-key (gensym 'lift)]
           #:extend-stop-ids? [extend-stop-ids? #t])
    (k:local-transformer-expand/capture-lifts s context stop-ids intdefs lift-key extend-stop-ids?)))
