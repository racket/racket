(define (typed-generator)
  (let ([g (redex-generator poly-stlc (typeof • M τ) 5)])
    (λ ()
      (match (g)
        [`(typeof • ,M ,τ)
         M]
        [#f #f]))))
