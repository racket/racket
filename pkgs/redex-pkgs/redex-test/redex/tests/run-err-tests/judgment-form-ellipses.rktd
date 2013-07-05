(#rx"incompatible ellipsis match counts"
 ([premise (bad-zip ((any_1 any_i) ...))])
 ([name bad-zip] [binder1 any_1] [binder2 any_i] [ellipsis ...])
 (let ()
   (define-language L)
   (define-judgment-form L
     #:mode (name I)
     [(name (binder1 ellipsis binder2 ellipsis))
      premise])
   (judgment-holds (name (1 2)))))
