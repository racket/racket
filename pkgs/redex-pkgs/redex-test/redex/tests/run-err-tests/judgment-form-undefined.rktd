("reference to judgment form q before its definition"
 ([use q]) ([def q])
 (let ()
   (judgment-holds (use 1))
   (define-language L)
   (define-judgment-form L 
     #:mode (def I)
     [(def 1)])
   #f))
