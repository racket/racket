("judgment form q applied before its definition"
 ([use q]) ([def q])
 (let ()
   (judgment-holds (use 1))
   (define-language L)
   (define-judgment-form L 
     mode : I
     [(def 1)])
   #f))