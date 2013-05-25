("reference to metafunction q before its definition"
 ([use q]) ([def q])
 (let ()
   (term (use))
   (define-language L)
   (define-metafunction L
     [(def) ()])
   #f))
