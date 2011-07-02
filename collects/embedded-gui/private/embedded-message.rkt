(module embedded-message mzscheme
  
  (require
   mred
   mzlib/class
   "snip-wrapper.rkt")
  
  (provide embedded-message%)

  (define embedded-message%
    (class snip-wrapper%
      
      (init label)
      
      (super-new
       (snip (let ([s (make-object string-snip% label)])
	       (send s set-style control-style)
	       s)))))
  )
