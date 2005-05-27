(module embedded-message mzscheme
  
  (require
   (lib "mred.ss" "mred")
   (lib "class.ss")
   "snip-wrapper.ss")
  
  (provide embedded-message%)

  (define embedded-message%
    (class snip-wrapper%
      
      (init label)
      
      (super-new
       (snip (let ([s (make-object string-snip% label)])
	       (send s set-style control-style)
	       s)))))
  )