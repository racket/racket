(module lang frtime/mzscheme-utils
  (require frtime/lang-ext)
  (require frtime/ft-qq)
  (require (as-is:unchecked frtime/frp-core
	    event-set? signal-value))
  
  (define (value-nowable? x)
    (or (not (signal? x))
	(not (event-set? (signal-value x)))))

  (define ((behaviorof pred) x)
    (let ([v (value-now x)])
      (or (undefined? v)
          (pred v))))


  ;(provide-for-syntax (rename frtime/mzscheme-utils syntax->list syntax->list))
  
  (provide value-nowable? behaviorof
	   (all-from frtime/mzscheme-utils)
           (all-from-except frtime/lang-ext lift)
           (all-from frtime/ft-qq)))
