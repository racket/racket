(module frtime-lang-only "lang-utils.ss"
  (require frtime/lang-ext)
  (require (as-is:unchecked frtime/core/frp
	    event-set? signal-value))
  
  (define (value-nowable? x)
    (or (not (signal? x))
	(not (event-set? (signal-value x)))))

  (define ((behaviorof pred) x)
    (let ([v (value-now x)])
      (or (undefined? v)
          (pred v))))

  (provide value-nowable? behaviorof
	   (all-from "lang-utils.ss")
           (all-from-except frtime/lang-ext lift)))
