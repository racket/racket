(module frtime-lang-only "lang-utils.rkt"
  (require (only-in frtime/lang-ext undefined? signal? value-now lift))
  (require (as-is:unchecked (except-in frtime/core/frp undefined? undefined) 
	    event-set? signal-value))
  
  (define (value-nowable? x)
    (or (not (signal? x))
	(not (event-set? (signal-value x)))))

  (define ((behaviorof pred) x)
    (let ([v (value-now x)])
      (or (undefined? v)
          (pred v))))

  (provide value-nowable? behaviorof
	   (all-from-out "lang-utils.rkt")
       (all-from-out frtime/lang-ext)))
