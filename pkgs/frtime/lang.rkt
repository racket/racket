(module lang frtime/lang-utils
  (require frtime/lang-ext)
  (require (as-is:unchecked (except-in frtime/core/frp undefined undefined?) event-set? signal-value))
  
  (define (value-nowable? x)
    (or (not (signal? x))
	(not (event-set? (signal-value x)))))

  (define ((behaviorof pred) x)
    (let ([v (value-now x)])
      (or (undefined? v)
          (pred v))))

  (provide value-nowable? behaviorof
           (all-from-out frtime/lang-utils)
           (except-out (all-from-out frtime/lang-ext) lift)))
