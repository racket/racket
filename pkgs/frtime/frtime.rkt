(module frtime "lang-utils.rkt"
  (require (except-in "lang-ext.rkt" lift deep-value-now))
  (require "frp-snip.rkt")
  (require (as-is:unchecked (except-in frtime/core/frp undefined undefined?)
                            event-set? signal-value))

  (define (value-nowable? x)
    (or (not (signal? x))
	(not (event-set? (signal-value x)))))

  (define ((behaviorof pred) x)
    (let ([v (value-now x)])
      (or (undefined? v)
          (pred v))))

  (provide value-nowable? behaviorof
           (all-from-out "lang-ext.rkt")
           (all-from-out "lang-utils.rkt")
           (all-from-out "frp-snip.rkt")))
