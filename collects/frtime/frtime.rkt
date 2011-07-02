(module frtime "lang-utils.rkt"
  (require (all-except "lang-ext.rkt" lift deep-value-now))
  (require "frp-snip.rkt")
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
           (all-from "lang-ext.rkt")
	   (all-from "lang-utils.rkt")
           (all-from "frp-snip.rkt")))
