(module reactive frtime/lang-utils
  (require "lang-ext.ss"
           "frp-snip.ss"
           frtime/frlibs/list
           frtime/frlibs/etc
           (as-is:unchecked frtime/core/frp
                            event-set? snap? signal-value))
  
  (snap? #t)
  
  (define (value-nowable? x)
    (or (not (signal? x))
        (not (event-set? (signal-value x)))))
  
  (define ((behaviorof pred) x)
    (let ([v (value-now x)])
      (or (undefined? v)
          (pred v))))
  
  (provide value-nowable? behaviorof
           (all-from frtime/frlibs/list)
           (all-from frtime/frlibs/etc)
           (all-from frtime/lang-utils)
           (all-from-except "lang-ext.ss" lift)
           (all-from "frp-snip.ss")))
