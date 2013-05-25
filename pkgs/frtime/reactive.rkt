(module reactive frtime/lang-utils
  (require (except-in "lang-ext.rkt" undefined undefined?)
           "frp-snip.rkt"
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
           (all-from-out frtime/frlibs/list)
           (all-from-out frtime/frlibs/etc)
           (all-from-out frtime/lang-utils)
           (except-out (all-from-out "lang-ext.rkt") lift)
           (all-from-out "frp-snip.rkt")))
