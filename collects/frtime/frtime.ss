(module frtime "mzscheme-utils.ss"
  (require (all-except "lang-ext.ss" lift deep-value-now))
  (require "frp-snip.ss")
  (require (as-is:unchecked frtime/core/frp
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
           (all-from "lang-ext.ss")
	   (all-from "mzscheme-utils.ss")
           (all-from "frp-snip.ss")))
