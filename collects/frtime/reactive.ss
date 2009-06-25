(module reactive "mzscheme-utils.ss"
  (require "lang-ext.ss")
  (require "frp-snip.ss")
  (require frtime/list)
  (require frtime/etc)
  (require (as-is:unchecked frtime/core/frp
                            event-set? snap? signal-value))

  (snap? #t)
  
  (define (value-nowable? x)
    (or (not (signal? x))
	(not (event-set? (signal-value x)))))

  (define ((behaviorof pred) x)
    (let ([v (value-now x)])
      (or (undefined? v)
          (pred v))))

  ;(provide-for-syntax (rename frtime/mzscheme-utils syntax->list syntax->list))
  
  (provide value-nowable? behaviorof
           (all-from frtime/list)
           (all-from frtime/etc)
	   (all-from "mzscheme-utils.ss")
           (all-from-except "lang-ext.ss" lift)
           (all-from "frp-snip.ss")))
