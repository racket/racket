(module frtime "mzscheme-utils.ss"
  (require "lang-ext.ss")
  (require "frp-snip.ss")
  (require "ft-qq.ss")
  (require (as-is:unchecked "frp-core.ss"
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
	   (all-from "mzscheme-utils.ss")
           (all-from-except "lang-ext.ss" lift)
           (all-from "frp-snip.ss")
           (all-from "ft-qq.ss")))
