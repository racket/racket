(module frtime-lang-only (lib "mzscheme-utils.ss" "frtime")
  (require (lib "lang-ext.ss" "frtime"))
  (require (lib "ft-qq.ss" "frtime"))
  (require (as-is:unchecked (lib "frp-core.ss" "frtime")
	    event-cons? signal-value))
  
  (define (value-nowable? x)
    (or (not (signal? x))
	(not (event-cons? (signal-value x)))))

  (define ((behaviorof pred) x)
    (let ([v (value-now x)])
      (or (undefined? v)
          (pred v))))


  ;(provide-for-syntax (rename (lib "mzscheme-utils.ss" "frtime") syntax->list syntax->list))
  
  (provide value-nowable? behaviorof
	   (all-from (lib "mzscheme-utils.ss" "frtime"))
           (all-from-except (lib "lang-ext.ss" "frtime") lift)
           (all-from (lib "ft-qq.ss" "frtime"))))
