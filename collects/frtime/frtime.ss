(module frtime (lib "mzscheme-utils.ss" "frtime")
  (require (lib "lang-ext.ss" "frtime"))
  (require (lib "frp-snip.ss" "frtime"))
  (require (lib "ft-qq.ss" "frtime"))
  
  (define (value-nowable? x)
    #t)

  (define ((behaviorof pred) x)
    (let ([v (value-now x)])
      (or (undefined? v)
          (pred v))))


  ;(provide-for-syntax (rename (lib "mzscheme-utils.ss" "frtime") syntax->list syntax->list))
  
  (provide value-nowable? behaviorof
	   (all-from (lib "mzscheme-utils.ss" "frtime"))
           (all-from (lib "lang-ext.ss" "frtime"))
           (all-from (lib "frp-snip.ss" "frtime"))
           (all-from (lib "ft-qq.ss" "frtime"))))
