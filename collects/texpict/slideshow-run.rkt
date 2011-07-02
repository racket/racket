(module slideshow-run "slideshow.rkt"
  (require (prefix slideshow: slideshow/run))

  (provide (all-from-except "slideshow.rkt" #%module-begin)
           (rename slideshow:#%module-begin #%module-begin)))
