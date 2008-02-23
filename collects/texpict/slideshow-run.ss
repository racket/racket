
(module slideshow-run "slideshow.ss"
  (require (prefix slideshow: slideshow/run))

  (provide (all-from-except "slideshow.ss" #%module-begin)
           (rename slideshow:#%module-begin #%module-begin)))
