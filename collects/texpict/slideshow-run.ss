
(module slideshow-run "slideshow.ss"
  (require (prefix slideshow: (lib "run.ss" "slideshow")))

  (provide (all-from-except "slideshow.ss" #%module-begin)
           (rename slideshow:#%module-begin #%module-begin)))
