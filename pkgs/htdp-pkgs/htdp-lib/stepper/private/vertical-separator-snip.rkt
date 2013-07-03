(module vertical-separator-snip mzscheme
  (require mred
           mzlib/class
           "mred-extensions.rkt")
  
  (provide snip-class)
  (define snip-class (make-object vertical-separator-snip-class%))
  (send snip-class set-classname (format "~s" `(lib "vertical-separator-snip.ss" "stepper" "private")))
  (send (get-the-snip-class-list) add snip-class))
