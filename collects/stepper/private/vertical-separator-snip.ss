(module vertical-separator-snip mzscheme
  (require (lib "mred.ss" "mred")
	   (lib "class.ss")
           "mred-extensions.ss")
  
  (provide snip-class)
  (define snip-class (make-object vertical-separator-snip-class%))
  (send snip-class set-classname (format "~s" `(lib "vertical-separator-snip.ss" "stepper" "private")))
  (send (get-the-snip-class-list) add snip-class))
