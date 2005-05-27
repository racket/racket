#|

This file is only here for backwards compatibility reasons.
Old, saved number snips expect to find this file to be 
load-able in mred.

|#

(module number-snip mzscheme
  (require (lib "mred.ss" "mred")
	   (lib "class.ss")
           (lib "framework.ss" "framework"))
  
  (provide snip-class)
  (define snip-class (make-object number-snip:snip-class%))
  (send snip-class set-classname (format "~s" `(lib "number-snip.ss" "drscheme" "private")))
  (send (get-the-snip-class-list) add snip-class))
