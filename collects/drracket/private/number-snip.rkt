#lang racket/base
  (require mred
           racket/class
           framework)
  
  (provide snip-class)
  (define snip-class (make-object number-snip:snip-class%))
  (send snip-class set-classname (format "~s" `(lib "number-snip.ss" "drracket" "private")))
  (send (get-the-snip-class-list) add snip-class)
