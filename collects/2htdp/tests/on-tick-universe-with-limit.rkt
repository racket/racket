#lang racket 

(require 2htdp/universe 2htdp/image)

(with-handlers ((exn? (lambda (w) 
                        (unless (string=? "stop!: the universe stopped: 3" (exn-message w))
                          (raise w)))))
  (universe 0 
            (on-tick (lambda (w) (make-bundle (add1 w) '() '())) 1/28 3)
            (on-msg void)
            (on-new cons))
  (error "the universe didn't stop properly"))
      