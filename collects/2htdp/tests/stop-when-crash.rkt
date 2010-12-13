#lang racket

(require 2htdp/universe 2htdp/image)

(with-handlers ((exn:fail? void))
  (big-bang 0
            (on-draw (λ _ (empty-scene 500 500)))
            (stop-when (λ _ (car '())))))

#| -----------------------------------------------------------------------------
(struct:object:...tdp/private/last.rkt:8:2
 `#<procedure:...p-when-crash.rkt:6:19>
 #(struct:object:checked-cell% ...) 
 #f
 #<custodian>
 #<procedure:...p-when-crash.rkt:6:19> 
 #f 
 501
 501
 #<procedure:void>
 #<procedure:void>
 #(struct:object:pasteboard% ...) 
 #f
 #<procedure:K>
 #f
 #f
 #<procedure:...p-when-crash.rkt:7:21> 
 #f
 0
 #f
 #f
 #f
 #<procedure:True>
 #f
 #f
 #f
 #<procedure:show-canvas>
 #f
 #<procedure:set-draw#!>
 1 
 #<procedure:handler> ...)
|#