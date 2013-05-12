#lang racket/base 
(require racket/contract 
         pict
         racket/bool 
         future-visualizer/trace
         "private/visualizer-gui.rkt"  
         "private/visualizer-drawing.rkt") 

(provide visualize-futures 
         (contract-out 
          [show-visualizer (->* () (#:timeline (listof indexed-future-event?)) void?)]
          [visualize-futures-thunk ((-> any/c) . -> . any/c)]
          [timeline-pict (->i ([indexed-fevents (listof indexed-future-event?)]) 
                              (#:x [x (or/c #f exact-nonnegative-integer?)] 
                               #:y [y (or/c #f exact-nonnegative-integer?)] 
                               #:width [width (or/c #f exact-nonnegative-integer?)]
                               #:height [height (or/c #f exact-nonnegative-integer?)] 
                               #:selected-event-index [i (or/c #f exact-nonnegative-integer?)])
                              #:pre 
                              (x y width height)
                              (implies (or x y width height)
                                       (and x y width height))
                              [p pict?])]
          [creation-tree-pict (->i ([indexed-fevents (listof indexed-future-event?)]) 
                                    (#:x [x (or/c #f exact-nonnegative-integer?)] 
                                     #:y [y (or/c #f exact-nonnegative-integer?)] 
                                     #:width [width (or/c #f exact-nonnegative-integer?)] 
                                     #:height [height (or/c #f exact-nonnegative-integer?)] 
                                     #:node-width [node-width (or/c #f exact-nonnegative-integer?)]
                                     #:padding [padding (or/c #f exact-nonnegative-integer?)] 
                                     #:zoom [zoom (between/c 1 5)]) 
                                    #:pre 
                                    (x y width height) 
                                    (implies (or x y width height) 
                                             (and x y width height)) 
                                    [p pict?])]))

(define-syntax-rule (visualize-futures e ...)
  (begin (start-future-tracing!)
         (begin0 (begin e ...)
           (stop-future-tracing!)
           (show-visualizer))))

;;visualize-futures-thunk : (-> any/c) -> any/c
(define (visualize-futures-thunk thunk) 
  (start-future-tracing!)
  (begin0 (thunk)
    (stop-future-tracing!)
    (show-visualizer)))
