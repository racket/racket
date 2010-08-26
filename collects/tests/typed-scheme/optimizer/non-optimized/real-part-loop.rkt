#lang typed/racket/base

(ann
 (let loop ([v 0.0+1.0i])
  (if (> (real-part v) 70000.2) 
      0
      (loop (+ v 3.6))))
 Integer)
