#lang typed/racket

(define: x : (MPairof Integer Integer) (mcons 1 2))
(set-mcar! x -7)
(mcar x)
(mcdr x)
