#lang racket

;; testing the combination of random-seed and world programming 
;; -----------------------------------------------------------------------------

(require 2htdp/universe 2htdp/image)

(define (main)
  (random-seed 1324)
  (big-bang
   '()
   #;
   (on-tick (λ (l) (cons (random 100) l)) 1/100 30)
   ;; it fails mostly with just time but not always, strange 
   
   (to-draw (λ (l) 
              (text (if (> (length l) 3)
			"ok"
			(~a "press a again: " (- 2 (length l))))
		    222
		    *color)))
   
   (on-key (λ (l ke) 
             (if (and (key=? "a" ke) (<= (length l) 3)) (cons (random 100) l) l)))
   
   (stop-when (λ (l) (>= (length l) 2)))))

(define *color 'blue)

;; -----------------------------------------------------------------------------
(require "test-aux.rkt")

(testing
  (check-equal? (main) (begin (set! *color 'red) (main))))
