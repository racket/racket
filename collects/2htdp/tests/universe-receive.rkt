#lang racket

(require 2htdp/universe)
(require 2htdp/image)

(launch-many-worlds
 (big-bang '*
           (on-tick (lambda (w) w) 1/3 2)
           (to-draw (λ (w) (empty-scene 200 200)))
           (register LOCALHOST))
 
 (universe '*
           (on-tick (lambda (w) (make-bundle '* '() '())) 1/2 2)
           (on-new (λ (u iw) (make-bundle '* (list (make-mail iw 'boo!)) '())))
           (on-msg (λ (u iw msg) (make-bundle '* empty empty)))))
