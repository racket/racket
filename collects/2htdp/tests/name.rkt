#lang racket
(require 2htdp/universe)
(require 2htdp/image)

(big-bang '*
         (name 'jimbob)
         (on-tick (λ (w) w) 1/3 2)
         (to-draw (λ (w) (empty-scene 200 200))))

