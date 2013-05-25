#lang racket/base
(require "config.rkt")
(provide get-planet-cache-path)

;; (n.b. this used to have the side effect of creating the path
;; if it didn't exist, but since this function may be run at
;; setup time and setup-time programs must not create this sort 
;; of directory, it doesn't do that anymore)

(define (get-planet-cache-path)
  (let ((path (build-path (PLANET-DIR) "cache.rktd")))
    path))
