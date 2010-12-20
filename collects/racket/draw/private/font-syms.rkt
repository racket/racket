#lang scheme/base

(provide family-symbol? style-symbol? weight-symbol? smoothing-symbol?)

(define (family-symbol? s)
  (memq s '(default decorative roman script
             swiss modern symbol system)))

(define (style-symbol? s)
  (memq s '(normal italic slant)))

(define (weight-symbol? s)
  (memq s '(normal bold light)))

(define (smoothing-symbol? s)
  (memq s '(default smoothed unsmoothed partly-smoothed)))

