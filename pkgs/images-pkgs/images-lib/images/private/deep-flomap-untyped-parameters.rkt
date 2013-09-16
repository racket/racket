#lang racket/base

(require unstable/parameter-group)

(provide (all-defined-out))

(define refractive-indexes
  #hash((diamond . 2.42)
        (cubic-zirconia . 2.15)
        (ruby . 1.76)
        (enamel . 1.63)
        (glass . 1.54)
        (wax . 1.43)
        (water . 1.33)
        (vacuum . 1.0)))

(define (->refractive-index idx)
  (cond [(symbol? idx)
         (hash-ref refractive-indexes idx
                   (λ () (error 'refractive-index
                                "`refractive-indexes' does not have a refractive index for ~e"
                                idx)))]
        [else  (real->double-flonum idx)]))

(define (list-real->double-flonum vs)
  (map real->double-flonum vs))

;; light parameters
(define light-direction (make-parameter '(0.0 -1.0 1.0) list-real->double-flonum))
(define light-intensity (make-parameter '(1.0 1.0 1.0) list-real->double-flonum))
(define ambient-intensity (make-parameter '(1.0 1.0 1.0) list-real->double-flonum))
(define reflected-intensity (make-parameter '(1.0 1.0 1.0) list-real->double-flonum))

;; material parameters
(define refractive-index (make-parameter (->refractive-index 'glass) ->refractive-index))
(define ideal-reflectance (make-parameter 1.0 real->double-flonum))
(define ideal-transmission (make-parameter 1.0 real->double-flonum))
(define transmission-density (make-parameter 0.65 real->double-flonum))
(define specular-reflectance (make-parameter 0.15 real->double-flonum))
(define specular-roughness (make-parameter 0.15 real->double-flonum))
(define specular-purity (make-parameter 1.0 real->double-flonum))
(define diffuse-reflectance (make-parameter 0.25 real->double-flonum))
(define ambient-reflectance (make-parameter 0.1 real->double-flonum))
(define ambient-transmission (make-parameter 0.7 real->double-flonum))
(define shadow-blur (make-parameter 0.02 real->double-flonum))

(define-parameter-group deep-flomap-lighting
  (light-direction light-intensity ambient-intensity reflected-intensity))

(define-parameter-group deep-flomap-material
  (refractive-index ideal-reflectance ideal-transmission transmission-density
                    specular-reflectance specular-roughness specular-purity
                    diffuse-reflectance ambient-reflectance ambient-transmission
                    shadow-blur))

(define matte-material
  (deep-flomap-material-value
   'vacuum 0.0 0.0 1.0
   0.0 1.0 1.0
   1.0 0.25 0.0
   0.0))

(define dull-plastic-material
  (deep-flomap-material-value
   'glass 0.0 0.0 1.0
   1.0 0.25 1.0
   1.0 0.25 0.0
   0.0))

(define wax-material
  (deep-flomap-material-value
   'wax 1.0 0.5 1.25
   0.5 0.5 0.5
   0.5 0.5 0.5
   0.04))

(define plastic-material
  (deep-flomap-material-value
   'glass 0.375 1.0 2.0
   0.25 0.15 1.0
   0.6 0.5 0.1
   0.03))

(define metal-material
  (deep-flomap-material-value
   3.0 0.3 0.0 1.0
   0.8 0.1 0.2
   0.2 0.8 0.0
   0.0))

(define porcelain-material
  (deep-flomap-material-value
   'enamel 0.9 0.5 1.5
   0.4 0.2 1.0
   0.5 0.5 0.5
   0.04))

(define frosted-glass-material
  (deep-flomap-material-value
   'glass 0.9 1.0 0.8
   0.4 0.2 1.0
   0.5 0.1 0.5
   0.04))

(define glass-material
  (deep-flomap-material-value
   'glass 1.0 1.0 0.65
   0.15 0.15 1.0
   0.25 0.1 0.7
   0.02))

(define diamond-material
  (deep-flomap-material-value
   'diamond 1.0 1.0 0.5
   0.15 0.15 1.0
   0.15 0.1 0.7
   0.02))
