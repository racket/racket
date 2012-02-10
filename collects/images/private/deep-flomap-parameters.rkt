#lang typed/racket/base

(require (except-in "deep-flomap-untyped-parameters.rkt"
                    light-direction light-intensity ambient-intensity reflected-intensity
                    refractive-index ideal-reflectance ideal-transmission transmission-density  
                    specular-reflectance specular-roughness specular-purity       
                    diffuse-reflectance ambient-reflectance ambient-transmission
                    shadow-blur
                    ->refractive-index))

(provide (all-from-out "deep-flomap-untyped-parameters.rkt"))

(require/typed/provide
 "deep-flomap-untyped-parameters.rkt"
 ;; lighting parameters
 [light-direction       (Parameterof (List Flonum Flonum Flonum))]
 [light-intensity       (Parameterof (List Flonum Flonum Flonum))]
 [ambient-intensity     (Parameterof (List Flonum Flonum Flonum))]
 [reflected-intensity   (Parameterof (List Flonum Flonum Flonum))]
 ;; material parameters
 [refractive-index      (Parameterof Flonum)]
 [ideal-reflectance     (Parameterof Flonum)]
 [ideal-transmission    (Parameterof Flonum)]
 [transmission-density  (Parameterof Flonum)]
 [specular-reflectance  (Parameterof Flonum)]
 [specular-roughness    (Parameterof Flonum)]
 [specular-purity       (Parameterof Flonum)]
 [diffuse-reflectance   (Parameterof Flonum)]
 [ambient-reflectance   (Parameterof Flonum)]
 [ambient-transmission  (Parameterof Flonum)]
 [shadow-blur           (Parameterof Flonum)]
 [->refractive-index    ((U Symbol Real) -> Flonum)])
