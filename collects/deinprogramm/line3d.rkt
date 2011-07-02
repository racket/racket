(module line3d mzscheme
  (require "world.rkt"
           "define-record-procedures.rkt")
  (require (only "DMdA-vanilla.rkt"
                 empty make-pair empty?
                 first rest))
  (provide make-vec3
           vec3-x
           vec3-y
           vec3-z
           add-vec3
           sub-vec3
           mult-vec3
           div-vec3
           dotproduct-vec3
           normquad-vec3
           norm-vec3
           normalize-vec3
           crossproduct-vec3
           make-vec4
           vec4-x
           vec4-y
           vec4-z
           vec4-w
           add-vec4
           sub-vec4
           mult-vec4
           div-vec4
           dotproduct-vec4
           normquad-vec4
           norm-vec4
           normalize-vec4
           expand-vec3
           make-matrix4x4
           create-matrix4x4
           transpose-matrix4x4
           multiply-matrix-vec4
           transform-vec3
           multiply-matrix
           create-translation-matrix
           create-rotation-x-matrix
           create-rotation-y-matrix
           create-rotation-z-matrix
           print-vec4
           print-matrix4x4 
           create-lookat-matrix
           create-projection-matrix
           create-viewport-matrix
           create-camera-matrix
           make-line3d
           line3d-a
           line3d-b
           line3d-color
           create-box
           transform-primitive-list
           render-scene
           )

  (require mzlib/include)
  (include "line3d.scm"))
