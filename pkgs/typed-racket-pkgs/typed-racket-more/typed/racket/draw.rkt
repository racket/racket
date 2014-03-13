#lang s-exp typed-racket/base-env/extra-env-lang

;; This module provides a base type environment including
;; racket/draw bindings

(begin
 (require racket/draw/private/bitmap
          racket/draw/private/bitmap-dc
          racket/draw/private/brush
          racket/draw/private/color
          racket/draw/private/font
          racket/draw/private/gl-config
          racket/draw/private/gradient
          racket/draw/private/pen
          racket/draw/private/region
          (for-syntax (only-in (rep type-rep) make-Instance))
          "private/gui-types.rkt"
          (for-syntax (submod "private/gui-types.rkt" #%type-decl)))

 (provide (all-from-out racket/draw/private/bitmap
                        racket/draw/private/bitmap-dc
                        racket/draw/private/brush
                        racket/draw/private/color
                        racket/draw/private/font
                        racket/draw/private/pen
                        racket/draw/private/region)
          LoadFileKind
          Font-Family
          Font-Style
          Font-Weight
          Font-Smoothing
          Font-Hinting
          Bitmap%
          Bitmap-DC%
          Brush-Style
          Brush%
          Brush-List%
          Color%
          Color-Database<%>
          DC<%>
          Font%
          Font-List%
          GL-Config%
          GL-Context<%>
          Linear-Gradient%
          Pen%
          Pen-List%
          Pen-Style
          Pen-Cap-Style
          Pen-Join-Style
          Point%
          Radial-Gradient%
          Region%))

(type-environment
 [the-brush-list (make-Instance (parse-type #'Brush-List%))]
 [the-pen-list (make-Instance (parse-type #'Pen-List%))]
 [the-font-list (make-Instance (parse-type #'Font-List%))]
 [make-bitmap
  (->optkey -PosInt -PosInt [Univ] #:backing-scale -Real #f
            (make-Instance (parse-type #'Bitmap%)))]
 [read-bitmap
  (->opt (Un -Pathlike) [-Symbol (Un (make-Instance (parse-type #'Color%)) (-val #f)) Univ]
         (make-Instance (parse-type #'Bitmap%)))]
 [make-color
  (->optkey -Byte -Byte -Byte
            [-Real]
            #:immutable? Univ #f
            (make-Instance (parse-type #'Color%)))]

 [bitmap% (parse-type #'Bitmap%)]
 [bitmap-dc% (parse-type #'Bitmap-DC%)]
 [brush% (parse-type #'Brush%)]
 [brush-list% (parse-type #'Brush-List%)]
 [color% (parse-type #'Color%)]
 [the-color-database (make-Instance (parse-type #'Color-Database<%>))]
 [font% (parse-type #'Font%)]
 [font-list% (parse-type #'Font-List%)]
 [gl-config% (parse-type #'GL-Config%)]
 [linear-gradient% (parse-type #'Linear-Gradient%)]
 [pen% (parse-type #'Pen%)]
 [radial-gradient% (parse-type #'Radial-Gradient%)]
 [region% (parse-type #'Region%)])
