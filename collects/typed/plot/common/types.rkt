#lang typed/racket/base

(require (for-syntax racket/base)
         (only-in typed/mred/mred Color%))

(provide (all-defined-out))

(define-type (Treeof A) (Rec T (U A (Listof T))))

(define-type Plot-Device% (Class () () ()))
(define-type 2D-Plot-Area% (Class () () ()))
(define-type 3D-Plot-Area% (Class () () ()))

;; ===================================================================================================
;; Struct types

(require/typed/provide
 plot/common/plot-device
 [plot-device%  Plot-Device%])

(require/typed/provide
 plot/plot2d/plot-area
 [2d-plot-area%  2D-Plot-Area%])

(require/typed/provide
 plot/plot3d/plot-area
 [3d-plot-area%  3D-Plot-Area%])

(require-typed-struct/provide
 mapped-function
 ([f    : (Any -> Any)]
  [fmap : ((Listof Any) -> (Listof Any))])
 plot/utils)

(require-typed-struct/provide
 ivl
 ([min : (Option Real)] [max : (Option Real)])
 plot/common/math)

(require-typed-struct/provide
 plot-time
 ([second : Nonnegative-Integer]
  [minute : Nonnegative-Integer]
  [hour   : Nonnegative-Integer]
  [day    : Integer])
 plot/common/date-time)

(require-typed-struct/provide
 invertible-function
 ([f : (Real -> Real)] [g : (Real -> Real)])
 plot/common/axis-transform)

(require-typed-struct/provide
 pre-tick
 ([value  : Real] [major? : Boolean])
 plot/common/ticks)

(require-typed-struct/provide
 (tick pre-tick)
 ([label : String])
 plot/common/ticks)

(define-type Ticks-Layout (Real Real -> (Listof pre-tick)))
(define-type Ticks-Format (Real Real (Listof pre-tick) -> (Listof String)))

(require-typed-struct/provide
 ticks
 ([layout : Ticks-Layout]
  [format : Ticks-Format])
 plot/common/ticks)

(require-typed-struct/provide
 legend-entry
 ([label : String]
  [draw  : ((Instance Plot-Device%) Real Real -> Void)])
 plot/common/legend)

(define-type Bounds-Fun ((Vectorof ivl) -> (Vectorof ivl)))
(define-type Ticks-Fun ((Vectorof ivl) -> Any))

(require-typed-struct/provide
 plot-element
 ([bounds-rect : (Option (Vectorof ivl))]
  [bounds-fun  : (Option Bounds-Fun)]
  [ticks-fun   : (Option Ticks-Fun)])
 plot/common/plot-element)

(require-typed-struct/provide
 (nonrenderer plot-element)
 ()
 plot/common/plot-element)

(require-typed-struct/provide
 (renderer2d plot-element)
 ([render-proc : (Option ((Instance 2D-Plot-Area%) -> (Treeof legend-entry)))])
 plot/common/plot-element)

(require-typed-struct/provide
 (renderer3d plot-element)
 ([render-proc : (Option ((Instance 3D-Plot-Area%) -> (Treeof legend-entry)))])
 plot/common/plot-element)

;; ===================================================================================================
;; Styles and colors

(require (for-syntax (only-in plot/common/contract known-point-symbols)))

(define-syntax (define-point-sym-type stx)
  (syntax-case stx ()
    [(_ name)
     (with-syntax ([(point-sym-types ...)  (map (λ (sym) `',sym) known-point-symbols)])
       (syntax/loc stx
         (define-type name (U point-sym-types ...))))]))

(define-point-sym-type Point-Sym)

(define-type Anchor
  (U 'top-left 'top 'top-right
     'left 'center 'right
     'bottom-left 'bottom 'bottom-right))

(define-type Font-Family
  (U 'default 'decorative 'roman 'script 
     'swiss 'modern 'symbol 'system))

(define-type Color
  (U (List Real Real Real) String Symbol (Instance Color%)))

(define-type Plot-Color
  (U Integer Color))

(define-type Plot-Brush-Style
  (U Integer 'transparent 'solid
     'bdiagonal-hatch 'fdiagonal-hatch 'crossdiag-hatch
     'horizontal-hatch 'vertical-hatch 'cross-hatch))

(define-type Plot-Pen-Style 
  (U Integer 'transparent 'solid 'dot 'long-dash 'short-dash 'dot-dash))

(define-type (Maybe-Function In Out) (U Out (In -> Out)))

(define-type (Plot-Colors In) (Maybe-Function In (Listof Plot-Color)))
(define-type (Plot-Pen-Styles In) (Maybe-Function In (Listof Plot-Pen-Style)))
(define-type (Pen-Widths In) (Maybe-Function In (Listof Real)))
(define-type (Plot-Brush-Styles In) (Maybe-Function In (Listof Plot-Brush-Style)))
(define-type (Alphas In) (Maybe-Function In (Listof Real)))
(define-type (Labels In) (Maybe-Function In (Listof (Option String))))

;; ===================================================================================================
;; Other argument and parameter types

(define-type Image-File-Format
  (U 'png 'jpeg 
     'xmb 'xpm 'bmp 
     'ps 'pdf 'svg))

(define-type Axis-Transform (Real Real invertible-function -> invertible-function))

(define-type Contour-Levels (U 'auto Positive-Integer (Listof Real)))
