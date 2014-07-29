#lang typed/racket

;; Type definitions for typed/racket/gui, typed/racket/draw,
;; and typed/racket/snip

;; racket/draw

(provide LoadFileKind
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
         Region%)

(define-type LoadFileKind
 (U 'unknown 'unknown/mask 'unknown/alpha
    'gif 'gif/mask 'gif/alpha
    'jpeg 'jpeg/alpha
    'png 'png/mask 'png/alpha
    'xbm 'xbm/alpha 'xpm 'xpm/alpha
    'bmp 'bmp/alpha))

(define-type Bitmap%
  (Class
   (init-rest (U (List Integer Integer)
                 (List Integer Integer Any)
                 (List Integer Integer Any Any)
                 (List Integer Integer Any Any Real)
                 (List (U Path-String Input-Port))
                 (List (U Path-String Input-Port) Any)
                 (List (U Path-String Input-Port)
                       Any (Option (Instance Color%)))
                 (List (U Path-String Input-Port)
                       Any (Option (Instance Color%)) Any)
                 (List (U Path-String Input-Port)
                       Any (Option (Instance Color%)) Any
                       Real)
                 (List Bytes Integer Integer)))
   [get-argb-pixels
    (case-> (Real Real
                  Exact-Nonnegative-Integer Exact-Nonnegative-Integer
                  Bytes -> Void)
            (Real Real
                  Exact-Nonnegative-Integer Exact-Nonnegative-Integer
                  Bytes Any -> Void)
            (Real Real
                  Exact-Nonnegative-Integer Exact-Nonnegative-Integer
                  Bytes Any Any -> Void))]
   [get-depth (-> Exact-Nonnegative-Integer)]
   [get-handle (-> Any)]
   [get-height (-> Exact-Positive-Integer)]
   [get-loaded-mask (-> (Option (Instance Bitmap%)))]
   [get-width (-> Exact-Positive-Integer)]
   [has-alpha-channel? (-> Boolean)]
   [is-color? (-> Boolean)]
   [load-file (case->
               ((U String Path Input-Port) -> Boolean)
               ((U String Path Input-Port) LoadFileKind -> Boolean)
               ((U String Path Input-Port) LoadFileKind (Option (Instance Color%)) -> Boolean)
               ((U String Path Input-Port) LoadFileKind (Option (Instance Color%)) Any -> Boolean))]
   [make-dc (-> (Instance Bitmap-DC%))]
   [ok? (-> Boolean)]
   [save-file (case->
               ((U Path-String Output-Port) (U 'png 'jpeg 'xbm 'xpm 'bmp) -> Boolean)
               ((U Path-String Output-Port) (U 'png 'jpeg 'xbm 'xpm 'bmp) Natural -> Boolean))]
   [set-argb-pixels
    (case-> (Real Real
                  Exact-Nonnegative-Integer Exact-Nonnegative-Integer
                  Bytes -> Void)
            (Real Real
                  Exact-Nonnegative-Integer Exact-Nonnegative-Integer
                  Bytes Any -> Void)
            (Real Real
                  Exact-Nonnegative-Integer Exact-Nonnegative-Integer
                  Bytes Any Any -> Void))]
   [set-loaded-mask ((Instance Bitmap%) -> Void)]))

(define-type Color%
  (Class (init-rest (U (List)
                       (List Byte Byte Byte)
                       (List Byte Byte Byte Real)
                       (List String)))
         [red (-> Byte)]
         [green (-> Byte)]
         [blue (-> Byte)]
         [alpha (-> Real)]
         [set (case->
               (Byte Byte Byte -> Void)
               (Byte Byte Byte Real -> Void))]
         [copy-from ((Instance Color%) -> (Instance Color%))]
         [is-immutable? (-> Boolean)]
         [ok? (-> #t)]))

(define-type Color-Database<%>
  (Class [find-color (String -> (Option (Instance Color%)))]
         [get-names (-> (Listof String))]))

(define-type Pen-Style
  (U 'transparent 'solid 'xor 'hilite
     'dot 'long-dash 'short-dash 'dot-dash
     'xor-dot 'xor-long-dash 'xor-short-dash
     'xor-dot-dash))

(define-type Pen-Cap-Style (U 'round 'projecting 'butt))
(define-type Pen-Join-Style (U 'round 'bevel 'miter))

(define-type Brush-Style
  (U 'transparent 'solid 'opaque
     'xor 'hilite 'panel
     'bdiagonal-hatch 'crossdiag-hatch
     'fdiagonal-hatch 'cross-hatch
     'horizontal-hatch 'vertical-hatch))

(define-type Brush%
  (Class (init [color (U String (Instance Color%)) #:optional]
               [style Brush-Style #:optional]
               [stipple (Option (Instance Bitmap%)) #:optional]
               [gradient (Option (U (Instance Radial-Gradient%)
                                    (Instance Linear-Gradient%))) #:optional]
               [transformation (Option (Vector (Vector Real Real Real
                                                       Real Real Real)
                                               Real Real Real Real Real))
                               #:optional])
         [get-color (-> (Instance Color%))]
         [get-gradient (-> (Option (U (Instance Radial-Gradient%)
                                      (Instance Linear-Gradient%))))]
         [get-handle (-> (Option Any))]
         [get-stipple (-> (Option (Instance Bitmap%)))]
         [get-style (-> Brush-Style)]
         [get-transformation (-> (Option (Vector (Vector Real Real Real
                                                       Real Real Real)
                                                 Real Real Real Real Real)))]
         [is-immutable? (-> Boolean)]
         [set-color (case-> ((U (Instance Color%) String) -> Void)
                            (Byte Byte Byte -> Void))]
         [set-stipple
          (case-> ((Option (Instance Bitmap%)) -> Void)
                  ((Option (Instance Bitmap%))
                   (Option (Vector (Vector Real Real Real
                                           Real Real Real)
                                   Real Real Real Real Real))
                   -> Void))]
         [set-style (Brush-Style -> Void)]))

(define-type Brush-List%
  (Class [find-or-create-brush
          (case->
           ((U (Instance Color%) String) Brush-Style -> (Option (Instance Brush%))))]))

(define-type Pen%
  (Class (init [color (U String (Instance Color%)) #:optional]
               [width Real #:optional]
               [style Pen-Style #:optional]
               [cap Pen-Cap-Style #:optional]
               [join Pen-Join-Style #:optional]
               [stipple (Option (Instance Bitmap%)) #:optional])
         [get-cap (-> Pen-Cap-Style)]
         [get-color (-> (Instance Color%))]
         [get-join (-> Pen-Join-Style)]
         [get-stipple (-> (Option (Instance Bitmap%)))]
         [get-width (-> Real)]
         [is-immutable? (-> Boolean)]
         [set-cap (Pen-Cap-Style -> Void)]
         [set-color (case-> ((U (Instance Color%) String) -> Void)
                            (Byte Byte Byte -> Void))]
         [set-join (Pen-Join-Style -> Void)]
         [set-stipple ((Option (Instance Bitmap%)) -> Void)]
         [set-style (Pen-Style -> Void)]
         [set-width (Real -> Void)]))

(define-type Pen-List%
  (Class [find-or-create-pen
          (case->
           ((U String (Instance Color%)) Real Pen-Style
            -> (Instance Pen%))
           ((U String (Instance Color%)) Real Pen-Style
            Pen-Cap-Style
            -> (Instance Pen%))
           ((U String (Instance Color%)) Real Pen-Style
            Pen-Cap-Style Pen-Join-Style
            -> (Instance Pen%)))]))

(define-type Linear-Gradient%
  (Class (init [x0 Real] [y0 Real]
               [x1 Real] [y1 Real]
               [stops (Listof (List Real (Instance Color%)))])
         [get-line (-> (values Real Real Real Real))]
         [get-stops (-> (Listof (List Real (Instance Color%))))]))

(define-type Radial-Gradient%
  (Class (init [x0 Real] [y0 Real] [r0 Real]
               [x1 Real] [y1 Real] [r1 Real]
               [stops (Listof (List Real (Instance Color%)))])
         [get-circles (-> (values Real Real Real Real Real Real))]
         [get-stops (-> (Listof (List Real (Instance Color%))))]))

(define-type DC<%>
  (Class [cache-font-metrics-key (-> Integer)]
         [clear (-> Void)]
         [copy
          (Real Real Nonnegative-Real Nonnegative-Real Real Real -> Void)]
         [draw-arc
          (Real Real Nonnegative-Real Nonnegative-Real Real Real -> Void)]
         [draw-bitmap
          (case->
           ((Instance Bitmap%) Real Real -> Boolean)
           ((Instance Bitmap%) Real Real (U 'solid 'opaque 'xor) -> Boolean)
           ((Instance Bitmap%) Real Real
            (U 'solid 'opaque 'xor) (Instance Color%) -> Boolean)
           ((Instance Bitmap%) Real Real
            (U 'solid 'opaque 'xor) (Instance Color%)
            (Option (Instance Bitmap%)) -> Boolean))]
         [draw-bitmap-section
          (case->
           ((Instance Bitmap%) Real Real Real Real
            Nonnegative-Real Nonnegative-Real
            -> Boolean)
           ((Instance Bitmap%) Real Real Real Real
            Nonnegative-Real Nonnegative-Real
            (U 'solid 'opaque 'xor) -> Boolean)
           ((Instance Bitmap%) Real Real Real Real
            Nonnegative-Real Nonnegative-Real
            (U 'solid 'opaque 'xor) (Instance Color%) -> Boolean)
           ((Instance Bitmap%) Real Real Real Real
            Nonnegative-Real Nonnegative-Real
            (U 'solid 'opaque 'xor) (Instance Color%)
            (Option (Instance Bitmap%)) -> Boolean))]
         [draw-ellipse (Real Real Nonnegative-Real Nonnegative-Real -> Void)]
         [draw-line (Real Real Real Real -> Void)]
         [draw-lines (->* ((U (Listof (Instance Point%))
                              (Listof (Pairof Real Real))))
                          (Real Real)
                          Void)]
         [draw-path (->* ((Instance DC-Path%))
                         (Real Real (U 'odd-even 'winding))
                         Void)]
         [draw-point (Real Real -> Void)]
         [draw-polygon (->* ((U (Listof (Instance Point%))
                                (Listof (Pairof Real Real))))
                            (Real Real (U 'odd-even 'winding))
                            Void)]
         [draw-rectangle (Real Real Nonnegative-Real Nonnegative-Real -> Void)]
         [draw-rounded-rectangle
          (case-> (Real Real Nonnegative-Real Nonnegative-Real -> Void)
                  (Real Real Nonnegative-Real Nonnegative-Real Real -> Void))]
         [draw-spline (Real Real Real Real Real Real -> Void)]
         [draw-text (case-> (String Number Number -> Void)
                            (String Number Number Any -> Void)
                            (String Number Number Any Natural -> Void)
                            (String Number Number Any Natural Real -> Void))]
         [end-doc (-> Void)]
         [end-page (-> Void)]
         [erase (-> Void)]
         [flush (-> Void)]
         [get-alpha (-> Nonnegative-Real)]
         [get-background (-> (Instance Color%))]
         [get-brush (-> (Instance Brush%))]
         [get-char-height (-> Nonnegative-Real)]
         [get-char-width (-> Nonnegative-Real)]
         [get-clipping-region (-> (Option (Instance Region%)))]
         [get-device-scale (-> (Values Nonnegative-Real Nonnegative-Real))]
         [get-font (-> (Instance Font%))]
         [get-gl-context (-> (Option (Instance GL-Context<%>)))]
         [get-initial-matrix (-> (Vector Real Real Real Real Real Real))]
         [get-origin (-> (Values Real Real))]
         [get-pen (-> (Instance Pen%))]
         [get-path-bounding-box (-> (Instance DC-Path%)
                                    (U 'path 'stroke 'fill)
                                    (values Real Real Real Real))]
         [get-rotation (-> Real)]
         [get-scale (-> (Values Real Real))]
         [get-size (-> (Values Nonnegative-Real Nonnegative-Real))]
         [get-smoothing (-> (U 'unsmoothed 'smoothed 'aligned))]
         [get-text-background (-> (Instance Color%))]
         [get-text-extent
          (case->
           (String ->
                   (values Nonnegative-Real Nonnegative-Real
                           Nonnegative-Real Nonnegative-Real))
           (String (Option (Instance Font%)) ->
                   (values Nonnegative-Real Nonnegative-Real
                           Nonnegative-Real Nonnegative-Real))
           (String (Option (Instance Font%)) Any ->
                   (values Nonnegative-Real Nonnegative-Real
                           Nonnegative-Real Nonnegative-Real))
           (String (Option (Instance Font%)) Any Natural ->
                   (values Nonnegative-Real Nonnegative-Real
                           Nonnegative-Real Nonnegative-Real)))]
         [get-text-foreground (-> (Instance Color%))]
         [get-text-mode (-> (U 'solid 'transparent))]
         [get-transformation
          (-> (Vector (Vector Real Real Real Real Real Real)
                      Real Real Real Real Real))]
         [glyph-exists? (Char -> Boolean)]
         [ok? (-> Boolean)]
         [resume-flush (-> Void)]
         [rotate (Real -> Void)]
         [scale (Real Real -> Void)]
         [set-alpha (Nonnegative-Real -> Void)]
         [set-background ((U (Instance Color%) String) -> Void)]
         [set-brush (case->
                     ((Instance Brush%) -> Void)
                     ((U (Instance Color%) String) Brush-Style -> Void))]
         [set-clipping-rect
          (Real Real Nonnegative-Real Nonnegative-Real -> Void)]
         [set-clipping-region ((Option (Instance Region%)) -> Void)]
         [set-font ((Instance Font%) -> Void)]
         [set-initial-matrix ((Vector Real Real Real Real Real Real) -> Void)]
         [set-origin (Real Real -> Void)]
         [set-pen (case->
                   ((Instance Pen%) -> Void)
                   ((U (Instance Color%) String) Real Pen-Style -> Void))]
         [set-rotation (Real -> Void)]
         [set-scale (Real Real -> Void)]
         [set-smoothing ((U 'unsmoothed 'smoothed 'aligned) -> Void)]
         [set-text-background ((U (Instance Color%) String) -> Void)]
         [set-text-foreground ((U (Instance Color%) String) -> Void)]
         [set-text-mode ((U 'solid 'transparent) -> Void)]
         [set-transformation
          ((Vector (Vector Real Real Real Real Real Real)
                      Real Real Real Real Real)
           -> Void)]
         [start-doc (String -> Void)]
         [start-page (-> Void)]
         [suspend-flush (-> Void)]
         [transform ((Vector Real Real Real Real Real Real) -> Void)]
         [translate (Real Real -> Void)]
         [try-color ((Instance Color%) (Instance Color%) -> Void)]))

(define-type DC-Path%
  (Class [append (-> (Instance DC-Path%) Void)]
         [arc (->* (Real Real Real Real Real Real)
                   (Any)
                   Void)]
         [close (-> Void)]
         [curve-to (-> Real Real Real Real Real Real Void)]
         [ellipse (-> Real Real Real Real Void)]
         [get-bounding-box (-> (values Real Real Real Real))]
         [line-to (-> Real Real Void)]
         [lines (->* ((U (Listof (Instance Point%))
                         (Listof (Pairof Real Real))))
                     (Real Real)
                     Void)]
         [move-to (-> Real Real Void)]
         [open? (-> Boolean)]
         [rectangle (-> Real Real Real Real Void)]
         [reset (-> Void)]
         [reverse (-> Void)]
         [rotate (-> Real Void)]
         [rounded-rectangle
          (->* (Real Real Real Real)
               (Real)
               Void)]
         [scale (-> Real Real Void)]
         [text-outline (->* ((Instance Font%) String Real Real)
                            (Any)
                            Void)]
         [transform (-> (Vector Real Real Real Real Real Real) Void)]
         [translate (-> Real Real Void)]))

(define-type Region%
  (Class (init [dc (Option (Instance DC<%>))])
         [get-bounding-box (-> (Values Real Real Real Real))]
         [get-dc (-> (Option (Instance DC<%>)))]
         [in-region? (Real Real -> Boolean)]
         [intersect ((Instance Region%) -> Void)]
         [is-empty? (-> Boolean)]
         [set-arc
          (Real Real Nonnegative-Real Nonnegative-Real
           Real Real -> Void)]
         [set-ellipse
          (Real Real Nonnegative-Real Nonnegative-Real -> Void)]
         [set-path
          (->* ((Instance DC-Path%))
               (Real Real (U 'odd-even 'winding))
               Void)]
         [set-polygon
          (case->
           ((U (Listof (Instance Point%)) (Listof (Pairof Real Real)))
            -> Void)
           ((U (Listof (Instance Point%)) (Listof (Pairof Real Real)))
            Real -> Void)
           ((U (Listof (Instance Point%)) (Listof (Pairof Real Real)))
            Real Real -> Void)
           ((U (Listof (Instance Point%)) (Listof (Pairof Real Real)))
            Real Real (U 'odd-even 'winding)
            -> Void))]
         [set-rectangle
          (Real Real Real Real -> Void)]
         [set-rounded-rectangle
          (case->
           (Real Real Real Real -> Void)
           (Real Real Real Real Real -> Void))]
         [subtract ((Instance Region%) -> Void)]
         [union ((Instance Region%) -> Void)]
         [xor ((Instance Region%) -> Void)]))

(define-type Point%
  (Class (init-rest (U (List) (List Real Real)))
         [get-x (-> Real)]
         [get-y (-> Real)]
         [set-x (Real -> Void)]
         [set-y (Real -> Void)]))

(define-type GL-Config%
  (Class [get-accum-size (-> Natural)]
         [get-depth-size (-> Natural)]
         [get-double-buffered (-> Boolean)]
         [get-legacy? (-> Boolean)]
         [get-multisample-size (-> Natural)]
         [get-share-context (-> (Option (Instance GL-Context<%>)))]
         [get-stencil-size (-> Natural)]
         [get-stereo (-> Boolean)]
         [set-accum-size (Integer -> Void)]
         [set-depth-size (Integer -> Void)]
         [set-double-buffered (Any -> Void)]
         [set-legacy? (-> Any Void)]
         [set-multisample-size (Integer -> Void)]
         [set-share-context ((Option (Instance GL-Context<%>)) -> Void)]
         [set-stencil-size (Integer -> Void)]
         [set-stereo (Any -> Void)]))

(define-type GL-Context<%>
  (Class [call-as-current
          (case-> ((-> Any) -> Any)
                  ((-> Any) (Evtof Any) -> Any)
                  ((-> Any) (Evtof Any) Any -> Any))]
         ;; FIXME: a typed/ffi binding with Opaque cpointer type
         ;; would be better here
         [get-handle (-> Any)]
         [ok? (-> Boolean)]
         [swap-buffers (-> Void)]))

(define-type Bitmap-DC%
  (Class #:implements DC<%>
         (init [bitmap (Option (Instance Bitmap%))])
         [get-argb-pixels
          (case-> (Real Real Integer Integer Bytes -> Void)
                  (Real Real Integer Integer Bytes Any -> Void)
                  (Real Real Integer Integer Bytes Any Any -> Void))]
         [get-bitmap (-> (Option (Instance Bitmap%)))]
         [get-pixel (Real Real (Instance Color%) -> Boolean)]
         [set-argb-pixels
          (case-> (Real Real Integer Integer Bytes -> Void)
                  (Real Real Integer Integer Bytes Any -> Void)
                  (Real Real Integer Integer Bytes Any Any -> Void))]
         [set-bitmap ((Option (Instance Bitmap%)) -> Void)]
         [set-pixel (Real Real (Instance Color%) -> Boolean)]))

(define-type Font-List%
  (Class
   [find-or-create-font
    (case-> (Integer (U Symbol String) Symbol Symbol -> (Instance Font%))
            (Integer (U Symbol String) Symbol Symbol Any -> (Instance Font%))
            (Integer (U Symbol String) Symbol Symbol Any Any -> (Instance Font%))
            (Integer (U Symbol String) Symbol Symbol Any Any Any -> (Instance Font%))
            (Integer (U Symbol String) Symbol Symbol Any Any Any Any -> (Instance Font%))
            (Integer (U Symbol String) Symbol Symbol Any Any Any Any Font-Hinting -> (Instance Font%)))]))

(define-type Font-Family
  (U 'default 'decorative 'roman 'script 'swiss
     'modern 'symbol 'system))

(define-type Font-Style (U 'normal 'italic 'slant))

(define-type Font-Weight (U 'normal 'bold 'light))

(define-type Font-Smoothing (U 'default 'partly-smoothed
                               'smoothed 'unsmoothed))

(define-type Font-Hinting (U 'aligned 'unaligned))

(define-type Font%
  (Class [get-face (-> (Option String))]
         [get-family (-> Font-Family)]
         [get-hinting (-> Font-Hinting)]
         [get-point-size (-> Positive-Integer)]
         [get-size-in-pixels (-> Boolean)]
         [get-smoothing (-> Font-Smoothing)]
         [get-style (-> Font-Style)]
         [get-underlined (-> Boolean)]
         [get-weight (-> Font-Weight)]
         [screen-glyph-exists?
          (case-> (Char -> Boolean)
                  (Char Any -> Boolean))]))

;; racket/gui

(provide Area<%>
         Area-Container<%>
         Area-Container-Window<%>
         Button%
         Canvas<%>
         Canvas%
         Check-Box%
         Checkable-Menu-Item%
         Choice%
         Clipboard-Client%
         Clipboard<%>
         Combo-Field%
         Control<%>
         Column-Control-Event%
         Control-Event%
         Control-Event-Type
         Cursor%
         Dialog%
         Event%
         Frame%
         Gauge%
         Group-Box-Panel%
         Grow-Box-Spacer-Pane%
         Horizontal-Pane%
         Horizontal-Panel%
         Key-Event%
         Labelled-Menu-Item<%>
         List-Box%
         List-Control<%>
         Menu%
         Menu-Bar%
         Menu-Item<%>
         Menu-Item%
         Menu-Item-Container<%>
         Message%
         Mouse-Event%
         Pane%
         Panel%
         Popup-Menu%
         Printer-DC%
         Radio-Box%
         Selectable-Menu-Item<%>
         Separator-Menu-Item%
         Scroll-Event%
         Slider%
         Subarea<%>
         Subwindow<%>
         Tab-Panel%
         Text-Field%
         Timer%
         Top-Level-Window<%>
         Vertical-Pane%
         Vertical-Panel%
         Window<%>)

(define-type Area<%>
  (Class [get-graphical-min-size (-> (Values Natural Natural))]
         [get-parent (-> (Option (Instance Area-Container<%>)))]
         [get-top-level-window (-> (U (Instance Frame%) (Instance Dialog%)))]
         [min-width (case-> (-> Integer)
                            (Integer -> Void))]
         [min-height (case-> (-> Integer)
                             (Integer -> Void))]
         [stretchable-height
          (case-> (-> Boolean)
                  (Any -> Void))]
         [stretchable-width
          (case-> (-> Boolean)
                  (Any -> Void))]))

(define-type Window<%>
  (Class #:implements Area<%>
         [accept-drop-files (case-> (-> Boolean)
                                    (Any -> Void))]
         [client->screen
          (Integer Integer -> (Values Integer Integer))]
         [enable (Any -> Void)]
         [focus (-> Void)]
         [get-client-handle (-> Any)]
         [get-client-size (-> (Values Natural Natural))]
         [get-cursor (-> (Option (Instance Cursor%)))]
         [get-handle (-> Any)]
         [get-height (-> Natural)]
         [get-label
          (-> (U String (Instance Bitmap%) (U 'app 'caution 'stop)
                 (List (Instance Bitmap%) String (U 'left 'top 'right 'bottom))
                 #f))]
         [get-plain-label (-> (Option String))]
         [get-size (-> (Values Natural Natural))]
         [get-width (-> Natural)]
         [get-x (-> Integer)]
         [get-y (-> Integer)]
         [has-focus? (-> Boolean)]
         [is-shown? (-> Boolean)]
         [on-drop-file (Path -> Void)]
         [on-focus (Any -> Void)]
         [on-move (Integer Integer -> Void)]
         [on-size (Natural Natural -> Void)]
         [on-subwindow-char ((Instance Window<%>) (Instance Key-Event%) -> Boolean)]
         [on-subwindow-event ((Instance Window<%>) (Instance Mouse-Event%) -> Boolean)]
         [on-subwindow-focus ((Instance Window<%>) Any -> Void)]
         [on-superwindow-enable (Any -> Void)]
         [on-superwindow-show (Any -> Void)]
         [popup-menu ((Instance Popup-Menu%) Natural Natural -> Void)]
         [refresh (-> Void)]
         [screen->client
          (Integer Integer -> (Values Integer Integer))]
         [set-cursor ((Option (Instance Cursor%)) -> Void)]
         [set-label (String -> Void)]
         [show (Any -> Void)]
         [warp-pointer (Integer Integer -> Void)]))

(define-type Area-Container<%>
  (Class #:implements Area<%>
         [add-child ((Instance Subwindow<%>) -> Void)]
         [after-new-child ((Instance Subarea<%>) -> Void)]
         [begin-container-sequence (-> Void)]
         [border (case-> (-> Natural) (Natural -> Void))]
         [change-children
          (((Listof (Instance Subarea<%>))
            ->
            (Listof (Instance Subarea<%>)))
           -> Void)]
         [container-flow-modified (-> Void)]
         [container-size ((Listof (List Natural Natural Any Any))
                          -> (Values Natural Natural))]
         [delete-child ((Instance Subwindow<%>) -> Void)]
         [end-container-sequence (-> Void)]
         [get-alignment (-> (Values (U 'right 'center 'left)
                                    (U 'bottom 'center 'top)))]
         [get-children (-> (Listof (Instance Subarea<%>)))]
         [place-children ((Listof (List Natural Natural Any Any))
                          Natural Natural
                          -> (Listof (List Natural Natural Natural Natural)))]
         [reflow-container (-> Void)]
         [set-alignment ((U 'right 'center 'left) (U 'bottom 'center 'top)
                         -> Void)]
         [spacing (case-> (-> Natural) (Natural -> Void))]))

(define-type Area-Container-Window<%>
  (Class #:implements Area-Container<%>
         #:implements Window<%>))

(define-type Top-Level-Window<%>
  (Class #:implements Area-Container-Window<%>
         (augment [can-close? (-> Boolean)])
         [can-exit? (-> Boolean)]
         [get-eventspace (-> Any)] ; FIXME: Eventspace type
         [move (Integer Integer -> Void)]
         [on-activate (Any -> Void)]
         (augment [on-close (-> Void)])
         [on-exit (-> Void)]
         [on-message (Any -> Void)]
         (augment [display-changed (-> Any)])
         [set-icon
          (case->
           ((Instance Bitmap%) -> Void)
           ((Instance Bitmap%) (Instance Bitmap%)  -> Void)
           ((Instance Bitmap%) (Instance Bitmap%) (U 'small 'large 'both)
            -> Void))]))

(define-type Subarea<%>
  (Class #:implements Area<%>))

(define-type Subwindow<%>
  (Class #:implements Subarea<%>
         #:implements Window<%>))

(define-type Canvas<%>
  (Class #:implements Subwindow<%>
         [accept-tab-focus (case-> [-> Boolean]
                                   [Any -> Void])]
         [flush (-> Void)]
         [get-canvas-background (-> (Option (Instance Color%)))]
         [get-dc (-> (Instance DC<%>))]
         [min-client-height
          (case-> (-> Natural)
                  (Natural -> Void))]
         [min-client-width
          (case-> (-> Natural)
                  (Natural -> Void))]
         [on-char ((Instance Key-Event%) -> Void)]
         [on-event ((Instance Mouse-Event%) -> Void)]
         [on-paint (-> Void)]
         [on-tab-in (-> Void)]
         [resume-flush (-> Void)]
         [set-canvas-background ((Instance Color%) -> Void)]
         [set-resize-corner (Any -> Void)]
         [suspend-flush (-> Void)]))

(define-type Canvas%
  (Class #:implements Canvas<%>
         (init [parent (Instance Area-Container<%>)] ; FIXME
               [style (Listof (U 'border 'control-border 'combo
                                 'vscroll 'hscroll 'resize-corner
                                 'gl 'no-autoclear 'transparent
                                 'no-focus 'deleted))
                      #:optional]
               [paint-callback ((Instance Canvas%) (Instance DC<%>) -> Any)
                               #:optional]
               [label (Option String) #:optional]
               [gl-config (Option Any) #:optional]
               [enabled Any #:optional]
               [vert-margin Natural #:optional]
               [horiz-margin Natural #:optional]
               [min-width (Option Natural) #:optional]
               [min-height (Option Natural) #:optional]
               [stretchable-width Any #:optional]
               [stretchable-height Any #:optional])
         [get-scroll-page ((U 'horizontal 'vertical) -> Exact-Positive-Integer)]
         [get-scroll-pos ((U 'horizontal 'vertical) -> Exact-Positive-Integer)]
         [get-scroll-range ((U 'horizontal 'vertical) -> Exact-Positive-Integer)]
         [get-view-start (-> (Values Natural Natural))]
         [get-virtual-size (-> (Values Natural Natural))]
         [init-auto-scrollbars
          ((Option Natural) (Option Natural) Real Real -> Void)]
         [init-manual-scrollbars
          ((Option Natural) (Option Natural)
           Exact-Positive-Integer Exact-Positive-Integer
           Natural Natural -> Void)]
         [make-bitmap
          (Exact-Positive-Integer Exact-Positive-Integer -> (Instance Bitmap%))]
         [on-paint (-> Void)]
         [on-scroll (Any -> Void)]
         [refresh-now
          (->* ()
               ((-> (Instance DC<%>) Any) #:flush? Any)
               Void)]
         [scroll ((Option Real) (Option Real) -> Void)]
         [set-scroll-page
          ((U 'horizontal 'vertical) Exact-Positive-Integer -> Void)]
         [set-scroll-pos
          ((U 'horizontal 'vertical) Exact-Positive-Integer -> Void)]
         [set-scroll-range
          ((U 'horizontal 'vertical) Exact-Positive-Integer -> Void)]
         [show-scrollbars (Any Any -> Void)]
         [swap-gl-buffers (-> Void)]
         [with-gl-context ((-> Any) [#:fail (-> Any)] -> Any)]))

(define-type Cursor%
  (Class (init [id  Symbol])
         [ok? (-> Boolean)]))

(define-type Frame%
  (Class #:implements Top-Level-Window<%>
         (init [label String]
               [parent (Option (Instance Frame%)) #:optional]
               [width (Option Integer) #:optional]
               [height (Option Integer) #:optional]
               [x (Option Integer) #:optional]
               [y (Option Integer) #:optional]
               [style (Listof (U 'no-resize-border 'no-caption
                                 'no-system-menu 'hide-menu-bar
                                 'toolbar-button 'float 'metal))
                      #:optional]
               [enabled Any #:optional]
               [border Natural #:optional]
               [spacing Natural #:optional]
               [alignment (List (U 'left 'center 'right)
                                (U 'top 'center 'bottom))
                          #:optional]
               [min-width (Option Natural) #:optional]
               [min-height (Option Natural) #:optional]
               [stretchable-width Any #:optional]
               [stretchable-height Any #:optional])
         [create-status-line (-> Void)]
         [get-menu-bar (-> (Option (Instance Menu-Bar%)))]
         [has-status-line? (-> Boolean)]
         [iconize (Any -> Void)]
         [is-iconized? (-> Boolean)]
         [is-maximized? (-> Boolean)]
         [maximize (Any -> Void)]
         [modified (case-> (-> Boolean) (Any -> Void))]
         [on-menu-char ((Instance Key-Event%) -> Boolean)]
         [on-toolbar-button-click (-> Void)]
         [set-status-text (String -> Void)]))

(define-type Dialog%
  (Class #:implements Top-Level-Window<%>
         (init [label String]
               [parent (U #f (Instance Dialog%) (Instance Frame%))
                       #:optional]
               [width (Option Natural) #:optional]
               [height (Option Natural) #:optional]
               [x (Option Natural) #:optional]
               [y (Option Natural) #:optional]
               [style (Listof (U 'no-caption 'resize-border
                                 'no-sheet 'close-button))
                      #:optional]
               [enabled Any #:optional]
               [border Natural #:optional]
               [spacing Natural #:optional]
               [alignment (List (U 'left 'center 'right)
                                (U 'top 'center 'bottom))
                          #:optional]
               [min-width (Option Natural) #:optional]
               [min-height (Option Natural) #:optional]
               [stretchable-width Any #:optional]
               [stretchable-height Any #:optional])
         [show-without-yield (-> Void)]))

(define-type Text-Field%
  (Class #:implements Control<%>
         (init [label (Option String)]
               [parent (Instance Area-Container<%>)] ; FIXME
               [callback ((Instance Text-Field%)
                          (Instance Control-Event%) -> Any)
                         #:optional]
               [init-value String #:optional]
               [style (Listof (U 'single 'multiple 'hscroll 'password
                                 'vertical-label 'horizontal-label
                                 'deleted))
                      #:optional]
               [font (Instance Font%) #:optional]
               [enabled Any #:optional]
               [vert-margin Natural #:optional]
               [horiz-margin Natural #:optional]
               [min-width (Option Natural) #:optional]
               [min-height (Option Natural) #:optional]
               [stretchable-width Any #:optional]
               [stretchable-height Any #:optional])
         [get-editor (-> (Instance Text%))]
         [get-field-background (-> (Instance Color%))]
         [get-value (-> String)]
         [set-field-background ((Instance Color%) -> Void)]
         [set-value (String -> Void)]))

(define-type Combo-Field%
  (Class #:implements Text-Field%
         (init [label (Option String)]
               [choices (Listof String)]
               [parent (Instance Area-Container<%>)] ; FIXME
               [callback ((Instance Combo-Field%)
                          (Instance Control-Event%) -> Any)
                         #:optional]
               [init-value String #:optional]
               [style (Listof (U 'vertical-label 'horizontal-label
                                 'deleted))
                      #:optional]
               [font (Instance Font%) #:optional]
               [enabled Any #:optional]
               [vert-margin Natural #:optional]
               [horiz-margin Natural #:optional]
               [min-width (Option Natural) #:optional]
               [min-height (Option Natural) #:optional]
               [stretchable-width Any #:optional]
               [stretchable-height Any #:optional])
         [append (String -> Void)]
         [get-menu (-> (Instance Popup-Menu%))]
         [on-popup ((Instance Control-Event%) -> Void)]))

(define-type Check-Box%
  (Class #:implements Control<%>
         (init [label (U String (Instance Bitmap%))]
               [parent (Instance Area-Container<%>)] ; FIXME
               [callback ((Instance Check-Box%)
                          (Instance Control-Event%) -> Any)
                         #:optional]
               [style (Listof 'deleted) #:optional]
               [value Any #:optional]
               [font (Instance Font%) #:optional]
               [enabled Any #:optional]
               [vert-margin Natural #:optional]
               [horiz-margin Natural #:optional]
               [min-width (Option Natural) #:optional]
               [min-height (Option Natural) #:optional]
               [stretchable-width Any #:optional]
               [stretchable-height Any #:optional])
         [get-value (-> Boolean)]
         [set-label ((U String (Instance Bitmap%)) -> Void)]
         [set-value (Any -> Void)]))

(define-type Gauge%
  (Class #:implements Control<%>
         (init [label (Option String)]
               [range Integer]
               [parent (Instance Area-Container<%>)] ; FIXME
               [style (Listof (U 'horizontal 'vertical
                                 'vertical-label 'horizontal-label
                                 'deleted)) #:optional]
               [font (Instance Font%) #:optional]
               [enabled Any #:optional]
               [vert-margin Natural #:optional]
               [horiz-margin Natural #:optional]
               [min-width (Option Natural) #:optional]
               [min-height (Option Natural) #:optional]
               [stretchable-width Any #:optional]
               [stretchable-height Any #:optional])
         [get-range (-> Positive-Integer)]
         [get-value (-> Natural)]
         [set-range (Integer -> Void)]
         [set-value (Integer -> Void)]))

(define-type Radio-Box%
  (Class #:implements Control<%>
         (init [label (Option String)]
               [choices (U (Listof String) (Listof (Instance Bitmap%)))]
               [parent (Instance Area-Container<%>)] ; FIXME
               [callback ((Instance Radio-Box%)
                          (Instance Control-Event%) -> Any)
                         #:optional]
               [style (Listof (U 'horizontal 'vertical
                                 'vertical-label 'horizontal-label
                                 'deleted))
                 #:optional]
               [selection (Option Integer) #:optional]
               [font (Instance Font%) #:optional]
               [enabled Any #:optional]
               [vert-margin Natural #:optional]
               [horiz-margin Natural #:optional]
               [min-width (Option Natural) #:optional]
               [min-height (Option Natural) #:optional]
               [stretchable-width Any #:optional]
               [stretchable-height Any #:optional])
         [enable (case-> (Any -> Void)
                         (Integer Any -> Void))]
         [get-item-label (Integer -> String)]
         [get-item-plain-label (Integer -> String)]
         [get-number (-> Natural)]
         [get-selection (-> (Option Natural))]
         [is-enabled? (case-> (-> Boolean)
                              (Integer -> Boolean))]
         [set-selection ((Option Integer) -> Void)]))

(define-type Slider%
  (Class #:implements Control<%>
         (init [label (Option String)]
               [min-value Integer]
               [max-value Integer]
               [parent (Instance Area-Container<%>)] ; FIXME
               [callback ((Instance Slider%)
                          (Instance Control-Event%) -> Any)
                         #:optional]
               [init-value Integer #:optional]
               [style (Listof (U 'horizontal 'vertical 'plain
                                 'vertical-label 'horizontal-label
                                 'deleted))
                 #:optional]
               [font (Instance Font%) #:optional]
               [enabled Any #:optional]
               [vert-margin Natural #:optional]
               [horiz-margin Natural #:optional]
               [min-width (Option Natural) #:optional]
               [min-height (Option Natural) #:optional]
               [stretchable-width Any #:optional]
               [stretchable-height Any #:optional])
         [get-value (-> Natural)]
         [set-value (Integer -> Void)]))

(define-type Choice%
  (Class #:implements List-Control<%>
         (init [label String]
               [parent (Instance Area-Container<%>)] ; FIXME
               [choices (Listof String)]
               [callback ((Instance Choice%)
                          (Instance Control-Event%) -> Any)
                         #:optional]
               [style (Listof (U 'single 'multiple 'hscroll 'password
                                 'vertical-label 'horizontal-label
                                 'deleted))
                      #:optional]
               [selection Integer #:optional]
               [font (Instance Font%) #:optional]
               [enabled Any #:optional]
               [vert-margin Natural #:optional]
               [horiz-margin Natural #:optional]
               [min-width (Option Natural) #:optional]
               [min-height (Option Natural) #:optional]
               [stretchable-width Any #:optional]
               [stretchable-height Any #:optional])))

(define-type List-Box%
  (Class #:implements List-Control<%>
         (init [label (Option String)]
               [choices (Listof String)]
               [parent (Instance Area-Container<%>)] ; FIXME
               [callback ((Instance List-Box%)
                          (Instance Control-Event%) -> Any)
                         #:optional]
               [style (Listof (U 'single 'multiple 'extended
                                 'vertical-label 'horizontal-label
                                 'variable-columns 'column-headers
                                 'clickable-headers 'reorderable-headers
                                 'deleted))
                      #:optional]
               [selection (Option Integer) #:optional]
               [font (Instance Font%) #:optional]
               [label-font (Instance Font%) #:optional]
               [enabled Any #:optional]
               [vert-margin Natural #:optional]
               [horiz-margin Natural #:optional]
               [min-width (Option Natural) #:optional]
               [min-height (Option Natural) #:optional]
               [stretchable-width Any #:optional]
               [stretchable-height Any #:optional]
               [columns (Pairof String (Listof String)) #:optional]
               [column-order (Option (Listof Integer)) #:optional])
         [append (case-> (String -> Void)
                         (String Any -> Void))]
         [append-column (String -> Void)]
         [delete-column (Integer -> Void)]
         [get-column-labels
          (-> (Pairof String (Listof String)))]
         [get-column-order (-> (Listof Natural))]
         [get-column-width
          (Integer -> (Values Natural Natural Natural))]
         [get-data (Integer -> Any)]
         [get-first-visible-item (-> Natural)]
         [get-label-font (-> (Instance Font%))]
         [get-selections (-> (Listof Natural))]
         [is-selected? (Integer -> Boolean)]
         [number-of-visible-items (-> Positive-Integer)]
         [select (case-> (Integer -> Void)
                         (Integer Any -> Void))]
         [set ((Listof String) -> Void)]
         [set-column-label (Integer String -> Void)]
         [set-column-order ((Listof Integer) -> Void)]
         [set-column-width
          (Integer Integer Integer Integer -> Void)]
         [set-data (Integer Any -> Void)]
         [set-first-visible-item (Integer -> Void)]
         [set-string
          (case-> (Integer String -> Void)
                  (Integer String Integer -> Void))]))

(define-type Menu%
  (Class #:implements Menu-Item-Container<%>
         #:implements Labelled-Menu-Item<%>
         (init [label String]
               [parent (U (Instance Menu%) (Instance Popup-Menu%)
                          (Instance Menu-Bar%))]
               [help-string (Option String) #:optional]
               [demand-callback ((Instance Menu%) -> Any)
                                #:optional])))

(define-type Menu-Bar%
  (Class #:implements Menu-Item-Container<%>
         (init [parent (U (Instance Frame%) 'root)]
               [demand-callback ((Instance Menu-Bar%) -> Any)
                                #:optional])
         [enable (Any -> Void)]
         [get-frame (-> (U (Instance Frame%) 'root))]
         [is-enabled? (-> Boolean)]))

(define-type Menu-Item<%>
  (Class [delete (-> Void)]
         [get-parent (-> (U (Instance Menu%) (Instance Popup-Menu%)
                            (Instance Menu-Bar%)))]
         [is-deleted? (-> Boolean)]
         [restore (-> Void)]))

(define-type Separator-Menu-Item%
  (Class #:implements Menu-Item<%>
         (init [parent (U (Instance Menu%) (Instance Popup-Menu%))
                       #:optional])))

(define-type Labelled-Menu-Item<%>
  (Class #:implements Menu-Item<%>
         [enable (Any -> Void)]
         [get-help-string (-> (Option String))]
         [get-label (-> String)]
         [get-plain-label (-> String)]
         [is-enabled? (-> Boolean)]
         [on-demand (-> Void)]
         [set-help-string ((Option String) -> Void)]
         [set-label (String -> Void)]))

(define-type Selectable-Menu-Item<%>
  (Class #:implements Labelled-Menu-Item<%>
         [command ((Instance Control-Event%) -> Void)]
         [get-shortcut (-> (U Char Symbol #f))]
         [get-shortcut-prefix
          (-> (Listof (U 'alt 'cmd 'meta 'ctl 'shift 'option)))]
         [set-shortcut ((U Char Symbol #f) -> Void)]
         [set-shortcut-prefix
          ((Listof (U 'alt 'cmd 'meta 'ctl 'shift 'option)) -> Void)]))

(define-type Checkable-Menu-Item%
  (Class #:implements Selectable-Menu-Item<%>
         (init [label String]
               [parent (U (Instance Menu%) (Instance Popup-Menu%))]
               [callback ((Instance Checkable-Menu-Item%) (Instance Control-Event%)
                          -> Any)]
               [shortcut (U Char Symbol #f) #:optional]
               [help-string (Option String) #:optional]
               [demand-callback
                ((Instance Checkable-Menu-Item%) -> Any)
                #:optional]
               [shortcut-prefix (Listof (U 'alt 'cmd 'meta 'ctl
                                           'shift 'option))
                                #:optional])
         [check (Any -> Void)]
         [is-checked? (-> Boolean)]))

(define-type Menu-Item-Container<%>
  (Class [get-items (-> (Listof (Instance Menu-Item<%>)))]
         [on-demand (-> Void)]))

(define-type Menu-Item%
  (Class #:implements Selectable-Menu-Item<%>
         (init [label String]
               [parent (U (Instance Menu%) (Instance Popup-Menu%))]
               [callback ((Instance Menu-Item%) (Instance Control-Event%)
                          -> Any)]
               [shortcut (U Char Symbol #f) #:optional]
               [help-string (Option String) #:optional]
               [demand-callback
                ((Instance Menu-Item%) -> Any)
                #:optional]
               [shortcut-prefix (Listof (U 'alt 'cmd 'meta 'ctl
                                           'shift 'option))
                                #:optional])))

(define-type Popup-Menu%
  (Class #:implements Menu-Item-Container<%>
         (init [title (Option String) #:optional]
               [popdown-callback
                ((Instance Popup-Menu%) (Instance Control-Event%) -> Any)
                #:optional]
               [demand-callback (Any -> Any) #:optional]
               [font (Instance Font%) #:optional])
         [get-font (-> (Instance Font%))]
         [get-popup-target (-> (Option (U (Instance Window<%>)
                                          (Instance Editor<%>))))]
         [set-min-width (Natural -> Void)]))

(define-type Clipboard-Client%
  (Class [add-type (String -> Void)]
         [get-data (String -> (U Bytes String #f))]
         [get-types (-> (Listof String))]
         [on-replaced (-> Void)]))

(define-type Clipboard<%>
  (Class [get-clipboard-bitmap (Integer -> (Option (Instance Bitmap%)))]
         [get-clipboard-data (String Integer -> (U Bytes String #f))]
         [get-clipboard-string (Integer -> String)]
         [same-clipboard-client?
          ((Instance Clipboard-Client%) -> Boolean)]
         [set-clipboard-bitmap
          ((Instance Bitmap%) Integer -> Void)]
         [set-clipboard-client
          ((Instance Clipboard-Client%) Integer -> Void)]
         [set-clipboard-string
          (String Integer -> Void)]))

(define-type Control-Event-Type
  (U 'button 'check-box 'choice
     'list-box 'list-box-dclick 'list-box-column
     'text-field 'text-field-enter
     'menu 'slider 'radio-box 'tab-panel
     'menu-popdown 'menu-popdown-none))

(define-type Event%
  (Class (init [time-stamp Integer #:optional])
         [get-time-stamp (-> Integer)]
         [set-time-stamp (Integer -> Void)]))

(define-type Control-Event%
  (Class #:implements Event%
         (init [event-type Control-Event-Type]
               [time-stamp Integer #:optional])
         [get-event-type (-> Control-Event-Type)]
         [set-event-type (Control-Event-Type -> Void)]))

(define-type Column-Control-Event%
  (Class #:implements Control-Event%
         (init [column Integer]
               [event-type 'list-box-column]
               [time-stamp Integer #:optional])
         [get-column (-> Natural)]
         [set-column (Integer -> Void)]))

(define-type Key-Event%
  (Class #:implements Event%
         (init [key-code (U Char Symbol) #:optional]
               [x Integer #:optional]
               [y Integer #:optional]
               [shift-down Any #:optional]
               [control-down Any #:optional]
               [meta-down Any #:optional]
               [alt-down Any #:optional]
               [time-stamp Integer #:optional]
               [caps-down Any #:optional])
         [get-alt-down (-> Boolean)]
         [get-caps-down (-> Boolean)]
         [get-control-down (-> Boolean)]
         [get-key-code (-> (U Char Symbol))]
         [get-key-release-code (-> (U Char Symbol))]
         [get-meta-down (-> Boolean)]
         [get-other-altgr-key-code (-> (Option (U Char Symbol)))]
         [get-other-caps-key-code (-> (Option (U Char Symbol)))]
         [get-other-shift-key-code (-> (Option (U Char Symbol)))]
         [get-shift-down (-> Boolean)]
         [get-x (-> Integer)]
         [get-y (-> Integer)]
         [set-alt-down (Any -> Void)]
         [set-caps-down (Any -> Void)]
         [set-control-down (Any -> Void)]
         [set-key-code ((U Char Symbol) -> Void)]
         [set-key-release-code ((U Char Symbol) -> Void)]
         [set-meta-down (Any -> Void)]
         [set-other-altgr-key-code ((U Char Symbol False) -> Void)]
         [set-other-caps-key-code ((U Char Symbol False) -> Void)]
         [set-other-shift-key-code ((U Char Symbol False) -> Void)]
         [set-shift-down (Any -> Void)]
         [set-x (Integer -> Void)]
         [set-y (Integer -> Void)]))

(define-type Mouse-Event%
  (Class #:implements Event%
         (init [event-type (U 'enter 'leave 'left-down 'left-up
                              'middle-down 'middle-up
                              'right-down 'right-up 'motion)]
               [left-down Any #:optional]
               [middle-down Any #:optional]
               [right-down Any #:optional]
               [x Integer #:optional]
               [y Integer #:optional]
               [shift-down Any #:optional]
               [control-down Any #:optional]
               [meta-down Any #:optional]
               [alt-down Any #:optional]
               [time-stamp Integer #:optional]
               [caps-down Any #:optional])
         [button-changed? (case-> (-> Boolean)
                                  ((U 'left 'middle 'right 'any) -> Boolean))]
         [button-down? (case-> (-> Boolean)
                               ((U 'left 'middle 'right 'any) -> Boolean))]
         [button-up? (case-> (-> Boolean)
                             ((U 'left 'middle 'right 'any) -> Boolean))]
         [dragging? (-> Boolean)]
         [entering? (-> Boolean)]
         [get-event-type (-> (U 'enter 'leave 'left-down 'left-up
                                'middle-down 'middle-up
                                'right-down 'right-up 'motion))]
         [get-alt-down (-> Boolean)]
         [get-caps-down (-> Boolean)]
         [get-control-down (-> Boolean)]
         [get-left-down (-> Boolean)]
         [get-meta-down (-> Boolean)]
         [get-middle-down (-> Boolean)]
         [get-right-down (-> Boolean)]
         [get-shift-down (-> Boolean)]
         [get-x (-> Integer)]
         [get-y (-> Integer)]
         [leaving? (-> Boolean)]
         [moving? (-> Boolean)]
         [set-event-type ((U 'enter 'leave 'left-down 'left-up
                             'middle-down 'middle-up
                             'right-down 'right-up 'motion)
                          ->
                          Void)]
         [set-alt-down (Any -> Void)]
         [set-caps-down (Any -> Void)]
         [set-control-down (Any -> Void)]
         [set-left-down (Any -> Void)]
         [set-meta-down (Any -> Void)]
         [set-middle-down (Any -> Void)]
         [set-right-down (Any -> Void)]
         [set-shift-down (Any -> Void)]
         [set-x (Integer -> Void)]
         [set-y (Integer -> Void)]))

(define-type Scroll-Event%
  (Class #:implements Event%
         (init [event-type (U 'top 'buttom 'line-up 'line-down
                              'page-up 'page-down 'thumb)
                           #:optional]
               [direction (U 'horizontal 'vertical)
                          #:optional]
               [position Integer #:optional]
               [time-stamp Integer #:optional])
         [get-direction (-> (U 'horizontal 'vertical))]
         [get-event-type (-> (U 'top 'buttom 'line-up 'line-down
                                'page-up 'page-down 'thumb))]
         [get-position (-> Natural)]
         [set-direction ((U 'horizontal 'vertical) -> Void)]
         [set-event-type ((U 'top 'buttom 'line-up 'line-down
                             'page-up 'page-down 'thumb)
                          ->
                          Void)]
         [set-position (Integer -> Void)]))

(define-type Pane%
  (Class #:implements Area-Container<%>
         #:implements Subarea<%>
         (init [parent (U (Instance Frame%) (Instance Dialog%)
                          (Instance Panel%) (Instance Pane%))]
               [vert-margin Natural #:optional]
               [horiz-margin Natural #:optional]
               [border Natural #:optional]
               [spacing Natural #:optional]
               [alignment (List (U 'left 'center 'right)
                                (U 'top 'center 'bottom))
                          #:optional]
               [min-width (Option Natural) #:optional]
               [min-height (Option Natural) #:optional]
               [stretchable-width Any #:optional]
               [stretchable-height Any #:optional])))

(define-type Grow-Box-Spacer-Pane%
  (Class #:implements Pane%
          (init [parent (Instance Area-Container<%>)] ; FIXME
                [vert-margin Natural #:optional]
                [horiz-margin Natural #:optional]
                [border Natural #:optional]
                [spacing Natural #:optional]
                [alignment (List (U 'left 'center 'right)
                                 (U 'top 'center 'bottom))
                           #:optional]
                [min-width (Option Natural) #:optional]
                [min-height (Option Natural) #:optional]
                [stretchable-width Any #:optional]
                [stretchable-height Any #:optional])))

(define-type Horizontal-Pane%
  (Class #:implements Pane%
         (init [parent (Instance Area-Container<%>)] ; FIXME
               [vert-margin Natural #:optional]
               [horiz-margin Natural #:optional]
               [border Natural #:optional]
               [spacing Natural #:optional]
               [alignment (List (U 'left 'center 'right)
                                (U 'top 'center 'bottom))
                          #:optional]
               [min-width (Option Natural) #:optional]
               [min-height (Option Natural) #:optional]
               [stretchable-width Any #:optional]
               [stretchable-height Any #:optional])))

(define-type Vertical-Pane%
  (Class #:implements Pane%
         (init [parent (Instance Area-Container<%>)] ; FIXME
               [vert-margin Natural #:optional]
               [horiz-margin Natural #:optional]
               [border Natural #:optional]
               [spacing Natural #:optional]
               [alignment (List (U 'left 'center 'right)
                                (U 'top 'center 'bottom))
                          #:optional]
               [min-width (Option Natural) #:optional]
               [min-height (Option Natural) #:optional]
               [stretchable-width Any #:optional]
               [stretchable-height Any #:optional])))

(define-type Panel%
  (Class #:implements Area-Container-Window<%>
         #:implements Subwindow<%>
         (init [parent (Instance Area-Container<%>)] ; FIXME
               [style (Listof (U 'border 'deleted
                                 'hscroll 'auto-hscroll
                                 'vscroll 'auto-vscroll))
                      #:optional]
               [enabled Any #:optional]
               [vert-margin Natural #:optional]
               [horiz-margin Natural #:optional]
               [border Natural #:optional]
               [spacing Natural #:optional]
               [alignment (List (U 'left 'center 'right)
                                (U 'top 'center 'bottom))
                          #:optional]
               [min-width (Option Natural) #:optional]
               [min-height (Option Natural) #:optional]
               [stretchable-width Any #:optional]
               [stretchable-height Any #:optional])))

(define-type Horizontal-Panel%
  (Class #:implements Panel%
         (init [parent (Instance Area-Container<%>)] ; FIXME
               [style (Listof (U 'border 'deleted
                                 'hscroll 'auto-hscroll
                                 'vscroll 'auto-vscroll))
                      #:optional]
               [enabled Any #:optional]
               [vert-margin Natural #:optional]
               [horiz-margin Natural #:optional]
               [border Natural #:optional]
               [spacing Natural #:optional]
               [alignment (List (U 'left 'center 'right)
                                (U 'top 'center 'bottom))
                          #:optional]
               [min-width (Option Natural) #:optional]
               [min-height (Option Natural) #:optional]
               [stretchable-width Any #:optional]
               [stretchable-height Any #:optional])
         [set-orientation (Boolean -> Void)]
         [get-orientation (-> Boolean)]))

(define-type Vertical-Panel%
  (Class #:implements Panel%
         (init [parent (Instance Area-Container<%>)] ; FIXME
               [style (Listof (U 'border 'deleted
                                 'hscroll 'auto-hscroll
                                 'vscroll 'auto-vscroll))
                      #:optional]
               [enabled Any #:optional]
               [vert-margin Natural #:optional]
               [horiz-margin Natural #:optional]
               [border Natural #:optional]
               [spacing Natural #:optional]
               [alignment (List (U 'left 'center 'right)
                                (U 'top 'center 'bottom))
                          #:optional]
               [min-width (Option Natural) #:optional]
               [min-height (Option Natural) #:optional]
               [stretchable-width Any #:optional]
               [stretchable-height Any #:optional])
         [set-orientation (Boolean -> Void)]
         [get-orientation (-> Boolean)]))

(define-type Group-Box-Panel%
  (Class #:implements Vertical-Panel%
         (init [label String]
               [parent (Instance Area-Container<%>)] ; FIXME
               [style (Listof 'deleted) #:optional]
               [font (Instance Font%) #:optional]
               [enabled Any #:optional]
               [vert-margin Natural #:optional]
               [horiz-margin Natural #:optional]
               [border Natural #:optional]
               [spacing Natural #:optional]
               [alignment (List (U 'left 'center 'right)
                                (U 'top 'center 'bottom))
                          #:optional]
               [min-width (Option Natural) #:optional]
               [min-height (Option Natural) #:optional]
               [stretchable-width Any #:optional]
               [stretchable-height Any #:optional])))

(define-type Tab-Panel%
  (Class #:implements Vertical-Panel%
         (init [choices (Listof String)]
               [parent (Instance Area-Container<%>)] ; FIXME
               [callback
                ((Instance Tab-Panel%) (Instance Control-Event%)
                 -> Any)
                #:optional]
               [style (Listof (U 'no-border 'deleted))
                      #:optional]
               [font (Instance Font%) #:optional]
               [enabled Any #:optional]
               [vert-margin Natural #:optional]
               [horiz-margin Natural #:optional]
               [border Natural #:optional]
               [spacing Natural #:optional]
               [alignment (List (U 'left 'center 'right)
                                (U 'top 'center 'bottom))
                          #:optional]
               [min-width (Option Natural) #:optional]
               [min-height (Option Natural) #:optional]
               [stretchable-width Any #:optional]
               [stretchable-height Any #:optional])
         [append (String -> Void)]
         [delete (Integer -> Void)]
         [get-item-label (Integer -> String)]
         [get-number (-> Natural)]
         [get-selection (-> (Option Natural))]
         [set ((Listof String) -> Void)]
         [set-item-label (Integer String -> Void)]
         [set-selection (Integer -> Void)]))

(define-type Control<%>
  (Class #:implements Subwindow<%>
         [command ((Instance Control-Event%) -> Void)]))

(define-type List-Control<%>
  (Class #:implements Control<%>
         [append (String -> Void)]
         [clear (-> Void)]
         [delete (Integer -> Void)]
         [find-string (String -> (Option Natural))]
         [get-number (-> Natural)]
         [get-selection (-> (Option Natural))]
         [get-string (Integer -> String)]
         [get-string-selection (-> (Option String))]
         [set-selection (Integer -> Void)]
         [set-string-selection (String -> Void)]))

(define-type Message%
  (Class #:implements Control<%>
         (init [parent (Instance Area-Container<%>)] ; FIXME
               [label (U String (Instance Bitmap%)
                         (U 'app 'caution 'stop))]
               [style (Listof 'deleted) #:optional]
               [font (Instance Font%) #:optional]
               [enabled Any #:optional]
                 [vert-margin Natural #:optional]
               [horiz-margin Natural #:optional]
               [border Natural #:optional]
               [spacing Natural #:optional]
               [alignment (List (U 'left 'center 'right)
                                (U 'top 'center 'bottom))
                          #:optional]
               [min-width (Option Natural) #:optional]
               [min-height (Option Natural) #:optional]
               [stretchable-width Any #:optional]
               [stretchable-height Any #:optional]
               [auto-resize Any #:optional])
         [set-label ((U String (Instance Bitmap%)) -> Void)]
         [auto-resize (case-> (-> Boolean)
                              (Any -> Void))]))

(define-type Editor-Canvas%
  (Class #:implements Canvas<%>
         (init [parent (Instance Area-Container<%>)] ; FIXME
               [editor (U (Instance Pasteboard%) (Instance Text%) #f) #:optional]
               [style (Listof (U 'no-border 'control-border 'combo
                                 'no-hscroll 'no-vscroll
                                 'hide-hscroll 'hide-vscroll
                                 'auto-vscoll 'auto-hscroll
                                 'resize-corner 'no-focus 'deleted
                                 'transparent))
                      #:optional]
               [scrolls-per-page Positive-Integer #:optional]
               [label (Option String) #:optional]
               [wheel-step (Option Positive-Integer) #:optional]
               [line-count (Option Positive-Integer) #:optional]
               [horizontal-inset Natural #:optional]
               [vertical-inset Natural #:optional]
               [enabled Any #:optional]
               [vert-margin Natural #:optional]
               [horiz-margin Natural #:optional]
               [min-width (Option Natural) #:optional]
               [min-height (Option Natural) #:optional]
               [stretchable-width Any #:optional]
               [stretchable-height Any #:optional])
         [allow-scroll-to-last
          (case-> (-> Boolean) (Any -> Void))]
         [allow-tab-exit
          (case-> (-> Boolean) (Any -> Void))]
         [call-as-primary-owner ((-> Any) -> Any)]
         [force-display-focus
          (case-> (-> Boolean) (Any -> Void))]
         [get-editor (-> (Option (U (Instance Text%) (Instance Pasteboard%))))]
         [get-line-count (-> (Option Positive-Integer))]
         [horizontal-inset
          (case-> (-> Positive-Integer)
                  (Positive-Integer -> Void))]
         [lazy-refresh
          (case-> (-> Boolean) (Any -> Void))]
         [on-char ((Instance Key-Event%) -> Void)]
         [on-event ((Instance Mouse-Event%) -> Void)]
         [on-focus (Any -> Void)]
         [on-paint (-> Void)]
         [on-size (Natural Natural -> Void)]
         [scroll-to
          (case-> (Real Real
                   Nonnegative-Real Nonnegative-Real
                   Any -> Boolean)
                  (Real Real
                   Nonnegative-Real Nonnegative-Real
                   Any (U 'start 'end 'none) -> Boolean))]
         [scroll-with-bottom-base
          (case-> (-> Boolean) (Any -> Void))]
         [set-editor
          (case-> ((Option (U (Instance Text%) (Instance Pasteboard%))) -> Void)
                  ((Option (U (Instance Text%) (Instance Pasteboard%))) Any -> Void))]
         [set-line-count ((U #f Positive-Integer) -> Void)]
         [vertical-inset
          (case-> (-> Positive-Integer)
                  (Positive-Integer -> Void))]
         [wheel-step
          (case-> (-> (Option Positive-Integer))
                  ((Option Positive-Integer) -> Void))]))

(define-type Timer%
  (Class (init [notify-callback (-> Any) #:optional]
               [interval (Option Integer) #:optional]
               [just-once? Any #:optional])
         [interval (-> (Option Natural))]
         [notify (-> Void)]
         [start (case-> (Integer -> Void)
                        (Integer Any -> Void))]
         [stop (-> Void)]))

(define-type Text%
  (Class #:implements Editor<%>
         (init [line-spacing Nonnegative-Real #:optional]
               [tab-stops (Listof Real) #:optional]
               [auto-wrap Any #:optional])
         [after-change-style (Integer Integer -> Void)]
         [after-delete (Integer Integer -> Void)]
         [after-insert (Integer Integer -> Void)]
         [after-merge-snips (Integer -> Void)]
         [after-set-position (-> Void)]
         [after-set-size-constraint (-> Void)]
         [after-split-snip (Integer -> Void)]
         [call-clickback (Integer Integer -> Void)]
         [can-change-style? (Integer Integer -> Boolean)]
         [can-delete? (Integer Integer -> Boolean)]
         [can-insert? (Integer Integer -> Boolean)]
         [can-set-size-constraint? (-> Boolean)]
         [caret-hidden? (-> Boolean)]
         [change-style
          (case-> ((U #f (Instance Style-Delta%) (Instance Style<%>)) -> Void)
                  ((U #f (Instance Style-Delta%) (Instance Style<%>))
                   (U Integer 'start) -> Void)
                  ((U #f (Instance Style-Delta%) (Instance Style<%>))
                   (U Integer 'start) (U Integer 'end) -> Void)
                  ((U #f (Instance Style-Delta%) (Instance Style<%>))
                   (U Integer 'start) (U Integer 'end) Any -> Void))]
         [copy
          (case-> (-> Void)
                  (Any -> Void)
                  (Any Integer -> Void)
                  (Any Integer (U Integer 'start) -> Void)
                  (Any Integer (U Integer 'start) (U Integer 'end) -> Void))]
         [copy-self-to ((U (Instance Text%) (Instance Pasteboard%)) -> Void)]
         [cut
          (case-> (-> Void)
                  (Any -> Void)
                  (Any Integer -> Void)
                  (Any Integer (U Integer 'start) -> Void)
                  (Any Integer (U Integer 'start) (U Integer 'end) -> Void))]
         [delete
          (case-> (-> Void)
                  ((U Integer 'start) -> Void)
                  ((U Integer 'start) (U Integer 'back) -> Void)
                  ((U Integer 'start) (U Integer 'back) Any -> Void))]
         [do-copy (Integer Integer Integer Any -> Void)]
         [do-paste (Integer Integer -> Void)]
         [do-paste-x-selection (Integer Integer -> Void)]
         [erase (-> Void)]
         [extend-position (Integer -> Void)]
         [find-line
          (case-> (Real -> Natural) (Real (Option (Boxof Any)) -> Void))]
         [find-newline
          (case-> (-> (Option Natural))
                  ((U 'forward 'backward) -> (Option Natural))
                  ((U 'forward 'backward) (U Integer 'start) -> (Option Natural))
                  ((U 'forward 'backward) (U Integer 'start) (U Integer 'eof) -> (Option Natural)))]
         [find-next-non-string-snip ((Option (Instance Snip%)) -> (Option (Instance Snip%)))]
         [find-position
          (case-> (Real Real -> Natural)
                  (Real Real (Option (Boxof Any)) -> Void)
                  (Real Real (Option (Boxof Any)) (Option (Boxof Any)) -> Void)
                  (Real Real (Option (Boxof Any))
                   (Option (Boxof Any)) (Option (Boxof Any)) -> Void))]
         [find-position-in-line
          (case-> (Integer Real -> Natural)
                  (Integer Real (Option (Boxof Any)) -> Void)
                  (Integer Real (Option (Boxof Any)) (Option (Boxof Any)) -> Void)
                  (Integer Real (Option (Boxof Any))
                   (Option (Boxof Any)) (Option (Boxof Any)) -> Void))]
         [find-snip
          (case-> (Integer (U 'before-or-none 'before 'after 'after-or-none)
                   -> (Option (Instance Snip%)))
                  (Integer (U 'before-or-none 'before 'after 'after-or-none)
                   (Option (Boxof Natural)) -> (Option (Instance Snip%))))]
         [find-string
          (case-> (String -> (Option Natural))
                  (String (U 'forward 'backward) -> (Option Natural))
                  (String (U 'forward 'backward) (U Integer 'start) -> (Option Natural))
                  (String (U 'forward 'backward) (U Integer 'start)
                   (U Integer 'eof) -> (Option Natural))
                  (String (U 'forward 'backward) (U Integer 'start)
                   (U Integer 'eof) Any -> (Option Natural))
                  (String (U 'forward 'backward) (U Integer 'start)
                   (U Integer 'eof) Any Any -> (Option Natural)))]
         [find-string-all
          (case-> (String -> (Listof Natural))
                  (String (U 'forward 'backward) -> (Listof Natural))
                  (String (U 'forward 'backward) (U Integer 'start) -> (Listof Natural))
                  (String (U 'forward 'backward) (U Integer 'start)
                   (U Integer 'eof) -> (Listof Natural))
                  (String (U 'forward 'backward) (U Integer 'start)
                   (U Integer 'eof) Any -> (Listof Natural))
                  (String (U 'forward 'backward) (U Integer 'start)
                   (U Integer 'eof) Any Any -> (Listof Natural)))]
         [find-wordbreak ((Option (Boxof Integer)) (Option (Boxof Integer))
                          (U 'caret 'line 'selection 'user1 'user2) -> Void)]
         [flash-off (-> Void)]
         [flash-on
          (case-> (Integer Integer -> Void)
                  (Integer Integer Any -> Void)
                  (Integer Integer Any Any -> Void)
                  (Integer Integer Any Any Integer -> Void))]
         [get-anchor (-> Boolean)]
         [get-between-threshold (-> Nonnegative-Real)]
         [get-character (Integer -> Char)]
         [get-end-position (-> Natural)]
         [get-extend-start-position (-> Natural)]
         [get-extend-end-position (-> Natural)]
         [get-file-format (-> (U 'standard 'text 'text-force-cr))]
         [get-line-spacing (-> Nonnegative-Real)]
         [get-overwrite-mode (-> Boolean)]
         [get-padding (-> (Values Nonnegative-Real Nonnegative-Real
                                  Nonnegative-Real Nonnegative-Real))]
         [get-position
          (case-> ((Option (Boxof Integer)) -> Void)
                  ((Option (Boxof Integer)) (Option (Boxof Integer)) -> Void))]
         [get-region-data (Integer Integer -> (Option Editor-Data%))]
         [get-revision-number (-> Nonnegative-Real)]
         [get-snip-position ((Instance Snip%) -> (Option Natural))]
         #| FIXME: final
         [get-snip-position-and-location
          (case->
           ((Instance Snip%) (Option (Boxof Integer)) -> (Option Natural))
           ((Instance Snip%) (Option (Boxof Integer))
            (Option (Boxof Real)) -> (Option Natural))
           ((Instance Snip%) (Option (Boxof Integer))
            (Option (Boxof Real)) (Option (Boxof Real)) -> (Option Natural)))]
         |#
         [get-start-position (-> Natural)]
         [get-styles-sticky (-> Boolean)]
         [get-tabs
          (case-> (-> (Listof Real))
                  ((Option (Boxof Integer)) -> (Listof Real))
                  ((Option (Boxof Integer)) (Option (Boxof Real)) -> (Listof Real))
                  ((Option (Boxof Integer)) (Option (Boxof Real))
                   (Option (Boxof Any)) -> (Listof Real)))]
         [get-text (Integer (U Integer 'eof) -> String)]
         [get-top-line-base (-> Nonnegative-Real)]
         [get-visible-line-range
          (case-> ((Option (Boxof Integer)) (Option (Boxof Real)) -> Void)
                  ((Option (Boxof Integer)) (Option (Boxof Real)) Any -> Void))]
         [get-visible-position-range
          (case-> ((Option (Boxof Integer)) (Option (Boxof Real)) -> Void)
                  ((Option (Boxof Integer)) (Option (Boxof Real)) Any -> Void))]
         #| FIXME
         [get-wordbreak-map (-> (Option (Instance Editor-Wordbreak-Map%)))]
         |#
         [hide-caret (-> Any Void)]
         [insert
          (case->
           ;; collapsed cases for contract generation
           ((U Char String (Instance Snip%)) -> Void)
           ((U Char String Integer (Instance Snip%))
            (U String Integer) -> Void)
           ((U Char String Integer (Instance Snip%))
            (U String Integer)
            (U Integer 'same)
            -> Void)
           ((U String Integer (Instance Snip%))
            (U String Integer)
            (U Integer 'same)
            Any
            -> Void)
           (Integer String Integer (U Integer 'same) Any -> Void))]
         [kill (case-> (-> Void)
                       (Integer -> Void)
                       (Integer Integer Integer -> Void))]
         [last-line (-> Natural)]
         [last-paragraph (-> Natural)]
         [last-position (-> Natural)]
         [line-end-position (case-> (Integer -> Integer) (Integer Any -> Integer))]
         [line-length (Integer -> Natural)]
         [line-location (case-> (Integer -> Integer) (Integer Any -> Integer))]
         [line-paragraph (Integer -> Natural)]
         [line-start-position (case-> (Integer -> Integer) (Integer Any -> Integer))]
         [move-position
          (case-> ((U 'home 'end 'right 'left 'up 'down) -> Void)
                  ((U 'home 'end 'right 'left 'up 'down) Any -> Void)
                  ((U 'home 'end 'right 'left 'up 'down) Any
                   (U 'simple 'word 'page 'line) -> Void))]
         [on-change-style (Integer Integer -> Void)]
         [on-default-char ((Instance Key-Event%) -> Void)]
         [on-default-event ((Instance Mouse-Event%) -> Void)]
         [on-delete (Integer Integer -> Void)]
         [on-insert (Integer Integer -> Void)]
         [on-new-string-snip (-> (Instance String-Snip%))]
         #| FIXME
         [on-new-tab-snip (-> (Instance Tab-Snip%))]
         |#
         [on-reflow (-> Void)]
         [on-set-size-constraint (-> Void)]
         [paragraph-end-line (Integer -> Integer)]
         [paragraph-end-position
          (case-> (Integer -> Natural)
                  (Integer Any -> Natural))]
         [paragraph-start-line (Integer -> Integer)]
         [paragraph-start-position
          (case-> (Integer -> Natural)
                  (Integer Any -> Natural))]
         [paste
          (case-> (-> Void)
                  (Integer -> Void)
                  (Integer (U Integer 'start 'end) -> Void)
                  (Integer (U Integer 'start 'end) (U Integer 'same) -> Void))]
         [paste-next (-> Void)]
         [paste-x-selection
          (case-> (-> Void)
                  (Integer -> Void)
                  (Integer (U Integer 'start 'end) -> Void)
                  (Integer (U Integer 'start 'end) (U Integer 'same) -> Void))]
         [position-line (case-> (Integer -> Integer) (Integer Any -> Integer))]
         [position-location
          (case-> (Integer -> Integer)
                  (Integer (Option (Boxof Real)) -> Integer)
                  (Integer (Option (Boxof Real)) (Option (Boxof Real)) -> Integer)
                  (Integer (Option (Boxof Real)) (Option (Boxof Real))
                   Any -> Integer)
                  (Integer (Option (Boxof Real)) (Option (Boxof Real))
                   Any Any -> Integer)
                  (Integer (Option (Boxof Real)) (Option (Boxof Real))
                   Any Any Any -> Integer))]
         [position-locations
          (case-> (Integer -> Integer)
                  (Integer (Option (Boxof Real)) -> Integer)
                  (Integer (Option (Boxof Real)) (Option (Boxof Real)) -> Integer)
                  (Integer (Option (Boxof Real)) (Option (Boxof Real))
                   (Option (Boxof Real)) -> Integer)
                  (Integer (Option (Boxof Real)) (Option (Boxof Real))
                   (Option (Boxof Real)) (Option (Boxof Real)) -> Integer)
                  (Integer (Option (Boxof Real)) (Option (Boxof Real))
                   (Option (Boxof Real)) (Option (Boxof Real)) Any -> Integer)
                  (Integer (Option (Boxof Real)) (Option (Boxof Real))
                   (Option (Boxof Real)) (Option (Boxof Real)) Any Any -> Integer))]
         [position-paragraph (case-> (Integer -> Integer) (Integer Any -> Integer))]
         [read-from-file
          (case-> ((Instance Editor-Stream-In%) -> Boolean)
                  ((Instance Editor-Stream-In%) Any -> Boolean)
                  ((Instance Editor-Stream-In%) (U Integer 'start) Any -> Boolean))]
         [remove-clickback (Integer Integer -> Void)]
         [scroll-to-position
          (case-> (Integer -> Boolean)
                  (Integer Any -> Boolean)
                  (Integer Any (U Integer 'same) -> Boolean)
                  (Integer Any (U Integer 'same) (U 'start 'end 'none) -> Boolean))]
         [set-anchor (Any -> Void)]
         [set-autowrap-bitmap ((Option (Instance Bitmap%)) -> (Option (Instance Bitmap%)))]
         [set-between-threshold (Real -> Void)]
         [set-clickback
          (case-> (Integer Integer ((Instance Text%) Natural Natural -> Any) -> Void)
                  (Integer Integer ((Instance Text%) Natural Natural -> Any)
                   (Option (Instance Style-Delta%)) -> Void)
                  (Integer Integer ((Instance Text%) Natural Natural -> Any)
                   (Option (Instance Style-Delta%)) Any -> Void))]
         [set-file-format ((U 'standard 'text 'text-force-cr) -> Void)]
         [set-line-spacing (Real -> Void)]
         [set-overwrite-mode (Any -> Void)]
         [set-padding (Real Real Real Real -> Void)]
         [set-paragraph-alignment (Integer (U 'left 'center 'right) -> Void)]
         [set-paragraph-margins (Integer Real Real Real -> Void)]
         [set-position
          (case-> (Integer -> Void)
                  (Integer (U Integer 'same) -> Void)
                  (Integer (U Integer 'same) Any -> Void)
                  (Integer (U Integer 'same) Any Any -> Void)
                  (Integer (U Integer 'same) Any Any (U 'default 'x 'local) -> Void))]
         [set-position-bias-scroll
          (case-> ((U 'start-only 'start 'none 'end 'end-only)
                   Integer -> Void)
                  ((U 'start-only 'start 'none 'end 'end-only)
                   Integer (U Integer 'same) -> Void)
                  ((U 'start-only 'start 'none 'end 'end-only)
                   Integer (U Integer 'same) Any -> Void)
                  ((U 'start-only 'start 'none 'end 'end-only)
                   Integer (U Integer 'same) Any Any -> Void)
                  ((U 'start-only 'start 'none 'end 'end-only)
                   Integer (U Integer 'same) Any Any (U 'default 'x 'local) -> Void))]
         [set-region-data (Integer Integer (Instance Editor-Data%) -> Void)]
         [set-styles-sticky (Any -> Void)]
         [set-tabs (case-> ((Listof Real) -> Void)
                           ((Listof Real) Real -> Void)
                           ((Listof Real) Real Any -> Void))]
         [set-wordbreak-func
          (((Instance Text%) (Option (Boxof Natural)) (Option (Boxof Natural))
            Symbol -> Any)
           -> Void)]
         #| FIXME
         [set-wordbreak-map ((Option (Instance Editor-Wordbreak-Map%)) -> Void)]
         |#
         [split-snip (Integer -> Void)]
         [write-to-file
          (case-> ((Instance Editor-Stream-Out%) -> Boolean)
                  ((Instance Editor-Stream-Out%) Integer -> Boolean)
                  ((Instance Editor-Stream-Out%) Integer (U Integer 'eof) -> Boolean))]
         (augment
          [after-change-style (Integer Integer -> Void)]
          [after-delete (Integer Integer -> Void)]
          [after-insert (Integer Integer -> Void)]
          [after-merge-snips (Integer -> Void)]
          [after-set-position (-> Void)]
          [after-set-size-constraint (-> Void)]
          [after-split-snip (Integer -> Void)]
          [can-change-style? (Integer Integer -> Boolean)]
          [can-delete? (Integer Integer -> Boolean)]
          [can-insert? (Integer Integer -> Boolean)]
          [can-set-size-constraint? (-> Boolean)]
          [on-change-style (Integer Integer -> Void)]
          [on-delete (Integer Integer -> Void)]
          [on-insert (Integer Integer -> Void)]
          [on-reflow (-> Void)]
          [on-set-size-constraint (-> Void)])))

(define-type Button%
  (Class #:implements Control<%>
         (init [label (U String (Instance Bitmap%)
                         (List (Instance Bitmap%) String
                               (U 'left 'top 'right 'bottom)))]
               [parent (Instance Area-Container<%>)] ; FIXME
               [callback ((Instance Button%) (Instance Control-Event%) -> Any)
                         #:optional]
               [style (Listof (U 'border 'deleted)) #:optional]
               [font (Instance Font%) #:optional]
               [enabled Any #:optional]
               [vert-margin Natural #:optional]
               [horiz-margin Natural #:optional]
               [min-width (Option Natural) #:optional]
               [min-height (Option Natural) #:optional]
               [stretchable-width Any #:optional]
               [stretchable-height Any #:optional])))

(define-type Printer-DC%
  (Class #:implements DC<%>
         (init [parent (U (Instance Frame%) (Instance Dialog%)
                          #f)
                       #:optional])))

;; editor classes

(provide Editor<%>
         Editor-Admin%
         Editor-Canvas%
         Editor-Data%
         Editor-Data-Class%
         Editor-Stream-In%
         Editor-Stream-Out%
         Editor-Stream-In-Base%
         Editor-Stream-Out-Base%
         Keymap%
         Pasteboard%
         Text%)

(define-type Edit-Operation
  (U 'undo 'redo 'clear 'cut 'copy 'paste
     'kill 'select-all 'insert-text-box
     'insert-pasteboard-box 'insert-image))

(define-type Load/Save-Format
  (U 'guess 'same 'copy 'standard 'text 'text-force-cr))

(define-type Editor<%>
  (Class [add-canvas ((Instance Editor-Canvas%) -> Void)]
         [add-undo ((-> Any) -> Void)]
         [adjust-cursor
          ((Instance Mouse-Event%) -> (Option (Instance Cursor%)))]
         [after-edit-sequence (-> Void)]
         [after-load-file (Any -> Void)]
         [after-save-file (Any -> Void)]
         [auto-wrap
          (case-> (-> Boolean) (Any -> Void))]
         [begin-edit-sequence
          (case-> (-> Void)
                  (Any -> Void)
                  (Any Any -> Void))]
         [begin-write-header-footer-to-file
          ((Instance Editor-Stream-Out%) String (Boxof Integer) -> Void)]
         [blink-caret (-> Void)]
         [can-do-edit-operation?
          (case-> (Edit-Operation -> Boolean)
                  (Edit-Operation Any -> Boolean))]
         [can-load-file? (Path Load/Save-Format -> Boolean)]
         [can-save-file? (Path Load/Save-Format -> Boolean)]
         [clear (-> Void)]
         [clear-undos (-> Void)]
         [copy (case-> (-> Void) (Any -> Void) (Any Integer -> Void))]
         [copy-self (-> (U (Instance Text%) (Instance Pasteboard%)))]
         [copy-self-to
          ((U (Instance Text%) (Instance Pasteboard%)) -> Void)]
         [cut (case-> (-> Void) (Any -> Void) (Any Integer -> Void))]
         [dc-location-to-editor-location (Real Real -> (Values Real Real))]
         [default-style-name (-> String)]
         [do-edit-operation
          (case-> (Edit-Operation -> Void)
                  (Edit-Operation Any -> Void)
                  (Edit-Operation Any Integer -> Void))]
         [editor-location-to-dc-location (Real Real -> (Values Real Real))]
         [end-edit-sequence (-> Void)]
         [end-write-header-footer-to-file
          ((Instance Editor-Stream-Out%) Integer -> Void)]
         [find-first-snip (-> (Option (Instance Snip%)))]
         [find-scroll-line (Real -> Natural)]
         [get-active-canvas (-> (Option (Instance Editor-Canvas%)))]
         [get-admin (-> (Option (Instance Editor-Admin%)))]
         [get-canvas (-> (Option (Instance Editor-Canvas%)))]
         [get-canvases (-> (Listof (Instance Editor-Canvas%)))]
         [get-dc (-> (Option (Instance DC<%>)))]
         [get-descent (-> Nonnegative-Real)]
         [get-extent
          ((Option (Boxof Real)) (Option (Boxof Real)) -> Void)]
         [get-file ((Option Path) -> (Option Path-String))]
         [get-filename
          ((Option (Boxof Any)) -> (Option Path-String))]
         [get-flattened-text (-> String)]
         [get-focus-snip (-> (Option (Instance Snip%)))]
         [get-inactive-caret-threshold (-> (U 'no-caret 'show-inactive-caret 'show-caret))]
         [get-keymap (-> (Option Keymap%))]
         [get-load-overwrites-styles (-> Boolean)]
         [get-max-height (-> (U Nonnegative-Real 'none))]
         [get-max-undo-history (-> (U Natural 'forever))]
         [get-max-view-size (-> (Values Real Real))]
         [get-max-width (-> (U Nonnegative-Real 'none))]
         [get-min-height (-> (U Nonnegative-Real 'none))]
         [get-min-width (-> (U Nonnegative-Real 'none))]
         [get-paste-text-only (-> Boolean)]
         [get-snip-data ((Instance Snip%) -> (Option (Instance Editor-Data%)))]
         [get-snip-location
          (case-> ((Instance Snip%) -> Boolean)
                  ((Instance Snip%) (Option (Boxof Real)) -> Boolean)
                  ((Instance Snip%) (Option (Boxof Real))
                   (Option (Boxof Real)) -> Boolean)
                  ((Instance Snip%) (Option (Boxof Real))
                   (Option (Boxof Real)) Any -> Boolean))]
         [get-space (-> Nonnegative-Real)]
         [get-style-list (-> (Instance Style-List%))]
         [get-view-size
          ((Option (Boxof Real)) (Option (Boxof Real)) -> Void)]
         [global-to-local
          ((Option (Boxof Real)) (Option (Boxof Real)) -> Void)]
         ;; FIXME: finality
         #|
         [in-edit-sequence? (-> Boolean)]
         |#
         [insert ((Instance Snip%) -> Void)]
         [insert-box ((U 'text 'pasteboard) -> Void)]
         [insert-file
          (case->
           (Path-String -> Boolean)
           (Path-String (U 'guess 'same 'copy 'standard
                           'text 'text-force-cr)
                        -> Boolean)
           (Path-String (U 'guess 'same 'copy 'standard
                           'text 'text-force-cr)
                        Any -> Boolean))]
         [insert-image
          (case->
           (-> Void)
           ((Option Path-String) -> Void)
           ((Option Path-String) (U 'unknown 'unknown/mask 'unknown/alpha
                                    'gif 'gif/mask 'gif/alpha
                                    'jpeg 'png 'png/mask 'png/alpha
                                    'xbm 'xpm 'bmp 'pict)
            -> Void)
           ((Option Path-String) (U 'unknown 'unknown/mask 'unknown/alpha
                                    'gif 'gif/mask 'gif/alpha
                                    'jpeg 'png 'png/mask 'png/alpha
                                    'xbm 'xpm 'bmp 'pict)
            Any -> Void)
           ((Option Path-String) (U 'unknown 'unknown/mask 'unknown/alpha
                                    'gif 'gif/mask 'gif/alpha
                                    'jpeg 'png 'png/mask 'png/alpha
                                    'xbm 'xpm 'bmp 'pict)
            Any Any -> Void))]
         [insert-port
          (case->
           (Input-Port -> (U 'standard 'text 'text-force-cr))
           (Input-Port (U 'guess 'same 'copy 'standard
                           'text 'text-force-cr)
                       -> (U 'standard 'text 'text-force-cr))
           (Input-Port (U 'guess 'same 'copy 'standard
                           'text 'text-force-cr)
                       Any -> (U 'standard 'text 'text-force-cr)))]
         [invalidate-bitmap-cache
          (case->
           (Real -> Void)
           (Real Real -> Void)
           (Real Real (U Real 'end 'display-end) -> Void)
           (Real Real (U Real 'end 'display-end) (U Real 'end 'display-end) -> Void))]
         [is-locked? (-> Boolean)]
         [is-modified? (-> Boolean)]
         [is-printing? (-> Boolean)]
         [kill (case-> (-> Void) (Integer -> Void))]
         [load-file
          (case->
           (-> Boolean)
           ((Option Path-String) -> Boolean)
           ((Option Path-String) Load/Save-Format
            -> Boolean)
           ((Option Path-String) Load/Save-Format
            Any -> Boolean))]
         [local-to-global
          ((Option (Boxof Real)) (Option (Boxof Real)) -> Void)]
         [locations-computed? (-> Boolean)]
         [lock (Any -> Void)]
         ;; FIXME: we don't handle final methods with contracts
         #|
         [locked-for-flow? (-> Boolean)]
         [locked-for-read? (-> Boolean)]
         [locked-for-write? (-> Boolean)]
         |#
         [needs-update
          ((Instance Snip%) Real Real Real Real -> Void)]
         [num-scroll-lines (-> Natural)]
         [on-change (-> Void)]
         [on-char ((Instance Key-Event%) -> Void)]
         [on-default-char ((Instance Key-Event%) -> Void)]
         [on-default-event ((Instance Mouse-Event%) -> Void)]
         [on-display-size (-> Void)]
         [on-display-size-when-ready (-> Void)]
         [on-edit-sequence (-> Void)]
         [on-event ((Instance Mouse-Event%) -> Void)]
         [on-focus (Any -> Void)]
         [on-load-file (Path Load/Save-Format -> Void)]
         [on-local-char ((Instance Key-Event%) -> Void)]
         [on-local-event ((Instance Mouse-Event%) -> Void)]
         [on-new-box ((U 'text 'pasteboard) -> (Instance Snip%))]
         [on-new-image-snip
          (Path (U 'unknown 'unknown/mask 'unknown/alpha
                   'gif 'gif/mask 'gif/alpha
                   'jpeg 'png 'png/mask 'png/alpha
                   'xbm 'xpm 'bmp 'pict)
                Any Any -> (Instance Snip%))]
         [on-paint
          (Any (Instance DC<%>) Real Real Real Real
               Real Real (U 'no-caret 'show-inactive-caret
                            'show-caret (Pairof Integer Integer))
               -> Void)]
         [on-save-file (Path Load/Save-Format -> Void)]
         [on-snip-modified ((Instance Snip%) Any -> Void)]
         [on-goodbye-event ((Instance DC<%>) Real Real Real Real
                            (Instance Mouse-Event%) -> Void)]
         [own-caret (Any -> Void)]
         [paste (case-> (-> Void) (Integer -> Void))]
         [paste-x-selection (case-> (-> Void) (Integer -> Void))]
         [print
          (case-> (-> Void)
                  (Any -> Void)
                  (Any Any -> Void)
                  (Any Any (U 'standard 'postscript 'pdf) -> Void)
                  (Any Any (U 'standard 'postscript 'pdf)
                       (U #f (Instance Frame%) (Instance Dialog%))
                       -> Void)
                  (Any Any (U 'standard 'postscript 'pdf)
                       (U #f (Instance Frame%) (Instance Dialog%))
                       Any
                       -> Void)
                  (Any Any (U 'standard 'postscript 'pdf)
                       (U #f (Instance Frame%) (Instance Dialog%))
                       Any Any
                       -> Void))]
         [print-to-dc
          (case-> ((Instance DC<%>) -> Void)
                  ((Instance DC<%>) Integer -> Void))]
         [put-file ((Option Path) (Option Path) -> (Option Path-String))]
         [read-footer-from-file
          ((Instance Editor-Stream-In%) String -> Boolean)]
         [read-from-file
          (case->
           ((Instance Editor-Stream-In%) -> Boolean)
           ((Instance Editor-Stream-In%) Any -> Boolean))]
         [read-header-from-file
          ((Instance Editor-Stream-In%) String -> Boolean)]
         [redo (-> Void)]
         [refresh
          (Real Real Real Real (U 'no-caret 'show-inactive-caret 'show-caret
                                  (Pairof Integer Integer))
                (Option (Instance Color%))
                -> Void)]
         [refresh-delayed? (-> Boolean)]
         [release-snip ((Instance Snip%) -> Boolean)]
         [remove-canvas ((Instance Editor-Canvas%) -> Void)]
         [resized ((Instance Snip%) Any -> Void)]
         [save-file
          (case-> (-> Boolean)
                  ((Option Path-String) -> Boolean)
                  ((Option Path-String) Load/Save-Format -> Boolean)
                  ((Option Path-String) Load/Save-Format Any -> Boolean))]
         [save-port
          (case-> (Output-Port -> Boolean)
                  (Output-Port Load/Save-Format -> Boolean)
                  (Output-Port Load/Save-Format Any -> Boolean))]
         [scroll-editor-to
          (Real Real Real Real Any (U 'start 'end 'none) -> Boolean)]
         [scroll-line-location (Integer -> Nonnegative-Real)]
         [scroll-to
          (case->
           ((Instance Snip%) Real Real Real Real Any -> Boolean)
           ((Instance Snip%) Real Real Real Real Any (U 'start 'end 'none)
            -> Boolean))]
         [select-all (-> Void)]
         [set-active-canvas ((Instance Editor-Canvas%) -> Void)]
         [set-admin ((Option (Instance Editor-Admin%)) -> Void)]
         [set-caret-owner
          (case-> ((Option Snip%) -> Void)
                  ((Option Snip%) (U 'immediate 'display 'global) -> Void))]
         [set-cursor
          (case-> ((Option (Instance Cursor%)) -> Void)
                  ((Option (Instance Cursor%)) Any -> Void))]
         [set-filename
          (case-> ((Option Path-String) -> Void)
                  ((Option Path-String) Any -> Void))]
         [set-inactive-caret-threshold
          ((U 'no-caret 'show-inactive-caret 'show-caret) -> Void)]
         [set-keymap
          (case-> (-> Void) ((Option Keymap%) -> Void))]
         [set-load-overwrites-styles (Any -> Void)]
         [set-max-height ((U Integer 'none) -> Void)]
         [set-max-undo-history ((U Integer 'forever) -> Void)]
         [set-max-width ((U Integer 'none) -> Void)]
         [set-min-height ((U Integer 'none) -> Void)]
         [set-min-width ((U Integer 'none) -> Void)]
         [set-modified (Any -> Void)]
         [set-paste-text-only (Any -> Void)]
         [set-snip-data ((Instance Snip%) (Instance Editor-Data%) -> Void)]
         [set-style-list ((Instance Style-List%) -> Void)]
         [size-cache-invalid (-> Void)]
         [style-has-changed ((Option (Instance Style<%>)) -> Void)]
         [undo (-> Void)]
         [use-file-text-mode (case-> (-> Boolean) (Any -> Void))]
         [write-footers-to-file ((Instance Editor-Stream-Out%) -> Boolean)]
         [write-headers-to-file ((Instance Editor-Stream-Out%) -> Boolean)]
         [write-to-file ((Instance Editor-Stream-Out%) -> Boolean)]
         (augment [after-edit-sequence (-> Void)]
                  [after-load-file (Any -> Void)]
                  [after-save-file (Any -> Void)]
                  [can-load-file? (Path Load/Save-Format -> Boolean)]
                  [can-save-file? (Path Load/Save-Format -> Boolean)]
                  [on-change (-> Void)]
                  [on-display-size (-> Void)]
                  [on-edit-sequence (-> Void)]
                  [on-load-file (Path Load/Save-Format -> Void)]
                  [on-save-file (Path Load/Save-Format -> Void)]
                  [on-snip-modified ((Instance Snip%) Any -> Void)])))

(define-type Editor-Admin%
  (Class [get-dc
          (case-> (-> (Option (Instance DC<%>)))
                  ((Option (Boxof Real)) -> (Option (Instance DC<%>)))
                  ((Option (Boxof Real)) (Option (Boxof Real))
                   -> (Option (Instance DC<%>))))]
         [get-max-view
          (case->
           ((Option (Boxof Real)) (Option (Boxof Real))
            (Option (Boxof Real)) (Option (Boxof Real))
            -> Void)
           ((Option (Boxof Real)) (Option (Boxof Real))
            (Option (Boxof Real)) (Option (Boxof Real))
            Any -> Void))]
         [get-view
          (case->
           ((Option (Boxof Real)) (Option (Boxof Real))
            (Option (Boxof Real)) (Option (Boxof Real))
            -> Void)
           ((Option (Boxof Real)) (Option (Boxof Real))
            (Option (Boxof Real)) (Option (Boxof Real))
            Any -> Void))]
         [grab-caret
          (case-> (-> Void)
                  ((U 'immediate 'display 'global) -> Void))]
         [needs-update (Real Real Real Real -> Void)]
         [popup-menu ((Instance Popup-Menu%) Real Real -> Boolean)]
         [refresh-delayed? (-> Boolean)]
         [resized (Any -> Void)]
         [scroll-to
          (case-> (Real Real
                   Nonnegative-Real Nonnegative-Real
                   Any -> Boolean)
                  (Real Real
                   Nonnegative-Real Nonnegative-Real
                   Any (U 'start 'end 'none) -> Boolean))]
         [update-cursor (-> Void)]))

(define-type Editor-Data%
  (Class [get-dataclass (-> (Option (Instance Editor-Data-Class%)))]
         [get-next (-> (Option Editor-Data%))]
         [set-dataclass ((Instance Editor-Data-Class%) -> Void)]
         [set-next ((Option Editor-Data%) -> Void)]
         [write ((Instance Editor-Stream-Out%) -> Boolean)]))

(define-type Editor-Data-Class%
  (Class [get-classname (-> String)]
         [read ((Option Editor-Stream-In%) -> (Option Editor-Data%))]
         [set-classname (String -> Void)]))

(define-type Editor-Stream-In-Base%
  (Class [bad? (-> Boolean)]
         [read (-> (Vectorof Char) Natural)]
         [read-bytes (-> Bytes Natural)]
         [read-byte (-> (U Byte #f))]
         [seek (-> Integer Void)]
         [skip (-> Integer Void)]
         [tell (-> Natural)]))

(define-type Editor-Stream-Out-Base%
  (Class [bad? (-> Boolean)]
         [seek (-> Integer Void)]
         [tell (-> Natural)]
         [write (-> (Listof Char) Void)]
         [write-bytes (-> Bytes Void)]))

(define-type Editor-Stream-In%
  (Class (init-rest (List (Instance Editor-Stream-In-Base%)))
         [get
          ((U (Boxof Integer) (Boxof Real)) -> (Instance Editor-Stream-In%))]
         [get-bytes
          (case->
           (-> (Option Bytes))
           ((Option (Boxof Integer)) -> (Option Bytes)))]
         [get-exact (-> Integer)]
         [get-fixed ((Boxof Integer) -> (Instance Editor-Stream-In%))]
         [get-fixed-exact (-> Integer)]
         [get-inexact (-> Real)]
         [get-unterminated-bytes
          (case->
           (-> (Option Bytes))
           ((Option (Boxof Integer)) -> (Option Bytes)))]
         [jump-to (Integer -> Void)]
         [ok? (-> Boolean)]
         [remove-boundary (-> Void)]
         [set-boundary (Integer -> Void)]
         [skip (Integer -> Void)]
         [tell (-> Natural)]))

(define-type Editor-Stream-Out%
  (Class (init-rest (List (Instance Editor-Stream-Out-Base%)))
         [jump-to (Integer -> Void)]
         [ok? (-> Boolean)]
         [pretty-finish (-> Void)]
         [pretty-start (-> Void)]
         [put
          (case->
           ((U Bytes Integer Real) -> (Instance Editor-Stream-Out%))
           (Integer Bytes -> (Instance Editor-Stream-Out%)))]
         [put-fixed (Integer -> (Instance Editor-Stream-Out%))]
         [put-unterminated (Bytes -> (Instance Editor-Stream-Out%))]
         [tell (-> Natural)]))

(define-type Keymap%
  (Class [add-function (String (Any (Instance Event%) -> Any) -> Void)]
         [break-sequence (-> Void)]
         [call-function
          (case->
           (String Any (Instance Event%) -> Boolean)
           (String Any (Instance Event%) Any -> Boolean))]
         [chain-to-keymap ((Instance Keymap%) Any -> Void)]
         [get-double-click-interval (-> Natural)]
         [handle-key-event (Any (Instance Key-Event%) -> Boolean)]
         [handle-mouse-event (Any (Instance Mouse-Event%) -> Boolean)]
         [map-function (String String -> Void)]
         [remove-chained-keymap ((Instance Keymap%) -> Void)]
         [remove-grab-key-function (-> Void)]
         [remove-grab-mouse-function (-> Void)]
         [set-break-sequence-callback ((-> Any) -> Void)]
         [set-double-click-interval (Integer -> Void)]
         [set-grab-key-function
          (((Option String) (Instance Keymap%) Any
            (Instance Key-Event%) -> Any)
           -> Void)]
         [set-grab-mouse-function
          (((Option String) (Instance Keymap%) Any
            (Instance Mouse-Event%) -> Any)
           -> Void)]))

(define-type Pasteboard%
  (Class #:implements Editor<%>
         [add-selected
          (case-> ((Instance Snip%) -> Void)
                  (Real Real Nonnegative-Real Nonnegative-Real -> Void))]
         [after-delete ((Instance Snip%) -> Void)]
         [after-insert
          ((Instance Snip%) (Option (Instance Snip%)) Real Real -> Void)]
         [after-interactive-move ((Instance Mouse-Event%) -> Void)]
         [after-interactive-resize ((Instance Snip%) -> Void)]
         [after-move-to ((Instance Snip%) Real Real Any -> Void)]
         [after-reorder ((Instance Snip%) (Instance Snip%) Any -> Boolean)]
         [after-resize ((Instance Snip%) Real Real Any -> Void)]
         [after-select ((Instance Snip%) Any -> Void)]
         [can-delete? ((Instance Snip%) -> Boolean)]
         [can-insert?
          ((Instance Snip%) (Option (Instance Snip%)) Real Real -> Boolean)]
         [can-interactive-move? ((Instance Mouse-Event%) -> Boolean)]
         [can-interactive-resize? ((Instance Snip%) -> Boolean)]
         [can-move-to? ((Instance Snip%) Real Real Any -> Boolean)]
         [can-reorder? ((Instance Snip%) (Instance Snip%) Any -> Boolean)]
         [can-resize? ((Instance Snip%) Real Real -> Boolean)]
         [can-select? ((Instance Snip%) Any -> Boolean)]
         [change-style
          (case-> (-> Void)
                  ((U #f (Instance Style-Delta%) (Instance Style<%>)) -> Void)
                  ((U #f (Instance Style-Delta%) (Instance Style<%>))
                   (Option (Instance Snip%)) -> Void))]
         [delete (case-> (-> Void) ((Instance Snip%) -> Void))]
         [do-copy (Integer Any -> Void)]
         [do-paste (Integer -> Void)]
         [do-paste-x-selection (Integer -> Void)]
         [erase (-> Void)]
         [find-next-selected-snip
          ((Option (Instance Snip%)) -> (Option (Instance Snip%)))]
         [find-snip
          (case-> (Real Real -> (Option (Instance Snip%)))
                  (Real Real (Option (Instance Snip%))
                   -> (Option (Instance Snip%))))]
         [get-center (-> (Values Real Real))]
         [get-dragable (-> Boolean)]
         [get-scroll-step (-> Nonnegative-Real)]
         [get-selection-visible (-> Boolean)]
         [insert (case-> ((Instance Snip%) -> Void)
                         ((Instance Snip%) (Option (Instance Snip%)) Real Real -> Void)
                         ((Instance Snip%) Real Real -> Void)
                         ((Instance Snip%) (Option (Instance Snip%)) -> Void))]
         [interactive-adjust-mouse ((Boxof Real) (Boxof Real) -> Void)]
         [interactive-adjust-move
          ((Instance Snip%) (Boxof Real) (Boxof Real) -> Void)]
         [interactive-adjust-resize
          ((Instance Snip%) (Boxof Real) (Boxof Real) -> Void)]
         [is-selected? ((Instance Snip%) -> Boolean)]
         [lower ((Instance Snip%) -> Void)]
         [move
          (case-> (Real Real -> Void)
                  ((Instance Snip%) Real Real -> Void))]
         [move-to ((Instance Snip%) Real Real -> Void)]
         [no-selected (-> Void)]
         [on-delete ((Instance Snip%) -> Void)]
         [on-double-click
          ((Instance Snip%) (Instance Mouse-Event%) -> Void)]
         [on-insert
          ((Instance Snip%) (Option (Instance Snip%)) Real Real -> Void)]
         [on-interactive-move ((Instance Mouse-Event%) -> Void)]
         [on-interactive-resize ((Instance Snip%) -> Void)]
         [on-move-to ((Instance Snip%) Real Real Any -> Void)]
         [on-reorder ((Instance Snip%) (Instance Snip%) Any -> Void)]
         [on-resize ((Instance Snip%) Real Real -> Void)]
         [on-select ((Instance Snip%) Any -> Void)]
         [raise ((Instance Snip%) -> Void)]
         [remove ((Instance Snip%) -> Void)]
         [remove-selected ((Instance Snip%) -> Void)]
         [resize ((Instance Snip%) Real Real -> Void)]
         [set-after ((Instance Snip%) (Option (Instance Snip%)) -> Void)]
         [set-before ((Instance Snip%) (Option (Instance Snip%)) -> Void)]
         [set-dragable (Any -> Void)]
         [set-scroll-step (Real -> Void)]
         [set-selected ((Instance Snip%) -> Void)]
         [set-selection-visible (Any -> Void)]
         (augment [after-delete ((Instance Snip%) -> Void)]
                  [after-insert
                   ((Instance Snip%) (Option (Instance Snip%)) Real Real -> Void)]
                  [after-interactive-move ((Instance Mouse-Event%) -> Void)]
                  [after-interactive-resize ((Instance Snip%) -> Void)]
                  [after-move-to ((Instance Snip%) Real Real Any -> Void)]
                  [after-reorder ((Instance Snip%) (Instance Snip%) Any -> Boolean)]
                  [after-resize ((Instance Snip%) Real Real Any -> Void)]
                  [after-select ((Instance Snip%) Any -> Void)]
                  [can-delete? ((Instance Snip%) -> Boolean)]
                  [can-insert?
                   ((Instance Snip%) (Option (Instance Snip%)) Real Real -> Boolean)]
                  [can-interactive-move? ((Instance Mouse-Event%) -> Boolean)]
                  [can-interactive-resize? ((Instance Snip%) -> Boolean)]
                  [can-move-to? ((Instance Snip%) Real Real Any -> Boolean)]
                  [can-reorder? ((Instance Snip%) (Instance Snip%) Any -> Boolean)]
                  [can-resize? ((Instance Snip%) Real Real -> Boolean)]
                  [can-select? ((Instance Snip%) Any -> Boolean)]
                  [on-delete ((Instance Snip%) -> Void)]
                  [on-insert
                   ((Instance Snip%) (Option (Instance Snip%)) Real Real -> Void)]
                  [on-interactive-move ((Instance Mouse-Event%) -> Void)]
                  [on-interactive-resize ((Instance Snip%) -> Void)]
                  [on-move-to ((Instance Snip%) Real Real Any -> Void)]
                  [on-reorder ((Instance Snip%) (Instance Snip%) Any -> Void)]
                  [on-resize ((Instance Snip%) Real Real -> Void)]
                  [on-select ((Instance Snip%) Any -> Void)])))

;; racket/snip

(provide Readable-Snip<%>
         Snip%
         Snip-Admin%
         Snip-Class%
         Snip-Class-List<%>
         String-Snip%
         Style<%>
         Style-Delta%
         Style-List%
         Tab-Snip%)

(define-type Readable-Snip<%>
  (Class [read-special (Any (Option Natural) (Option Natural) (Option Natural) -> Any)]))

(define-type Snip-Edit-Operation
  (U 'undo 'redo 'clear 'cut 'copy
     'paste 'kill 'select-all
     'insert-text-box 'insert-pasteboard-box
     'insert-image))

(define-type Style-Delta%
  (Class (init-rest
          (U (List)
             (List (U 'change-nothing 'change-normal
                      'change-toggle-underline 'change-toggle-size-in-pixels
                      'change-normal-color 'change-bold))
             (List (U 'change-family 'change-style
                      'change-toggle-style 'change-weight
                      'change-toggle-weight 'change-smoothing
                      'change'toggle-smoothing 'change-alignment)
                   Symbol)
             (List (U 'change-size 'change-bigger 'change-smaller)
                   Byte)
             (List (U 'change-underline 'change-size-in-pixels)
                   Any)))
         [collapse ((Instance Style-Delta%) -> Boolean)]
         [copy ((Instance Style-Delta%) -> Void)]
         [equal? ((Instance Style-Delta%) -> Boolean)]
         [get-alignment-off (-> (U 'base 'top 'center 'bottom))]
         [get-alignment-on (-> (U 'base 'top 'center 'bottom))]
         #| FIXME
         [get-background-add (-> (Instance Add-Color<%>))]
         [get-background-mult (-> (Instance Mult-Color<%>))]
         |#
         [get-face (-> (Option String))]
         [get-family (-> Font-Family)]
         #| FIXME
         [get-foreground-add (-> (Instance Add-Color<%>))]
         [get-foreground-mult (-> (Instance Mult-Color<%>))]
         |#
         [get-size-add (-> Byte)]
         [get-size-in-pixels-off (-> Boolean)]
         [get-size-in-pixels-on (-> Boolean)]
         [get-size-mult (-> Real)]
         [get-smoothing-off (-> Font-Smoothing)]
         [get-smoothing-on (-> Font-Smoothing)]
         [get-style-off (-> Font-Style)]
         [get-style-on (-> Font-Style)]
         [get-transparent-text-backing-off (-> Boolean)]
         [get-transparent-text-backing-on (-> Boolean)]
         [get-underlined-off (-> Boolean)]
         [get-underlined-on (-> Boolean)]
         [get-weight-off (-> Font-Weight)]
         [get-weight-on (-> Font-Weight)]
         [set-alignment-off ((U 'base 'top 'center 'bottom) -> Void)]
         [set-alignment-on ((U 'base 'top 'center 'bottom) -> Void)]
         [set-delta
          (case-> ((U 'change-nothing 'change-normal 'change-toggle-underline
                      'change-toggle-size-in-pixels 'change-normal-color
                      'change-bold)
                   -> (Instance Style-Delta%))
                  ((U 'change-family 'change-style 'change-toggle-style 'change-weight
                      'change-toggle-weight 'change-smoothing 'change-toggle-smoothing
                      'change-alignment 'change-size 'change-bigger
                      'change-smaller 'change-underline 'change-size-in-pixel)
                   Any -> (Instance Style-Delta%)))]
         [set-delta-background ((U String (Instance Color%)) -> (Instance Style-Delta%))]
         [set-delta-face
          (case-> (String -> (Instance Style-Delta%))
                  (String Font-Family -> (Instance Style-Delta%)))]
         [set-delta-foreground ((U String (Instance Color%)) -> (Instance Style-Delta%))]
         [set-face ((Option String) -> Void)]
         [set-family (Font-Family -> Void)]
         [set-size-add (Byte -> Void)]
         [set-size-in-pixels-off (Any -> Void)]
         [set-size-in-pixels-on (Any -> Void)]
         [set-size-mult (Real -> Void)]
         [set-smoothing-off (Font-Smoothing -> Void)]
         [set-smoothing-on (Font-Smoothing -> Void)]
         [set-style-off (Font-Style -> Void)]
         [set-style-on (Font-Style -> Void)]
         [set-transparent-text-backing-off (Any -> Void)]
         [set-transparent-text-backing-on (Any -> Void)]
         [set-underlined-off (Any -> Void)]
         [set-underlined-on (Any -> Void)]
         [set-weight-off (Font-Weight -> Void)]
         [set-weight-on (Font-Weight -> Void)]))

(define-type Style<%>
  (Class [get-alignment (-> (U 'top 'center 'bottom))]
         [get-background (-> (Instance Color%))]
         [get-base-style (-> (Option (Instance Style<%>)))]
         [get-delta ((Instance Style-Delta%) -> Void)]
         [get-face (-> (Option String))]
         [get-family (-> (U 'default 'decorative 'roman 'script
                            'swiss 'modern 'symbol 'system))]
         [get-font (-> (Instance Font%))]
         [get-foreground (-> (Instance Color%))]
         [get-name (-> (Option String))]
         [get-shift-style (-> (Instance Style<%>))]
         [get-size (-> Byte)]
         [get-size-in-pixels (-> Boolean)]
         [get-smoothing
          (-> (U 'default 'partly-smoothed 'smoothed 'unsmoothed))]
         [get-style (-> (U 'normal 'italic 'slant))]
         [get-text-descent ((Instance DC<%>) -> Nonnegative-Real)]
         [get-text-height ((Instance DC<%>) -> Nonnegative-Real)]
         [get-text-space ((Instance DC<%>) -> Nonnegative-Real)]
         [get-text-width ((Instance DC<%>) -> Nonnegative-Real)]
         [get-transparent-text-backing (-> Boolean)]
         [get-underlined (-> Boolean)]
         [get-weight (-> (U 'normal 'bold 'light))]
         [is-join? (-> Boolean)]
         [set-base-style (Any -> (Instance Style<%>))]
         [set-delta ((Instance Style-Delta%) -> Void)]
         [set-shift-style ((Instance Style<%>) -> Void)]
         [switch-to ((Instance DC<%>) (Option (Instance Style<%>)) -> Void)]))

(define-type Style-List%
  (Class ; FIXME: this is a final method
         ; [basic-style (-> (Instance Style<%>))]
         [convert ((Instance Style<%>) -> (Instance Style<%>))]
         [find-named-style
          (String -> (Option (Instance Style<%>)))]
         [find-or-create-join-style
          ((Instance Style<%>) (Instance Style<%>) -> (Instance Style<%>))]
         [find-or-create-style
          ((Instance Style<%>) (Instance Style-Delta%) -> (Instance Style<%>))]
         [forget-notification (Any -> Void)]
         [index-to-style (Natural -> (Option (Instance Style<%>)))]
         [new-named-style (String (Instance Style<%>) -> (Instance Style<%>))]
         [notify-on-change
          (((Option (Instance Style<%>)) -> Any) -> Any)]
         [number (-> Natural)]
         [replace-named-style
          (String (Instance Style<%>) -> (Instance Style<%>))]
         [style-to-index
          ((Instance Style<%>) -> (Option Natural))]))

(define-type Snip-Admin%
  (Class [get-dc (-> (Option (Instance DC<%>)))]
         [get-editor (-> (U (Instance Text%) (Instance Pasteboard%)))]
         [get-view
          (case-> ((Option (Boxof Real)) (Option (Boxof Real))
                   (Option (Boxof Nonnegative-Real))
                   (Option (Boxof Nonnegative-Real))
                   -> Void)
                  ((Option (Boxof Real)) (Option (Boxof Real))
                   (Option (Boxof Nonnegative-Real))
                   (Option (Boxof Nonnegative-Real))
                   (Option (Instance Snip%))
                   -> Void))]
         [get-view-size ((Option (Boxof Nonnegative-Real))
                         (Option (Boxof Nonnegative-Real))
                         -> Void)]
         [modified ((Instance Snip%) Any -> Void)]
         [needs-update
          ((Instance Snip%) Real Real Real Real -> Void)]
         [popup-menu
          ((Instance Popup-Menu%) (Instance Snip%) Real Real -> Boolean)]
         [recounted ((Instance Snip%) Any -> Void)]
         [release-snip ((Instance Snip%) -> Boolean)]
         [resized ((Instance Snip%) Any -> Void)]
         [scroll-to
          (case->
           ((Instance Snip%) Real Real Real Real Any -> Boolean)
           ((Instance Snip%) Real Real Real Real Any (U 'start 'end 'none)
            -> Boolean))]
         [set-caret-owner
          ((Option Snip%) (U 'immediate 'display 'global) -> Void)]
         [update-cursor (-> Void)]
         [get-line-spacing (-> Nonnegative-Real)]
         [get-selected-text-color (-> (Option (Instance Color%)))]
         [call-with-busy-cursor ((-> Any) -> Any)]
         [get-tabs
          (case-> (-> (Listof Real))
                  ((Option (Boxof Natural)) -> (Listof Real))
                  ((Option (Boxof Natural)) (Option (Boxof Real))
                   -> (Listof Real))
                  ((Option (Boxof Natural)) (Option (Boxof Real))
                   (Option (Boxof Any)) -> (Listof Real)))]))

(define-type Snip-Class%
  (Class [get-classname (-> String)]
         [get-version (-> Integer)]
         [read ((Instance Editor-Stream-In%) -> (Option (Instance Snip%)))]
         [read-header ((Instance Editor-Stream-In%) -> Boolean)]
         [reading-version ((Instance Editor-Stream-In%) -> Integer)]
         [set-classname (String -> Void)]
         [set-version (Integer -> Void)]
         [write-header ((Instance Editor-Stream-Out%) -> Boolean)]))

(define-type Snip%
  (Class [adjust-cursor
          ((Instance DC<%>) Real Real Real Real
           (Instance Mouse-Event%) -> (Option (Instance Cursor%)))]
         [blink-caret
          ((Instance DC<%>) Real Real -> Void)]
         [can-do-edit-operation?
          (case-> (Snip-Edit-Operation -> Boolean)
                  (Snip-Edit-Operation Any -> Boolean))]
         [copy (-> (Instance Snip%))]
         [do-edit-operation
          (case-> (Snip-Edit-Operation -> Void)
                  (Snip-Edit-Operation Any -> Void)
                  (Snip-Edit-Operation Any Integer -> Void))]
         [draw
          ((Instance DC<%>) Real Real Real Real Real Real Real Real
           (U 'no-caret 'show-inactive-caret 'show-caret
              (Pairof Natural Natural)) -> Void)]
         [equal-to?
          ((Instance Snip%) (Any Any -> Boolean) -> Boolean)]
         [other-equal-to?
          ((Instance Snip%) (Any Any -> Boolean) -> Boolean)]
         [equal-hash-code-of ((Any -> Integer) -> Integer)]
         [equal-secondary-hash-code-of ((Any -> Integer) -> Integer)]
         [find-scroll-step (Real -> Natural)]
         [get-admin (-> (Option (Instance Snip-Admin%)))]
         [get-count (-> Integer)]
         [get-extent
          (case->
           ((Instance DC<%>) Real Real -> Void)
           ((Instance DC<%>) Real Real
            (Option (Boxof Nonnegative-Real)) -> Void)
           ((Instance DC<%>) Real Real
            (Option (Boxof Nonnegative-Real)) (Option (Boxof Nonnegative-Real))
            -> Void)
           ((Instance DC<%>) Real Real
            (Option (Boxof Nonnegative-Real)) (Option (Boxof Nonnegative-Real))
            (Option (Boxof Nonnegative-Real)) -> Void)
           ((Instance DC<%>) Real Real
            (Option (Boxof Nonnegative-Real)) (Option (Boxof Nonnegative-Real))
            (Option (Boxof Nonnegative-Real)) (Option (Boxof Nonnegative-Real))
            -> Void)
           ((Instance DC<%>) Real Real
            (Option (Boxof Nonnegative-Real)) (Option (Boxof Nonnegative-Real))
            (Option (Boxof Nonnegative-Real)) (Option (Boxof Nonnegative-Real))
            (Option (Boxof Nonnegative-Real)) -> Void)
           ((Instance DC<%>) Real Real
            (Option (Boxof Nonnegative-Real)) (Option (Boxof Nonnegative-Real))
            (Option (Boxof Nonnegative-Real)) (Option (Boxof Nonnegative-Real))
            (Option (Boxof Nonnegative-Real)) (Option (Boxof Nonnegative-Real))
            -> Void))]
         [get-flags (-> (Listof Symbol))]
         [get-num-scroll-steps (-> Natural)]
         [get-scroll-step-offset (Natural -> Nonnegative-Real)]
         [get-snipclass (-> (Option Snip-Class%))]
         [get-style (-> (Instance Style<%>))]
         [get-text
          (case-> (Natural Natural -> String)
                  (Natural Natural Any -> String))]
         [get-text!
          (String Natural Natural Natural -> Void)]
         [is-owned? (-> Boolean)]
         [match? ((Instance Snip%) -> Boolean)]
         [merge-with ((Instance Snip%) -> (Option (Instance Snip%)))]
         [next (-> (Option (Instance Snip%)))]
         [on-char
          ((Instance DC<%>) Real Real Real Real
           (Instance Key-Event%) -> Void)]
         [on-event
          ((Instance DC<%>) Real Real Real Real
           (Instance Mouse-Event%) -> Void)]
         [own-caret (Any -> Void)]
         [partial-offset ((Instance DC<%>) Real Real Natural -> Real)]
         [previous (-> (Option (Instance Snip%)))]
         [release-from-owner (-> Boolean)]
         [resize (Nonnegative-Real Nonnegative-Real -> Boolean)]
         [set-admin ((Option (Instance Snip-Admin%)) -> Void)]
         [set-count (Integer -> Void)]
         [set-flags ((Listof Symbol) -> Void)]
         [set-snipclass ((Instance Snip-Class%) -> Void)]
         [set-style ((Instance Style<%>) -> Void)]
         [set-unmodified (-> Void)]
         [size-cache-invalid (-> Void)]
         [split (Natural (Boxof (Instance Snip%))
                 (Boxof (Instance Snip%)) -> Void)]
         [write ((Instance Editor-Stream-Out%) -> Void)]))

(define-type Snip-Class-List<%>
  (Class [add ((Instance Snip-Class%) -> Void)]
         [find (String -> (Option (Instance Snip-Class%)))]
         [find-position ((Instance Snip-Class%) -> Natural)]
         [nth (Integer -> (Option (Instance Snip-Class%)))]
         [number (-> Natural)]))

(define-type String-Snip%
  (Class #:implements Snip%
         (init-rest (U (List) (List (U String Integer))))
         [insert (case-> (String Natural -> Void)
                         (String Natural Natural -> Void))]
         [read (Natural (Instance Editor-Stream-In%) -> Void)]))

(define-type Tab-Snip%
  (Class #:implements String-Snip%))

;; 7 Editor Classes
(provide Editor-Snip%
         Editor-Wordbreak-Map%)
(define-type Editor-Snip%
  (Class #:implements Snip%
         (init [editor (Option (Instance Editor<%>)) #:optional]
               [with-border? Any #:optional]
               [left-margin Natural #:optional]
               [top-margin Natural #:optional]
               [right-margin Natural #:optional]
               [bottom-margin Natural #:optional]
               [left-inset Natural #:optional]
               [top-inset Natural #:optional]
               [right-inset Natural #:optional]
               [bottom-inset Natural #:optional]
               [min-width Nonnegative-Real #:optional]
               [max-width Nonnegative-Real #:optional]
               [min-height Nonnegative-Real #:optional]
               [max-height Nonnegative-Real #:optional])
         [border-visible? (-> Boolean)]
         [get-align-top-line (-> Boolean)]
         [get-editor (-> (U #f (Instance Text%) (Instance Pasteboard%)))]
         [get-inset ((Boxof Natural) (Boxof Natural) (Boxof Natural) (Boxof Natural) -> Void)]
         [get-margin ((Boxof Natural) (Boxof Natural) (Boxof Natural) (Boxof Natural) -> Void)]
         [get-max-height (-> (U Nonnegative-Real 'none))]
         [get-max-width (-> (U Nonnegative-Real 'none))]
         [get-min-height (-> (U Nonnegative-Real 'none))]
         [get-min-width (-> (U Nonnegative-Real 'none))]
         [get-tight-text-fit (-> Boolean)]
         [resize (Nonnegative-Real Nonnegative-Real -> Boolean)]
         [set-align-top-line (Any -> Void)]
         [set-editor ((U (Instance Text%) (Instance Pasteboard%) #f) -> Void)]
         [set-inset (Natural Natural Natural Natural -> Void)]
         [set-margin (Natural Natural Natural Natural -> Void)]
         [set-max-height ((U Nonnegative-Real 'none) -> Void)]
         [set-max-width ((U Nonnegative-Real 'none) -> Void)]
         [set-min-height ((U Nonnegative-Real 'none) -> Void)]
         [set-min-width ((U Nonnegative-Real 'none) -> Void)]
         [set-tight-text-fit (Any -> Void)]
         [show-border (Any -> Void)]
         [style-background-used? (-> Boolean)]
         [use-style-background (Any -> Void)]))

(define-type Wordbreak-Map-Value (U 'caret 'line 'selection 'user1 'user2))
(define-type Editor-Wordbreak-Map%
  (Class
   [get-map (Char -> (Listof Wordbreak-Map-Value))]
   [set-map (Char (Listof Wordbreak-Map-Value) -> Void)]))
