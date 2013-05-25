#lang racket/base

(require racket/unit)

(provide graphics^ graphics:posn-less^ graphics:posn^)

(define-signature graphics:posn^
  (make-posn posn? posn-x posn-y set-posn-x! set-posn-y!))

(define-signature graphics:posn-less^
  (viewport?
   
   sixkey-value sixkey-shift sixkey-control sixkey-meta sixkey-alt
   
   sixmouse-x sixmouse-y sixmouse-left? sixmouse-middle? sixmouse-right?
   sixmouse?
   make-sixmouse
   
   open-graphics 
   close-graphics 
   graphics-open?
   
   make-rgb
   rgb-blue rgb-red rgb-green
   change-color 
   rgb?
   
   open-viewport 
   open-pixmap
   close-viewport    
   
   query-mouse-posn
   
   viewport-mouse-events
   viewport-key-events
   
   clear-viewport draw-viewport flip-viewport
   
   draw-line clear-line flip-line 
   draw-pixel clear-pixel flip-pixel 
   get-pixel get-color-pixel test-pixel
   
   draw-rectangle clear-rectangle flip-rectangle 
   draw-arc draw-solid-arc
   draw-ellipse clear-ellipse flip-ellipse 
   draw-polygon clear-polygon flip-polygon 
   draw-solid-rectangle clear-solid-rectangle flip-solid-rectangle 
   draw-solid-ellipse clear-solid-ellipse flip-solid-ellipse 
   draw-solid-polygon clear-solid-polygon flip-solid-polygon 
   
   get-string-size
   
   draw-string clear-string flip-string 
   
   draw-pixmap-posn
   draw-pixmap
   save-pixmap
   
   copy-viewport 
   
   default-display-is-color?
   
   viewport->snip
   
   viewport-dc viewport-buffer-dc))

(define-signature graphics^
  ((open graphics:posn-less^)
   (open graphics:posn^)))
