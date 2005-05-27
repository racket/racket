(module graphics-sig mzscheme
  (require (lib "unitsig.ss"))

  (provide graphics^ graphics:posn-less^ graphics:posn^)

  (define-signature graphics:posn^
    (make-posn posn? posn-x posn-y))

  (define-signature graphics:posn-less^
    (viewport?

     open-graphics 
     close-graphics 
     graphics-open?
     
     get-mouse-click
     ready-mouse-click
     ready-mouse-release
     mouse-click-posn
     query-mouse-posn
     viewport-flush-input
     left-mouse-click?
     middle-mouse-click?
     right-mouse-click?
     
     set-on-key-event
     set-on-tick-event
     stop-tick
     init-world
     get-key-press
     ready-key-press
     key-value
     
     make-rgb
     rgb-blue rgb-red rgb-green
     change-color 
     rgb?
     
     open-viewport 
     open-pixmap
     close-viewport    
     
     clear-viewport draw-viewport flip-viewport
     
     draw-line clear-line flip-line 
     draw-pixel clear-pixel flip-pixel 
     get-pixel get-color-pixel test-pixel
     
     draw-rectangle clear-rectangle flip-rectangle 
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
)



