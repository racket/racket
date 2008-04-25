#lang scheme

(provide core-draw^ draw^)

(require mzlib/unit)

;; xxx-solid-rect cannot be called xxx-solid-rectangle because that
;; interferes with the existing xxx-solid-rectangle name in our unit
;; calculus -- mf 

(define-signature core-draw^
  (start 
   start/cartesian-plane
   stop 
   ;; --- 
   start-and-export 
   ;; like start but also consumes a hashtable 
   ;; adds the procedures that can be called during a sequnce to the hashtable
   ;; --- the following can be called during a draw sequence --- 
   draw-circle
   draw-solid-disk
   draw-solid-rect
   draw-solid-line
   draw-solid-string
   clear-circle
   clear-solid-disk
   clear-solid-rect
   clear-solid-line
   clear-solid-string
   clear-all
   ;; --- stupid stuff --- 
   sleep-for-a-while
   wait-for-mouse-click ; -> posn
   get-key-event   ; -> (union #f char symbol)
   get-mouse-event ; -> (union #f posn)
   ;;
   ;; "hidden" access to viewports (for htdc/[i]draw mostly)
   get-@VP         ; -> Viewport
   begin-draw-sequence ; Viewport Viewport -> #t
   end-draw-sequence   ; -> #t
   ;; 
   big-bang        ; World -> true
   on-key-event    ; (union char symbol) World -> World 
   on-tick-event   ; World -> World 
   end-of-time     ; -> World
   ))

(define-signature draw^ extends core-draw^ ())
