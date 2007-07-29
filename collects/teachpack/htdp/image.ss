;; this is like (lib "htdp" "image.ss") 
;; except that it provides things with
;; provide-primitives for better error
;; reporting in the teaching languages.

(module image mzscheme
  (require (lib "image.ss" "htdp")
           (lib "prim.ss" "lang"))
  
  
  (provide-primitives 
   image-width
   image-height
   overlay
   overlay/xy
   
   pinhole-x
   pinhole-y
   move-pinhole
   put-pinhole
   
   rectangle
   circle
   ellipse
   triangle
   line
   star
   add-line
   text
   
   shrink
   shrink-tl
   shrink-tr
   shrink-bl
   shrink-br
   
   image-inside?
   find-image
   
   image->color-list
   color-list->image
   
   image->alpha-color-list
   alpha-color-list->image
   
   image-color?
   make-color
   color-red
   color-green
   color-blue
   color?
   make-alpha-color
   alpha-color-alpha
   alpha-color-red
   alpha-color-green
   alpha-color-blue
   alpha-color?))