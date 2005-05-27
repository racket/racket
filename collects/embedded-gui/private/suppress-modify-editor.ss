(module suppress-modify-editor mzscheme
  
  (require
   (lib "etc.ss")
   (lib "class.ss")
   (lib "mred.ss" "mred"))
  
  (provide suppress-modify-editor-mixin)
      
  ;; Ignore the modification of the pasteboard that is used for layout
  ;; Allow nested editors to percollate modify messages up through 
  (define (suppress-modify-editor-mixin %)
    (class %
      (inherit set-modified)
      #;(define/override (after-delete snip)
        (super after-delete snip)
        (set-modified false))
      #;(define/override (after-insert snip before x y)
        (super after-insert snip before x y)
        (set-modified false))
      #;(define/override (after-move-to snip x y dragging?)
        (super after-move-to snip x y dragging?)
        (set-modified false))
      #;(define/override (after-resize snip w h resized?)
        (super after-resize snip w h resized?)
        (set-modified false))
      (super-new)))
  )
