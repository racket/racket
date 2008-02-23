(module suppress-modify-editor mzscheme
  
  (require
   mzlib/etc
   mzlib/class
   mred)
  
  (provide suppress-modify-editor-mixin)
      
  ;; Ignore the modification of the pasteboard that is used for layout
  ;; Allow nested editors to percollate modify messages up through 
  (define (suppress-modify-editor-mixin %)
    (class %
      (inherit set-modified)
      (define/augment (after-delete snip)
        (set-modified false))
      (define/augment (after-insert snip before x y)
        (set-modified false))
      (define/augment (after-move-to snip x y dragging?)
        (set-modified false))
      (define/augment (after-resize snip w h resized?)
        (set-modified false))
      (super-new)))
  )
