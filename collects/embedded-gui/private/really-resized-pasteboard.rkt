#|

This module provides a really-resized pasteboard that calls the really-resized
method when a snip in the editor is really resized, not when pasteboard says
it's resized. This file was written because sometimes I need to override resized
to adjust my editors layout however the resized method of the pasteboard is
invoked whenever editor snips get focus, lose focus, get text typed into them,
get text deteleted from them, etc.

|#

(module really-resized-pasteboard mzscheme
  
  (require
   mzlib/etc
   mzlib/class
   mred)
  
  (provide
   really-resized-pasteboard-mixin
   really-resized-pasteboard%)
  
  (define (really-resized-pasteboard-mixin super%)
    (class super%
      (inherit refresh-delayed? get-snip-location)
      (field [snip-cache (make-hash-table)]
             [ignore-resizing? false])
      
      ;; Called whenever a snip within the editor is resized, just like the
      ;; resized method but excludes suchs events as when and editor-snip gets
      ;; focus.
      (define/public (really-resized snip) (void))
      
      #|
      snip : snip% object 
      redraw-now? : boolean
      |#
      (define/override (resized snip redraw-now?)
        (super resized snip redraw-now?)
        (unless ignore-resizing?
          (let ([size (snip-size snip)])
            ;; The snip is getting remove from hash table in  a way I
            ;; am not antisipating. I need to find it and then I can
            ;; remove this error catcher.
            (unless (equal? size (with-handlers ([exn? (lambda x 0)])
                                   (hash-table-get snip-cache snip)))
              (hash-table-put! snip-cache snip size)
              (really-resized snip)))))
      
      #|
      snip : snip% object 
      before : snip% object or #f 
      x : real number 
      y : real number
      |#
      (define/augment (after-insert snip before x y)
        (hash-table-put! snip-cache snip (snip-size snip))
        (inner (void) after-insert snip before x y))
      
      #|
      snip : snip% object
      |#
      (define/augment (after-delete snip)
        (hash-table-remove! snip-cache snip)
        (inner (void) after-delete snip))
      
      #;((is-a?/c snip%) . -> . (cons/p natural-number? natural-number?))
      ;; The width and height of the given snip in this pasteboard.
      (define (snip-size snip)
        (let ([top (box 0)]
              [bottom (box 0)]
              [left (box 0)]
              [right (box 0)])
          (fluid-let ([ignore-resizing? true])
            (get-snip-location snip left top false)
            (get-snip-location snip right bottom true))
          (cons (- (unbox right) (unbox left))
                (- (unbox bottom) (unbox top)))))
      
      (super-new)))
  
  (define really-resized-pasteboard%
    (really-resized-pasteboard-mixin
     pasteboard%))
  )
