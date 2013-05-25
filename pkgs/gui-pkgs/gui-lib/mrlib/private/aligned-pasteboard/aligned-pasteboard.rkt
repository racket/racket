(module aligned-pasteboard mzscheme
  
  (require
   mrlib/click-forwarding-editor
   "geometry-managed-pasteboard.rkt"
   "locked-pasteboard.rkt")
  
  (provide
   vertical-pasteboard%
   horizontal-pasteboard%)
  
  ;; contruct the basic mixin that both pasteboards will be created from
  (define (click/lock type)
    (click-forwarding-editor-mixin
     (locked-pasteboard-mixin
      (make-aligned-pasteboard type))))
  
  (define vertical-pasteboard%
    (click/lock 'vertical))
  
  (define horizontal-pasteboard%
    (click/lock 'horizontal)))
