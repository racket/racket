;; This module provides a mixin that locks a pasteboard to all mouse interaction. This
;; means that there is no interactive dragging, no keyboard deletion, no handles drawn
;; at the corners of the snips for dragging, and anything else that must be added.

(module locked-pasteboard mzscheme
  
  (require
   mzlib/class
   mred
   mzlib/etc
   mzlib/contract)
  
  (provide/contract
   (locked-pasteboard-mixin mixin-contract))
  
  ;; mixin to remove interactive movement of snips from pasteboards
  ;; STATUS: Look into and make sure I don't need to deal with the following.
  ;;           interactive-adjust-mouse, interactive-adjust-move, on-default-event
  ;;           interactive-adjust-resize
  (define locked-pasteboard-mixin
    (mixin ((class->interface pasteboard%)) ()
      (define/override (on-default-event event) (void))
      ;; The rest of the methods I believe to be redundant but
      ;; are overriden anyway for consistancy.
      (define/augment (can-interactive-move? event) false)
      (define/augment (can-interactive-resize? snip) false)
      (define/override (get-dragable) false)
      (define/override (get-selection-visible) false)
      (super-new)))
  )
