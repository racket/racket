(module single-line-text mzscheme
  
  (require
   mred
   framework
   mzlib/class)
  
  (provide single-line-text-mixin)
  
  (define single-line-text-mixin
    (mixin (editor:keymap<%>) ()
      #;(-> (listof keymap%))
      ;; the list of keymaps associated with this text
      (define/override (get-keymaps)
        (let ([keymap (make-object keymap%)])
          (send keymap add-function "do nothing" void)
          (send keymap map-function ":enter" "do nothing")
          (cons keymap (super get-keymaps))))
      (super-new)))
  
  #|
  (define f (new frame% (label "f")))
  (define e (new text%))
  (define c (new editor-canvas% (editor e) (parent f)))
  (send e insert (new editor-snip% (editor (new (single-line-text-mixin scheme:text%)))))
  (send f show #t)
  |#
  )
