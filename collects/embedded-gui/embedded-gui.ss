(module embedded-gui mzscheme
  
  (define-syntax (require/provide stx)
    (syntax-case stx ()
      [(_ filename ...)
      #'(begin (require filename ...)
               (provide (all-from filename) ...))]))
  
  (require/provide
   "private/interface.ss"
   
   "private/aligned-pasteboard.ss"
   "private/verthoriz-alignment.ss"
   "private/grid-alignment.ss"
   "private/snip-wrapper.ss"
   
   "private/on-show-editor.ss"
   
   "private/button-snip.ss"
   "private/embedded-message.ss"
   "private/fixed-width-label-snip.ss"
   "private/lines.ss"
   
   "private/stretchable-editor-snip.ss"
   "private/tabbable-text.ss"
   "private/grey-editor.ss"
   "private/single-line-text.ss"
   "private/cue-text.ss"
   
   "private/snip-lib.ss")
  )
