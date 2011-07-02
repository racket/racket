(module embedded-gui mzscheme
  
  (define-syntax (require/provide stx)
    (syntax-case stx ()
      [(_ filename ...)
      #'(begin (require filename ...)
               (provide (all-from filename) ...))]))
  
  (require/provide
   "private/interface.rkt"
   
   "private/aligned-pasteboard.rkt"
   "private/verthoriz-alignment.rkt"
   "private/grid-alignment.rkt"
   "private/snip-wrapper.rkt"
   
   "private/on-show-editor.rkt"
   
   "private/button-snip.rkt"
   "private/embedded-message.rkt"
   "private/fixed-width-label-snip.rkt"
   "private/lines.rkt"
   
   "private/stretchable-editor-snip.rkt"
   "private/tabbable-text.rkt"
   "private/grey-editor.rkt"
   "private/single-line-text.rkt"
   "private/cue-text.rkt"
   
   "private/snip-lib.rkt")
  )
