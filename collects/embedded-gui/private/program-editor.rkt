#|

This file contains a mixin that should be used my any embedded editor that contains
code. What is does is tells the drscheme frame that the program has changed and
drscheme can appropriately reset highlighting and display the "save" button. There
is a facility of drscheme to do this but to my knowledge it is not working properly.

|#

#|NOTES:

This code is copied from the test suite tool and the test suite tool should be
rewritten to use this copy of the program. This is not a trivial change since the
alert-of-modify method in test-suite is currently clearing the test cases to reset
them.

This code can be replaced by drscheme:unit:program-editor-mixin when I figure out how
to make the results of the test case boxes be reset when (and only when) highlighting
is being reset.
|#

(module program-editor mzscheme
  
  (require
   mred
   mzlib/etc
   mzlib/class
   framework
   "snip-lib.rkt")
  
  (provide program-editor-mixin
           program-editor%)
  
  (define (program-editor-mixin %)
    (class %
      (inherit get-admin begin-edit-sequence end-edit-sequence)
      (define (get-frame)
        ;; gets the top most editor in the tree of snips and editors
        (define (editor-root ed)
          (let ([parent (editor-parent ed)])
            (cond
              [(is-a? parent area<%>) parent]
              [(is-a? parent snip%)
               (editor-root (snip-parent parent))]
              [else false])))
        
        ;; gets the canvas or snip that the pasteboard is displayed in
        ;; status: what if there is more than one canvas?
        (define (editor-parent ed)
          (let ([admin (send ed get-admin)])
            (cond
              [(is-a? admin editor-snip-editor-admin<%>)
               (send admin get-snip)]
              [(is-a? admin editor-admin%)
               (send ed get-canvas)]
              [else false])))
        
        (let ([er (editor-root this)])
          (if er
              (send er get-top-level-window)
              false)))
      
      (define (alert-of-modify)
        (let ([frame (get-frame)])
          (when frame
            (send (send frame get-interactions-text) reset-highlighting)
            (send* (send frame get-definitions-text)
              (set-modified true)))))
      
      ;(define/override (on-insert start len)
      ;  (begin-edit-sequence)
      ;  (super on-insert start len)
      ;  (end-edit-sequence))
      
      (define/augment (after-insert start len)
        (alert-of-modify)
        ;(begin-edit-sequence)
        #;(super after-insert start len)
        ;(end-edit-sequence)
        )
      (define/augment (after-delete start len)
        (alert-of-modify)
        #;(super after-delete start len))
      (super-new)))
  
  (define program-editor%
    (program-editor-mixin color:text%))
  )
