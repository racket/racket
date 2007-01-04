#| This module provides a mixin that gives a snip a method called convert-to-string.
   This method finds the editor that contains the snip and if it's a text it replaces
   the snip with a string in that editor.
|#

(module convert-to-string mzscheme
  
  (require
   (lib "class.ss")
   (lib "mred.ss" "mred")
   (lib "framework.ss" "framework")
   (lib "embedded-gui.ss" "embedded-gui")
   (lib "contract.ss"))
  
  (provide/contract
   (convert-to-string-mixin mixin-contract)
   (convert-to-string<%> interface?))
  
   (define convert-to-string<%>
     (interface ()
       convert-to-string))
   
   (define convert-to-string-mixin
     (mixin ((class->interface snip%)) (convert-to-string<%>)
       (inherit get-admin get-text)
       
       (define/public (convert-to-string str)
         (let ([to-ed (snip-parent this)])
           (when (is-a? to-ed text%)
             (let ([this-pos (send to-ed get-snip-position this)])
               (when this-pos
                 (send to-ed begin-edit-sequence)
                 (send to-ed delete this-pos (add1 this-pos))
                 (send to-ed insert (string-length str) str this-pos)
                 (send to-ed end-edit-sequence))))))
       
       (super-new)))
  )
