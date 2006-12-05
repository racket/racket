(module tool mzscheme
  
  (provide tool@)
  
  (require
   (lib "class.ss") (lib "contract.ss")
   (lib "mred.ss" "mred") 
   (lib "unit.ss")
   (lib "string-constant.ss" "string-constants")
   (lib "tool.ss" "drscheme")
   "private/example-box.ss"
   "private/interactions-box.ss"
   (lib "text-syntax-object.ss" "test-suite" "private"))
  
  (define-unit extentions@
    (import drscheme:tool^ example-box^ interactions-box^)
    (export drscheme:tool-exports^)
      (define (phase1) (void))
      (define (phase2) (void))
      
      (define (frame-mixin %)
        (class %
          (inherit get-edit-target-object get-special-menu register-capability-menu-item)
          
          ;; this function is copied from the drscheme/private/unit.ss file
          (define (has-editor-on-demand menu-item)
            (let ([edit (get-edit-target-object)])
              (send menu-item enable (and edit (is-a? edit editor<%>)))))
          
          (super-new)
          
          (new menu-item%
               (label (string-constant profjBoxes-insert-java-examples))
               (parent (get-special-menu))
               (callback
                (lambda (menu event)
                  (let ([box (new example-box%)]
                        [text (get-edit-target-object)])
                    (when text
                      (send text begin-edit-sequence)
                      (send text insert box)
                      (send box take-caret)
                      (send text end-edit-sequence)))))
               (demand-callback has-editor-on-demand))
          (register-capability-menu-item 'profj:special:java-examples-box (get-special-menu))
          
          #;(new menu-item%
               (label (string-constant profjBoxes-insert-java-interactions))
               (parent (get-special-menu))
               (callback
                (lambda (menu event)
                  (let ([box (new interactions-box%)]
                        [text (get-edit-target-object)])
                    (when text
                      (send text begin-edit-sequence)
                      (send text insert box)
                      (send box take-caret)
                      (send text end-edit-sequence)))))
               (demand-callback has-editor-on-demand))
          ))

      (drscheme:get/extend:extend-unit-frame frame-mixin)
      (drscheme:language:register-capability 'profj:special:java-examples-box (flat-contract boolean?) #f))
  
  (define tool@
    (compound-unit/infer
      (import drscheme:tool^)
      (export drscheme:tool-exports^)
      (link extentions@ example-box@ interactions-box@ text->syntax-object@)))
  )
