(module text-box-tool mzscheme
  (require (lib "tool.ss" "drscheme")
           (lib "mred.ss" "mred")
           (lib "framework.ss" "framework")
           "text-snipclass.ss"
           (lib "unit.ss")
           (lib "class.ss")
	   (lib "contract.ss")
           (lib "string-constant.ss" "string-constants")
           (lib "include-bitmap.ss" "mrlib"))
  
  (provide tool@)

  (define tool@
    (unit 
      (import drscheme:tool^)
      (export drscheme:tool-exports^) 
      (define (phase1) (void))
      (define (phase2) (void))
            
      (define (text-box-mixin %)
        (class %
          (inherit get-special-menu get-edit-target-object register-capability-menu-item)
          (super-new)
          (new menu-item%
               (label (string-constant insert-text-box-item))
               (parent (get-special-menu))
               (callback
                (lambda (menu event)
                  (let ([c-box (new text-box%)]
                        [text (get-edit-target-object)])
                    (send text insert c-box)
                    (send text set-caret-owner c-box 'global)))))
          (register-capability-menu-item 'drscheme:special:slideshow-menu-item (get-special-menu))))
      
      (drscheme:get/extend:extend-unit-frame text-box-mixin)
      
      (drscheme:language:register-capability 'drscheme:special:insert-text-box (flat-contract boolean?) #t))))
