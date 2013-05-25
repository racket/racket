#lang racket/base
(require drscheme/tool
         mred
         framework
         "text-snipclass.rkt"
         mzlib/unit
         mzlib/class
         string-constants
         mrlib/include-bitmap)

(provide tool@)

(define tool@
  (unit
   (import drscheme:tool^)
   (export drscheme:tool-exports^)
   (define (phase1) (void))
   (define (phase2) (void))

   (define (text-box-mixin %)
     (class %
            (inherit get-insert-menu get-edit-target-object register-capability-menu-item)
            (super-new)
            (new menu-item%
                 (label (string-constant insert-text-box-item))
                 (parent (get-insert-menu))
                 (callback
                  (lambda (menu event)
                    (let ([c-box (new text-box%)]
                          [text (get-edit-target-object)])
                      (send text insert c-box)
                      (send text set-caret-owner c-box 'global)))))
            (register-capability-menu-item 'drscheme:special:insert-text-box (get-insert-menu))))

   (drscheme:get/extend:extend-unit-frame text-box-mixin)

   (drscheme:language:register-capability 'drscheme:special:insert-text-box boolean? #t)))
