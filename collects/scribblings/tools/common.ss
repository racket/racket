#reader scribble/reader
#lang scheme/base
(require (for-syntax scheme/base))

(require scribble/manual
         scribble/basic
         scheme/class
         scheme/contract)
(provide (all-from-out scribble/manual)
         (all-from-out scribble/basic)
         (all-from-out scheme/class)
         (all-from-out scheme/contract))

(require (for-label scheme/gui/base
                    scheme/class
                    scheme/contract
                    scheme/base
                    drscheme/tool-lib
                    framework))
(provide (for-label (all-from-out scheme/gui/base)
                    (all-from-out scheme/class)
                    (all-from-out scheme/contract)
                    (all-from-out scheme/base)
                    (all-from-out drscheme/tool-lib)
                    (all-from-out framework)))

(provide docs-get/extend)
(define-syntax (docs-get/extend stx)
  (syntax-case stx ()
    [(_ id)
     (identifier? #'id)
     (with-syntax ([get (datum->syntax 
                         #'id 
                         (string->symbol 
                          (format "drscheme:get/extend:get-~a" (syntax-e #'id))))]
                   [extend (datum->syntax 
                         #'id 
                         (string->symbol 
                          (format "drscheme:get/extend:extend-~a" (syntax-e #'id))))])
       #'(begin
           @defproc*[([(extend (mixin mixin-contract))
                       void?]
                      [(extend (mixin mixin-contract) (before boolean?))
                       void?])]{
                                Does stuff.
                                }
          @defproc[(get) class?]{Returns the class.}))]))