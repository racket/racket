#reader scribble/reader
#lang scheme/base
(require (for-syntax scheme/base))

(require scribble/manual
         scribble/basic
         scribble/extract
         scheme/class
         scheme/contract)
(provide (all-from-out scribble/manual)
         (all-from-out scribble/basic)
         (all-from-out scribble/extract)
         (all-from-out scheme/class)
         (all-from-out scheme/contract))

(require (for-label scheme/gui/base
                    scheme/class
                    scheme/contract
                    scheme/base
                    drscheme/tool-lib
		    mrlib/switchable-button
                    framework))
(provide (for-label (all-from-out scheme/gui/base)
                    (all-from-out scheme/class)
                    (all-from-out scheme/contract)
                    (all-from-out scheme/base)
                    (all-from-out drscheme/tool-lib)
                    (all-from-out mrlib/switchable-button)
                    (all-from-out framework)))

(provide tools-title tools-include)
(define (tools-title name)
  (title (tt (format "drscheme:~a" name))))
(define-syntax (tools-include stx)
  (syntax-case stx ()
    [(_ name)
     (string? (syntax-e #'name))
     (let ([name (syntax-e #'name)])
       (with-syntax ([rx (regexp (format "^drscheme:~a:" name))])
         #'(include-previously-extracted "tool-lib-extracts.ss" rx)))]))

(provide docs-get/extend)
(define-syntax (docs-get/extend stx)
  (syntax-case stx ()
    [(_ id)
     (identifier? #'id)
     (with-syntax ([get (datum->syntax
                         #'id
                         (string->symbol
                          (format "drscheme:get/extend:get-~a"
                                  (syntax-e #'id))))]
                   [extend (datum->syntax
                            #'id
                            (string->symbol
                             (format "drscheme:get/extend:extend-~a"
                                     (syntax-e #'id))))])
       #'(begin
           @defproc*[([(extend (mixin mixin-contract))
                       void?]
                      [(extend (mixin mixin-contract) (before boolean?))
                       void?])]{
             Adds a new mixin to the class eventually created in DrScheme.}
          @defproc[(get) class?]{
            Returns the class (with all registered mixins applied).}))]))
