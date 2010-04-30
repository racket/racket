#lang racket/base
(require (for-syntax racket/base))

(define-syntax-rule (provide-except-unsafe lib u! id ...)
  (begin
    (require lib)
    (provide (except-out (all-from-out lib) id ...))
    (define-syntax (u! stx)
      (syntax-case stx ()
        [(_) (with-syntax ([lib+ids (datum->syntax stx '(lib id ...))])
               #'(require (only-in . lib+ids)))]))))

(provide-except-unsafe 
 ffi/unsafe/objc objc-unsafe!
 
 objc_msgSend/typed
 objc_msgSendSuper/typed
 import-class
 import-protocol
 get-ivar set-ivar!
 selector
 tell tellv
 define-objc-class
 define-objc-mixin
 objc-is-a?)

(provide objc-unsafe!)
