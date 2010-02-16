#lang scheme/base

(require ffi/objc)

(error 'objc-unsafe! "only `for-label' use in the documentation")

(objc-unsafe!)

(provide (protect-out objc_msgSend/typed
                      objc_msgSendSuper/typed
                      import-class
                      import-protocol
                      get-ivar set-ivar!
                      selector
                      tell tellv
                      define-objc-class
                      define-objc-mixin
                      objc_lookUpClass
                      objc_getProtocol
                      sel_registerName
                      objc_allocateClassPair
                      objc_registerClassPair
                      object_getClass
                      class_addIvar
                      object_getInstanceVariable
                      object_setInstanceVariable
                      objc-is-a?)
         (all-from-out ffi/objc))
