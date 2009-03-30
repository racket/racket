#lang scheme/base

(require ffi/objc)

(error 'objc-unsafe! "only `for-label' use in the documentation")

(objc-unsafe!)

(provide (protect-out objc_msgSend/typed
                      objc_msgSendSuper/typed
                      import-class
                      get-ivar set-ivar!
                      selector
                      tell tellv
                      define-objc-class)
         (all-from-out ffi/objc))
