#lang scheme/base
(require ffi/unsafe/objc
         ffi/unsafe
         ffi/unsafe/alloc
         ffi/unsafe/define
         "../common/utils.rkt")

(provide cocoa-lib
         cf-lib
         define-cocoa
         define-cf
         define-appserv
         define-appkit
         define-mz
         as-objc-allocation
         retain release
         with-autorelease
         clean-menu-label
         ->wxb
         ->wx)

(define cocoa-lib (ffi-lib (format "/System/Library/Frameworks/Cocoa.framework/Cocoa")))
(define cf-lib (ffi-lib (format "/System/Library/Frameworks/CoreFoundation.framework/CoreFoundation")))
(define appserv-lib (ffi-lib (format "/System/Library/Frameworks/ApplicationServices.framework/ApplicationServices")))
(define appkit-lib (ffi-lib (format "/System/Library/Frameworks/AppKit.framework/AppKit")))

(define-ffi-definer define-cocoa cocoa-lib)
(define-ffi-definer define-cf cf-lib)
(define-ffi-definer define-appserv appserv-lib)
(define-ffi-definer define-appkit appkit-lib)

(define (objc-delete v)
  (tellv v release))

(define objc-allocator (allocator objc-delete))

(define-syntax-rule (as-objc-allocation expr)
  ((objc-allocator (lambda () expr))))

(define release ((deallocator) objc-delete))
(define retain ((retainer release car)
                (lambda (obj)
                  (tellv obj retain))))

(import-class NSAutoreleasePool)

;; Use `with-autorelease' and `call-with-autorelease' 
;; in atomic mode
(define-syntax-rule (with-autorelease expr ...)
  (call-with-autorelease (lambda () expr ...)))
(define (call-with-autorelease thunk)
  (let ([pool (tell (tell NSAutoreleasePool alloc) init)])
    (begin0
     (thunk)
     (release pool))))

(define (clean-menu-label str)
  (regexp-replace* #rx"&(.)" str "\\1"))

(define (->wxb wx)
  (make-weak-box wx))

(define (->wx wxb)
  (and wxb
       (weak-box-value wxb)))
