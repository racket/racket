#lang racket/base
(require ffi/unsafe/objc
         ffi/unsafe
         ffi/unsafe/alloc
         ffi/unsafe/define
         ffi/unsafe/nsalloc
         "../common/utils.rkt"
         "../../lock.rkt")

(provide 
 (protect-out cocoa-lib
              cf-lib
              define-cocoa
              define-cf
              define-appserv
              define-appkit
              as-objc-allocation
              as-objc-allocation-with-retain
              clean-up-deleted
              retain release
              clean-menu-label
              ->wxb
              ->wx
              old-cocoa?
              version-10.6-or-later?
              version-10.7-or-later?)
 with-autorelease
 call-with-autorelease
 define-mz)

(define cocoa-lib (ffi-lib (format "/System/Library/Frameworks/Cocoa.framework/Cocoa")))
(define cf-lib (ffi-lib (format "/System/Library/Frameworks/CoreFoundation.framework/CoreFoundation")))
(define appserv-lib (ffi-lib (format "/System/Library/Frameworks/ApplicationServices.framework/ApplicationServices")))
(define appkit-lib (ffi-lib (format "/System/Library/Frameworks/AppKit.framework/AppKit")))

(define-ffi-definer define-cocoa cocoa-lib)
(define-ffi-definer define-cf cf-lib)
(define-ffi-definer define-appserv appserv-lib)
(define-ffi-definer define-appkit appkit-lib)

(define delete-me null)

(define (objc-delete o)
  (tellv o release))

(define (clean-up-deleted)
  (free-remembered-now objc-delete))

(define objc-allocator (allocator remember-to-free-later))

(define-syntax-rule (as-objc-allocation expr)
  ((objc-allocator (lambda () expr))))

(define-syntax-rule (as-objc-allocation-with-retain expr)
  ((objc-allocator (lambda () (let ([v expr])
                                (tellv v retain)
                                v)))))

(define release ((deallocator) objc-delete))
(define retain ((retainer release car)
                (lambda (obj)
                  (tellv obj retain))))

(define (clean-menu-label str)
  (regexp-replace* #rx"&(.)" str "\\1"))

(define (->wxb wx)
  (make-weak-box wx))

(define (->wx wxb)
  (and wxb
       (weak-box-value wxb)))

(define-appkit NSAppKitVersionNumber _double)

(define old-cocoa? 
  ; earlier than 10.5?
  (NSAppKitVersionNumber . < . 949))
(define (version-10.6-or-later?)
  (NSAppKitVersionNumber . >= . 1038))
(define (version-10.7-or-later?)
  (NSAppKitVersionNumber . >= . 1138))
