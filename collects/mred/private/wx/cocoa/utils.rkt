#lang scheme/base
(require ffi/unsafe/objc
         ffi/unsafe
         ffi/unsafe/alloc
         "../common/utils.rkt")

(provide cocoa-lib
         cf-lib
         define-cocoa
         define-cf
         define-appserv
         define-mz
         as-objc-allocation
         retain release
         with-autorelease)

(define cocoa-lib (ffi-lib (format "/System/Library/Frameworks/Cocoa.framework/Cocoa")))
(define cf-lib (ffi-lib (format "/System/Library/Frameworks/CoreFoundation.framework/CoreFoundation")))
(define appserv-lib (ffi-lib (format "/System/Library/Frameworks/ApplicationServices.framework/ApplicationServices")))

(define-syntax define-cocoa/private
  (syntax-rules ()
    [(_ id type)
     (define-cocoa/private id id type)]
    [(_ id c-id type)
     (define id (get-ffi-obj 'c-id cocoa-lib type))]))

(define-syntax-rule (define-cocoa id type)
  (define-cocoa/private id id type))

(define-syntax-rule (define-cf id type)
  (define id (get-ffi-obj 'id cf-lib type)))

(define-syntax-rule (define-appserv id type)
  (define id (get-ffi-obj 'id appserv-lib type)))

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

(define-syntax-rule (with-autorelease expr)
  (call-with-autorelease (lambda () expr)))
(define (call-with-autorelease thunk)
  (let ([pool (as-objc-allocation
               (tell (tell NSAutoreleasePool alloc) init))])
    (begin0
     (thunk)
     (release pool))))
