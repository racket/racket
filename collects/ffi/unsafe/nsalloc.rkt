#lang racket/base
(require ffi/unsafe/objc
         ffi/unsafe
         ffi/unsafe/atomic)

(provide (protect-out with-autorelease
                      call-with-autorelease))

;; Make sure Foundation is loaded:
(void (ffi-lib "/System/Library/Frameworks/Foundation.framework/Foundation"
               #:fail (lambda () #f)))

(import-class NSAutoreleasePool)

(define-syntax-rule (with-autorelease expr ...)
  (call-with-autorelease (lambda () expr ...)))

(define (call-with-autorelease thunk)
  (unless NSAutoreleasePool
    (error 'NSAutoreleasePool "not available"))
  (dynamic-wind
   start-atomic
   (lambda ()
     (let ([pool (tell (tell NSAutoreleasePool alloc) init)])
       (dynamic-wind
           void
           thunk
           (lambda ()
             (tellv pool release)))))
   end-atomic))
