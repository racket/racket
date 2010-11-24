#lang racket/base
(require ffi/unsafe/objc
         ffi/unsafe
         "../../lock.rkt"
         "utils.rkt")

(provide 
 (protect-out _NSInteger _NSUInteger _OSStatus
              _CGFloat
              _NSPoint _NSPoint-pointer (struct-out NSPoint)
              _NSSize _NSSize-pointer (struct-out NSSize)
              _NSRect _NSRect-pointer (struct-out NSRect)
              _NSRange _NSRange-pointer (struct-out NSRange)
              NSObject 
              NSString _NSString
              NSNotFound))

(define _NSInteger _long)
(define _NSUInteger _ulong)

(define _OSStatus _sint32)

(define 64-bit? (= (ctype-sizeof _long) 8))

(define _CGFloat (make-ctype (if 64-bit? _double _float)
                             (lambda (v) (if (and (number? v)
                                                  (exact? v))
                                             (exact->inexact v)
                                             v))
                             #f))

(define-cstruct _NSPoint ([x _CGFloat]
                          [y _CGFloat]))
(define-cstruct _NSSize ([width _CGFloat]
                         [height _CGFloat]))

(define-cstruct _NSRect ([origin _NSPoint][size _NSSize]))

(define-cstruct _NSRange ([location _NSUInteger]
                          [length _NSUInteger]))

(import-class NSObject NSString)

(define strings (make-weak-hash))
(define _NSString (make-ctype _id
                              (lambda (v)
                                (or (hash-ref strings v #f)
                                    (let ([s (as-objc-allocation
                                              (tell (tell NSString alloc)
                                                    initWithUTF8String:
                                                    #:type _string
                                                    v))])
                                      (hash-set! strings v s)
                                      s)))
                              (lambda (v)
                                (atomically
                                 (with-autorelease
                                  (let ([s (tell #:type _bytes v UTF8String)])
                                    (bytes->string/utf-8 s)))))))

(define NSNotFound (if 64-bit?
                       #x7fffffffffffffff
                       #x7fffffff))
