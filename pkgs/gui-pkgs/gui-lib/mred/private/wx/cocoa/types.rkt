#lang racket/base
(require ffi/unsafe/objc
         ffi/unsafe
         ffi/unsafe/nsstring
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
              NSNotFound)
 _NSString)

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

(define NSNotFound (if 64-bit?
                       #x7fffffffffffffff
                       #x7fffffff))
