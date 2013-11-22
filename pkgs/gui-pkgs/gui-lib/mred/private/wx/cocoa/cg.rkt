#lang racket/base
(require ffi/unsafe
         ffi/unsafe/objc
         ffi/unsafe/alloc
         "types.rkt"
         "utils.rkt")

(provide (protect-out (all-defined-out)))

(define-cstruct _CGAffineTransform ([a _CGFloat]
                                    [b _CGFloat]
                                    [c _CGFloat]
                                    [d _CGFloat]
                                    [e _CGFloat]
                                    [f _CGFloat]))

(define _CGContextRef (_cpointer 'CGContextRef))
(define-appserv CGContextSynchronize (_fun _CGContextRef -> _void))
(define-appserv CGContextFlush (_fun _CGContextRef -> _void))
(define-appserv CGContextTranslateCTM (_fun _CGContextRef _CGFloat _CGFloat -> _void))
(define-appserv CGContextScaleCTM (_fun _CGContextRef _CGFloat _CGFloat -> _void))
(define-appserv CGContextRotateCTM (_fun _CGContextRef _CGFloat -> _void))
(define-appserv CGContextSaveGState (_fun _CGContextRef -> _void))
(define-appserv CGContextRestoreGState (_fun _CGContextRef -> _void))
(define-appserv CGContextConcatCTM (_fun _CGContextRef _CGAffineTransform -> _void))
(define-appserv CGContextSetRGBFillColor (_fun _CGContextRef _CGFloat _CGFloat _CGFloat _CGFloat -> _void))
(define-appserv CGContextFillRect (_fun _CGContextRef _NSRect -> _void))
(define-appserv CGContextClearRect (_fun _CGContextRef _NSRect -> _void))
(define-appserv CGContextAddRect (_fun _CGContextRef _NSRect -> _void))
(define-appserv CGContextAddLines (_fun _CGContextRef (v : (_vector i _NSPoint)) (_long = (vector-length v)) -> _void))
(define-appserv CGContextStrokePath (_fun _CGContextRef -> _void))
(define-appserv CGContextClipToRects (_fun _CGContextRef (_vector i _NSRect) _size -> _void))
(define-appserv CGContextSetAlpha (_fun _CGContextRef _CGFloat -> _void))

(define _CGLayerRef (_cpointer 'CGLayerRef))
(define-appserv CGLayerRelease (_fun _CGLayerRef ->  _void)
  #:wrap (deallocator))
(define-appserv CGLayerCreateWithContext (_fun _CGContextRef _NSSize _pointer -> _CGLayerRef)
  #:wrap (allocator CGLayerRelease))
(define-appserv CGLayerGetContext (_fun _CGLayerRef -> _CGContextRef))
(define-appserv CGLayerGetSize (_fun _CGLayerRef ->  _NSSize))
(define-appserv CGContextDrawLayerAtPoint (_fun _CGContextRef _NSPoint _CGLayerRef -> _void))
(define-appserv CGContextDrawLayerInRect (_fun _CGContextRef _NSRect _CGLayerRef -> _void))
