#lang racket/base
(require ffi/unsafe
         ffi/unsafe/objc
         "types.rkt"
         "utils.rkt")

(provide (protect-out (all-defined-out)))

(define _CGContextRef (_cpointer 'CGContextRef))
(define-appserv CGContextSynchronize (_fun _CGContextRef -> _void))
(define-appserv CGContextFlush (_fun _CGContextRef -> _void))
(define-appserv CGContextTranslateCTM (_fun _CGContextRef _CGFloat _CGFloat -> _void))
(define-appserv CGContextScaleCTM (_fun _CGContextRef _CGFloat _CGFloat -> _void))
(define-appserv CGContextRotateCTM (_fun _CGContextRef _CGFloat -> _void))
(define-appserv CGContextSaveGState (_fun _CGContextRef -> _void))
(define-appserv CGContextRestoreGState (_fun _CGContextRef -> _void))
(define-appserv CGContextSetRGBFillColor (_fun _CGContextRef _CGFloat _CGFloat _CGFloat _CGFloat -> _void))
(define-appserv CGContextFillRect (_fun _CGContextRef _NSRect -> _void))
(define-appserv CGContextAddRect (_fun _CGContextRef _NSRect -> _void))
(define-appserv CGContextAddLines (_fun _CGContextRef (v : (_vector i _NSPoint)) (_long = (vector-length v)) -> _void))
(define-appserv CGContextStrokePath (_fun _CGContextRef -> _void))
