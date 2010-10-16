#lang racket/base
(require ffi/unsafe
         ffi/unsafe/objc
         racket/class
         "../common/bstr.rkt"
         "utils.rkt"
         "types.rkt"
         "const.rkt"
         "cg.rkt"
         "../../lock.rkt"
         (only-in '#%foreign ffi-callback))

(provide bitmap->image)

(import-class NSImage NSGraphicsContext)

(define _CGImageRef (_cpointer 'CGImageRef))
(define _CGColorSpaceRef (_cpointer 'CGColorSpaceRef))
(define _CGDataProviderRef (_cpointer 'GCDataProviderRef))

(define _CGRect _NSRect)

(define _size_t _long)
(define _off_t _long)

(define-appserv CGColorSpaceCreateDeviceRGB (_fun -> _CGColorSpaceRef))
(define-appserv CGColorSpaceRelease (_fun _CGColorSpaceRef -> _void))

(define-appserv CGImageCreate (_fun _size_t ; w
                                    _size_t ; h
                                    _size_t ; bitsPerComponent
                                    _size_t ; bitsPerPixel
                                    _size_t ; bytesPerRow
                                    _CGColorSpaceRef ; colorspace
                                    _int ; bitmapInfo
                                    _CGDataProviderRef ; provider
                                    _pointer ; CGFloat decode[]
                                    _bool ; shouldInterpolate
                                    _int ; intent
                                    -> _CGImageRef))

(define-appserv CGContextDrawImage (_fun _CGContextRef _CGRect _CGImageRef -> _void))

(define free-it
  (ffi-callback free (list _pointer) _void #f #t))

(define-appserv CGDataProviderCreateWithData (_fun _pointer _pointer _size_t _fpointer
                                                   -> _CGDataProviderRef))
(define-appserv CGDataProviderRelease (_fun _CGDataProviderRef -> _void))

(define (get-image-bytes info)
  info)
(define (release-image-bytes info bytes)
  (void))
(define (get-bytes-at-position bytes dest-bytes start count)
  (memcpy dest-bytes (ptr-add bytes start) count))
(define (release-info info)
  (free info))

(define (bitmap->image bm)
  (let* ([w (send bm get-width)]
         [h (send bm get-height)]
         [str (make-bytes (* w h 4) 255)])
    (send bm get-argb-pixels 0 0 w h str #f)
    (let ([mask (send bm get-loaded-mask)])
      (when mask
        (send mask get-argb-pixels 0 0 w h str #t)))
    (atomically
     (let ([rgba (scheme_make_sized_byte_string (malloc (* w h 4) 'raw) (* w h 4) 0)])
       (memcpy rgba str (sub1 (* w h 4)))
       (let* ([cs (CGColorSpaceCreateDeviceRGB)]
              [provider (CGDataProviderCreateWithData #f rgba (* w h 4) free-it)]
              [image (CGImageCreate w
                                    h
                                    8
                                    32
                                    (* 4 w)
                                    cs
                                    (bitwise-ior kCGImageAlphaFirst
                                                 kCGBitmapByteOrder32Big)
                                    provider ; frees `rgba'
                                    #f
                                    #f
                                    0)])
         (CGDataProviderRelease provider)
         (CGColorSpaceRelease cs)
         ;; This works on 10.6 and later:
         #;
         (as-objc-allocation
          (tell (tell NSImage alloc) 
                initWithCGImage: #:type _CGImageRef image
                size: #:type _NSSize (make-NSSize w h)))
         ;; To work with older versions:
         (let* ([size (make-NSSize w h)]
                [i (as-objc-allocation
                    (tell (tell NSImage alloc) 
                          initWithSize: #:type _NSSize size))])
           (tellv i lockFocus)
           (CGContextDrawImage
            (tell #:type _CGContextRef (tell NSGraphicsContext currentContext) graphicsPort)
            (make-NSRect (make-NSPoint 0 0) size)
            image)
           (tellv i unlockFocus)
           i))))))

