#lang racket/base
(require ffi/unsafe
         ffi/unsafe/objc
         racket/class
         racket/draw/unsafe/cairo
         racket/draw/private/local
         racket/draw/unsafe/bstr
         racket/draw/private/bitmap
         "utils.rkt"
         "types.rkt"
         "const.rkt"
         "cg.rkt"
         "../../lock.rkt"
         (only-in '#%foreign ffi-callback))

(provide 
 (protect-out bitmap->image
              image->bitmap))

(import-class NSImage NSGraphicsContext)

(define NSCompositeCopy 1)

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
  (define w (send bm get-width))
  (define h (send bm get-height))
  (define s (if (version-10.6-or-later?)
                (send bm get-backing-scale)
                ;; In 10.5, `NSImage setSize:` clips instead of
                ;; scaling, so just use scale of 1:
                1))
  (cond
   [(= s 1) (bitmap->image* bm w h w h)]
   [else 
    (define (scale v) (inexact->exact (ceiling (* s v))))
    (define sw (scale w))
    (define sh (scale h))
    (define bm2 (make-bitmap sw sh))
    (define dc (send bm2 make-dc))
    (send dc set-scale s s)
    (send dc draw-bitmap bm 0 0)
    (bitmap->image* bm2 sw sh w h)]))

(define (bitmap->image* bm w h iw ih)
  (let ([str (make-bytes (* w h 4) 255)])
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
           (tellv i setSize: #:type _NSSize (make-NSSize iw ih))
           i))))))

(define (image->bitmap i)
  (let* ([s (tell #:type _NSSize i size)]
         [w (NSSize-width s)]
         [h (NSSize-height s)]
         [bm (make-object quartz-bitmap% 
                          (inexact->exact (ceiling w))
                          (inexact->exact (ceiling h)))]
         [surface (let ([s (send bm get-cairo-surface)])
                    (cairo_surface_flush s)
                    s)]
         [cg (cairo_quartz_surface_get_cg_context surface)]
         [gc (tell NSGraphicsContext
                   graphicsContextWithGraphicsPort: #:type _pointer cg
                   flipped: #:type _BOOL #f)])
    (CGContextSaveGState cg)
    (CGContextTranslateCTM cg 0 h)
    (CGContextScaleCTM cg 1 -1)
    (tellv NSGraphicsContext saveGraphicsState)
    (tellv NSGraphicsContext setCurrentContext: gc)
    (let ([r (make-NSRect (make-NSPoint 0 0) (make-NSSize w h))])
      (tellv i drawInRect: #:type _NSRect r fromRect: #:type _NSRect r 
             operation: #:type _int NSCompositeCopy fraction: #:type _CGFloat 1.0))
    (tellv NSGraphicsContext restoreGraphicsState)
    (CGContextRestoreGState cg)
    (cairo_surface_mark_dirty surface)
    bm))
