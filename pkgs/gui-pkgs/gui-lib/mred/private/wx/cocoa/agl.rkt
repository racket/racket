#lang racket/base
(require racket/class
         ffi/unsafe
         ffi/unsafe/define
         ffi/unsafe/alloc
         "../../lock.rkt"
         racket/draw/unsafe/cairo
         racket/draw/private/local
         racket/draw/private/gl-context
         racket/draw/private/gl-config
         racket/draw/private/bitmap)

(provide (protect-out create-gl-bitmap))

(define agl-lib 
  (ffi-lib "/System/Library/Frameworks/AGL.framework/AGL"))

(define-ffi-definer define-agl agl-lib)

(define _GLsizei _int)
(define _GLint _int)
(define _GLboolean _bool)
(define _AGLPixelFormat (_cpointer/null 'AGLPixelFormat))
(define _AGLContext (_cpointer/null 'AGLContext))

(define-agl aglChoosePixelFormat (_fun _pointer _GLint (_list i _GLint) -> _AGLPixelFormat))
(define-agl aglDestroyContext (_fun _AGLContext -> _GLboolean)
  #:wrap (deallocator))
(define-agl aglCreateContext (_fun _AGLPixelFormat _AGLContext -> _AGLContext)
  #:wrap (allocator aglDestroyContext))

(define-agl aglSetOffScreen (_fun _AGLContext _GLsizei _GLsizei _GLsizei _pointer
                                  -> _GLboolean))

(define-agl aglSetCurrentContext (_fun _AGLContext -> _GLboolean))

(define AGL_NONE                   0)
(define AGL_BUFFER_SIZE            2)
(define AGL_LEVEL                  3)
(define AGL_RGBA                   4)
(define AGL_DOUBLEBUFFER           5)
(define AGL_STEREO                 6)
(define AGL_AUX_BUFFERS            7)
(define AGL_RED_SIZE               8)
(define AGL_GREEN_SIZE             9)
(define AGL_BLUE_SIZE             10)
(define AGL_ALPHA_SIZE            11)
(define AGL_DEPTH_SIZE            12)
(define AGL_STENCIL_SIZE          13)
(define AGL_ACCUM_RED_SIZE        14)
(define AGL_ACCUM_GREEN_SIZE      15)
(define AGL_ACCUM_BLUE_SIZE       16)
(define AGL_ACCUM_ALPHA_SIZE      17)
(define AGL_PIXEL_SIZE            50)
(define AGL_OFFSCREEN             53)
(define AGL_SAMPLE_BUFFERS_ARB    55)
(define AGL_SAMPLES_ARB           56)
(define AGL_AUX_DEPTH_STENCIL     57)
(define AGL_COLOR_FLOAT           58)
(define AGL_MULTISAMPLE           59)
(define AGL_SUPERSAMPLE           60)
(define AGL_SAMPLE_ALPHA          61)

(define dummy-agl #f)
(define current-agl #f)

(define agl-context%
  (let ([orig-gl-context% gl-context%])
    (define gl-context%
      (class orig-gl-context%
        (init-field agl)

        (define/override (get-handle)
          agl)
        
        (define/override (do-call-as-current t)
          (dynamic-wind
              (lambda () 
                (atomically
                 (aglSetCurrentContext agl)
                 (set! current-agl agl)))
              t
              (lambda () 
                (atomically
                 (aglSetCurrentContext dummy-agl)
                 (set! current-agl #f)))))

        (define/override (do-swap-buffers)
          (void))

        (super-new)))
    gl-context%))
    

(define agl-bitmap%
  (let ([orig-bitmap% bitmap%])
    (define bitmap%
      (class orig-bitmap%
        (init agl)
        (super-new)

        (define ctx (make-object agl-context% agl))

        (define/override (get-bitmap-gl-context)
          ctx)

        (define/override (release-bitmap-storage)
          (set! ctx #f)
          (super release-bitmap-storage))))
    bitmap%))

(define (create-gl-bitmap w h conf)
  (let* ([share-context (send conf get-share-context)]
         [context-handle (if share-context (send share-context get-handle) #f)]
         [fmt (aglChoosePixelFormat
              #f
              0
              (append
               (list AGL_RGBA
                     AGL_PIXEL_SIZE 32
                     AGL_OFFSCREEN)
               (if (send conf get-stereo) (list AGL_STEREO) null)
               (list
                AGL_DEPTH_SIZE (send conf get-depth-size)
                AGL_STENCIL_SIZE (send conf get-stencil-size))
               (let ([as (send conf get-accum-size)])
                 (if (zero? as)
                     null
                     (list AGL_ACCUM_RED_SIZE as
                           AGL_ACCUM_GREEN_SIZE as
                           AGL_ACCUM_BLUE_SIZE as
                           AGL_ACCUM_ALPHA_SIZE as)))
               (let ([ms (send conf get-multisample-size)])
                 (if (zero? ms)
                     null
                     (list AGL_SAMPLE_BUFFERS_ARB 1
                           AGL_SAMPLES_ARB ms)))
               (list AGL_NONE)))])
    (and fmt
         (let ([agl (aglCreateContext fmt context-handle)]
               [d-agl (or dummy-agl
                          (let ([d (aglCreateContext fmt context-handle)])
                            (when d
                              (set! dummy-agl d)
                              d)))])
           (and agl
                d-agl
                (let ([bm (make-object agl-bitmap% agl w h #f #t)])
                  (and (send bm ok?)
                       (let ([s (send bm get-cairo-surface)])
                         (and (aglSetOffScreen agl w h
                                               (cairo_image_surface_get_stride s)
                                               (cairo_image_surface_get_data s))
                              bm)))))))))

