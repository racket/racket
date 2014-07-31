#lang racket/base
(require racket/class
         ffi/unsafe
         ffi/unsafe/define
         ffi/unsafe/alloc
         "../../lock.rkt"
         "utils.rkt"
         racket/draw/unsafe/cairo
         racket/draw/private/local
         racket/draw/private/gl-context
         racket/draw/private/gl-config
         racket/draw/private/bitmap)

(provide (protect-out create-gl-bitmap))

(define cgl-lib 
  (ffi-lib "/System/Library/Frameworks/OpenGL.framework/OpenGL"))

(define-ffi-definer define-cgl cgl-lib)

(define _GLsizei _int)
(define _GLenum _int)
(define _GLboolean _bool)
(define _GLint _int)
(define _GLuint _uint)
(define _CGLPixelFormatAttribute _int)
(define _CGLError _int)
(define _CGLPixelFormatObj (_cpointer/null 'CGLPixelFormatObj))
(define _CGLContextObj (_cpointer/null 'CGLContextObj))

(define (check-ok who r)
  (unless (zero? r)
    (error who "failed\n   error code: ~e" r)))

(define-cgl CGLChoosePixelFormat (_fun (_list i _CGLPixelFormatAttribute)
                                       (fmt : (_ptr o _CGLPixelFormatObj))
                                       (n : (_ptr o _GLint))
                                       -> (r : _CGLError)
                                       -> (and (zero? r) fmt)))
(define-cgl CGLDestroyPixelFormat (_fun _CGLPixelFormatObj
                                        -> (r : _CGLError)
                                        -> (check-ok 'CGLDestroyPixelFormat r)))

(define-cgl CGLDestroyContext (_fun _CGLContextObj
                                    -> (r : _CGLError)
                                    -> (check-ok 'CGLDestroyContext r))
  #:wrap (deallocator))
(define-cgl CGLCreateContext (_fun _CGLPixelFormatObj
                                   _CGLContextObj
                                   (ctx : (_ptr o _CGLContextObj))
                                   -> (r : _CGLError)
                                   -> (and (zero? r) ctx))
  #:wrap (allocator CGLDestroyContext))

(define-cgl CGLSetOffScreen (_fun _CGLContextObj _GLsizei _GLsizei _GLsizei _pointer
                                  -> (r : _CGLError)
                                  -> (check-ok 'CGLSetOffScreen r)))

(define-cgl CGLSetCurrentContext (_fun _CGLContextObj 
                                       -> (r : _CGLError)
                                       -> (check-ok 'CGLSetCurrentContext r)))

(define-cgl glGenFramebuffersEXT (_fun _GLint (fb : (_ptr o _GLuint))
                                       -> _void
                                       -> fb))
(define-cgl glBindFramebufferEXT (_fun _GLenum _GLuint
                                       -> _void))

(define-cgl glGenRenderbuffersEXT (_fun _GLsizei (txt : (_ptr o _GLuint))
                                        -> _void
                                        -> txt))
(define-cgl glBindRenderbufferEXT (_fun _GLenum _GLuint
                                        -> _void))
(define-cgl glRenderbufferStorageEXT (_fun _GLenum _GLenum _GLsizei _GLsizei
                                           -> _void))
(define-cgl glFramebufferRenderbufferEXT (_fun _GLenum _GLenum _GLenum _GLuint
                                            -> _void))
(define-cgl glReadPixels (_fun _GLint _GLint _GLsizei _GLsizei _GLenum _GLenum _pointer
                               -> _void))

(define GL_FRAMEBUFFER_EXT #x8D40)
(define GL_TEXTURE_2D #x0DE1)
(define GL_RENDERBUFFER_EXT #x8D41)
(define GL_RGBA #x1908)
(define GL_RGBA8 #x8058)
(define GL_DEPTH_COMPONENT16 #x81A5)
(define GL_UNSIGNED_BYTE #x1401)
(define GL_COLOR_ATTACHMENT0_EXT #x8CE0)
(define GL_DEPTH_ATTACHMENT_EXT #x8D00)

(define kCGLPFAAllRenderers           1)
(define kCGLPFADoubleBuffer           5)
(define kCGLPFAStereo                 6)
(define kCGLPFAAuxBuffers             7)
(define kCGLPFAColorSize              8)
(define kCGLPFAAlphaSize             11)
(define kCGLPFADepthSize             12)
(define kCGLPFAStencilSize           13)
(define kCGLPFAAccumSize             14)
(define kCGLPFAMinimumPolicy         51)
(define kCGLPFAMaximumPolicy         52)
(define kCGLPFAOffScreen             53)
(define kCGLPFAFullScreen            54)
(define kCGLPFASampleBuffers         55)
(define kCGLPFASamples               56)
(define kCGLPFAAuxDepthStencil       57)
(define kCGLPFAColorFloat            58)
(define kCGLPFAMultisample           59)
(define kCGLPFASupersample           60)
(define kCGLPFASampleAlpha           61)
(define kCGLPFARendererID            70)
(define kCGLPFASingleRenderer        71)
(define kCGLPFANoRecovery            72)
(define kCGLPFAAccelerated           73)
(define kCGLPFAClosestPolicy         74)
(define kCGLPFARobust                75)
(define kCGLPFABackingStore          76)
(define kCGLPFAMPSafe                78)
(define kCGLPFAWindow                80)
(define kCGLPFAMultiScreen           81)
(define kCGLPFACompliant             83)
(define kCGLPFADisplayMask           84)
(define kCGLPFAPBuffer               90)
(define kCGLPFARemotePBuffer         91)
(define kCGLPFAAllowOfflineRenderers   96)
(define kCGLPFAAcceleratedCompute    97)
(define kCGLPFAOpenGLProfile         99)
(define kCGLPFAVirtualScreenCount   128)

(define dummy-cgl #f)
(define current-cgl #f)

(define cgl-context%
  (let ([orig-gl-context% gl-context%])
    (define gl-context%
      (class orig-gl-context%
        (init-field cgl touched)

        (define/override (get-handle)
          cgl)
        
        (define/override (do-call-as-current t)
          (dynamic-wind
              (lambda ()
                (set-box! touched #t)
                (atomically
                 (CGLSetCurrentContext cgl)
                 (set! current-cgl cgl)))
              t
              (lambda () 
                (atomically
                 (CGLSetCurrentContext dummy-cgl)
                 (set! current-cgl #f)))))

        (define/override (do-swap-buffers)
          (void))

        (super-new)))
    gl-context%))
    

(define cgl-bitmap%
  (let ([orig-bitmap% bitmap%])
    (define bitmap%
      (class orig-bitmap%
        (init _cgl w h)
        (super-make-object w h)

        (define cgl _cgl)
        (define width w)
        (define height h)

        (define bstr (make-bytes (* w h 4)))
        (define row-bstr (make-bytes (* w w)))

        (define touched (box #f))
        
        (define ctx (make-object cgl-context% cgl touched))

        (define/override (get-bitmap-gl-context)
          ctx)

        (define/override (get-cairo-surface)
          (surface-flush)
          (super get-cairo-surface))

        (define/override (surface-flush)
          (when (version-10.7-or-later?)
            (define s (super get-cairo-surface))
            (atomically
             (CGLSetCurrentContext cgl)
             (glReadPixels 0 0 width height GL_RGBA GL_UNSIGNED_BYTE bstr)
             (CGLSetCurrentContext (or current-cgl dummy-cgl)))
            (cond
             [(system-big-endian?)
              ;; need ARGB
              (for ([i (in-range 0 (* width height 4) 4)])
                (define a (bytes-ref bstr (+ i 3)))
                (bytes-set! bstr (+ i 1) (bytes-ref bstr i))
                (bytes-set! bstr (+ i 2) (bytes-ref bstr (+ i 1)))
                (bytes-set! bstr (+ i 3) (bytes-ref bstr (+ i 2)))
                (bytes-set! bstr i a))]
             [else
              ;; need GBRA
              (for ([i (in-range 0 (* width height 4) 4)])
                (define g (bytes-ref bstr i))
                (bytes-set! bstr i (bytes-ref bstr (+ i 2)))
                (bytes-set! bstr (+ i 2) g))])          
            ;; flip upside-down
            (for ([i (in-range (quotient height 2))])
              (define above-row (ptr-add bstr (* 4 i width)))
              (define below-row (ptr-add bstr (* 4 (- height i) width)))
              (memcpy row-bstr above-row (* 4 width))
              (memcpy above-row below-row (* 4 width))
              (memcpy below-row row-bstr (* 4 width)))
            ;; assuming that stride = width
            (memcpy (cairo_image_surface_get_data s) bstr (* width height 4)))
          (super surface-flush))

        (define/override (release-bitmap-storage)
          (set! ctx #f)
          (super release-bitmap-storage))))
    bitmap%))

(define (create-gl-bitmap w h conf)
  (let* ([share-context (send conf get-share-context)]
         [context-handle (if share-context (send share-context get-handle) #f)]
         [fmt (CGLChoosePixelFormat
               (append
                (list kCGLPFASampleAlpha
                      kCGLPFAColorSize 32)
                (if (version-10.7-or-later?)
                    null ; must use framebuffers
                    (list kCGLPFAOffScreen))
                (if (send conf get-stereo) (list kCGLPFAStereo) null)
                (list
                 kCGLPFADepthSize (send conf get-depth-size)
                 kCGLPFAStencilSize (send conf get-stencil-size))
                (let ([as (send conf get-accum-size)])
                  (if (or (version-10.7-or-later?) ; deprecated in 10.7 and later
                          (zero? as))
                      null
                      (list kCGLPFAAccumSize as)))
                (let ([ms (send conf get-multisample-size)])
                  (if (zero? ms)
                      null
                      (list kCGLPFASampleBuffers 1
                            kCGLPFASamples ms)))
                (list 0)))])
    (and fmt
         (let ([cgl (CGLCreateContext fmt context-handle)]
               [d-cgl (or dummy-cgl
                          (let ([d (CGLCreateContext fmt #f)])
                            (when d
                              (set! dummy-cgl d)
                              d)))])
           (and cgl
                d-cgl
                (let ([bm (make-object cgl-bitmap% cgl w h #f #t)])
                  (and (send bm ok?)
                       (let ([s (send bm get-cairo-surface)])
                         (and (cond
                               [(version-10.7-or-later?)
                                (atomically
                                 (CGLSetCurrentContext cgl)

                                 (define fb (glGenFramebuffersEXT 1))
                                 (glBindFramebufferEXT GL_FRAMEBUFFER_EXT fb)

                                 (define rb (glGenRenderbuffersEXT 1))
                                 (glBindRenderbufferEXT GL_RENDERBUFFER_EXT rb)
                                 (glRenderbufferStorageEXT GL_RENDERBUFFER_EXT GL_RGBA8 w h)
                                 (glFramebufferRenderbufferEXT GL_FRAMEBUFFER_EXT GL_COLOR_ATTACHMENT0_EXT
                                                               GL_RENDERBUFFER_EXT rb)
                                 
                                 (unless (zero? (send conf get-depth-size))
                                   (define rb2 (glGenRenderbuffersEXT 1))
                                   (glBindRenderbufferEXT GL_RENDERBUFFER_EXT rb2)
                                   (glRenderbufferStorageEXT GL_RENDERBUFFER_EXT GL_DEPTH_COMPONENT16 w h)
                                   (glFramebufferRenderbufferEXT GL_FRAMEBUFFER_EXT GL_DEPTH_ATTACHMENT_EXT
                                                                 GL_RENDERBUFFER_EXT rb2))

                                 (CGLSetCurrentContext (or current-cgl dummy-cgl)))]
                               [else
                                (CGLSetOffScreen cgl w h
                                                 (cairo_image_surface_get_stride s)
                                                 (cairo_image_surface_get_data s))])
                              bm)))))))))

