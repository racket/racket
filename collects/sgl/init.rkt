#lang racket/base
(require ffi/unsafe)

;; Apple's GL implementation seg faults when GL commands are used
;; without a context --- which is fair according to the GL spec, but
;; not nice for Racket users. To avoid crashes, install a dummy
;; context if none is already current. On other platforms, this
;; module ends up doing nothing.

(when (eq? (system-type) 'macosx)
  (define agl-lib (ffi-lib "/System/Library/Frameworks/AGL.framework/AGL"))

  (define _GLint _int)
  (define _GLboolean _bool)
  (define _AGLPixelFormat (_cpointer/null 'AGLPixelFormat))
  (define _AGLContext (_cpointer/null 'AGLContext))

  (define-syntax-rule (define-agl name type)
    (define name (get-ffi-obj 'name agl-lib type (lambda () void))))

  (define-agl aglSetCurrentContext (_fun _AGLContext -> _GLboolean))
  (define-agl aglGetCurrentContext (_fun -> _AGLContext))

  (define-agl aglChoosePixelFormat (_fun _pointer _GLint (_list i _GLint) -> _AGLPixelFormat))

  (define-agl aglCreateContext (_fun _AGLPixelFormat _AGLContext -> _AGLContext))

  (define AGL_NONE                   0)
  (define AGL_RGBA                   4)
  (define AGL_PIXEL_SIZE            50)
  (define AGL_OFFSCREEN             53)

  (unless (aglGetCurrentContext)
    (let ([fmt (aglChoosePixelFormat
                #f
                0
                (list AGL_RGBA
                      AGL_PIXEL_SIZE 32
                      AGL_OFFSCREEN
                      AGL_NONE))])
      (when fmt
           (let ([d (aglCreateContext fmt #f)])
             (when d
               (void (aglSetCurrentContext d))))))))

