#lang racket/base
(require racket/class
         ffi/unsafe
         ffi/unsafe/define
         ffi/unsafe/alloc
         racket/draw/private/gl-config
         (prefix-in draw: racket/draw/private/gl-context)
         "types.rkt"
         "utils.rkt")

(provide
 (protect-out create-gl-context))

(define opengl32-lib (ffi-lib "opengl32.dll"))

(define-ffi-definer define-opengl32 opengl32-lib)

(define _HGLRC (_cpointer/null 'HGLRC))

(define-cstruct _PIXELFORMATDESCRIPTOR
  ([nSize _WORD]
   [nVersion _WORD]
   [dwFlags _DWORD]
   [iPixelType _BYTE]
   [cColorBits _BYTE]
   [cRedBits _BYTE]
   [cRedShift _BYTE]
   [cGreenBits _BYTE]
   [cGreenShift _BYTE]
   [cBlueBits _BYTE]
   [cBlueShift _BYTE]
   [cAlphaBits _BYTE]
   [cAlphaShift _BYTE]
   [cAccumBits _BYTE]
   [cAccumRedBits _BYTE]
   [cAccumGreenBits _BYTE]
   [cAccumBlueBits _BYTE]
   [cAccumAlphaBits _BYTE]
   [cDepthBits _BYTE]
   [cStencilBits _BYTE]
   [cAuxBuffers _BYTE]
   [iLayerType _BYTE]
   [bReserved _BYTE]
   [dwLayerMask _DWORD]
   [dwVisibleMask _DWORD]
   [dwDamageMask _DWORD]))

(define-gdi32 ChoosePixelFormat (_wfun _HDC _PIXELFORMATDESCRIPTOR-pointer -> _int))
(define-gdi32 SetPixelFormat (_wfun _HDC _int _PIXELFORMATDESCRIPTOR-pointer -> _BOOL))
(define-gdi32 DescribePixelFormat (_wfun _HDC _int _UINT _PIXELFORMATDESCRIPTOR-pointer -> (r : _int)
                                         -> (if (zero? r)
                                                (failed 'DescribePixelFormat)
                                                r)))
(define-gdi32 SwapBuffers (_wfun _HDC -> _BOOL))

(define-opengl32 wglDeleteContext (_wfun _HGLRC -> (r : _BOOL)
                                         -> (unless r (failed 'wglDeleteContext)))
  #:wrap (deallocator))
(define-opengl32 wglCreateContext (_wfun _HDC -> _HGLRC)
  #:wrap (allocator wglDeleteContext))

(define-opengl32 wglMakeCurrent (_wfun _HDC _HGLRC -> _BOOL))

;; ----------------------------------------

(define gl-context% 
  (class draw:gl-context%
    (init-field [hglrc hglrc]
                [hdc hdc])

    (define/override (draw:do-call-as-current t)
      (dynamic-wind
          (lambda ()
            (wglMakeCurrent hdc hglrc))
          t
          (lambda ()
            (wglMakeCurrent #f #f))))
        
    (define/override (draw:do-swap-buffers)
      (SwapBuffers hdc))
    
    (super-new)))

;; ----------------------------------------

(define PFD_DOUBLEBUFFER            #x00000001)
(define PFD_STEREO                  #x00000002)
(define PFD_DRAW_TO_WINDOW          #x00000004)
(define PFD_DRAW_TO_BITMAP          #x00000008)
(define PFD_SUPPORT_GDI             #x00000010)
(define PFD_SUPPORT_OPENGL          #x00000020)
(define PFD_NEED_PALETTE            #x00000080)
(define PFD_NEED_SYSTEM_PALETTE     #x00000100)
(define PFD_GENERIC_ACCELERATED     #x00001000)
(define PFD_TYPE_RGBA    0)
(define PFD_MAIN_PLANE   0)

(define (create-gl-context hdc config offscreen?)
  (let* ([config (or config (new gl-config%))]
         [accum (send config get-accum-size)]
         [pfd
          (make-PIXELFORMATDESCRIPTOR
           (ctype-sizeof _PIXELFORMATDESCRIPTOR)
           1 ; version
           (bitwise-ior
            PFD_SUPPORT_OPENGL
            (if (send config get-stereo) PFD_STEREO 0)
            (if (and (not offscreen?) 
                     (send config get-double-buffered))
                PFD_DOUBLEBUFFER
                0)
            (if offscreen?
                (bitwise-ior PFD_DRAW_TO_BITMAP
                             PFD_SUPPORT_GDI)
                (bitwise-ior PFD_DRAW_TO_WINDOW)))
           PFD_TYPE_RGBA ; color type
           (if offscreen? 32 24) ; prefered color depth
           0 0 0 0 0 0 ; color bits (ignored)
           0 ; no alpha buffer
           0 ; alpha bits (ignored)
           (* 4 accum) ; no accumulation buffer
           accum accum accum accum ; accum bits
           (if offscreen? 32 (send config get-depth-size)) ; depth buffer
           (send config get-stencil-size) ; stencil buffer
           0 ; no auxiliary buffers
           PFD_MAIN_PLANE ; main layer
           0 ; reserved 
           0 0 0 ; no layer, visible, damage masks
           )]
         [pixelFormat (ChoosePixelFormat hdc pfd)])
    (and (not (zero? pixelFormat))
         (SetPixelFormat hdc pixelFormat pfd)
         (begin
           (DescribePixelFormat hdc pixelFormat (ctype-sizeof _PIXELFORMATDESCRIPTOR) pfd)
           (when (not (zero? (bitwise-and (PIXELFORMATDESCRIPTOR-dwFlags pfd)
                                          PFD_NEED_PALETTE)))
             (log-error "don't know how to create a GL palette, yet"))
           (let ([hglrc (wglCreateContext hdc)])
             (and hglrc
                  (new gl-context% [hglrc hglrc] [hdc hdc])))))))
