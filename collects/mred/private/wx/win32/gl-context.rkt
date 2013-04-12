#lang racket/base
(require racket/class
         ffi/unsafe
         ffi/unsafe/define
         ffi/unsafe/alloc
         racket/draw/private/gl-config
         (prefix-in draw: racket/draw/private/gl-context)
	 "../../lock.rkt"
	 "wndclass.rkt"
	 "const.rkt"
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
(define-opengl32 wglGetCurrentContext (_wfun -> _HGLRC))
(define-opengl32 wglGetCurrentDC (_wfun -> _HDC))

(define-opengl32 wglGetProcAddress (_wfun _string -> _fpointer))

(define tried-multisample? #f)
(define wglChoosePixelFormatARB #f)

(define looked-for-createcontextattribs? #f)
(define wglCreateContextAttribsARB #f)

;; ----------------------------------------

(define gl-context% 
  (class draw:gl-context%
    (init-field [hglrc hglrc]
                [hdc hdc])

    (define/override (get-handle) hglrc)

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

(define (call-with-context hdc hglrc f)
  (atomically
   (let ([old-hdc (wglGetCurrentDC)]
	 [old-hglrc (wglGetCurrentContext)])
     (dynamic-wind
	 (lambda ()
	   (wglMakeCurrent hdc hglrc))
	 f
	 (lambda ()
	   (wglMakeCurrent old-hdc old-hglrc))))))

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

(define (create-gl-context hdc config offscreen? 
                           #:try-ms? [try-ms? (not offscreen?)])
  (when try-ms? (unless tried-multisample? (init-multisample! config)))
  (unless looked-for-createcontextattribs? (init-createcontextattribs! config))
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
           (if offscreen? 32 24) ; preferred color depth
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
         [ms (send config get-multisample-size)]
         [pixelFormat (or
		       (and try-ms?
			    wglChoosePixelFormatARB 
			    (or (choose-multisample hdc config offscreen? ms)
				(choose-multisample hdc config offscreen? 2)))
		       (ChoosePixelFormat hdc pfd))]
         [share-context (send config get-share-context)]
         [context-handle (if share-context (send share-context get-handle) #f)])
    (and (not (zero? pixelFormat))
	 (and (SetPixelFormat hdc pixelFormat pfd)
	      (begin
		(DescribePixelFormat hdc pixelFormat (ctype-sizeof _PIXELFORMATDESCRIPTOR) pfd)
		(when (not (zero? (bitwise-and (PIXELFORMATDESCRIPTOR-dwFlags pfd)
					       PFD_NEED_PALETTE)))
		      (log-error "don't know how to create a GL palette, yet"))
		(let ([hglrc (if wglCreateContextAttribsARB
                                 (wglCreateContextAttribsARB hdc context-handle (vector 0))
                                 (wglCreateContext hdc))])
		  (and hglrc
		       (new gl-context% [hglrc hglrc] [hdc hdc]))))))))

(define (with-dummy-context config thunk)
  ;; To create a gl context, we need a separate window 
  ;; (because you can't change a window's pixel format
  ;; after it is set).
  ;; So, create a dummy window to make a context to
  ;; try to do whatever needs doing.
  (let ([hwnd (CreateWindowExW 0
			       "PLTFrame"
			       ""
			       WS_POPUP
			       0 0 1 1
			       #f
			       #f
			       hInstance
			       #f)])
    (when hwnd
      (let ([hdc (GetDC hwnd)])
	(let ([c (create-gl-context hdc config #f #:try-ms? #f)])
	  (when c
	    (call-with-context
	     (get-field hdc c)
	     (get-field hglrc c)
             thunk)))
        (ReleaseDC hwnd hdc)))))

(define (init-createcontextattribs! config)
  ;; look for wglCreateContextAttribsARB which is a beefed
  ;; up version of wglCreateContext
  (set! looked-for-createcontextattribs? #t)
  (with-dummy-context config
    (lambda ()
      (set! wglCreateContextAttribsARB
            (let ([f (wglGetProcAddress "wglCreateContextAttribsARB")])
              (and f
                   ((allocator wglDeleteContext)
                    (function-ptr f (_wfun _HDC
                                           _HGLRC
                                           (_vector i _int)
                                           -> _HGLRC)))))))))

(define (init-multisample! config)
  ;; To create a multisampled context, we need
  ;; wglChoosePixelFormatARB().
  ;; To look up wglChoosePixelFormatARB(), we need
  ;; an existing gl context.
  (with-dummy-context config
    (lambda ()
      (set! wglChoosePixelFormatARB
            (let ([f (wglGetProcAddress "wglChoosePixelFormatARB")])
              (and f
                   (function-ptr f (_wfun _HDC 
                                          (_vector i _int)
                                          (_vector i _float)
                                          (_UINT  = 1)
                                          (formats : (_ptr o _int))
                                          (num-formats : (_ptr o _UINT))
                                          -> (r : _BOOL)
                                          -> (and r formats))))))
      (set! tried-multisample? #t))))

(define GL_TRUE 1)
(define GL_FALSE 0)
(define WGL_DRAW_TO_WINDOW_ARB    #x2001)
(define WGL_DRAW_TO_BITMAP_ARB    #x2002)
(define WGL_SUPPORT_OPENGL_ARB    #x2010)
(define WGL_ACCELERATION_ARB      #x2003)
(define WGL_FULL_ACCELERATION_ARB #x2027)
(define WGL_COLOR_BITS_ARB        #x2014)
(define WGL_ALPHA_BITS_ARB        #x201B)
(define WGL_DEPTH_BITS_ARB        #x2022)
(define WGL_STENCIL_BITS_ARB      #x2023)
(define WGL_DOUBLE_BUFFER_ARB     #x2011)
(define WGL_SAMPLE_BUFFERS_ARB    #x2041)
(define WGL_SAMPLES_ARB           #x2042)

;; The multisampling substitute for ChoosePixelFormat:
(define (choose-multisample hdc config offscreen? ms)
  (and 
   wglChoosePixelFormatARB 
   (wglChoosePixelFormatARB 
    hdc
    (vector (if offscreen?
		WGL_DRAW_TO_BITMAP_ARB
		WGL_DRAW_TO_WINDOW_ARB)
	    GL_TRUE
	    WGL_SUPPORT_OPENGL_ARB
	    GL_TRUE
	    WGL_ACCELERATION_ARB
	    WGL_FULL_ACCELERATION_ARB
	    WGL_COLOR_BITS_ARB
	    24
	    WGL_ALPHA_BITS_ARB
	    8
	    WGL_DEPTH_BITS_ARB
	    (send config get-depth-size) ; 32 for offscreen?
	    WGL_STENCIL_BITS_ARB
	    (send config get-stencil-size)
	    WGL_DOUBLE_BUFFER_ARB
	    (if (and (not offscreen?)
		     (send config get-double-buffered))
		GL_TRUE
		GL_FALSE)
	    WGL_SAMPLE_BUFFERS_ARB
	    GL_TRUE
	    WGL_SAMPLES_ARB
	    ms
	    0
	    0)
    (vector 0.0 0.0))))
