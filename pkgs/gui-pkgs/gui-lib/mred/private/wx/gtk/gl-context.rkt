#lang racket/base
(require racket/class
         racket/promise
         ffi/unsafe
         ffi/unsafe/define
         ffi/unsafe/alloc
         ffi/cvector
         (prefix-in draw: racket/draw/private/gl-context)
         racket/draw/private/gl-config
         "../../lock.rkt"
         "types.rkt"
         "utils.rkt"
         "window.rkt"
         "x11.rkt")

(provide
 (protect-out prepare-widget-gl-context
              create-widget-gl-context

              create-and-install-gl-context
              get-gdk-pixmap
              install-gl-context))

(define (ffi-lib/complaint-on-failure name vers)
  (ffi-lib name vers
           #:fail (lambda ()
                    (log-warning "could not load library ~a ~a"
                                 name vers)
                    #f)))

;; ===================================================================================================
;; X11/GLX FFI

(define x-lib (ffi-lib/complaint-on-failure "libX11" '("")))
(define gl-lib (ffi-lib/complaint-on-failure "libGL" '("1" "")))

(define-ffi-definer define-x x-lib
  #:default-make-fail make-not-available)

(define-ffi-definer define-glx gl-lib
  #:default-make-fail make-not-available)

;; X #defines/typedefs/enums
(define _Display (_cpointer 'Display))
(define _XID _ulong)
(define True 1)
(define None 0)

;; GLX #defines/typedefs/enums
(define _GLXFBConfig (_cpointer 'GLXFBConfig))
(define _GLXContext (_cpointer/null 'GLXContext))
(define _XVisualInfo (_cpointer 'XVisualInfo))

(define GLX_DOUBLEBUFFER     5)
(define GLX_STEREO           6)
(define GLX_DEPTH_SIZE       12)
(define GLX_STENCIL_SIZE     13)
(define GLX_ACCUM_RED_SIZE   14)
(define GLX_ACCUM_GREEN_SIZE 15)
(define GLX_ACCUM_BLUE_SIZE  16)
(define GLX_ACCUM_ALPHA_SIZE 17)
;; GLX 1.3 and later:
(define GLX_X_RENDERABLE     #x8012)
(define GLX_RGBA_TYPE        #x8014)
;; GLX 1.4 and later:
(define GLX_SAMPLES          #x186a1)

(define-x XFree (_fun _pointer -> _int)
  #:wrap (deallocator))

(define-glx glXQueryVersion
  (_fun _Display (major : (_ptr o _int)) (minor : (_ptr o _int))
        -> (ret : _bool)
        -> (values ret major minor)))

(define-glx glXChooseFBConfig
  (_fun _Display _int (_list i _int) (len : (_ptr o _int))
        -> (_cvector o _GLXFBConfig len))
  #:wrap (allocator (λ (v) (XFree (cvector-ptr v)))))

(define-glx glXCreateNewContext
  (_fun _Display _GLXFBConfig _int _GLXContext _bool -> _GLXContext))

(define-glx glXDestroyContext
  (_fun _Display _GLXContext -> _void))

(define-glx glXMakeCurrent
  (_fun _Display _XID _GLXContext -> _bool))

(define-glx glXSwapBuffers
  (_fun _Display _XID -> _void))

(define-glx glXIsDirect
  (_fun _Display _GLXContext -> _bool))

(define-glx glXGetVisualFromFBConfig
  (_fun _Display _GLXFBConfig -> _XVisualInfo)
  #:wrap (allocator XFree))

(define-glx glXCreateGLXPixmap
  (_fun _Display _XVisualInfo _XID -> _XID))

(define-glx glXDestroyGLXPixmap
  (_fun _Display _XID -> _void))

(define-gtk gtk_widget_get_display (_fun _GtkWidget -> _GdkDisplay))
(define-gtk gtk_widget_get_screen (_fun _GtkWidget -> _GdkScreen))
(define-gtk gtk_widget_get_window (_fun _GtkWidget -> _GtkWindow))

;; ===================================================================================================

(define lazy-get-glx-version
  (delay
    (define-values (worked? glx-major glx-minor)
      (glXQueryVersion (gdk_x11_display_get_xdisplay (gdk_display_get_default))))
    
    (unless worked?
      (error 'get-glx-version "can't get GLX version using default display"))
    
    (define glx-version (+ glx-major (/ glx-minor 10)))
    
    (when (< glx-version #e1.3)
      (error 'get-glx-version "need GLX version 1.3 or greater; given version ~a.~a"
             glx-major glx-minor))
    
    glx-version))

;; -> positive-exact-rational
(define (get-glx-version)
  (force lazy-get-glx-version))

;; ===================================================================================================

(define gl-context% 
  (class draw:gl-context%
    (init-field gl display drawable pixmap)
    
    (define/override (get-handle) gl)
    
    (define/public (get-gtk-display) display)
    (define/public (get-gtk-drawable) drawable)
    (define/public (get-glx-pixmap) pixmap)
    
    (define (get-drawable-xid)
      (if pixmap pixmap (gdk_x11_drawable_get_xid drawable)))
    
    (define/override (draw:do-call-as-current t)
      (define xdisplay (gdk_x11_display_get_xdisplay display))
      (dynamic-wind
       (lambda ()
         (glXMakeCurrent xdisplay (get-drawable-xid) gl))
       t
       (lambda ()
         (glXMakeCurrent xdisplay 0 #f))))
    
    (define/override (draw:do-swap-buffers)
      (glXSwapBuffers (gdk_x11_display_get_xdisplay display)
                      (get-drawable-xid)))
    
    (super-new)))

;; ===================================================================================================

;; (or/c #f _GtkWidget) -> _GdkDisplay
(define (gtk-maybe-widget-get-display widget)
  (cond [widget  (gtk_widget_get_display widget)]
        [else    (gdk_display_get_default)]))

;; (or/c #f _GtkWidget) -> _GdkScreen
(define (gtk-maybe-widget-get-screen widget)
  (cond [widget  (gtk_widget_get_screen widget)]
        [else    (gdk_screen_get_default)]))

;; (or/c #f _GtkWidget) _GdkDrawable gl-config% boolean? -> gl-context%
;;   where _GdkDrawable = (or/c _GtkWindow _GdkPixmap)
(define (make-gtk-drawable-gl-context widget drawable conf wants-double?)
  (define glx-version (get-glx-version))
  
  ;; If widget isn't #f, use its display and screen
  (define display (gtk-maybe-widget-get-display widget))
  (define screen (gtk-maybe-widget-get-screen widget))
  
  ;; Get the X objects wrapped by the GDK objects
  (define xdisplay (gdk_x11_display_get_xdisplay display))
  (define xscreen (gdk_x11_screen_get_screen_number screen))
  
  ;; Create an attribute list using the GL config
  (define xattribs
    (append
     ;; Be aware: we may get double buffering even if we don't ask for it
     (if wants-double?
         (if (send conf get-double-buffered) (list GLX_DOUBLEBUFFER True) null)
         null)
     (if (send conf get-stereo) (list GLX_STEREO True) null)
     ;; Only ask for multisampling of GLX 1.4 or higher
     (if (>= glx-version #e1.4) (list GLX_SAMPLES (send conf get-multisample-size)) null)
     ;; Finish out with standard GLX 1.3 attributes
     (list
      GLX_X_RENDERABLE True  ; yes, we want to use OpenGL to render today
      GLX_DEPTH_SIZE (send conf get-depth-size)
      GLX_STENCIL_SIZE (send conf get-stencil-size)
      GLX_ACCUM_RED_SIZE (send conf get-accum-size)
      GLX_ACCUM_GREEN_SIZE (send conf get-accum-size)
      GLX_ACCUM_BLUE_SIZE (send conf get-accum-size)
      GLX_ACCUM_ALPHA_SIZE (send conf get-accum-size)
      None)))
  
  ;; Get all framebuffer configs for this display and screen that match the requested attributes
  (define cfgs (glXChooseFBConfig xdisplay xscreen xattribs))
  
  (cond
    [(zero? (cvector-length cfgs))  #f]
    [else
     ;; The framebuffer configs are sorted best-first, so choose the first
     (define cfg (cvector-ref cfgs 0))
     (define share-gl
       (let ([share-ctxt  (send conf get-share-context)])
         (and share-ctxt (send share-ctxt get-handle))))
     
     ;; Get a rendering context and wrap it
     (define gl (glXCreateNewContext xdisplay cfg GLX_RGBA_TYPE share-gl #t))
     ;; The above will return a direct rendering context when it can
     (cond
       [gl
        ;; If there's no widget, this is for a pixmap, so get the stupid GLX wrapper for it or
        ;; indirect rendering may crash on some systems (notably mine)
        (define pixmap
          (if widget #f (glXCreateGLXPixmap xdisplay
                                            (glXGetVisualFromFBConfig xdisplay cfg)
                                            (gdk_x11_drawable_get_xid drawable))))
        
        (define ctxt (new gl-context% [gl gl] [display display] [drawable drawable] [pixmap pixmap]))
        ;; Refcount these so they don't go away until the finalizer below destroys the GLXContext
        (g_object_ref display)
        (g_object_ref drawable)
        (register-finalizer
         ctxt
         (λ (ctxt)
           (define gl (send ctxt get-handle))
           (define display (send ctxt get-gtk-display))
           (define drawable (send ctxt get-gtk-drawable))
           (define pixmap (send ctxt get-glx-pixmap))
           (define xdisplay (gdk_x11_display_get_xdisplay display))
           (when pixmap (glXDestroyGLXPixmap xdisplay pixmap))
           (glXDestroyContext xdisplay gl)
           (g_object_unref drawable)
           (g_object_unref display)))
        ctxt]
       [else  #f])]))

(define (make-gtk-widget-gl-context widget conf)
  (atomically
   (make-gtk-drawable-gl-context widget (gtk_widget_get_window widget) conf #t)))

(define (make-gtk-pixmap-gl-context pixmap conf)
  (atomically
   (make-gtk-drawable-gl-context #f pixmap conf #f)))

;; ===================================================================================================

(define widget-config-hash (make-weak-hasheq))

(define (prepare-widget-gl-context widget conf)
  (hash-set! widget-config-hash widget (if conf conf (make-object gl-config%))))

(define (create-widget-gl-context widget)
  (define conf (hash-ref widget-config-hash widget #f))
  (and conf (make-gtk-widget-gl-context widget conf)))

(define-local-member-name
  get-gdk-pixmap
  install-gl-context)

(define (create-and-install-gl-context bm conf)
  (define ctxt (make-gtk-pixmap-gl-context (send bm get-gdk-pixmap) conf))
  (and ctxt (send bm install-gl-context ctxt)))
