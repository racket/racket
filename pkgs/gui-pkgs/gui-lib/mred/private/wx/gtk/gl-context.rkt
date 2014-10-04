#lang racket/base
(require racket/class
         racket/promise
         racket/string
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
(define _XErrorEvent (_cpointer 'XErrorEvent))
(define _XID _ulong)
(define True 1)
(define False 0)
(define None 0)
(define Success 0)

;; GLX #defines/typedefs/enums
(define _GLXFBConfig (_cpointer 'GLXFBConfig))
(define _GLXContext (_cpointer/null 'GLXContext))
(define _XVisualInfo (_cpointer 'XVisualInfo))
;; Attribute tokens for glXGetConfig variants (all GLX versions):
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
(define GLX_SAMPLE_BUFFERS   #x186a0)
;; Attribute tokens for glXCreateContextAttribsARB (also GLX 1.4 and later):
(define GLX_CONTEXT_MAJOR_VERSION_ARB #x2091)
(define GLX_CONTEXT_MINOR_VERSION_ARB #x2092)
(define GLX_CONTEXT_FLAGS_ARB         #x2094)
(define GLX_CONTEXT_PROFILE_MASK_ARB  #x9126)
;; GLX_CONTEXT_FLAGS_ARB bits
(define GLX_CONTEXT_DEBUG_BIT_ARB              #x1)
(define GLX_CONTEXT_FORWARD_COMPATIBLE_BIT_ARB #x2)
;; GLX_CONTEXT_PROFILE_MASK_ARB bits
(define GLX_CONTEXT_CORE_PROFILE_BIT_ARB          #x1)
(define GLX_CONTEXT_COMPATIBILITY_PROFILE_BIT_ARB #x2)

(define-x XFree (_fun _pointer -> _int)
  #:wrap (deallocator))

(define-x XSetErrorHandler
  (_fun (_fun _Display _XErrorEvent -> _int)
        -> (_fun _Display _XErrorEvent -> _int)))

(define-x XSync
  (_fun _Display _int -> _void))

(define-glx glXQueryVersion
  (_fun _Display (major : (_ptr o _int)) (minor : (_ptr o _int))
        -> (ret : _bool)
        -> (values ret major minor)))

(define-glx glXQueryExtensionsString
  (_fun _Display _int -> _string/utf-8))

(define-glx glXChooseFBConfig
  (_fun _Display _int (_list i _int) (len : (_ptr o _int))
        -> (_cvector o _GLXFBConfig len))
  #:wrap (allocator (λ (v) (XFree (cvector-ptr v)))))

(define-glx glXGetFBConfigAttrib
  (_fun _Display _GLXFBConfig _int (out : (_ptr o _int))
        -> (ret : _int)
        -> (values ret out)))

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

(define-glx glXGetProcAddressARB
  (_fun _string -> _pointer))

(define lazy-glXCreateContextAttribsARB
  (delay
    (function-ptr (glXGetProcAddressARB "glXCreateContextAttribsARB")
                  (_fun _Display _GLXFBConfig _GLXContext _bool (_list i _int)
                        -> _GLXContext))))

(define (glXCreateContextAttribsARB . args)
  (apply (force lazy-glXCreateContextAttribsARB) args))

(define-gtk gtk_widget_get_display (_fun _GtkWidget -> _GdkDisplay))
(define-gtk gtk_widget_get_screen (_fun _GtkWidget -> _GdkScreen))
(define-gtk gtk_widget_get_window (_fun _GtkWidget -> _GtkWindow))

;; ===================================================================================================
;; GLX versions and extensions queries

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

(define lazy-glx-extensions
  (delay
    (define str
      (glXQueryExtensionsString (gdk_x11_display_get_xdisplay (gdk_display_get_default))
                                (gdk_x11_screen_get_screen_number (gdk_screen_get_default))))
    (string-split str)))

(define lazy-GLX_ARB_create_context?
  (delay (member "GLX_ARB_create_context"
                 (force lazy-glx-extensions))))

(define lazy-GLX_ARB_create_context_profile?
  (delay (member "GLX_ARB_create_context_profile"
                 (force lazy-glx-extensions))))

;; ===================================================================================================
;; Wrapper for the _GLXContext (if we can get one from GLX)

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
;; Getting OpenGL contexts

;; STUPIDITY ALERT

;; Apparently, the designers of glXCreateNewContext and glXCreateContextAttribsARB didn't trust us to
;; check return values or output arguments, so when these functions fail, they raise an X error and
;; send an error code to the X error handler. X errors, by default, *terminate the program* and print
;; an annoyingly vague, barely helpful error message.

;; This is especially bad with glXCreateContextAttribsARB, which always fails (i.e. crashes the
;; program) if we ask for an unsupported OpenGL version. Worse, this is the only way to find out
;; which OpenGL versions are available!

;; So we override the X error handler to silently fail, and sync right after the calls to make sure
;; the errors are processed immediately. With glXCreateContextAttribsARB, we then try the next lowest
;; OpenGL version. If all attempts to get a context fail, we return #f.

(define (null-x-error-handler xdisplay xerrorevent)
  ;; Do nothing
  0)

;; _Display _GLXFBConfig _GLXContext -> _GLXContext
(define (glx-create-new-context xdisplay cfg share-gl)
  ;; Sync right now, or the sync further on could crash Racket with an [xcb] error about events
  ;; happening out of sequence
  (XSync xdisplay False)
  (define old-handler #f)
  (dynamic-wind
   (λ ()
     (set! old-handler (XSetErrorHandler null-x-error-handler)))
   (λ ()
     (define gl (glXCreateNewContext xdisplay cfg GLX_RGBA_TYPE share-gl #t))
     (unless gl (log-warning "gl-config: unable to get OpenGL context"))
     gl)
   (λ ()
     ;; Sync to ensure errors are processed while we're throwing them away
     (XSync xdisplay False)
     (XSetErrorHandler old-handler))))

;; OpenGL core versions we'll try to get, in order
(define core-gl-versions '((4 5) (4 4) (4 3) (4 2) (4 1) (4 0) (3 3) (3 2) (3 1) (3 0)))

;; _Display _GLXFBConfig _GLXContext -> _GLXContext
(define (glx-create-core-context xdisplay cfg share-gl)
  ;; Sync right now, or the sync further on could crash Racket with an [xcb] error about events
  ;; happening out of sequence
  (XSync xdisplay False)
  (let/ec return
    (define old-handler #f)
    (dynamic-wind
     (λ ()
       (set! old-handler (XSetErrorHandler null-x-error-handler)))
     (λ ()
       (for ([gl-version  (in-list core-gl-versions)])
         (define gl-major (car gl-version))
         (define gl-minor (cadr gl-version))
         (define context-attribs
           (list GLX_CONTEXT_MAJOR_VERSION_ARB gl-major
                 GLX_CONTEXT_MINOR_VERSION_ARB gl-minor
                 GLX_CONTEXT_PROFILE_MASK_ARB GLX_CONTEXT_CORE_PROFILE_BIT_ARB
                 None))
         (define gl
           (glXCreateContextAttribsARB xdisplay cfg share-gl #t context-attribs))
         (when gl (return gl))))
     (λ ()
       ;; Sync to ensure errors are processed while we're throwing them away
       (XSync xdisplay False)
       (XSetErrorHandler old-handler)))
    (log-warning "gl-config: unable to get core context; falling back")
    (glx-create-new-context xdisplay cfg share-gl)))

;; ===================================================================================================

;; (or/c #f _GtkWidget) -> _GdkDisplay
(define (gtk-maybe-widget-get-display widget)
  (cond [widget  (gtk_widget_get_display widget)]
        [else    (gdk_display_get_default)]))

;; (or/c #f _GtkWidget) -> _GdkScreen
(define (gtk-maybe-widget-get-screen widget)
  (cond [widget  (gtk_widget_get_screen widget)]
        [else    (gdk_screen_get_default)]))

;; _Display _GLXFBConfig int int -> int
(define (glx-get-fbconfig-attrib xdisplay cfg attrib bad-value)
  (define-values (err value) (glXGetFBConfigAttrib xdisplay cfg attrib))
  (if (= err Success) value bad-value))

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
     ;; Finish out with standard GLX 1.3 attributes
     (list
      GLX_X_RENDERABLE True  ; yes, we want to use OpenGL to render today
      GLX_DEPTH_SIZE (send conf get-depth-size)
      GLX_STENCIL_SIZE (send conf get-stencil-size)
      GLX_ACCUM_RED_SIZE (send conf get-accum-size)
      GLX_ACCUM_GREEN_SIZE (send conf get-accum-size)
      GLX_ACCUM_BLUE_SIZE (send conf get-accum-size)
      GLX_ACCUM_ALPHA_SIZE (send conf get-accum-size)
      ;; GLX_SAMPLES is handled below - GLX regards it as an absolute lower bound, which makes it
      ;; too easy for user programs to fail to get a context
      None)))
  
  (define multisample-size (send conf get-multisample-size))
  
  ;; Get all framebuffer configs for this display and screen that match the requested attributes,
  ;; then sort them to put the best in front
  ;; GLX already sorts them pretty well, so we just need a stable sort on multisamples at the moment
  (define cfgs
    (let* ([cfgs  (cvector->list (glXChooseFBConfig xdisplay xscreen xattribs))]
           ;; Keep all configs with multisample size <= requested (i.e. make multisample-size an
           ;; abolute upper bound)
           [cfgs  (if (< glx-version #e1.4)
                      cfgs
                      (filter (λ (cfg)
                                (define m (glx-get-fbconfig-attrib xdisplay cfg GLX_SAMPLES 0))
                                (<= m multisample-size))
                              cfgs))]
           ;; Sort all configs by multisample size, decreasing
           [cfgs  (if (< glx-version #e1.4)
                      cfgs
                      (sort cfgs >
                            #:key (λ (cfg) (glx-get-fbconfig-attrib xdisplay cfg GLX_SAMPLES 0))
                            #:cache-keys? #t))])
      cfgs))
  
  (cond
    [(null? cfgs)  #f]
    [else
     ;; The framebuffer configs are sorted best-first, so choose the first
     (define cfg (car cfgs))
     (define share-gl
       (let ([share-ctxt  (send conf get-share-context)])
         (and share-ctxt (send share-ctxt get-handle))))
     
     ;; Get a GL context
     (define gl
       (if (and (>= glx-version #e1.4)
                (not (send conf get-legacy?))
                (force lazy-GLX_ARB_create_context?)
                (force lazy-GLX_ARB_create_context_profile?))
           ;; If the GLX version is high enough, legacy? is #f, and GLX has the right extensions,
           ;; try to get a core-profile context
           (glx-create-core-context xdisplay cfg share-gl)
           ;; Otherwise use the old method
           (glx-create-new-context xdisplay cfg share-gl)))
     ;; The above will return a direct rendering context when it can
     ;; If it doesn't, the context will be version 1.4 or lower, unless GLX is implemented with
     ;; proprietary extensions (NVIDIA's drivers sometimes do this)
     
     ;; Now wrap the GLX context in a gl-context%
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
