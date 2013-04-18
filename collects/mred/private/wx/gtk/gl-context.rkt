#lang racket/base
(require racket/class
         ffi/unsafe
         ffi/unsafe/define
         ffi/unsafe/alloc
         (prefix-in draw: racket/draw/private/gl-context)
         racket/draw/private/gl-config
         "types.rkt"
         "utils.rkt")

(provide
 (protect-out prepare-widget-gl-context
              create-widget-gl-context

              create-and-install-gl-context
              get-gdk-pixmap
              install-gl-context))

(define (ffi-lib/complaint-on-failure name vers)
  (ffi-lib name vers
           #:fail (lambda ()
                    (log-warning "could not load GL library ~a ~a"
                                 name vers)
                    #f)))

(define gdkglext-lib (ffi-lib/complaint-on-failure "libgdkglext-x11-1.0" '("0")))
(define gtkglext-lib (ffi-lib/complaint-on-failure "libgtkglext-x11-1.0" '("0")))

(define-ffi-definer define-gdkglext gdkglext-lib
  #:default-make-fail make-not-available)
(define-ffi-definer define-gtkglext gtkglext-lib
  #:default-make-fail make-not-available)

(define _GdkGLContext (_cpointer/null 'GdkGLContext))
(define _GdkGLDrawable (_cpointer 'GdkGLDrawable))
(define _GdkGLConfig (_cpointer 'GdkGLConfig))
(define _GdkGLPixmap _GdkGLDrawable)
(define _GdkPixmap _pointer)

(define-gdkglext gdk_gl_init (_fun (_ptr i _int)
                                   (_ptr i _pointer)
                                   -> _void)
  #:fail (lambda () void))

(define-gtkglext gdk_gl_config_new (_fun (_list i _int) -> (_or-null _GdkGLConfig))
  #:fail (lambda () (lambda args #f)))
(define-gtkglext gdk_gl_config_new_for_screen (_fun _GdkScreen (_list i _int) -> (_or-null _GdkGLConfig)))

(define-gtk gtk_widget_get_screen (_fun _GtkWidget -> _GdkScreen))

(define-gtkglext gtk_widget_set_gl_capability (_fun _GtkWidget
                                                    _GdkGLConfig
                                                    _GdkGLContext
                                                    _gboolean
                                                    _int
                                                    -> _gboolean)
  #:fail (lambda () (lambda args #f)))

(define-gtkglext gtk_widget_get_gl_context (_fun _GtkWidget -> _GdkGLContext)
  #:fail (lambda () (lambda args #f)))
(define-gtkglext gtk_widget_get_gl_window (_fun _GtkWidget -> _GdkGLDrawable))

(define-gdkglext gdk_gl_context_destroy (_fun _GdkGLContext -> _void)
  #:wrap (deallocator))

(define-gdkglext gdk_gl_context_new (_fun _GdkGLDrawable _GdkGLContext _gboolean _int
                                          -> _GdkGLContext)
  #:wrap (allocator gdk_gl_context_destroy))

(define-gdkglext gdk_gl_drawable_gl_begin (_fun _GdkGLDrawable
                                                _GdkGLContext
                                                -> _gboolean))
(define-gdkglext gdk_gl_drawable_gl_end (_fun _GdkGLDrawable -> _void))
(define-gdkglext gdk_gl_drawable_swap_buffers (_fun _GdkGLDrawable -> _void))

(define-gdkglext gdk_pixmap_set_gl_capability (_fun _GdkPixmap _GdkGLConfig _pointer
						    -> _GdkGLPixmap))

(define GDK_GL_RGBA_TYPE 0)

(define GDK_GL_USE_GL                     1)
(define GDK_GL_BUFFER_SIZE                2)
(define GDK_GL_LEVEL                      3)
(define GDK_GL_RGBA                       4)
(define GDK_GL_DOUBLEBUFFER               5)
(define GDK_GL_STEREO                     6)
(define GDK_GL_AUX_BUFFERS                7)
(define GDK_GL_RED_SIZE                   8)
(define GDK_GL_GREEN_SIZE                 9)
(define GDK_GL_BLUE_SIZE                  10)
(define GDK_GL_ALPHA_SIZE                 11)
(define GDK_GL_DEPTH_SIZE                 12)
(define GDK_GL_STENCIL_SIZE               13)
(define GDK_GL_ACCUM_RED_SIZE             14)
(define GDK_GL_ACCUM_GREEN_SIZE           15)
(define GDK_GL_ACCUM_BLUE_SIZE            16)
(define GDK_GL_ACCUM_ALPHA_SIZE           17)
(define GDK_GL_SAMPLE_BUFFERS             100000)
(define GDK_GL_SAMPLES                    100001)
(define GDK_GL_ATTRIB_LIST_NONE 0)

;; ----------------------------------------

(define (config->GdkGLConfig d conf can-double?)
  (gdk_gl_config_new (append
		      (list GDK_GL_RGBA)
                      (if can-double?
                          (if (send conf get-double-buffered) (list GDK_GL_DOUBLEBUFFER) null)
                          null)
		      (if (send conf get-stereo) (list GDK_GL_STEREO) null)
		      (list
		       GDK_GL_DEPTH_SIZE (send conf get-depth-size)
		       GDK_GL_STENCIL_SIZE (send conf get-stencil-size)
		       GDK_GL_ACCUM_RED_SIZE (send conf get-accum-size)
		       GDK_GL_ACCUM_GREEN_SIZE (send conf get-accum-size)
		       GDK_GL_ACCUM_BLUE_SIZE (send conf get-accum-size)
		       GDK_GL_ACCUM_ALPHA_SIZE (send conf get-accum-size))
		      #;
		      (list GDK_GL_SAMPLES (send conf get-multisample-size))
		      (list GDK_GL_ATTRIB_LIST_NONE))))

;; ----------------------------------------

(define gl-context% 
  (class draw:gl-context%
    (init-field [gl gl]
                [drawable drawable])

    (define/override (get-handle) gl)

    (define/override (draw:do-call-as-current t)
      (dynamic-wind
          (lambda ()
            (gdk_gl_drawable_gl_begin drawable gl))
          t
          (lambda ()
            (gdk_gl_drawable_gl_end drawable))))
        
    (define/override (draw:do-swap-buffers)
      (gdk_gl_drawable_swap_buffers drawable))
    
    (super-new)))

;; ----------------------------------------

(define inited? #f)
(define (init!)
  (unless inited? 
    (set! inited? #t)
    (gdk_gl_init 0 #f)))

(define (prepare-widget-gl-context gtk config)
  (init!)
  (let ([share-context (and config (send config get-share-context))]
        [config (config->GdkGLConfig #f ; (gtk_widget_get_screen gtk)
                                     (or config
                                         (new gl-config%))
                                     #t)])
    (when config
	  (gtk_widget_set_gl_capability gtk
					config
					(if share-context
                                            (send share-context get-handle)
                                            #f)
					#t
					0))))

(define (create-widget-gl-context gtk)
  (init!)
  (let ([gl (gtk_widget_get_gl_context gtk)])
    (and gl
	 (new gl-context% 
	      [gl gl]
	      [drawable (gtk_widget_get_gl_window gtk)]))))


(define-local-member-name
  get-gdk-pixmap
  install-gl-context)

(define (create-and-install-gl-context bm config)
  (init!)
  (let* ([share-context (send config get-share-context)]
         [context-handle (if share-context (send share-context get-handle) #f)]
         [config (config->GdkGLConfig #f config #f)])
    (when config
      (let ([gdkpx (send bm get-gdk-pixmap)])
        (let ([glpx (gdk_pixmap_set_gl_capability gdkpx config #f)])
          (and glpx
               (let ([gl
		      ;; currently uses "indirect" mode --- can we
		      ;; reliably use direct in some environments?
		      (gdk_gl_context_new glpx context-handle #f GDK_GL_RGBA_TYPE)])
                 (and gl
                      (send bm install-gl-context
			    (new gl-context% 
				 [gl gl]
				 [drawable glpx]))))))))))
