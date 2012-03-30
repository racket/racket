;; By Brendan Burns, with modifications by Scott Owens
(module gl-frame racket/gui
  (require sgl/gl
           sgl/gl-vectors)
  (provide set-gl-draw-fn
           set-gl-init-fn
           init-textures
           image->gl-vector
           bitmap->gl-vector
           gl-load-texture
           get-texture
           add-key-mapping
           clear-key-mappings
           gl-run)
  
  (define gl-draw void)
  (define gl-init 
    (lambda ()
      (glShadeModel GL_SMOOTH)
      (glClearColor 0.0 0.0 0.0 0.5)
      (glClearDepth 1)
      (glEnable GL_DEPTH_TEST)
      (glDepthFunc GL_LEQUAL)
      (glHint GL_PERSPECTIVE_CORRECTION_HINT GL_NICEST)))
  
  (define (set-gl-draw-fn fn)
    (set! gl-draw fn))
  
  (define (set-gl-init-fn fn)
    (set! gl-init fn))
    
  ;; A function that recorrects for a new aspect ratio when the window is resized
  (define (gl-resize width height)
    (glViewport 0 0 width height)
    
    (glMatrixMode GL_PROJECTION)
    (glLoadIdentity)
    (gluPerspective 45 (/ width height) 0.1 100)
    
    (glMatrixMode GL_MODELVIEW)
    (glLoadIdentity))
  
  (define (recursive-handle-key list code)
    (cond
      ((empty? list) void)
      ((equal? (caar list) code) ((car (cdr (car list)))))
      (else (recursive-handle-key (rest list) code))))
  
  (define *key-mappings* '())
  
  (define (add-key-mapping key fn)
    (set! *key-mappings* (cons (list key fn) *key-mappings*)))
  
  (define (clear-key-mappings)
    (set! *key-mappings* '()))
  
  (define (gl-handlekey key)
    (recursive-handle-key *key-mappings* (send key get-key-code)))
  
  (define glcanvas%
    (class canvas%
      (inherit refresh with-gl-context swap-gl-buffers)

      (define init? #f)
      (define/override (on-paint)
        (with-gl-context 
         (lambda ()
           (unless init?
             (gl-init)
             (set! init? #t))
           (gl-draw)
           (swap-gl-buffers)))
        (queue-callback (lambda () (refresh)) #f))
      
      (define/override (on-size w h)
        (with-gl-context 
         (lambda ()
           (gl-resize w h)))
        (refresh))
      
      (define/override (on-char key)
        (gl-handlekey key)
        (refresh))
      (super-new (style '(gl no-autoclear)))))
  
  (define (gl-run)
    (let* ((frame (new frame% (label "OpenGL Window") 
                              (width 640) 
                              (height 480)))
           (glcanvas (new glcanvas% (parent frame))))
      (unless (send (send (send glcanvas get-dc) get-gl-context) ok?)
        (display "Error: OpenGL context failed to initialize")
        (newline)
        (exit))
      (send frame show #t)))
  
  (define *textures* '())
  
  (define init-textures
    (lambda (count)
      (set! *textures* (glGenTextures count))))
  
  (define (bitmap->gl-vector bmp)
    (let* (
           (dc (instantiate bitmap-dc% (bmp)))
           (pixels (* (send bmp get-width) (send bmp get-height)))
           (vec (make-gl-ubyte-vector (* pixels 3)))
           (data (make-bytes (* pixels 4)))
           (i 0)
           )
      (send dc get-argb-pixels 0 0 (send bmp get-width) (send bmp get-height) data)
      (letrec
          ([loop
            (lambda ()
              (when (< i pixels)
                  (begin
                    (gl-vector-set! vec (* i  3) 
                                    (bytes-ref data (+ (* i 4) 1)))
                    (gl-vector-set! vec (+ (* i 3) 1) 
                                    (bytes-ref data (+ (* i 4) 2)))
                    (gl-vector-set! vec (+ (* i 3) 2) 
                                    (bytes-ref data (+ (* i 4) 3)))
                    (set! i (+ i 1))
                    (loop))))])
        (loop))
      (send dc set-bitmap #f)
      (list (send bmp get-width) (send bmp get-height) vec)))
  
  (define (image->gl-vector file) (bitmap->gl-vector (make-object bitmap% file 'unknown #f)))
  
  (define gl-load-texture
    (lambda (image-vector width height min-filter mag-filter ix)
      (glBindTexture GL_TEXTURE_2D (gl-vector-ref *textures* ix))
      (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER min-filter)
      (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER mag-filter)
      (let* ((new-width 128)
             (new-height 128)
             (new-img-vec (make-gl-ubyte-vector (* new-width new-height 3))))
        (gluScaleImage GL_RGB
                       width height GL_UNSIGNED_BYTE image-vector
                       new-width new-height GL_UNSIGNED_BYTE new-img-vec)
        (if (or (= min-filter GL_LINEAR_MIPMAP_NEAREST)
                (= mag-filter GL_LINEAR_MIPMAP_NEAREST))
            (gluBuild2DMipmaps GL_TEXTURE_2D 3 new-width new-height GL_RGB GL_UNSIGNED_BYTE new-img-vec)
            (glTexImage2D GL_TEXTURE_2D 0 3 new-width new-height 0 GL_RGB GL_UNSIGNED_BYTE new-img-vec))))
    )
  
  (define get-texture
    (lambda (ix)
      (gl-vector-ref *textures* ix)))
  )
