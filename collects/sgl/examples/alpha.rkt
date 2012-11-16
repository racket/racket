;; By Brendan Burns
(module alpha mzscheme
  ;; Standard requires
  (require 
   mred
   sgl/gl
   sgl/gl-vectors
   "gl-frame.rkt")
  
  ;; texture
  ;; This needs to be here and not in gl-init because of some weirdness in the
  ;; threading on windows causes the window to attempt to be drawn prior to the
  ; texture being fully loaded.
  (define *texture* (image->gl-vector (get-file)))
  
  ;; Init function
  (define (my-gl-init)
    (let ((res *texture*))
      ;; Same texture, three smoothing styles...
      (init-textures 3)
      (unless (gl-load-texture (list-ref res 2) (list-ref res 0) (list-ref res 1)
                               GL_NEAREST GL_NEAREST 0)
        (error "Couldn't load texture"))
      (unless (gl-load-texture (list-ref res 2) (list-ref res 0) (list-ref res 1)
                               GL_LINEAR GL_LINEAR 1)
        (error "Couldn't load texture"))
      (unless (gl-load-texture (list-ref res 2) (list-ref res 0) (list-ref res 1)
                               GL_LINEAR GL_LINEAR_MIPMAP_NEAREST 2)
        (error "Couldn't load texture"))
      
      ;; Set-up alpha blending 50% transparency
      (glColor4d 1 1 1 0.5)
      (glBlendFunc GL_SRC_ALPHA GL_ONE)
      (glEnable GL_BLEND)
      
      ;; Standard Init
      (glEnable GL_TEXTURE_2D)
      (glShadeModel GL_SMOOTH)
      (glClearColor 0.0 0.0 0.0 0.5)
      (glClearDepth 1)
      (glEnable GL_DEPTH_TEST)
      (glDepthFunc GL_LEQUAL)
      (glHint GL_PERSPECTIVE_CORRECTION_HINT GL_NICEST)
      
      ;; default light
      (glEnable GL_LIGHT0)
      (glEnable GL_LIGHTING)))
  
  ;; Our user interaction variables
  (define *xrot* 0)
  (define *yrot* 0)
  (define *zrot* 0)
  (define *z* -5)
  (define *blend* #f)
  (define *tex* 0)
  
  ;; Our main function that does the drawing
  (define (my-gl-draw)
    ;; erase the background
    (glClear (+ GL_COLOR_BUFFER_BIT GL_DEPTH_BUFFER_BIT))
    ;; turn blending on/off
    (if *blend* (glEnable GL_BLEND) (glDisable GL_BLEND))
    
    ;; draw cube.
    (glLoadIdentity)
    (glTranslated 0 0 *z*)
    (glRotated *xrot* 1 0 0)
    (glRotated *yrot* 0 1 0)
    (glRotated *zrot* 0 0 1)
    (glBindTexture GL_TEXTURE_2D (get-texture *tex*))
    (glBegin GL_QUADS)
    ; front
    (glNormal3d 0 0 1)
    (glTexCoord2i 0 0)
    (glVertex3i -1 -1 1)
    (glTexCoord2i 1 0)
    (glVertex3i 1 -1 1)
    (glTexCoord2i 1 1)
    (glVertex3i 1 1 1)
    (glTexCoord2i 0 1)
    (glVertex3i -1 1 1)
    ; back
    (glNormal3d 0 0 -1)
    (glTexCoord2i 1 0)
    (glVertex3i -1 -1 -1)
    (glTexCoord2i 1 1)
    (glVertex3i 1 -1 -1)
    (glTexCoord2i 0 1)
    (glVertex3i 1 1 -1)
    (glTexCoord2i 0 0)
    (glVertex3i -1 1 -1)
    ; top
    (glNormal3d 0 1 0)
    (glTexCoord2i 0 1)
    (glVertex3i -1 1 -1)
    (glTexCoord2i 0 0)
    (glVertex3i 1 1 -1)
    (glTexCoord2i 1 0)
    (glVertex3i 1 1 1)
    (glTexCoord2i 1 1)
    (glVertex3i -1 1 1)
    ; bottom
    (glNormal3d 0 -1 0)
    (glTexCoord2i 1 1)  
    (glVertex3i -1 -1 -1)
    (glTexCoord2i 0 1)
    (glVertex3i -1 -1 1)
    (glTexCoord2i 0 0)
    (glVertex3i 1 -1 1)
    (glTexCoord2i 1 0)
    (glVertex3i 1 -1 -1)
    ; right
    (glNormal3d 1 0 0)
    (glTexCoord2i 1 0)
    (glVertex3i 1 -1 -1)
    (glTexCoord2i 1 1)
    (glVertex3i 1 -1 1)
    (glTexCoord2i 0 1)
    (glVertex3i 1 1 1)
    (glTexCoord2i 0 0)
    (glVertex3i 1 1 -1)
    ;left
    (glNormal3d -1 0 0)
    (glTexCoord2i 0 0)
    (glVertex3i -1 -1 -1)
    (glTexCoord2i 1 0)
    (glVertex3i -1 1 -1)
    (glTexCoord2i 1 1)
    (glVertex3i -1 1 1)
    (glTexCoord2i 0 1)
    (glVertex3i -1 -1 1)
    
    (glEnd)
    (set! *xrot* (+ *xrot* 0.3))
    (set! *yrot* (+ *yrot* 0.2))
    (set! *zrot* (+ *zrot* 0.4))
    
    (glFlush))
  
  ;; Move forward
  (add-key-mapping #\i (lambda () (set! *z* (+ *z* 0.3))))
  ;; Move backward
  (add-key-mapping #\k (lambda () (set! *z* (- *z* 0.3))))
  ;; Turn blending on/off
  (add-key-mapping #\b (lambda () (set! *blend* (not *blend*))))
  ;; Cycle textures
  (add-key-mapping #\t (lambda () (set! *tex* (modulo (+ *tex* 1) 3))))
  
  ;; Set the init function
  (set-gl-init-fn my-gl-init)
  ;; Set the draw function
  (set-gl-draw-fn my-gl-draw)
  (gl-run))
