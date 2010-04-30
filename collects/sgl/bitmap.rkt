
(module bitmap mzscheme
  (require mred
           mzlib/class
           sgl/gl-vectors
           sgl
           sgl/gl
           mzlib/kw)
  
  (provide bitmap->gl-list)
  
  (define (argb->rgba argb)
    (let* ((length (bytes-length argb))
           (rgba (make-gl-ubyte-vector length)))
      (let loop ((i 0))
        (when (< i length)
          (gl-vector-set! rgba (+ i 0) (bytes-ref argb (+ i 1)))
          (gl-vector-set! rgba (+ i 1) (bytes-ref argb (+ i 2)))
          (gl-vector-set! rgba (+ i 2) (bytes-ref argb (+ i 3)))
          (gl-vector-set! rgba (+ i 3) (bytes-ref argb (+ i 0)))
          (loop (+ i 4))))
      rgba))
  
  (define (bitmap->argb bmp bmp-mask)
    (let* ((width (send bmp get-width))
           (height (send bmp get-height))
           (argb (make-bytes (* 4 width height) 255)))
      (send bmp get-argb-pixels 0 0 width height argb #f)
      (when bmp-mask
        (send bmp-mask get-argb-pixels 0 0 width height argb #t))
      argb))
  
  (define/kw (bitmap->gl-list bm 
                              #:key 
                              [with-gl (lambda (f) (f))]
                              [mask (send bm get-loaded-mask)])
    (let ([w (send bm get-width)]
          [h (send bm get-height)]
          [rgba (argb->rgba (bitmap->argb bm mask))])
    (with-gl
     (lambda ()
       (let ((tex (gl-vector-ref (glGenTextures 1) 0))
             (list-id (gl-gen-lists 1)))
         (glBindTexture GL_TEXTURE_2D tex)
         (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MIN_FILTER GL_LINEAR)
         (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_MAG_FILTER GL_LINEAR)
         (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_S GL_CLAMP)
         (glTexParameteri GL_TEXTURE_2D GL_TEXTURE_WRAP_T GL_CLAMP)
         (glTexImage2D GL_TEXTURE_2D 0 GL_RGBA w h 0
                       GL_RGBA GL_UNSIGNED_BYTE rgba)
         
         (gl-new-list list-id 'compile)
         (gl-enable 'texture-2d)
         (glBindTexture GL_TEXTURE_2D tex)
         (gl-material-v 'front 'ambient-and-diffuse
                        (gl-float-vector 1 1 1 1))
         (gl-begin 'polygon)
         (gl-tex-coord 0.0 0.0)
         (gl-vertex 0.0 0.0 0.0)
         (gl-tex-coord 1.0 0.0)
         (gl-vertex 1.0 0.0 0.0)
         (gl-tex-coord 1.0 1.0)
         (gl-vertex 1.0 1.0 0.0)
         (gl-tex-coord 0.0 1.0)
         (gl-vertex 0.0 1.0 0.0)
         (gl-end)
         (gl-disable 'texture-2d)
         (gl-end-list)
         
         list-id))))))
