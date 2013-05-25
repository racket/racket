;; sgl -- An OpenGL extension of Racket
;;
;; Copyright (C) 2007-2013 PLT Design Inc.
;; Copyright (C) 2003-2007 Scott Owens <sowens@cs.utah.edu>
;;
;; This  library is  free  software; you  can  redistribute it  and/or
;; modify it under the terms  of the GNU Lesser General Public License
;; as published by the Free Software Foundation; either version 2.1 of
;; the License, or (at your option) any later version.
;;
;; This library is distributed in the hope that it will be useful, but
;; WITHOUT  ANY  WARRANTY;  without   even  the  implied  warranty  of
;; MERCHANTABILITY or  FITNESS FOR A  PARTICULAR PURPOSE. See  the GNU
;; Lesser General Public License for more details.

#lang mzscheme

(require mzlib/etc
         "gl-vectors.rkt"
         "gl.rkt")

(define-syntax (_provide stx)
  (syntax-case stx ()
    [(_ x ...)
     (begin
       #;
       (for-each
        (lambda (x)
          (syntax-case x (rename)
            [(rename _ n)
             (display (syntax-object->datum #'n))]
            [_ (display (syntax-object->datum x))])
          (newline))
        (syntax->list #'(x ...)))
       #'(provide x ...))]))

(define (combine-syms strs)
    (string-append "(or/c" 
                   (apply
                    string-append
                    (map (lambda (s)
                           (format " '~s" s))
                         strs))
                   ")"))

(define-syntax-set (multi-arg multi-type-v)

  (define (iota n)
    (if (= 0 n) null (cons n (iota (sub1 n)))))

  (define (get-possible-types-v ts)
    (combine-str
     (map (lambda (t)
            (case t
              [(iv) "gl-int-vector?"]
              [(sv) "gl-short-vector?"]
              [(bv) "gl-byte-vector?"]
              [(uiv) "gl-uint-vector?"]
              [(usv) "gl-ushort-vector?"]
              [(ubv) "gl-ubyte-vector?"]
              [(dv) "gl-double-vector?"]
              [(fv) "gl-float-vector?"]
              [else (error (format "~a?" t))]))
          ts)))

  (define (combine-str strs)
    (string-append "(or/c" 
                   (apply
                    string-append
                    (map (lambda (s)
                           (string-append " " s))
                         strs))
                   ")"))

  (define (multi-arg/proc stx)
    (syntax-case stx ()
      [(_ name gl-name ((pre-arg-name pre-arg) ...) (num-arg ...))
       (let ([build-clause
              (lambda (num-arg)
                (with-syntax ([(arg ...)
                               (generate-temporaries (iota num-arg))]
                              [gl-name
                               (datum->syntax-object
                                #'gl-name
                                (string->symbol
                                 (format "~a~ad"
                                         (syntax-object->datum #'gl-name)
                                         num-arg))
                                #'gl-name
                                #'gl-name)])
                  #`((pre-arg-name ... arg ...)
                     (if (and (real? arg) ...)
                       (gl-name pre-arg ... arg ...)
                       (raise-argument-error
                        'name "(listof real?)" (list arg ...))))))])
         (with-syntax ([(clauses ...)
                        (map build-clause
                             (syntax-object->datum  #'(num-arg ...)))])
           #`(define name
               (case-lambda clauses ...))))]))

  (define (multi-type-v/proc stx)
    (syntax-case stx ()
      [(_ name gl-name ((pre-arg-name pre-arg ) ...)
          (length ...) (type ...) num? )
       (with-syntax ([arg (car (generate-temporaries (list #'name)))])
         (let* ([num? (syntax-object->datum #'num?)]
                [lengths (syntax-object->datum #'(length ...))]
                [build-clause
                 (lambda (type)
                   (with-syntax ([pred?
                                  (case type
                                    [(dv) #'gl-double-vector?]
                                    [(fv) #'gl-float-vector?]
                                    [(iv) #'gl-int-vector?]
                                    [(sv) #'gl-short-vector?]
                                    [(bv) #'gl-byte-vector?]
                                    [(uiv) #'gl-uint-vector?]
                                    [(usv) #'gl-ushort-vector?]
                                    [(ubv) #'gl-ubyte-vector?])]
                                 [(clause ...)
                                  (map
                                   (lambda (length)
                                     (with-syntax ([name
                                                    (datum->syntax-object
                                                     #'gl-name
                                                     (string->symbol
                                                      (format "~a~a~a"
                                                              (syntax-object->datum #'gl-name)
                                                              (if num? length "")
                                                              type))
                                                     #'gl-name
                                                     #'gl-name)])
                                       #`((#,length) (name pre-arg ... arg))))
                                   lengths)])
                     #`((pred? arg)
                        (case (gl-vector-length arg)
                          clause ...
                          [else (error
                                 'name
                                 "expects vector with length in ~a: given vector has length ~a"
                                 '(length ...)
                                 (gl-vector-length arg))]))))]
                [types (syntax-object->datum #'(type ...))])
           (with-syntax ([(clause ...) (map build-clause types)])
             #`(define (name pre-arg-name ... arg)
                 (cond
                   clause ...
                   [else
                    (raise-argument-error 'name
                                      #,(get-possible-types-v types)
                                      arg)])))))])))

(define-for-syntax (translate-cname name)
  (let* ([r (symbol->string name)]
         [r (regexp-replace* #rx"_" r "-")]
         [r (regexp-replace #rx"^GLU?-" r "")]
         [r (string-downcase r)])
    (string->symbol r)))

(define-syntax (make-enum-table stx)
  (syntax-case stx ()
    [(_ name const ...)
     (with-syntax ([(sym ...)
                    (map translate-cname
                         (syntax-object->datum #'(const ...)))])
       (if (< (length (syntax->list #'(const ...))) 8)
         (quasisyntax/loc stx
           (define name
             (let ([l `((sym . ,const) ...)])
               (lambda (enum-sym name)
                 (let ([v (assq enum-sym l)])
                   (unless v
                     (raise-argument-error name
                                           (combine-syms '(sym ...))
                                           enum-sym))
                   (cdr v))))))
         (quasisyntax/loc stx
           (define name
             (let ([ht (make-hash-table)])
               (for-each (lambda (key value)
                           (hash-table-put! ht key value))
                         '(sym ...) (list const ...))
               (lambda (enum-sym name)
                 (let ([v (hash-table-get ht enum-sym (lambda () #f))])
                   (unless v
                     (raise-argument-error name
                                           (combine-syms '(sym ...))
                                           enum-sym))
                   v)))))))]))

(define-syntax (make-inv-enum-table stx)
  (syntax-case stx ()
    [(_ name const ...)
     (with-syntax ([(sym ...)
                    (map translate-cname
                         (syntax-object->datum #'(const ...)))])
       (quasisyntax/loc stx
         (define name
           (let ([l `((,const . sym) ...)])
             (lambda (enum-val)
               (cdr (assq enum-val l)))))))]))

(define check-length
  (case-lambda
    [(name v desired-length sym)
     (unless (= desired-length (gl-vector-length v))
       (error name "expects vector of length ~a for ~a: argument vector has length ~a"
              desired-length sym (gl-vector-length v)))]
    [(name v desired-length)
     (unless (= desired-length (gl-vector-length v))
       (error name "expects vector of length ~a: argument vector has length ~a"
              desired-length (gl-vector-length v)))]))

;; 2.5
(_provide get-error)
(make-inv-enum-table get-error-table
                     GL_NO_ERROR
                     GL_INVALID_ENUM
                     GL_INVALID_VALUE
                     GL_INVALID_OPERATION
                     GL_STACK_OVERFLOW
                     GL_STACK_UNDERFLOW
                     GL_OUT_OF_MEMORY)
(define (get-error)
  (get-error-table (glGetError)))

;; 2.6.1
(_provide (rename gl-begin begin) (rename glEnd end))
(make-enum-table begin-table
                 GL_LINES
                 GL_LINE_LOOP
                 GL_LINE_STRIP
                 GL_POINTS
                 GL_POLYGON
                 GL_QUADS
                 GL_QUAD_STRIP
                 GL_TRIANGLES
                 GL_TRIANGLE_FAN
                 GL_TRIANGLE_STRIP)
(define (gl-begin enum)
  (glBegin (begin-table enum 'begin)))

;; 2.6.2
(_provide (rename glEdgeFlag edge-flag))

;; 2.7
(_provide vertex vertex-v
          tex-coord tex-coord-v
          multi-tex-coord multi-tex-coord-v
          (rename glNormal3d normal) normal-v
          color color-v
          (rename glSecondaryColor3d secondary-color) secondary-color-v
          (rename glIndexd index) index-v)

(multi-arg vertex glVertex () (2 3 4))
(multi-type-v vertex-v glVertex () (2 3 4) (dv iv fv sv) #t)
(multi-arg tex-coord glTexCoord () (1 2 3 4))
(multi-type-v tex-coord-v glTexCoord () (1 2 3 4) (dv iv fv sv) #t)
(make-enum-table multi-tex-coord-table
                 GL_TEXTURE0 GL_TEXTURE1 GL_TEXTURE2 GL_TEXTURE3 GL_TEXTURE4
                 GL_TEXTURE5 GL_TEXTURE6 GL_TEXTURE7 GL_TEXTURE8 GL_TEXTURE9
                 GL_TEXTURE10 GL_TEXTURE11 GL_TEXTURE12 GL_TEXTURE13
                 GL_TEXTURE14 GL_TEXTURE15 GL_TEXTURE16 GL_TEXTURE17
                 GL_TEXTURE18 GL_TEXTURE19 GL_TEXTURE20 GL_TEXTURE21
                 GL_TEXTURE22 GL_TEXTURE23 GL_TEXTURE24 GL_TEXTURE25
                 GL_TEXTURE26 GL_TEXTURE27 GL_TEXTURE28 GL_TEXTURE29
                 GL_TEXTURE30 GL_TEXTURE31)
(multi-arg multi-tex-coord glMultiTexCoord
           ((e (multi-tex-coord-table e 'multi-tex-coord)))
           (1 2 3 4))
(multi-type-v multi-tex-coord-v glMultiTexCoord
              ((e (multi-tex-coord-table e 'multi-tex-coord)))
              (1 2 3 4)
              (sv iv fv dv)
              #t)
(multi-type-v normal-v glNormal () (3) (dv iv fv sv bv) #t)
(multi-arg color glColor () (3 4))
(multi-type-v color-v glColor () (3 4) (dv iv uiv fv ubv bv usv sv) #t)
(multi-type-v secondary-color-v glSecondaryColor () (3) (bv sv iv fv dv ubv usv uiv) #t)
(multi-type-v index-v glIndex () (1) (dv iv fv sv ubv) #f)

;; 2.8, 2.9 not implemented

;; 2.10
(_provide (rename glRectd rect) rect-v)
(multi-type-v rect-v glRect () (4) (dv iv fv sv) #f)

;; 2.11.1
(_provide (rename glDepthRange depth-range) (rename glViewport viewport))

;; 2.11.2
(_provide matrix-mode load-matrix mult-matrix
          load-transpose-matrix mult-transpose-matrix
          (rename glLoadIdentity load-identity)
          (rename glRotated rotate)
          (rename glTranslated translate)
          (rename glScaled scale)
          (rename glFrustum frustum)
          (rename glOrtho ortho)
          active-texture
          (rename glPushMatrix push-matrix)
          (rename glPopMatrix pop-matrix))

(make-enum-table matrix-mode-table
                 GL_MODELVIEW GL_PROJECTION GL_TEXTURE GL_COLOR)
(define (matrix-mode x)
  (glMatrixMode (matrix-mode-table x 'matrix-mode)))
(define-values (glLoadMatrixfv glLoadMatrixdv glMultMatrixfv glMultMatrixdv
                glLoadTransposeMatrixfv glLoadTransposeMatrixdv
                glMultTransposeMatrixfv glMultTransposeMatrixdv)
  (values glLoadMatrixf glLoadMatrixd glMultMatrixf glMultMatrixd
          glLoadTransposeMatrixf glLoadTransposeMatrixd
          glMultTransposeMatrixf glMultTransposeMatrixd))
(multi-type-v load-matrix glLoadMatrix () (16) (fv dv) #f)
(multi-type-v mult-matrix glMultMatrix () (16) (fv dv) #f)
(multi-type-v load-transpose-matrix glLoadTransposeMatrix () (16) (fv dv) #f)
(multi-type-v mult-transpose-matrix glMultTransposeMatrix () (16) (fv dv) #f)

(define (active-texture texture)
  (glActiveTexture (multi-tex-coord-table texture 'active-texture texture)))

;; 2.11.3
(_provide enable disable)
(make-enum-table enable-table
                 GL_VERTEX_ARRAY GL_NORMAL_ARRAY GL_FOG_COORD_ARRAY
                 GL_COLOR_ARRAY GL_SECONDARY_COLOR_ARRAY GL_INDEX_ARRAY
                 GL_TEXTURE_COORD_ARRAY GL_EDGE_FLAG_ARRAY
                 GL_NORMALIZE GL_RESCALE_NORMAL
                 GL_CLIP_PLANE0 GL_CLIP_PLANE1 GL_CLIP_PLANE2 GL_CLIP_PLANE3
                 GL_CLIP_PLANE4 GL_CLIP_PLANE5
                 GL_FOG GL_COLOR_SUM
                 GL_LIGHTING GL_COLOR_MATERIAL
                 GL_LIGHT0 GL_LIGHT1 GL_LIGHT2 GL_LIGHT3 GL_LIGHT4
                 GL_LIGHT5 GL_LIGHT6 GL_LIGHT7
                 GL_POINT_SMOOTH GL_LINE_SMOOTH GL_LINE_STIPPLE GL_CULL_FACE
                 GL_POLYGON_SMOOTH GL_POLYGON_OFFSET_POINT
                 GL_POLYGON_OFFSET_LINE GL_POLYGON_OFFSET_FILL
                 GL_POLYGON_STIPPLE
                 GL_MULTISAMPLE GL_SAMPLE_ALPHA_TO_COVERAGE
                 GL_SAMPLE_ALPHA_TO_ONE GL_SAMPLE_COVERAGE
                 GL_TEXTURE_1D GL_TEXTURE_2D GL_TEXTURE_3D
                 GL_TEXTURE_CUBE_MAP
                 GL_TEXTURE_GEN_S GL_TEXTURE_GEN_T
                 GL_TEXTURE_GEN_R GL_TEXTURE_GEN_Q
                 GL_SCISSOR_TEST GL_ALPHA_TEST GL_STENCIL_TEST
                 GL_DEPTH_TEST GL_BLEND GL_DITHER
                 GL_INDEX_LOGIC_OP GL_LOGIC_OP GL_COLOR_LOGIC_OP
                 GL_COLOR_TABLE GL_POST_CONVOLUTION_COLOR_TABLE
                 GL_POST_COLOR_MATRIX_COLOR_TABLE
                 GL_CONVOLUTION_1D GL_CONVOLUTION_2D GL_SEPARABLE_2D
                 GL_HISTOGRAM GL_MINMAX
                 GL_MAP1_VERTEX_3 GL_MAP1_VERTEX_4 GL_MAP1_INDEX
                 GL_MAP1_COLOR_4 GL_MAP1_NORMAL
                 GL_MAP1_TEXTURE_COORD_1 GL_MAP1_TEXTURE_COORD_2
                 GL_MAP1_TEXTURE_COORD_3 GL_MAP1_TEXTURE_COORD_4
                 GL_MAP2_VERTEX_3 GL_MAP2_VERTEX_4 GL_MAP2_INDEX
                 GL_MAP2_COLOR_4 GL_MAP2_NORMAL
                 GL_MAP2_TEXTURE_COORD_1 GL_MAP2_TEXTURE_COORD_2
                 GL_MAP2_TEXTURE_COORD_3 GL_MAP2_TEXTURE_COORD_4
                 GL_AUTO_NORMAL)
(define (enable x)
  (glEnable (enable-table x 'enable)))
(define (disable x)
  (glDisable (enable-table x 'disable)))

;; 2.11.4
(_provide tex-gen tex-gen-v)
(make-enum-table tex-gen-coord-table GL_S GL_T GL_R GL_Q)
(make-enum-table tex-gen-pname-table
                 GL_TEXTURE_GEN_MODE GL_OBJECT_PLANE GL_EYE_PLANE)
(make-enum-table tex-gen-param-table
                 GL_OBJECT_LINEAR GL_EYE_LINEAR GL_SPHERE_MAP
                 GL_REFLECTION_MAP GL_NORMAL_MAP)
(define (tex-gen c p n)
  (let ([cv (tex-gen-coord-table c 'tex-gen)]
        [pv (tex-gen-pname-table p 'tex-gen)])
    (unless (= pv GL_TEXTURE_GEN_MODE)
      (error 'tex-gen "does not accept ~a, use tex-gen-v instead" p))
    (glTexGeni cv pv (tex-gen-param-table n 'tex-gen))))
(define (tex-gen-v c p v)
  (let ([cv (tex-gen-coord-table c 'tex-gen-v)]
        [pv (tex-gen-pname-table p 'tex-gen-v)])
    (when (= pv GL_TEXTURE_GEN_MODE)
      (error 'tex-gen-v "does not accept ~a, use tex-gen instead" p))
    (let ([f (cond [(gl-int-vector? v) glTexGeniv]
                   [(gl-float-vector? v) glTexGenfv]
                   [(gl-double-vector? v) glTexGendv]
                   [else (raise-argument-error
                          'tex-gen-v
                          "(or/c gl-int-vector? gl-float-vector? gl-double-vector?)"
                          2 c p v)])])
      (check-length 'tex-gen-v v 4)
      (f cv pv v))))

;; 2.12
(_provide clip-plane)
(make-enum-table clip-plane-table
                 GL_CLIP_PLANE0 GL_CLIP_PLANE1 GL_CLIP_PLANE2
                 GL_CLIP_PLANE3 GL_CLIP_PLANE4 GL_CLIP_PLANE5)
(define (clip-plane p eqn)
  (let ([v (clip-plane-table p 'clip-plane)])
    (unless (gl-double-vector? eqn)
      (raise-argument-error 'clip-plane "gl-double-vector?" 1 p eqn))
    (check-length 'clip-plane eqn 4)
    (glClipPlane v eqn)))

;; 2.13
(_provide raster-pos raster-pos-v
          window-pos window-pos-v)
(multi-arg raster-pos glRasterPos () (2 3 4))
(multi-type-v raster-pos-v glRasterPos () (2 3 4) (dv iv fv sv) #t)
(multi-arg window-pos glWindowPos () (2 3))
(multi-type-v window-pos-v glWindowPos () (2 3) (dv iv fv sv) #t)

;; 2.14.1
(_provide front-face)
(make-enum-table front-face-table GL_CCW GL_CW)
(define (front-face x)
  (glFrontFace (front-face-table x 'front-face)))

;; 2.14.2
(_provide material material-v light light-v light-model light-model-v)
(make-enum-table face-table GL_FRONT GL_BACK GL_FRONT_AND_BACK)
(make-enum-table material-pname-table
                 GL_AMBIENT GL_DIFFUSE GL_AMBIENT_AND_DIFFUSE
                 GL_SPECULAR GL_EMISSION GL_SHININESS GL_COLOR_INDEXES)

(define (get-f v iv fv name a1 a2)
  (cond [(gl-int-vector? v) iv]
        [(gl-float-vector? v) fv]
        [else (raise-argument-error name
                                    "(or/c gl-int-vector? gl-float-vector?)"
                                    2 a1 a2 v)]))
(define (do-f n v0 v1 i f name a0 a1)
  (unless (real? n)
    (raise-argument-error name "real?" 2 a0 a1 n))
  (if (exact-integer? n)
    (i v0 v1 n)
    (f v0 v1 n)))

(define (material face pname param)
  (let ([v0 (face-table face 'material)]
        [v1 (material-pname-table pname 'material)])
    (unless (= v1 GL_SHININESS)
      (error 'material "does not accept ~a, use material-v instead" pname))
    (do-f param v0 v1 glMateriali glMaterialf 'material face pname)))

(define (material-v face pname params)
  (let ([v0 (face-table face 'material-v)]
        [v1 (material-pname-table pname 'material-v)]
        [f (get-f params glMaterialiv glMaterialfv 'material-v face pname)])
    (check-length 'material-v params
                  (cond [(= GL_SHININESS v1)     1]
                        [(= GL_COLOR_INDEXES v1) 3]
                        [else                    4])
                  pname)
    (f v0 v1 params)))

(make-enum-table light-light-table
                 GL_LIGHT0 GL_LIGHT1 GL_LIGHT2 GL_LIGHT3
                 GL_LIGHT4 GL_LIGHT5 GL_LIGHT6 GL_LIGHT7)
(make-enum-table light-pname-table
                 GL_AMBIENT GL_DIFFUSE GL_SPECULAR GL_POSITION
                 GL_SPOT_DIRECTION
                 GL_SPOT_EXPONENT GL_SPOT_CUTOFF
                 GL_CONSTANT_ATTENUATION GL_LINEAR_ATTENUATION
                 GL_QUADRATIC_ATTENUATION)
(define (light light pname param)
  (let ([v0 (light-light-table light 'light)]
        [v1 (light-pname-table pname 'light)])
    (unless (memv v1 `(,GL_SPOT_EXPONENT ,GL_SPOT_CUTOFF
                       ,GL_CONSTANT_ATTENUATION ,GL_LINEAR_ATTENUATION
                       ,GL_QUADRATIC_ATTENUATION))
      (error 'light "does not accept ~a, use light-v instead" pname))
    (do-f param v0 v1 glLighti glLightf 'light light pname)))

(define (light-v light pname params)
  (let ([v0 (light-light-table light 'light-v)]
        [v1 (light-pname-table pname 'light-v)]
        [f (get-f params glLightiv glLightfv 'light-v light pname)])
    (check-length
     'light-v params
     (cond [(= GL_SPOT_DIRECTION v1) 3]
           [(memv v1 `(,GL_AMBIENT ,GL_DIFFUSE ,GL_SPECULAR ,GL_POSITION)) 4]
           [else 1])
     pname)
    (f v0 v1 params)))

(make-enum-table light-model-table
                 GL_LIGHT_MODEL_AMBIENT
                 GL_LIGHT_MODEL_COLOR_CONTROL
                 GL_LIGHT_MODEL_LOCAL_VIEWER
                 GL_LIGHT_MODEL_TWO_SIDE)

(define (light-model pname param)
  (let ([v (light-model-table pname 'light-model)])
    (when (= GL_LIGHT_MODEL_AMBIENT v)
      (error 'light-model "does not accept ~a, use light-model-v instead" pname))
    (unless (real? param)
      (raise-argument-error 'light-model "real?" 1 pname param))
    (if (exact-integer? param)
      (glLightModeli v param)
      (glLightModelf v param))))

(define (light-model-v pname params)
  (let ([v (light-model-table pname 'light-model-v)]
        [f (cond [(gl-int-vector? params) glLightModeliv]
                 [(gl-float-vector? params) glLightModelfv]
                 [else (raise-argument-error 'light-model-v
                                             "(or/c gl-int-vector? gl-float-vector?)"
                                             1 pname params)])])
    (check-length 'light-model-v params
                  (if (= GL_LIGHT_MODEL_AMBIENT v) 4 1)
                  pname)
    (f v params)))

;; 2.14.3
(_provide color-material)
(make-enum-table color-material-mode-table
                 GL_EMISSION GL_AMBIENT GL_DIFFUSE
                 GL_SPECULAR GL_AMBIENT_AND_DIFFUSE)
(define (color-material x y)
  (glColorMaterial (face-table x 'color-material)
                   (color-material-mode-table y 'color-material)))

;; 2.14.7
(_provide shade-model)
(make-enum-table shade-model-table GL_FLAT GL_SMOOTH)
(define (shade-model x)
  (glShadeModel (shade-model-table x 'shade-model)))

;; 3.3
(_provide (rename glPointSize point-size)
          point-parameter point-parameter-v)
(make-enum-table point-parameter-table
                 GL_POINT_SIZE_MIN GL_POINT_SIZE_MAX
                 GL_POINT_DISTANCE_ATTENUATION
                 GL_POINT_FADE_THRESHOLD_SIZE)
(define (point-parameter pname param)
  (let ([v (point-parameter-table pname 'point-parameter)])
    (when (= GL_POINT_DISTANCE_ATTENUATION v)
      (error 'point-parameter
             "does not accept ~a, use point-parameter-v instead" pname))
    (unless (real? param)
      (raise-argument-error 'point-parameter "real?" 1 pname param))
    (if (exact-integer? param)
      (glPointParameteri v param)
      (glPointParameterf v param))))
(define (point-parameter-v pname params)
  (let ([v (point-parameter-table pname 'point-parameter)]
        [f (cond [(gl-int-vector? params) glPointParameteriv]
                 [(gl-float-vector? params) glPointParameterfv]
                 [else (raise-argument-error 'point-parameter-v
                                             "(or/c gl-int-vector? gl-float-vector?)"
                                             1 pname params)])])
    (check-length 'point-parameter-v
                  (if (= GL_POINT_DISTANCE_ATTENUATION v) 3 1)
                  pname)
    (f v params)))

;; 3.4
(_provide (rename glLineWidth line-width))

;; 3.4.2
(_provide (rename glLineStipple line-stipple))

;; 3.5.1
(_provide cull-face)
(define (cull-face x)
  (glCullFace (face-table x)))

;; 3.5.2
;; polygon-stipple

;;3.5.4
(_provide polygon-mode)
(make-enum-table polygon-mode-mode-table GL_POINT GL_LINE GL_FILL)
(define (polygon-mode x y)
  (glPolygonMode (face-table x 'polygon-mode)
                 (polygon-mode-mode-table y 'polygon-mode)))

;; 3.5.5
(_provide (rename glPolygonOffset polygon-offset))

;; 3.6.1
(_provide pixel-store)
(make-enum-table pixel-store-table
                 GL_UNPACK_SWAP_BYTES GL_UNPACK_LSB_FIRST
                 GL_UNPACK_ROW_LENGTH GL_UNPACK_SKIP_ROWS
                 GL_UNPACK_SKIP_PIXELS GL_UNPACK_ALIGNMENT
                 GL_UNPACK_IMAGE_HEIGHT GL_UNPACK_SKIP_IMAGES)
(define (pixel-store pname param)
  (let ([v (pixel-store-table pname 'pixel-store)])
    (unless (real? param)
      (raise-argument-error 'pixel-store "real?" 1 pname param))
    (if (exact-integer? param)
      (glPixelStorei v param)
      (glPixelStoref v param))))

;; 3.6.3, 3.6.4, 3.6.5, 3.7, 3.8, 3.10 not implemented

;; 4.1.2
(_provide (rename glScissor scissor))

;; 4.1.3
(_provide (rename glSampleCoverage sample-coverage))

;; 4.1.4
(_provide alpha-func)
(make-enum-table func-table
                 GL_NEVER GL_ALWAYS GL_LESS GL_LEQUAL GL_EQUAL
                 GL_GEQUAL GL_GREATER GL_NOTEQUAL)
(define (alpha-func func ref)
  (glAlphaFunc (func-table func 'alpha-func) ref))

;; 4.1.5
(_provide stencil-func stencil-op)
(define (stencil-func func ref mask)
  (glStencilFunc (func-table func 'stencil-func) ref mask))

(make-enum-table stencil-op-table
                 GL_KEEP GL_ZERO GL_REPLACE GL_INCR GL_DECR GL_INVERT
                 GL_INCR_WRAP GL_DECR_WRAP)
(define (stencil-op sfail dpfail dppass)
  (glStencilOp (stencil-op-table sfail 'stencil-op)
               (stencil-op-table dpfail 'stencil-op)
               (stencil-op-table dppass 'stencil-op)))

;; 4.1.6
(_provide depth-func)
(define (depth-func func)
  (glDepthFunc (func-table func 'depth-func)))

;; 4.1.7
(_provide begin-query end-query
          (rename glGenQueries gen-queries)
          (rename glDeleteQueries delete-queries))
(make-enum-table query-table GL_SAMPLES_PASSED)
(define (begin-query target id)
  (glBeginQuery (query-table target 'begin-query) id))
(define (end-query target)
  (glEndQuery (query-table target 'end-query)))


;; 4.1.8
(_provide blend-equation blend-func blend-func-separate
          (rename glBlendColor blend-color))

(make-enum-table blend-equation-table
                 GL_FUNC_ADD GL_FUNC_SUBTRACT GL_FUNC_REVERSE_SUBTRACT
                 GL_MIN GL_MAX)
(define (blend-equation func)
  (glBlendEquation (blend-equation-table func 'blend-equation)))

(make-enum-table blend-func-table
                 GL_ZERO GL_ONE
                 GL_SRC_COLOR GL_ONE_MINUS_SRC_COLOR
                 GL_DST_COLOR GL_ONE_MINUS_DST_COLOR
                 GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA
                 GL_DST_ALPHA GL_ONE_MINUS_DST_ALPHA
                 GL_CONSTANT_COLOR GL_ONE_MINUS_CONSTANT_COLOR
                 GL_CONSTANT_ALPHA GL_ONE_MINUS_CONSTANT_ALPHA
                 GL_SRC_ALPHA_SATURATE)
(define (blend-func src dest)
  (glBlendFunc (blend-func-table src 'blend-func)
               (blend-func-table dest 'blend-func)))

(define (blend-func-separate src dest src-alpha dst-alpha)
  (glBlendFuncSeparate (blend-func-table src 'blend-func)
                       (blend-func-table dest 'blend-func)
                       (blend-func-table src-alpha 'blend-func)
                       (blend-func-table dst-alpha 'blend-func)))

;; 4.1.10
(provide logic-op)
(make-enum-table logic-op-table
                 GL_CLEAR GL_AND GL_AND_REVERSE GL_COPY GL_AND_INVERTED
                 GL_NOOP GL_XOR GL_OR GL_NOR GL_EQUIV GL_INVERT GL_OR_REVERSE
                 GL_COPY_INVERTED GL_OR_INVERTED GL_NAND GL_SET)
(define (logic-op op)
  (glLogicOp logic-op-table op 'logic-op))

;; 4.2.1
(provide draw-buffer)
(make-enum-table draw-buffer-table
                 GL_NONE GL_FRONT_LEFT GL_FRONT_RIGHT GL_BACK_LEFT
                 GL_BACK_RIGHT GL_FRONT GL_BACK GL_LEFT GL_RIGHT
                 GL_FRONT_AND_BACK
                 GL_AUX0 GL_AUX1 GL_AUX2 GL_AUX3)
(define (draw-buffer buf)
  (glDrawBuffer (draw-buffer-table buf 'draw-buffer)))

;; 4.2.2
(_provide (rename glIndexMask index-mask)
          (rename glColorMask color-mask)
          (rename glDepthMask depth-mask)
          (rename glStencilMask stencil-mask))

;; 4.2.3
(_provide clear
          (rename glClearColor clear-color)
          (rename glClearIndex clear-index)
          (rename glClearDepth clear-depth)
          (rename glClearStencil clear-stencil)
          (rename glClearAccum clear-accum))
(make-enum-table clear-table
                 GL_ACCUM_BUFFER_BIT GL_COLOR_BUFFER_BIT
                 GL_DEPTH_BUFFER_BIT GL_STENCIL_BUFFER_BIT)
(define (clear . x)
  (glClear (apply bitwise-ior (map (lambda (x) (clear-table x 'clear)) x))))

;; 4.2.4
(_provide accum)
(make-enum-table accum-table
                 GL_ACCUM GL_MULT GL_RETURN GL_MULT GL_ADD)
(define (accum op value)
  (glAccum (accum-table op 'accum) value))

;; 4.3.2 not implemented

;; 4.3.3
(_provide copy-pixels)
(make-enum-table copy-pixels-table
                 GL_COLOR GL_STENCIL GL_DEPTH)
(define (copy-pixels a b c d e)
  (glCopyPixels a b c d (copy-pixels-table e 'copy-pixels)))

;; 5.1
(_provide ;map1 map2
 eval-coord eval-coord-v map-grid eval-mesh eval-point)
(multi-arg eval-coord glEvalCoord () (1 2))
(multi-type-v eval-coord-v glEvalCoord () (1 2) (dv fv) #t)
(define map-grid
  (case-lambda
    [(n a b) (glMapGrid1d n a b)]
    [(m a b n c d) (glMapGrid2d m a b n c d)]))
(make-enum-table eval-mesh-table GL_POINT GL_LINE)
(define eval-mesh
  (case-lambda
    [(e a b) (glEvalMesh1 (eval-mesh-table e 'eval-mesh) a b)]
    [(e a b c d) (glEvalMesh2 (eval-mesh-table e 'eval-mesh) a b c d)]))
(define eval-point
  (case-lambda
    [(x) (glEvalPoint1 x)]
    [(x y) (glEvalPoint2 x y)]))

;; 5.2
(_provide (rename glInitNames init-names)
          (rename glPopName pop-name)
          (rename glPushName push-name)
          (rename glLoadName load-name)
          render-mode
          select-buffer->gl-uint-vector)
(make-enum-table render-mode-table GL_RENDER GL_SELECT GL_FEEDBACK)
(define (render-mode x)
  (glRenderMode (render-mode-table x 'render-mode)))

;; 5.3
(_provide feedback-buffer->gl-float-vector
          (rename glPassThrough pass-through))

;; 5.4
(_provide new-list
          (rename glEndList end-list)
          (rename glCallList call-list)
          ;; call-lists
          (rename glListBase list-base)
          (rename glGenLists gen-lists)
          (rename glIsList is-list)
          (rename glDeleteLists delete-lists))
(make-enum-table new-list-table GL_COMPILE GL_COMPILE_AND_EXECUTE)
(define (new-list n mode)
  (glNewList n (new-list-table mode 'new-list)))

;; 5.5
(_provide (rename glFlush flush)
          (rename glFinish finish))

;; 5.6
(_provide hint)
(make-enum-table hint-target-table
                 GL_PERSPECTIVE_CORRECTION_HINT GL_POINT_SMOOTH_HINT
                 GL_LINE_SMOOTH_HINT GL_POLYGON_SMOOTH_HINT GL_FOG_HINT
                 GL_GENERATE_MIPMAP_HINT GL_TEXTURE_COMPRESSION_HINT)
(make-enum-table hint-hint-table GL_FASTEST GL_NICEST GL_DONT_CARE)
(define (hint target hint)
  (glHint (hint-target-table target 'hint)
          (hint-hint-table hint 'hint)))

;; 6.1.1
(_provide ;glGetBooleanv glGetIntegerv glGetFloatv glGetDoublev
 is-enabled)
(define (is-enabled e)
  (glIsEnabled (enable-table e 'is-enabled)))

;; 6.1.3, 6.1.4, 6.1.5, 6.1.7, 6.1.8, 6.1.9, 6.1.10 not implemented

;; 6.1.11
(_provide ;get-pointer-v
          get-string)

(make-enum-table get-string-table
                 GL_VENDOR GL_RENDERER GL_VERSION GL_EXTENSIONS)
(define (get-string x)
  (glGetString (get-string-table x 'get-string)))

;; 6.1.12
(_provide (rename glIsQuery is-query)
          ;; get-query get-query-object
          )

;; 6.1.13
(_provide (rename glIsBuffer is-buffer)
          ;; get-buffer-sub-data get-buffer-pointer-v
          )

;; 6.1.14
(_provide push-attrib push-client-attrib
          (rename glPopAttrib pop-attrib)
          (rename glPopClientAttrib pop-client-attrib))
(make-enum-table push-attrib-table
                 GL_ACCUM_BUFFER_BIT GL_COLOR_BUFFER_BIT GL_CURRENT_BIT
                 GL_DEPTH_BUFFER_BIT GL_ENABLE_BIT GL_EVAL_BIT GL_FOG_BIT GL_HINT_BIT
                 GL_LIGHTING_BIT GL_LINE_BIT GL_LIST_BIT GL_MULTISAMPLE_BIT
                 GL_PIXEL_MODE_BIT GL_POINT_BIT GL_POLYGON_BIT GL_POLYGON_STIPPLE_BIT
                 GL_SCISSOR_BIT GL_STENCIL_BUFFER_BIT GL_TEXTURE_BIT
                 GL_TRANSFORM_BIT GL_VIEWPORT_BIT GL_ALL_ATTRIB_BITS)
(define (push-attrib . x)
  (glPushAttrib
   (apply bitwise-ior (map (lambda (x) (push-attrib-table x 'clear)) x))))
(make-enum-table push-client-attrib-table
                 GL_CLIENT_VERTEX_ARRAY_BIT
                 GL_CLIENT_PIXEL_STORE_BIT
                 GL_CLIENT_ALL_ATTRIB_BITS)
(define (push-client-attrib . x)
  (glPushClientAttrib
   (apply bitwise-ior
          (map (lambda (x) (push-client-attrib-table x 'clear)) x))))

;; 2
(_provide u-get-string
          (rename gluCheckExtension check-extension))
(make-enum-table u-get-string-table GLU_VERSION GLU_EXTENSIONS)
(define (u-get-string x)
  (gluGetString (u-get-string-table x 'u-get-string)))

;; 3 not implemented

;; 4.1
(_provide (rename gluOrtho2D ortho-2d)
          (rename gluPerspective perspective)
          (rename gluLookAt look-at)
          pick-matrix)
(define (pick-matrix a b c d v)
  (unless (gl-int-vector? v)
    (raise-argument-error 'pick-matrix
                          "gl-int-vector?"
                          4 a b c d v))
  (check-length 'pick-matrix v 4)
  (gluPickMatrix a b c d v))

;; 4.2
(_provide project un-project un-project4)
(define (project a b c d e f)
  (unless (gl-double-vector? d)
    (raise-argument-error 'project "gl-double-vector?" 3 a b c d e f))
  (unless (gl-double-vector? e)
    (raise-argument-error 'project "gl-double-vector?" 4 a b c d e f))
  (unless (gl-int-vector? f)
    (raise-argument-error 'project "gl-double-vector?" 5 a b c d e f))
  (check-length 'project d 16)
  (check-length 'project e 16)
  (check-length 'project f 4)
  (gluProject a b c d e f))

(define (un-project a b c d e f)
  (unless (gl-double-vector? d)
    (raise-argument-error 'un-project "gl-double-vector?" 3 a b c d e f))
  (unless (gl-double-vector? e)
    (raise-argument-error 'un-project "gl-double-vector?" 4 a b c d e f))
  (unless (gl-int-vector? f)
    (raise-argument-error 'un-project "gl-double-vector?" 5 a b c d e f))
  (check-length 'un-project d 16)
  (check-length 'un-project e 16)
  (check-length 'un-project f 4)
  (gluUnProject a b c d e f))

(define (un-project4 a b c d e f g h i)
  (unless (gl-double-vector? e)
    (raise-argument-error 'un-project "gl-double-vector?" 4 a b c d e f g h i))
  (unless (gl-double-vector? f)
    (raise-argument-error 'un-project "gl-double-vector?" 5 a b c d e f g h i))
  (unless (gl-int-vector? g)
    (raise-argument-error 'un-project "gl-double-vector?" 6 a b c d e f g h i))
  (check-length 'un-project4 e 16)
  (check-length 'un-project4 f 16)
  (check-length 'un-project4 g 4)
  (gluUnProject4 a b c d e f g h i))

;; 5 not implemented

;; 6.1
(_provide (rename gluNewQuadric new-quadric))

;; 6.2 not implemented

;; 6.3
(_provide quadric-normals
          (rename gluQuadricTexture quadric-texture)
          quadric-orientation quadric-draw-style)

(make-enum-table quadric-normals-table GLU_NONE GLU_FLAT GLU_SMOOTH)
(define (quadric-normals q e)
  (gluQuadricNormals q (quadric-normals-table e 'quadric-normals)))

(make-enum-table quadric-orientation-table GLU_INSIDE GLU_OUTSIDE)
(define (quadric-orientation q e)
  (gluQuadricOrientation q (quadric-orientation-table e 'quadric-normals)))

(make-enum-table quadric-draw-style-table
                 GLU_POINT GLU_LINE GLU_SILHOUETTE GLU_FILL)
(define (quadric-draw-style q e)
  (gluQuadricDrawStyle q (quadric-draw-style-table e 'quadric-draw-style)))

;; 6.4
(_provide (rename gluCylinder cylinder)
          (rename gluDisk disk)
          (rename gluSphere sphere)
          (rename gluPartialDisk partial-disk))

;; 7 not implemented

;; 8
(_provide ;error-string
          )

;; Utils

(_provide process-selection (struct selection-record (min-z max-z stack)))
;; A selection-record is
;; (make-selection-record number number (listof positive-int))
(define-struct selection-record (min-z max-z stack))

;; process-selection : gl-uint-vector int -> (listof selection-record)
(define (process-selection v hits)
  (unless (gl-uint-vector? v)
    (raise-argument-error 'process-selection "gl-uint-vector?" 0 v hits))
  (let ([index 0])
    (let loop ([hit 0])
      (if (>= hit hits)
        null
        (let ([stack-size (gl-vector-ref v index)])
          (cons (make-selection-record
                 (gl-vector-ref v (add1 index))
                 (gl-vector-ref v (+ index 2))
                 (begin (set! index (+ 3 index))
                        (let loop ([j 0])
                          (if (< j stack-size)
                            (cons (gl-vector-ref v index)
                                  (begin (set! index (add1 index))
                                         (loop (add1 j))))
                            null))))
                (loop (add1 hit))))))))

(provide get-gl-version-number get-glu-version-number)
(define (get-gl-version-number)
  (let ([x (regexp-match "^([0-9]*)\\.([0-9*])" (get-string 'version))])
    (string->number (string-append (cadr x) (caddr x)))))
(define (get-glu-version-number)
  (let ([x (regexp-match "^([0-9]*)\\.([0-9*])" (u-get-string 'version))])
    (string->number (string-append (cadr x) (caddr x)))))
