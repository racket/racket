#lang mzscheme
(require mzlib/foreign
         "gl-types.rkt"
         "gl-vectors.rkt"
         "init.rkt")

(unsafe!)

(define stype (system-type))

(define gl-lib (case stype
                 [(windows) (ffi-lib "opengl32")]
                 [(macosx) (ffi-lib "/System/Library/Frameworks/OpenGL.framework/Versions/A/Libraries/libGL")]
                 [else (ffi-lib "libGL" '("1" ""))]))
(define glu-lib (case stype
                  [(windows) (ffi-lib "glu32")]
                  [(macosx) (ffi-lib "/System/Library/Frameworks/OpenGL.framework/Versions/A/Libraries/libGLU")]
                  [else (ffi-lib "libGLU" '("1" ""))]))

(define win32?
  (and (eq? 'windows stype)
       (equal? "win32\\i386" (path->string (system-library-subpath #f)))))

(define-syntax _fun*
  (syntax-rules ()
    [(_fun* x ...)
     (if win32? (_fun #:abi 'stdcall x ...) (_fun x ...))]))

(define wglGetProcAddress
  (if (eq? 'windows stype)
      (get-ffi-obj 'wglGetProcAddress gl-lib (_fun* _string -> _fpointer))
      (lambda (x) #f)))

(define (unavailable name fun-type)
  (if (eq? 'windows stype)
      ;; Windows: try to get proc via wglGetProcAddress;
      ;; note that we need to delay the lookup until the
      ;; function is called, because wglGetProcAddress is
      ;; sensitive to the current GL context
      (lambda ()
	 (let ([sname (symbol->string name)])
	   (lambda args
	     (let ([f (wglGetProcAddress sname)])
	       (if f
		   (apply (function-ptr f fun-type) args)
		   (error name "unavailable on this system"))))))
      ;; Other platforms: proc is not available
      (lambda () (lambda x (error name "unavailable on this system")))))

(define-syntax define-foreign-lib
  (syntax-rules (->)
    [(_ lib name type ... ->)
     (define-foreign-lib lib name type ... -> _void)]
    [(_ lib name type ...)
     (begin
       ;; (printf "~a\n" 'name)
       (provide name)
       (define name
	 (let ([fun-type (_fun* type ...) ])
	   (get-ffi-obj 'name lib fun-type
			(unavailable 'name fun-type)))))]))

(define-syntax define-foreign
  (syntax-rules ()
    [(_ args ...) (define-foreign-lib gl-lib args ...)]))

(define-for-syntax (get-type x err)
  (case (syntax-object->datum x)
    [(b) #'_gl-byte]
    [(s) #'_gl-short]
    [(i) #'_gl-int]
    [(f) #'_gl-float]
    [(d) #'_gl-double]
    [(ub) #'_gl-ubyte]
    [(us) #'_gl-ushort]
    [(ui) #'_gl-uint]
    [(bv) #'_gl-bytev]
    [(sv) #'_gl-shortv]
    [(iv) #'_gl-intv]
    [(fv) #'_gl-floatv]
    [(dv) #'_gl-doublev]
    [(ubv) #'_gl-ubytev]
    [(usv) #'_gl-ushortv]
    [(uiv) #'_gl-uintv]
    [else (raise-syntax-error #f "unknown GL type abbreviation" err x)]))

(define-for-syntax (get-vtype x err)
  (case (syntax-object->datum x)
    [(bv) #'_gl-byte]
    [(sv) #'_gl-short]
    [(iv) #'_gl-int]
    [(fv) #'_gl-float]
    [(dv) #'_gl-double]
    [(ubv) #'_gl-ubyte]
    [(usv) #'_gl-ushort]
    [(uiv) #'_gl-uint]
    [else (raise-syntax-error #f "unknown GL type abbreviation" err x)]))

(define-for-syntax (type-map type convert)
  (syntax-case type (: =)
    [(label : type) #`(label : #,(type-map #'type convert))]
    [(type = expr) #`(#,(type-map #'type convert) = expr)]
    [(label : type = expr) #`(label : #,(type-map #'type convert) = expr)]
    [_ (convert type)]))

(define-syntax (define-foreign-tparm stx)
  (syntax-case stx (->)
    [(_ name (suffix ...) type ...)
     (let* ([name-sym (syntax-object->datum #'name)]
            [build-def
             (lambda (suffix)
               (with-syntax ([new-name
                              (datum->syntax-object
                               #'name
                               (string->symbol
                                (format "~a~a"
                                        name-sym
                                        (syntax-object->datum suffix)))
                               #'name)]
                             [(new-type ...)
                              (map
                               (lambda (type)
                                 (type-map
                                  type
                                  (lambda (type)
                                    (syntax-case type (T outT)
                                      [T (get-type suffix stx)]
                                      [(T n)
                                       #`(_cvector o
                                                   #,(get-vtype suffix stx)
                                                   n)]
                                      [_ type]))))
                               (syntax->list #'(type ...)))])
                 #'(define-foreign new-name new-type ...)))])
       (with-syntax ([(defs ...)
                      (map build-def (syntax->list #'(suffix ...)))])
         #'(begin defs ...)))]))

;; 2.5
(define-foreign glGetError -> _gl-enum)

;; 2.6.1
(define-foreign glBegin _gl-enum ->)
(define-foreign glEnd ->)

;; 2.6.2
(define-foreign glEdgeFlag _gl-boolean ->)
(define-foreign glEdgeFlagv _gl-booleanv ->)

;; 2.7
(define-foreign-tparm glVertex2 (s i f d) T T ->)
(define-foreign-tparm glVertex3 (s i f d) T T T ->)
(define-foreign-tparm glVertex4 (s i f d) T T T T ->)
(define-foreign-tparm glVertex2 (sv iv fv dv) T ->)
(define-foreign-tparm glVertex3 (sv iv fv dv) T ->)
(define-foreign-tparm glVertex4 (sv iv fv dv) T ->)
(define-foreign-tparm glTexCoord1 (s i f d) T ->)
(define-foreign-tparm glTexCoord2 (s i f d) T T ->)
(define-foreign-tparm glTexCoord3 (s i f d) T T T ->)
(define-foreign-tparm glTexCoord4 (s i f d) T T T T ->)
(define-foreign-tparm glTexCoord1 (sv iv fv dv) T ->)
(define-foreign-tparm glTexCoord2 (sv iv fv dv) T ->)
(define-foreign-tparm glTexCoord3 (sv iv fv dv) T ->)
(define-foreign-tparm glTexCoord4 (sv iv fv dv) T ->)
(define-foreign-tparm glMultiTexCoord1 (s i f d) _gl-enum T ->)
(define-foreign-tparm glMultiTexCoord2 (s i f d) _gl-enum T T ->)
(define-foreign-tparm glMultiTexCoord3 (s i f d) _gl-enum T T T ->)
(define-foreign-tparm glMultiTexCoord4 (s i f d) _gl-enum T T T T ->)
(define-foreign-tparm glMultiTexCoord1 (sv iv fv dv) _gl-enum T ->)
(define-foreign-tparm glMultiTexCoord2 (sv iv fv dv) _gl-enum T ->)
(define-foreign-tparm glMultiTexCoord3 (sv iv fv dv) _gl-enum T ->)
(define-foreign-tparm glMultiTexCoord4 (sv iv fv dv) _gl-enum T ->)
(define-foreign-tparm glNormal3 (b s i f d) T T T ->)
(define-foreign-tparm glNormal3 (bv sv iv fv dv) T ->)
(define-foreign-tparm glFogCoord (f d) T ->)
(define-foreign-tparm glFogCoord (fv dv) T ->)
(define-foreign-tparm glColor3 (b s i f d ub us ui) T T T ->)
(define-foreign-tparm glColor4 (b s i f d ub us ui) T T T T ->)
(define-foreign-tparm glColor3 (bv sv iv fv dv ubv usv uiv) T ->)
(define-foreign-tparm glColor4 (bv sv iv fv dv ubv usv uiv) T ->)
(define-foreign-tparm glSecondaryColor3 (b s i f d ub us ui) T T T ->)
(define-foreign-tparm glSecondaryColor3 (bv sv iv fv dv ubv usv uiv) T ->)
(define-foreign-tparm glIndex (s i f d ub) T ->)
(define-foreign-tparm glIndex (sv iv fv dv ubv) T ->)

;; 2.8
#|
(define-foreign glVertexPointer _gl-int _gl-enum _gl-sizei _gl-voidv ->)
(define-foreign glNormalPointer _gl-enum _gl-sizei _gl-voidv ->)
(define-foreign glColorPointer _gl-int _gl-enum _gl-sizei _gl-voidv ->)
(define-foreign glSecondaryColorPointer
  _gl-int _gl-enum _gl-sizei _gl-voidv ->)
(define-foreign glIndexPointer _gl-enum _gl-sizei _gl-voidv ->)
(define-foreign glFogCoordPointer _gl-enum _gl-sizei _gl-voidv ->)
(define-foreign glTexCoordPointer _gl-int _gl-enum _gl-sizei _gl-voidv ->)
(define-foreign glEdgeFlagPointer _gl-sizei _gl-voidv ->)
(define-foreign glEnableClientState _gl-enum ->)
(define-foreign glDisableClientState _gl-enum ->)
(define-foreign glClientActiveTexture _gl-enum ->)
(define-foreign glArrayElement _gl-int ->)
(define-foreign glDrawArrays _gl-enum _gl-int _gl-sizei ->)
(define-foreign glMultiDrawArrays _gl-enum _gl-intv _gl-sizeiv _gl-sizei ->)
(define-foreign glDrawElements _gl-enum _gl-sizei _gl-enum _gl-voidv ->)
(define-foreign glMultiDrawElements
  _gl-enum _gl-sizeiv _gl-enum _gl-voidvv _gl-sizei ->)
(define-foreign glDrawRangeElements
  _gl-enum _gl-uint _gl-uint _gl-sizei _gl-enum _gl-voidv ->)
(define-foreign glInterleavedArrays _gl-enum _gl-sizei _gl-voidv)
|#

;; 2.9
#|
(define-foreign glBindBuffer _gl-enum _gl-uint ->)
(define-foreign glDeleteBuffers _gl-sizei gl_uintv ->)
(define-foreign glGenBuffers _gl-sizei gl_uintv ->)
(define-foreign glBufferData _gl-enum _gl-sizeiptr _gl_voidv _gl-enum ->)
(define-foreign glBufferSubData _gl-enum _gl-intptr _gl-sizeiptr _gl_voidv ->)
(define-foreign glMapBuffer _gl-enum _gl-enum -> _gl-voidv)
(define-foreign glUnmapBuffer _gl-enum -> _gl-boolean)
|#

;; 2.10
(define-foreign-tparm glRect (s i f d) T T T T ->)
(define-foreign-tparm glRect (sv iv fv dv) T ->)

;; 2.11.1
(define-foreign glDepthRange _gl-clampd _gl-clampd ->)
(define-foreign glViewport _gl-int _gl-int _gl-sizei _gl-sizei ->)

;; 2.11.2
(define-foreign glMatrixMode _gl-enum ->)
(define-foreign glLoadMatrixf _gl-floatv ->)
(define-foreign glLoadMatrixd _gl-doublev ->)
(define-foreign glMultMatrixf _gl-floatv ->)
(define-foreign glMultMatrixd _gl-doublev ->)
(define-foreign glLoadTransposeMatrixf _gl-floatv ->)
(define-foreign glLoadTransposeMatrixd _gl-doublev ->)
(define-foreign glMultTransposeMatrixf _gl-floatv ->)
(define-foreign glMultTransposeMatrixd _gl-doublev ->)
(define-foreign glLoadIdentity ->)
(define-foreign-tparm glRotate (f d) T T T T ->)
(define-foreign-tparm glTranslate (f d) T T T ->)
(define-foreign-tparm glScale (f d) T T T ->)
(define-foreign glFrustum _gl-double _gl-double _gl-double
                          _gl-double _gl-double _gl-double ->)
(define-foreign glOrtho _gl-double _gl-double _gl-double
                        _gl-double _gl-double _gl-double ->)
(define-foreign glActiveTexture _gl-enum ->)
(define-foreign glPushMatrix ->)
(define-foreign glPopMatrix ->)

;; 2.11.3
(define-foreign glEnable _gl-enum ->)
(define-foreign glDisable _gl-enum ->)

;; 2.11.4
(define-foreign-tparm glTexGen (i f d) _gl-enum _gl-enum T ->)
(define-foreign-tparm glTexGen (iv fv dv) _gl-enum _gl-enum T ->)

;; 2.12
(define-foreign glClipPlane _gl-enum _gl-doublev ->)

;; 2.13
(define-foreign-tparm glRasterPos2 (s i f d) T T ->)
(define-foreign-tparm glRasterPos3 (s i f d) T T T ->)
(define-foreign-tparm glRasterPos4 (s i f d) T T T T ->)
(define-foreign-tparm glRasterPos2 (sv iv fv dv) T ->)
(define-foreign-tparm glRasterPos3 (sv iv fv dv) T ->)
(define-foreign-tparm glRasterPos4 (sv iv fv dv) T ->)
(define-foreign-tparm glWindowPos2 (s i f d) T T ->)
(define-foreign-tparm glWindowPos3 (s i f d) T T T ->)
(define-foreign-tparm glWindowPos2 (sv iv fv dv) T ->)
(define-foreign-tparm glWindowPos3 (sv iv fv dv) T ->)

;; 2.14.1
(define-foreign glFrontFace _gl-enum ->)

;; 2.14.2
(define-foreign-tparm glMaterial (i f) _gl-enum _gl-enum T ->)
(define-foreign-tparm glMaterial (iv fv) _gl-enum _gl-enum T ->)
(define-foreign-tparm glLight (i f) _gl-enum _gl-enum T ->)
(define-foreign-tparm glLight (iv fv) _gl-enum _gl-enum T ->)
(define-foreign-tparm glLightModel (i f) _gl-enum T ->)
(define-foreign-tparm glLightModel (iv fv) _gl-enum T ->)

;; 2.14.3
(define-foreign glColorMaterial _gl-enum _gl-enum ->)

;; 2.14.7
(define-foreign glShadeModel _gl-enum ->)

;; 3.3
(define-foreign glPointSize _gl-float ->)
(define-foreign-tparm glPointParameter (i f) _gl-enum _gl-float ->)
(define-foreign-tparm glPointParameter (iv fv) _gl-enum _gl-floatv ->)

;; 3.4
(define-foreign glLineWidth _gl-float ->)

;; 3.4.2
(define-foreign glLineStipple _gl-int _gl-ushort ->)

;; 3.5.1
(define-foreign glCullFace _gl-enum ->)

;; 3.5.2
(define-foreign glPolygonStipple _gl-ubytev ->)

;; 3.5.4
(define-foreign glPolygonMode _gl-enum _gl-enum ->)

;; 3.5.5
(define-foreign glPolygonOffset _gl-float _gl-float ->)

;; 3.6.1
(define-foreign-tparm glPixelStore (i f) _gl-enum T ->)

;; 3.6.3
(define-foreign-tparm glPixelTransfer (i f) _gl-enum T ->)
(define-foreign-tparm glPixelMap (uiv usv fv)
                      _gl-enum [_gl-sizei = (cvector-length l)] [l : T] ->)
(define-foreign glColorTable
                _gl-enum _gl-enum _gl-sizei _gl-enum _gl-enum _gl-voidv ->)
(define-foreign-tparm glColorTableParameter (iv fv) _gl-enum _gl-enum T ->)
(define-foreign glCopyColorTable
                _gl-enum _gl-enum _gl-int _gl-int _gl-sizei ->)
(define-foreign glColorSubTable
                _gl-enum _gl-sizei _gl-sizei _gl-enum _gl-enum _gl-voidv ->)
(define-foreign glCopyColorSubTable
                _gl-enum _gl-sizei _gl-int _gl-int _gl-sizei ->)
(define-foreign glConvolutionFilter2D _gl-enum _gl-enum _gl-sizei _gl-sizei
                                      _gl-enum _gl-enum _gl-voidv ->)
(define-foreign-tparm glConvolutionParameter (iv fv) _gl-enum _gl-enum T ->)
(define-foreign glConvolutionFilter1D
                _gl-enum _gl-enum _gl-sizei _gl-enum _gl-enum _gl-voidv ->)
(define-foreign glSeparableFilter2D _gl-enum _gl-enum _gl-sizei _gl-sizei
                                    _gl-enum _gl-enum _gl-voidv _gl-voidv ->)
(define-foreign glCopyConvolutionFilter2D
                _gl-enum _gl-enum _gl-int _gl-int _gl-sizei _gl-sizei ->)
(define-foreign glCopyConvolutionFilter1D
                _gl-enum _gl-enum _gl-int _gl-int _gl-sizei ->)
(define-foreign glHistogram _gl-enum _gl-sizei _gl-enum _gl-boolean ->)
(define-foreign glMinmax _gl-enum _gl-enum _gl-boolean ->)

;; 3.6.4
(define-foreign glDrawPixels
                _gl-sizei _gl-sizei _gl-enum _gl-enum _gl-voidv ->)
(define-foreign glPixelZoom _gl-float _gl-float ->)

;; 3.6.5
(define-foreign-tparm  glConvolutionParameter (i f) _gl-enum _gl-enum T ->)

;; 3.7
(define-foreign glBitmap _gl-sizei _gl-sizei _gl-float _gl-float _gl-float
                         _gl-float _gl-ubytev ->)

;; 3.8.1
(define-foreign glTexImage3D _gl-enum _gl-int _gl-int _gl-sizei _gl-sizei
                             _gl-sizei _gl-int _gl-enum _gl-enum _gl-voidv ->)
(define-foreign glTexImage2D _gl-enum _gl-int _gl-int _gl-sizei _gl-sizei
                             _gl-int _gl-enum _gl-enum _gl-voidv ->)
(define-foreign glTexImage1D _gl-enum _gl-int _gl-int _gl-sizei _gl-int 
                             _gl-enum _gl-enum _gl-voidv ->)

;; 3.8.2
(define-foreign glCopyTexImage2D _gl-enum _gl-int _gl-enum _gl-int _gl-int
                                 _gl-sizei _gl-sizei _gl-int ->)
(define-foreign glCopyTexImage1D _gl-enum _gl-int _gl-enum _gl-int _gl-int
                                 _gl-sizei _gl-int ->)
(define-foreign glTexSubImage3D
                _gl-enum _gl-int _gl-int _gl-int _gl-int _gl-sizei _gl-sizei 
                _gl-sizei _gl-enum _gl-enum _gl-voidv ->)
(define-foreign glTexSubImage2D
                _gl-enum _gl-int _gl-int _gl-int _gl-sizei _gl-sizei 
                _gl-enum _gl-enum _gl-voidv ->)
(define-foreign glTexSubImage1D _gl-enum _gl-int _gl-int _gl-sizei 
                                _gl-enum _gl-enum _gl-voidv ->)
(define-foreign glCopyTexSubImage3D
                _gl-enum _gl-int _gl-int _gl-int _gl-int _gl-int _gl-int 
                _gl-sizei _gl-sizei ->)
(define-foreign glCopyTexSubImage2D
                _gl-enum _gl-int _gl-int _gl-int _gl-int _gl-int
                _gl-sizei _gl-sizei ->)
(define-foreign glCopyTexSubImage1D
                _gl-enum _gl-int _gl-int _gl-int _gl-int _gl-sizei ->)

;; 3.8.3
(define-foreign glCompressedTexImage1D
                _gl-enum _gl-int _gl-enum _gl-sizei _gl-int _gl-sizei
                _gl-voidv ->)
(define-foreign glCompressedTexImage2D
                _gl-enum _gl-int _gl-enum _gl-sizei _gl-sizei _gl-int
                _gl-sizei _gl-voidv ->)
(define-foreign glCompressedTexImage3D
                _gl-enum _gl-int _gl-enum _gl-sizei _gl-sizei _gl-sizei
                _gl-int _gl-sizei _gl-voidv ->)
(define-foreign glCompressedTexSubImage1D
                _gl-enum _gl-int _gl-int _gl-sizei _gl-enum _gl-sizei
                _gl-voidv ->)
(define-foreign glCompressedTexSubImage2D
                _gl-enum _gl-int _gl-int _gl-int _gl-sizei _gl-sizei _gl-enum
                _gl-sizei _gl-voidv ->)
(define-foreign glCompressedTexSubImage3D
                _gl-enum _gl-int _gl-int _gl-int _gl-int _gl-sizei _gl-sizei
                _gl-sizei _gl-enum _gl-sizei _gl-voidv ->)

;; 3.8.4
(define-foreign-tparm glTexParameter (i f) _gl-enum _gl-enum T ->)
(define-foreign-tparm glTexParameter (iv fv) _gl-enum _gl-enum T ->)

;; 3.8.12
(define-foreign glBindTexture _gl-enum _gl-uint ->)
(define-foreign glDeleteTextures
                [_gl-sizei = (cvector-length v)] [v : _gl-uintv] ->)
(define-foreign glGenTextures
                [n : _gl-sizei] [r : (_cvector o _gl-uint n)] -> _void -> r)
(define-foreign glAreTexturesResident
                [n : _gl-sizei = (cvector-length v)] [v : _gl-uintv]
                [r : (_cvector o _gl-boolean n)] ->
                [r2 : _gl-boolean] -> (values r2 r))

;; 3.8.13
(define-foreign-tparm glTexEnv (i f) _gl-enum _gl-enum T ->)
(define-foreign-tparm glTexEnv (iv fv) _gl-enum _gl-enum T ->)

;; 3.10
(define-foreign-tparm glFog (i f) _gl-enum T ->)
(define-foreign-tparm glFog (iv fv) _gl-enum T ->)

;; 4.1.2
(define-foreign glScissor _gl-int _gl-int _gl-sizei _gl-sizei ->)

;; 4.1.3
(define-foreign glSampleCoverage _gl-clampf _gl-boolean ->)

;; 4.1.4
(define-foreign glAlphaFunc _gl-enum _gl-clampf ->)

;; 4.1.5
(define-foreign glStencilFunc _gl-enum _gl-int _gl-uint ->)
(define-foreign glStencilOp _gl-enum _gl-enum _gl-enum ->)

;; 4.1.6
(define-foreign glDepthFunc _gl-enum ->)

;; 4.1.7
(define-foreign glBeginQuery _gl-enum _gl-uint ->)
(define-foreign glEndQuery _gl-enum ->)
(define-foreign glGenQueries
                [n : _gl-sizei] [r : (_cvector o _gl-uint n)] -> _void -> r)
(define-foreign glDeleteQueries [_gl-sizei = (cvector-length v)]
                                [v : _gl-uintv] ->)

;; 4.1.8
(define-foreign glBlendEquation _gl-enum ->)
(define-foreign glBlendFuncSeparate _gl-enum _gl-enum _gl-enum _gl-enum ->)
(define-foreign glBlendFunc _gl-enum _gl-enum ->)
(define-foreign glBlendColor _gl-clampf _gl-clampf _gl-clampf _gl-clampf ->)

;; 4.1.10
(define-foreign glLogicOp _gl-enum ->)

;; 4.2.1
(define-foreign glDrawBuffer _gl-enum ->)

;; 4.2.2
(define-foreign glIndexMask _gl-uint ->)
(define-foreign glColorMask
                _gl-boolean _gl-boolean _gl-boolean _gl-boolean ->)
(define-foreign glDepthMask _gl-boolean ->)
(define-foreign glStencilMask _gl-uint ->)

;; 4.2.3
(define-foreign glClear _gl-bitfield ->)
(define-foreign glClearColor _gl-clampf _gl-clampf _gl-clampf _gl-clampf ->)
(define-foreign glClearIndex _gl-float ->)
(define-foreign glClearDepth _gl-clampd ->)
(define-foreign glClearStencil _gl-int ->)
(define-foreign glClearAccum _gl-float _gl-float _gl-float _gl-float ->)

;; 4.2.4
(define-foreign glAccum _gl-enum _gl-float ->)

;; 4.3.2
(define-foreign glReadPixels _gl-int _gl-int _gl-sizei _gl-sizei
                             _gl-enum _gl-enum _gl-voidv ->)
(define-foreign glReadBuffer _gl-enum ->)

;; 4.3.3
(define-foreign glCopyPixels _gl-int _gl-int _gl-sizei _gl-sizei _gl-enum ->)

;; 5.1
;; Map1 and 2 appear to have a bug in the spec where the last argument is not
;; declared as a pointer
(define-foreign-tparm glMap1 (f) _gl-enum T T _gl-int _gl-int _gl-floatv ->)
(define-foreign-tparm glMap1 (d) _gl-enum T T _gl-int _gl-int _gl-doublev ->)
(define-foreign-tparm glMap2 (f)
                      _gl-enum T T _gl-int _gl-int T T _gl-int _gl-int
                      _gl-floatv ->)
(define-foreign-tparm glMap2 (d)
                      _gl-enum T T _gl-int _gl-int T T _gl-int _gl-int
                      _gl-doublev ->)
(define-foreign-tparm glEvalCoord1 (f d) T ->)
(define-foreign-tparm glEvalCoord2 (f d) T T ->)
(define-foreign-tparm glEvalCoord1 (fv dv) T ->)
(define-foreign-tparm glEvalCoord2 (fv dv) T ->)
(define-foreign-tparm glMapGrid1 (f d) _gl-int T T ->)
(define-foreign-tparm glMapGrid2 (f d) _gl-int T T _gl-int T T ->)
(define-foreign glEvalMesh1 _gl-enum _gl-int _gl-int ->)
(define-foreign glEvalMesh2 _gl-enum _gl-int _gl-int _gl-int _gl-int ->)
(define-foreign glEvalPoint1 _gl-int ->)
(define-foreign glEvalPoint2 _gl-int _gl-int ->)

;; 5.2
(define-foreign glInitNames ->)
(define-foreign glPopName ->)
(define-foreign glPushName _gl-uint ->)
(define-foreign glLoadName _gl-uint ->)
(define-foreign glRenderMode _gl-enum -> _gl-int)
(define-struct select-buffer-object (ptr len))
(provide select-buffer->gl-uint-vector)
(define (select-buffer->gl-uint-vector sbo)
  (unless (select-buffer-object? sbo)
    (raise-type-error 'select-buffer->gl-uint-vector "select-buffer-object" sbo))
  (let* ([l (select-buffer-object-len sbo)]
         [p (select-buffer-object-ptr sbo)]
         [v (make-gl-uint-vector l)])
    (let loop ([i 0])
      (when (< i l)
        (gl-vector-set! v i (ptr-ref p _gl-uint i))
        (loop (add1 i))))
    v))

(define-foreign glSelectBuffer
  [n : _gl-sizei] [mem : _pointer = (malloc n _gl-uint 'raw)] ->
  _void ->
  (let ([o (make-select-buffer-object mem n)])
    (register-finalizer o (lambda (sbo) (free (select-buffer-object-ptr sbo))))
    o))

;; 5.3
(define-struct feedback-buffer-object (ptr len))
(provide feedback-buffer->gl-float-vector)
(define (feedback-buffer->gl-float-vector fbo)
  (unless (feedback-buffer-object? fbo)
    (raise-type-error 'feedback-buffer->gl-uint-vector
                      "feedback-buffer-object" fbo))
  (let* ([l (feedback-buffer-object-len fbo)]
         [p (feedback-buffer-object-ptr fbo)]
         [v (make-gl-float-vector l)])
    (let loop ([i 0])
      (when (< i l)
        (gl-vector-set! v i (ptr-ref p _gl-float i))
        (loop (add1 i))))
    v))
(define-foreign glFeedbackBuffer
  [n : _gl-sizei] _gl-enum [mem : _pointer = (malloc _gl-float n 'raw)] ->
  _void ->
  (let ([o (make-feedback-buffer-object mem n)])
    (register-finalizer o (lambda (fbo) (free (feedback-buffer-object-ptr fbo))))
    o))
(define-foreign glPassThrough _gl-float ->)

;; 5.4
(define-foreign glNewList _gl-uint _gl-enum ->)
(define-foreign glEndList ->)
(define-foreign glCallList _gl-uint ->)
(define-foreign glCallLists _gl-sizei _gl-enum _gl-voidv ->)
(define-foreign glListBase _gl-uint ->)
(define-foreign glGenLists _gl-sizei -> _gl-uint)
(define-foreign glIsList _gl-uint -> _gl-boolean)
(define-foreign glDeleteLists _gl-uint _gl-sizei ->)

;; 5.5
(define-foreign glFlush ->)
(define-foreign glFinish ->)

;; 5.6
(define-foreign glHint _gl-enum _gl-enum ->)

;; 6.1.1
(define-foreign glGetBooleanv
  _gl-enum [n : _?] [r : (_cvector o _gl-boolean n)] ->
  _void -> r)
(define-foreign glGetIntegerv
  _gl-enum [n : _?] [r : (_cvector o _gl-int n)] -> _void -> r)
(define-foreign glGetFloatv
  _gl-enum [n : _?] [r : (_cvector o _gl-float n)] ->
  _void -> r)
(define-foreign glGetDoublev
  _gl-enum [n : _?] [r : (_cvector o _gl-double n)] ->
  _void -> r)
(define-foreign glIsEnabled _gl-enum -> _gl-boolean)

;; 6.1.3
(define-foreign glGetClipPlane
  _gl-enum [r : (_cvector o _gl-double 4)] -> _void -> r)
(define-foreign-tparm glGetLight (iv fv)
  _gl-enum _gl-enum [n : _?] [r : (T n)] -> _void -> r)
(define-foreign-tparm glGetMaterial (iv fv)
  _gl-enum _gl-enum [n : _?] [r : (T n)] -> _void -> r)
(define-foreign-tparm glGetTexEnv (iv fv)
  _gl-enum _gl-enum [n : _?] [r : (T n)] -> _void -> r)
(define-foreign-tparm glGetTexGen (iv fv dv)
  _gl-enum _gl-enum [n : _?] [r : (T n)] -> _void -> r)
(define-foreign-tparm glGetTexParameter (iv fv)
  _gl-enum _gl-enum [n : _?] [r : (T n)] -> _void -> r)
(define-foreign-tparm glGetTexLevelParameter (iv fv)
                      _gl-enum _gl-int _gl-enum [n : _?] [r : (T n)] ->
                      _void -> r)
(define-foreign-tparm glGetPixelMap (uiv usv fv)
                      _gl-enum [n : _?] [r : (T n)] -> _void -> r)
(define-foreign-tparm glGetMap (iv fv dv)
                      _gl-enum _gl-enum [n : _?] [r : (T n)] -> _void -> r)
(define-foreign-tparm glGetBufferParameter (iv)
                      _gl-enum _gl-enum [n : _?] [r : (T n)] -> _void -> r)


;; 6.1.4
(define-foreign glGetTexImage _gl-enum _gl-int _gl-enum _gl-enum _gl-voidv ->)
(define-foreign glGetCompressedTexImage _gl-enum _gl-int _gl-voidv ->)
(define-foreign glIsTexture _gl-uint -> _gl-boolean)

;; 6.1.5
(define-foreign glGetPolygonStipple _gl-voidv ->)

;; 6.1.7
(define-foreign glGetColorTable _gl-enum _gl-enum _gl-enum _gl-voidv ->)

;; 6.1.8
(define-foreign glGetConvolutionFilter
  _gl-enum _gl-enum _gl-enum _gl-voidv ->)
(define-foreign glGetSeparableFilter
  _gl-enum _gl-enum _gl-enum _gl-voidv _gl-voidv _gl-voidv ->)
(define-foreign-tparm glGetConvolutionParameter (iv fv)
  _gl-enum _gl-enum [n : _?] [r : (T n)] -> _void -> r)

;; 6.1.9
(define-foreign glGetHistogram
  _gl-enum _gl-boolean _gl-enum _gl-enum _gl-voidv ->)
(define-foreign glResetHistogram _gl-enum ->)
(define-foreign-tparm glGetHistogramParameter (iv fv)
  _gl-enum _gl-enum [n : _?] [r : (T n)] -> _void -> r)

;; 6.1.10
(define-foreign glGetMinmax
  _gl-enum _gl-boolean _gl-enum _gl-enum _gl-voidv ->)
(define-foreign glResetMinmax _gl-enum ->)
(define-foreign-tparm glGetMinmaxParameter (iv fv)
  _gl-enum _gl-enum [n : _?] [r : (T n)] -> _void -> r)

;; 6.1.11
#|
(define-foreign glGetPointerv _gl-enum _gl-voidvv ->)
|#
(define-foreign glGetString _gl-enum -> _string)

;; 6.1.12
(define-foreign glIsQuery _gl-uint -> _gl-boolean)
(define-foreign-tparm glGetQuery (iv)
  _gl-enum _gl-enum [n : _?] [r : (T n)] -> _void -> r)
(define-foreign-tparm glGetQueryObject (iv uiv)
  _gl-uint _gl-enum [n : _?] [r : (T n)] -> _void -> r)

;; 6.1.13
(define-foreign glIsBuffer _gl-uint -> _gl-boolean)
#|
(define-foreign glGetBufferSubData
  _gl-enum _gl-intptr _gl-sizeiptr _gl-voidv ->)
(define-foreign glGetBufferPointerv _gl-enum _gl-enum _gl-voidvv ->)
|#

;; 6.1.14
(define-foreign glPushAttrib _gl-bitfield ->)
(define-foreign glPushClientAttrib _gl-bitfield ->)
(define-foreign glPopAttrib ->)
(define-foreign glPopClientAttrib ->)

(define-syntax define-foreignu
  (syntax-rules ()
    [(_ args ...) (define-foreign-lib glu-lib args ...)]))

;; 2
(define-foreignu gluGetString _gl-enum -> _string)
(define-foreignu gluCheckExtension _string _string -> _gl-boolean)

;; 3.1
(define-foreignu gluScaleImage _gl-enum _gl-sizei _gl-sizei _gl-enum _gl-voidv
                               _gl-sizei _gl-sizei _gl-enum _gl-voidv ->)

;; 3.2
(define-foreignu gluBuild1DMipmaps
  _gl-enum _gl-int _gl-sizei _gl-enum _gl-enum _gl-voidv ->)
(define-foreignu gluBuild2DMipmaps
  _gl-enum _gl-int _gl-sizei _gl-sizei _gl-enum _gl-enum
  _gl-voidv ->)
(define-foreignu gluBuild3DMipmaps
  _gl-enum _gl-int _gl-sizei _gl-sizei _gl-sizei
  _gl-enum _gl-enum _gl-voidv ->)
(define-foreignu gluBuild1DMipmapLevels
  _gl-enum _gl-int _gl-sizei _gl-enum _gl-enum
  _gl-int _gl-int _gl-int _gl-voidv ->)
(define-foreignu gluBuild2DMipmapLevels
  _gl-enum _gl-int _gl-sizei _gl-sizei _gl-enum _gl-enum
  _gl-int _gl-int _gl-int _gl-voidv ->)
(define-foreignu gluBuild3DMipmapLevels
  _gl-enum _gl-int _gl-sizei _gl-sizei _gl-sizei
  _gl-enum _gl-enum _gl-int _gl-int _gl-int _gl-voidv ->)

;; 4.1
(define-foreignu gluOrtho2D _gl-double _gl-double _gl-double _gl-double ->)
(define-foreignu gluPerspective
  _gl-double _gl-double _gl-double _gl-double ->)
(define-foreignu gluLookAt _gl-double _gl-double _gl-double
                           _gl-double _gl-double _gl-double
                           _gl-double _gl-double _gl-double ->)
(define-foreignu gluPickMatrix
  _gl-double _gl-double _gl-double _gl-double _gl-intv ->)

;; 4.2
(define-foreignu gluProject _gl-double _gl-double _gl-double
                            _gl-doublev _gl-doublev _gl-intv
                            [r1 : (_ptr o _gl-double)]
                            [r2 : (_ptr o _gl-double)]
                            [r3 : (_ptr o _gl-double)] ->
                            _void -> (gl-double-vector r1 r2 r3))
(define-foreignu gluUnProject _gl-double _gl-double _gl-double
                              _gl-doublev _gl-doublev _gl-intv
                              [r1 : (_ptr o _gl-double)]
                              [r2 : (_ptr o _gl-double)]
                              [r3 : (_ptr o _gl-double)] ->
                              _void -> (gl-double-vector r1 r2 r3))
(define-foreignu gluUnProject4 _gl-double _gl-double _gl-double _gl-double
                               _gl-doublev _gl-doublev _gl-intv
                               _gl-clampd _gl-clampd
                               [r1 : (_ptr o _gl-double)]
                               [r2 : (_ptr o _gl-double)]
                               [r3 : (_ptr o _gl-double)]
                               [r4 : (_ptr o _gl-double)] ->
                               _void -> (gl-double-vector r1 r2 r3 r4))

;; 5.1
#|
(define-foreignu gluNewTess -> _glu-tessalator*)
(define-foreignu gluDeleteTess _glu-tessalator* ->)
|#

;; 5.2
#|
(define-foreignu gluTessBeginPolygon _glu-tessalator* _gl-voidv ->)
(define-foreignu gluTessBeginContour _glu-tessalator* ->)
(define-foreignu gluTessVertex _glu-tessalator* _gl-doublev _gl-voidv ->)
(define-foreignu gluTessEndContour _glu-tessalator* ->)
(define-foreignu gluTessEndPolygon _glu-tessalator* ->)
|#

;; 5.3
#|
(define-foreignu gluTessCallback _glu-tessalator _gl-enum ??? ->)
|#

;; 5.4
#|
(define-foreignu gluTessProperty _glu-tessalator* _gl-enum _gl-double ->)
(define-foreignu gluGetTessProperty _glu-tessalator* _gl-enum _gl-doublev ->)
(define-foreignu gluTessNormal _glu-tessalator* _gl-double _gl-double _gl-double ->)
|#

;; 5.7
#|
(define-foreignu gluBeginPolygon _glu-tessalator* ->)
(define-foreignu gluNextContour _glu-tessalator* _gl-enum ->)
(define-foreignu gluEndPolygon _glu-tessalator* ->)
|#

  ;; 6.1
(define _glu-quadric
  (_cpointer 'quadric _pointer #f
             (lambda (q*) (register-finalizer q* gluDeleteQuadric) q*)))

(define-foreignu gluNewQuadric -> _glu-quadric)

;; Don't use define-foreign, because this shouldn't be provided
(define gluDeleteQuadric
  (with-handlers ([exn:fail:filesystem?
                   (lambda (ex)
                     (lambda x
                       (error 'gluDeleteQuadric
                              "unavailable on this system")))])
    (get-ffi-obj 'gluDeleteQuadric glu-lib (_fun* _glu-quadric -> _void))))

;; 6.2
;;(define-foreignu gluQuadricCallback
;;                 _glu-quadric [_gl-enum = GLU_ERROR] (_fun* _gl-enum -> _void) ->)

;; 6.3
(define-foreignu gluQuadricNormals _glu-quadric _gl-enum ->)
(define-foreignu gluQuadricTexture _glu-quadric _gl-boolean ->)
(define-foreignu gluQuadricOrientation _glu-quadric _gl-enum ->)
(define-foreignu gluQuadricDrawStyle _glu-quadric _gl-enum ->)

;; 6.4
(define-foreignu gluSphere _glu-quadric _gl-double _gl-int _gl-int ->)
(define-foreignu gluCylinder
  _glu-quadric _gl-double _gl-double _gl-double _gl-int _gl-int ->)
(define-foreignu gluDisk _glu-quadric _gl-double _gl-double _gl-int _gl-int ->)
(define-foreignu gluPartialDisk
  _glu-quadric _gl-double _gl-double _gl-int _gl-int _gl-double _gl-double ->)

;; 7.1
(define _glu-nurbs
  (_cpointer 'nurbs _pointer #f
             (lambda (q*) (register-finalizer q* gluDeleteNurbsRenderer) q*)))

(define-foreignu gluNewNurbsRenderer -> _glu-nurbs)
(define-foreignu gluDeleteNurbsRenderer _glu-nurbs ->)


;; 7.2
#|
(define-foreignu gluNurbsCallback _glu-nurbs* _gl-enum ??? ->)
(define-foreignu gluNurbsCallbackData _glu-nurbs* _gl-voidv ->)
|#

;; 7.3
(define-foreignu gluBeginCurve _glu-nurbs ->)
(define-foreignu gluNurbsCurve
  _glu-nurbs _gl-int _gl-floatv _gl-int _gl-floatv _gl-int _gl-enum ->)
(define-foreignu gluEndCurve _glu-nurbs ->)

;; 7.4

(define-foreignu gluBeginSurface _glu-nurbs ->)
(define-foreignu gluNurbsSurface
  _glu-nurbs _gl-int _gl-floatv _gl-int _gl-floatv _gl-int
  _gl-int _gl-floatv _gl-int _gl-int _gl-enum ->)
(define-foreignu gluEndSurface _glu-nurbs ->)


;; 7.5
#|
(define-foreignu gluBeginTrim _glu-nurbs* ->)
(define-foreignu gluPwlCurve _glu-nurbs* _gl-int _gl-floatv _gl-int _gl-enum ->)
(define-foreignu gluEndTrim _glu-nurbs* ->)
|#

;; 7.6
(define-foreignu gluNurbsProperty _glu-nurbs _gl-enum _gl-float ->)
#|
(define-foreignu gluLoadSamplingMatrix _glu-nurbs* _gl-floatv _gl-floatv _gl-intv ->)
(define-foreignu gluGetNurbsProperty _glu-nurbs* _gl-enum _gl-floatv ->)
|#

;; 8
(define-foreignu gluErrorString _gl-enum -> _string)


(define-syntax define-enum
  (syntax-rules ()
    [(_ d v)   (begin (provide d) (define d v))]
    [(_ _ d v) (define-enum d v)]))

;; Enumeration constants for version 1.0 and 1.1
;; These values are all taken from MESA's gl.h.
;; Although the standard doesn't define these, they seem to be used on all
;; platforms.
(define-enum GL_FALSE #x0)
(define-enum GL_TRUE #x1)
(define-enum GL_BYTE #x1400)
(define-enum GL_UNSIGNED_BYTE #x1401)
(define-enum GL_SHORT #x1402)
(define-enum GL_UNSIGNED_SHORT #x1403)
(define-enum GL_INT #x1404)
(define-enum GL_UNSIGNED_INT #x1405)
(define-enum GL_FLOAT #x1406)
(define-enum GL_DOUBLE #x140A)
(define-enum GL_2_BYTES #x1407)
(define-enum GL_3_BYTES #x1408)
(define-enum GL_4_BYTES #x1409)
(define-enum GL_POINTS #x0000)
(define-enum GL_LINES #x0001)
(define-enum GL_LINE_LOOP #x0002)
(define-enum GL_LINE_STRIP #x0003)
(define-enum GL_TRIANGLES #x0004)
(define-enum GL_TRIANGLE_STRIP #x0005)
(define-enum GL_TRIANGLE_FAN #x0006)
(define-enum GL_QUADS #x0007)
(define-enum GL_QUAD_STRIP #x0008)
(define-enum GL_POLYGON #x0009)
(define-enum GL_VERTEX_ARRAY #x8074)
(define-enum GL_NORMAL_ARRAY #x8075)
(define-enum GL_COLOR_ARRAY #x8076)
(define-enum GL_INDEX_ARRAY #x8077)
(define-enum GL_TEXTURE_COORD_ARRAY #x8078)
(define-enum GL_EDGE_FLAG_ARRAY #x8079)
(define-enum GL_VERTEX_ARRAY_SIZE #x807A)
(define-enum GL_VERTEX_ARRAY_TYPE #x807B)
(define-enum GL_VERTEX_ARRAY_STRIDE #x807C)
(define-enum GL_NORMAL_ARRAY_TYPE #x807E)
(define-enum GL_NORMAL_ARRAY_STRIDE #x807F)
(define-enum GL_COLOR_ARRAY_SIZE #x8081)
(define-enum GL_COLOR_ARRAY_TYPE #x8082)
(define-enum GL_COLOR_ARRAY_STRIDE #x8083)
(define-enum GL_INDEX_ARRAY_TYPE #x8085)
(define-enum GL_INDEX_ARRAY_STRIDE #x8086)
(define-enum GL_TEXTURE_COORD_ARRAY_SIZE #x8088)
(define-enum GL_TEXTURE_COORD_ARRAY_TYPE #x8089)
(define-enum GL_TEXTURE_COORD_ARRAY_STRIDE #x808A)
(define-enum GL_EDGE_FLAG_ARRAY_STRIDE #x808C)
(define-enum GL_VERTEX_ARRAY_POINTER #x808E)
(define-enum GL_NORMAL_ARRAY_POINTER #x808F)
(define-enum GL_COLOR_ARRAY_POINTER #x8090)
(define-enum GL_INDEX_ARRAY_POINTER #x8091)
(define-enum GL_TEXTURE_COORD_ARRAY_POINTER #x8092)
(define-enum GL_EDGE_FLAG_ARRAY_POINTER #x8093)
(define-enum GL_V2F #x2A20)
(define-enum GL_V3F #x2A21)
(define-enum GL_C4UB_V2F #x2A22)
(define-enum GL_C4UB_V3F #x2A23)
(define-enum GL_C3F_V3F #x2A24)
(define-enum GL_N3F_V3F #x2A25)
(define-enum GL_C4F_N3F_V3F #x2A26)
(define-enum GL_T2F_V3F #x2A27)
(define-enum GL_T4F_V4F #x2A28)
(define-enum GL_T2F_C4UB_V3F #x2A29)
(define-enum GL_T2F_C3F_V3F #x2A2A)
(define-enum GL_T2F_N3F_V3F #x2A2B)
(define-enum GL_T2F_C4F_N3F_V3F #x2A2C)
(define-enum GL_T4F_C4F_N3F_V4F #x2A2D)
(define-enum GL_MATRIX_MODE #x0BA0)
(define-enum GL_MODELVIEW #x1700)
(define-enum GL_PROJECTION #x1701)
(define-enum GL_TEXTURE #x1702)
(define-enum GL_POINT_SMOOTH #x0B10)
(define-enum GL_POINT_SIZE #x0B11)
(define-enum GL_POINT_SIZE_GRANULARITY #x0B13)
(define-enum GL_POINT_SIZE_RANGE #x0B12)
(define-enum GL_LINE_SMOOTH #x0B20)
(define-enum GL_LINE_STIPPLE #x0B24)
(define-enum GL_LINE_STIPPLE_PATTERN #x0B25)
(define-enum GL_LINE_STIPPLE_REPEAT #x0B26)
(define-enum GL_LINE_WIDTH #x0B21)
(define-enum GL_LINE_WIDTH_GRANULARITY #x0B23)
(define-enum GL_LINE_WIDTH_RANGE #x0B22)
(define-enum GL_POINT #x1B00)
(define-enum GL_LINE #x1B01)
(define-enum GL_FILL #x1B02)
(define-enum GL_CW #x0900)
(define-enum GL_CCW #x0901)
(define-enum GL_FRONT #x0404)
(define-enum GL_BACK #x0405)
(define-enum GL_POLYGON_MODE #x0B40)
(define-enum GL_POLYGON_SMOOTH #x0B41)
(define-enum GL_POLYGON_STIPPLE #x0B42)
(define-enum GL_EDGE_FLAG #x0B43)
(define-enum GL_CULL_FACE #x0B44)
(define-enum GL_CULL_FACE_MODE #x0B45)
(define-enum GL_FRONT_FACE #x0B46)
(define-enum GL_POLYGON_OFFSET_FACTOR #x8038)
(define-enum GL_POLYGON_OFFSET_UNITS #x2A00)
(define-enum GL_POLYGON_OFFSET_POINT #x2A01)
(define-enum GL_POLYGON_OFFSET_LINE #x2A02)
(define-enum GL_POLYGON_OFFSET_FILL #x8037)
(define-enum GL_COMPILE #x1300)
(define-enum GL_COMPILE_AND_EXECUTE #x1301)
(define-enum GL_LIST_BASE #x0B32)
(define-enum GL_LIST_INDEX #x0B33)
(define-enum GL_LIST_MODE #x0B30)
(define-enum GL_NEVER #x0200)
(define-enum GL_LESS #x0201)
(define-enum GL_EQUAL #x0202)
(define-enum GL_LEQUAL #x0203)
(define-enum GL_GREATER #x0204)
(define-enum GL_NOTEQUAL #x0205)
(define-enum GL_GEQUAL #x0206)
(define-enum GL_ALWAYS #x0207)
(define-enum GL_DEPTH_TEST #x0B71)
(define-enum GL_DEPTH_BITS #x0D56)
(define-enum GL_DEPTH_CLEAR_VALUE #x0B73)
(define-enum GL_DEPTH_FUNC #x0B74)
(define-enum GL_DEPTH_RANGE #x0B70)
(define-enum GL_DEPTH_WRITEMASK #x0B72)
(define-enum GL_DEPTH_COMPONENT #x1902)
(define-enum GL_LIGHTING #x0B50)
(define-enum GL_LIGHT0 #x4000)
(define-enum GL_LIGHT1 #x4001)
(define-enum GL_LIGHT2 #x4002)
(define-enum GL_LIGHT3 #x4003)
(define-enum GL_LIGHT4 #x4004)
(define-enum GL_LIGHT5 #x4005)
(define-enum GL_LIGHT6 #x4006)
(define-enum GL_LIGHT7 #x4007)
(define-enum GL_SPOT_EXPONENT #x1205)
(define-enum GL_SPOT_CUTOFF #x1206)
(define-enum GL_CONSTANT_ATTENUATION #x1207)
(define-enum GL_LINEAR_ATTENUATION #x1208)
(define-enum GL_QUADRATIC_ATTENUATION #x1209)
(define-enum GL_AMBIENT #x1200)
(define-enum GL_DIFFUSE #x1201)
(define-enum GL_SPECULAR #x1202)
(define-enum GL_SHININESS #x1601)
(define-enum GL_EMISSION #x1600)
(define-enum GL_POSITION #x1203)
(define-enum GL_SPOT_DIRECTION #x1204)
(define-enum GL_AMBIENT_AND_DIFFUSE #x1602)
(define-enum GL_COLOR_INDEXES #x1603)
(define-enum GL_LIGHT_MODEL_TWO_SIDE #x0B52)
(define-enum GL_LIGHT_MODEL_LOCAL_VIEWER #x0B51)
(define-enum GL_LIGHT_MODEL_AMBIENT #x0B53)
(define-enum GL_FRONT_AND_BACK #x0408)
(define-enum GL_SHADE_MODEL #x0B54)
(define-enum GL_FLAT #x1D00)
(define-enum GL_SMOOTH #x1D01)
(define-enum GL_COLOR_MATERIAL #x0B57)
(define-enum GL_COLOR_MATERIAL_FACE #x0B55)
(define-enum GL_COLOR_MATERIAL_PARAMETER #x0B56)
(define-enum GL_NORMALIZE #x0BA1)
(define-enum GL_CLIP_PLANE0 #x3000)
(define-enum GL_CLIP_PLANE1 #x3001)
(define-enum GL_CLIP_PLANE2 #x3002)
(define-enum GL_CLIP_PLANE3 #x3003)
(define-enum GL_CLIP_PLANE4 #x3004)
(define-enum GL_CLIP_PLANE5 #x3005)
(define-enum GL_ACCUM_RED_BITS #x0D58)
(define-enum GL_ACCUM_GREEN_BITS #x0D59)
(define-enum GL_ACCUM_BLUE_BITS #x0D5A)
(define-enum GL_ACCUM_ALPHA_BITS #x0D5B)
(define-enum GL_ACCUM_CLEAR_VALUE #x0B80)
(define-enum GL_ACCUM #x0100)
(define-enum GL_ADD #x0104)
(define-enum GL_LOAD #x0101)
(define-enum GL_MULT #x0103)
(define-enum GL_RETURN #x0102)
(define-enum GL_ALPHA_TEST #x0BC0)
(define-enum GL_ALPHA_TEST_REF #x0BC2)
(define-enum GL_ALPHA_TEST_FUNC #x0BC1)
(define-enum GL_BLEND #x0BE2)
(define-enum GL_BLEND_SRC #x0BE1)
(define-enum GL_BLEND_DST #x0BE0)
(define-enum GL_ZERO #x0)
(define-enum GL_ONE #x1)
(define-enum GL_SRC_COLOR #x0300)
(define-enum GL_ONE_MINUS_SRC_COLOR #x0301)
(define-enum GL_SRC_ALPHA #x0302)
(define-enum GL_ONE_MINUS_SRC_ALPHA #x0303)
(define-enum GL_DST_ALPHA #x0304)
(define-enum GL_ONE_MINUS_DST_ALPHA #x0305)
(define-enum GL_DST_COLOR #x0306)
(define-enum GL_ONE_MINUS_DST_COLOR #x0307)
(define-enum GL_SRC_ALPHA_SATURATE #x0308)
(define-enum GL_FEEDBACK #x1C01)
(define-enum GL_RENDER #x1C00)
(define-enum GL_SELECT #x1C02)
(define-enum GL_2D #x0600)
(define-enum GL_3D #x0601)
(define-enum GL_3D_COLOR #x0602)
(define-enum GL_3D_COLOR_TEXTURE #x0603)
(define-enum GL_4D_COLOR_TEXTURE #x0604)
(define-enum GL_POINT_TOKEN #x0701)
(define-enum GL_LINE_TOKEN #x0702)
(define-enum GL_LINE_RESET_TOKEN #x0707)
(define-enum GL_POLYGON_TOKEN #x0703)
(define-enum GL_BITMAP_TOKEN #x0704)
(define-enum GL_DRAW_PIXEL_TOKEN #x0705)
(define-enum GL_COPY_PIXEL_TOKEN #x0706)
(define-enum GL_PASS_THROUGH_TOKEN #x0700)
(define-enum GL_FEEDBACK_BUFFER_POINTER #x0DF0)
(define-enum GL_FEEDBACK_BUFFER_SIZE #x0DF1)
(define-enum GL_FEEDBACK_BUFFER_TYPE #x0DF2)
(define-enum GL_SELECTION_BUFFER_POINTER #x0DF3)
(define-enum GL_SELECTION_BUFFER_SIZE #x0DF4)
(define-enum GL_FOG #x0B60)
(define-enum GL_FOG_MODE #x0B65)
(define-enum GL_FOG_DENSITY #x0B62)
(define-enum GL_FOG_COLOR #x0B66)
(define-enum GL_FOG_INDEX #x0B61)
(define-enum GL_FOG_START #x0B63)
(define-enum GL_FOG_END #x0B64)
(define-enum GL_LINEAR #x2601)
(define-enum GL_EXP #x0800)
(define-enum GL_EXP2 #x0801)
(define-enum GL_LOGIC_OP #x0BF1)
(define-enum GL_INDEX_LOGIC_OP #x0BF1)
(define-enum GL_COLOR_LOGIC_OP #x0BF2)
(define-enum GL_LOGIC_OP_MODE #x0BF0)
(define-enum GL_CLEAR #x1500)
(define-enum GL_SET #x150F)
(define-enum GL_COPY #x1503)
(define-enum GL_COPY_INVERTED #x150C)
(define-enum GL_NOOP #x1505)
(define-enum GL_INVERT #x150A)
(define-enum GL_AND #x1501)
(define-enum GL_NAND #x150E)
(define-enum GL_OR #x1507)
(define-enum GL_NOR #x1508)
(define-enum GL_XOR #x1506)
(define-enum GL_EQUIV #x1509)
(define-enum GL_AND_REVERSE #x1502)
(define-enum GL_AND_INVERTED #x1504)
(define-enum GL_OR_REVERSE #x150B)
(define-enum GL_OR_INVERTED #x150D)
(define-enum GL_STENCIL_TEST #x0B90)
(define-enum GL_STENCIL_WRITEMASK #x0B98)
(define-enum GL_STENCIL_BITS #x0D57)
(define-enum GL_STENCIL_FUNC #x0B92)
(define-enum GL_STENCIL_VALUE_MASK #x0B93)
(define-enum GL_STENCIL_REF #x0B97)
(define-enum GL_STENCIL_FAIL #x0B94)
(define-enum GL_STENCIL_PASS_DEPTH_PASS #x0B96)
(define-enum GL_STENCIL_PASS_DEPTH_FAIL #x0B95)
(define-enum GL_STENCIL_CLEAR_VALUE #x0B91)
(define-enum GL_STENCIL_INDEX #x1901)
(define-enum GL_KEEP #x1E00)
(define-enum GL_REPLACE #x1E01)
(define-enum GL_INCR #x1E02)
(define-enum GL_DECR #x1E03)
(define-enum GL_NONE #x0)
(define-enum GL_LEFT #x0406)
(define-enum GL_RIGHT #x0407)
(define-enum GL_FRONT_LEFT #x0400)
(define-enum GL_FRONT_RIGHT #x0401)
(define-enum GL_BACK_LEFT #x0402)
(define-enum GL_BACK_RIGHT #x0403)
(define-enum GL_AUX0 #x0409)
(define-enum GL_AUX1 #x040A)
(define-enum GL_AUX2 #x040B)
(define-enum GL_AUX3 #x040C)
(define-enum GL_COLOR_INDEX #x1900)
(define-enum GL_RED #x1903)
(define-enum GL_GREEN #x1904)
(define-enum GL_BLUE #x1905)
(define-enum GL_ALPHA #x1906)
(define-enum GL_LUMINANCE #x1909)
(define-enum GL_LUMINANCE_ALPHA #x190A)
(define-enum GL_ALPHA_BITS #x0D55)
(define-enum GL_RED_BITS #x0D52)
(define-enum GL_GREEN_BITS #x0D53)
(define-enum GL_BLUE_BITS #x0D54)
(define-enum GL_INDEX_BITS #x0D51)
(define-enum GL_SUBPIXEL_BITS #x0D50)
(define-enum GL_AUX_BUFFERS #x0C00)
(define-enum GL_READ_BUFFER #x0C02)
(define-enum GL_DRAW_BUFFER #x0C01)
(define-enum GL_DOUBLEBUFFER #x0C32)
(define-enum GL_STEREO #x0C33)
(define-enum GL_BITMAP #x1A00)
(define-enum GL_COLOR #x1800)
(define-enum GL_DEPTH #x1801)
(define-enum GL_STENCIL #x1802)
(define-enum GL_DITHER #x0BD0)
(define-enum GL_RGB #x1907)
(define-enum GL_RGBA #x1908)
(define-enum GL_MAX_LIST_NESTING #x0B31)
(define-enum GL_MAX_ATTRIB_STACK_DEPTH #x0D35)
(define-enum GL_MAX_MODELVIEW_STACK_DEPTH #x0D36)
(define-enum GL_MAX_NAME_STACK_DEPTH #x0D37)
(define-enum GL_MAX_PROJECTION_STACK_DEPTH #x0D38)
(define-enum GL_MAX_TEXTURE_STACK_DEPTH #x0D39)
(define-enum GL_MAX_EVAL_ORDER #x0D30)
(define-enum GL_MAX_LIGHTS #x0D31)
(define-enum GL_MAX_CLIP_PLANES #x0D32)
(define-enum GL_MAX_TEXTURE_SIZE #x0D33)
(define-enum GL_MAX_PIXEL_MAP_TABLE #x0D34)
(define-enum GL_MAX_VIEWPORT_DIMS #x0D3A)
(define-enum GL_MAX_CLIENT_ATTRIB_STACK_DEPTH #x0D3B)
(define-enum GL_ATTRIB_STACK_DEPTH #x0BB0)
(define-enum GL_CLIENT_ATTRIB_STACK_DEPTH #x0BB1)
(define-enum GL_COLOR_CLEAR_VALUE #x0C22)
(define-enum GL_COLOR_WRITEMASK #x0C23)
(define-enum GL_CURRENT_INDEX #x0B01)
(define-enum GL_CURRENT_COLOR #x0B00)
(define-enum GL_CURRENT_NORMAL #x0B02)
(define-enum GL_CURRENT_RASTER_COLOR #x0B04)
(define-enum GL_CURRENT_RASTER_DISTANCE #x0B09)
(define-enum GL_CURRENT_RASTER_INDEX #x0B05)
(define-enum GL_CURRENT_RASTER_POSITION #x0B07)
(define-enum GL_CURRENT_RASTER_TEXTURE_COORDS #x0B06)
(define-enum GL_CURRENT_RASTER_POSITION_VALID #x0B08)
(define-enum GL_CURRENT_TEXTURE_COORDS #x0B03)
(define-enum GL_INDEX_CLEAR_VALUE #x0C20)
(define-enum GL_INDEX_MODE #x0C30)
(define-enum GL_INDEX_WRITEMASK #x0C21)
(define-enum GL_MODELVIEW_MATRIX #x0BA6)
(define-enum GL_MODELVIEW_STACK_DEPTH #x0BA3)
(define-enum GL_NAME_STACK_DEPTH #x0D70)
(define-enum GL_PROJECTION_MATRIX #x0BA7)
(define-enum GL_PROJECTION_STACK_DEPTH #x0BA4)
(define-enum GL_RENDER_MODE #x0C40)
(define-enum GL_RGBA_MODE #x0C31)
(define-enum GL_TEXTURE_MATRIX #x0BA8)
(define-enum GL_TEXTURE_STACK_DEPTH #x0BA5)
(define-enum GL_VIEWPORT #x0BA2)
(define-enum GL_AUTO_NORMAL #x0D80)
(define-enum GL_MAP1_COLOR_4 #x0D90)
(define-enum GL_MAP1_GRID_DOMAIN #x0DD0)
(define-enum GL_MAP1_GRID_SEGMENTS #x0DD1)
(define-enum GL_MAP1_INDEX #x0D91)
(define-enum GL_MAP1_NORMAL #x0D92)
(define-enum GL_MAP1_TEXTURE_COORD_1 #x0D93)
(define-enum GL_MAP1_TEXTURE_COORD_2 #x0D94)
(define-enum GL_MAP1_TEXTURE_COORD_3 #x0D95)
(define-enum GL_MAP1_TEXTURE_COORD_4 #x0D96)
(define-enum GL_MAP1_VERTEX_3 #x0D97)
(define-enum GL_MAP1_VERTEX_4 #x0D98)
(define-enum GL_MAP2_COLOR_4 #x0DB0)
(define-enum GL_MAP2_GRID_DOMAIN #x0DD2)
(define-enum GL_MAP2_GRID_SEGMENTS #x0DD3)
(define-enum GL_MAP2_INDEX #x0DB1)
(define-enum GL_MAP2_NORMAL #x0DB2)
(define-enum GL_MAP2_TEXTURE_COORD_1 #x0DB3)
(define-enum GL_MAP2_TEXTURE_COORD_2 #x0DB4)
(define-enum GL_MAP2_TEXTURE_COORD_3 #x0DB5)
(define-enum GL_MAP2_TEXTURE_COORD_4 #x0DB6)
(define-enum GL_MAP2_VERTEX_3 #x0DB7)
(define-enum GL_MAP2_VERTEX_4 #x0DB8)
(define-enum GL_COEFF #x0A00)
(define-enum GL_DOMAIN #x0A02)
(define-enum GL_ORDER #x0A01)
(define-enum GL_FOG_HINT #x0C54)
(define-enum GL_LINE_SMOOTH_HINT #x0C52)
(define-enum GL_PERSPECTIVE_CORRECTION_HINT #x0C50)
(define-enum GL_POINT_SMOOTH_HINT #x0C51)
(define-enum GL_POLYGON_SMOOTH_HINT #x0C53)
(define-enum GL_DONT_CARE #x1100)
(define-enum GL_FASTEST #x1101)
(define-enum GL_NICEST #x1102)
(define-enum GL_SCISSOR_TEST #x0C11)
(define-enum GL_SCISSOR_BOX #x0C10)
(define-enum GL_MAP_COLOR #x0D10)
(define-enum GL_MAP_STENCIL #x0D11)
(define-enum GL_INDEX_SHIFT #x0D12)
(define-enum GL_INDEX_OFFSET #x0D13)
(define-enum GL_RED_SCALE #x0D14)
(define-enum GL_RED_BIAS #x0D15)
(define-enum GL_GREEN_SCALE #x0D18)
(define-enum GL_GREEN_BIAS #x0D19)
(define-enum GL_BLUE_SCALE #x0D1A)
(define-enum GL_BLUE_BIAS #x0D1B)
(define-enum GL_ALPHA_SCALE #x0D1C)
(define-enum GL_ALPHA_BIAS #x0D1D)
(define-enum GL_DEPTH_SCALE #x0D1E)
(define-enum GL_DEPTH_BIAS #x0D1F)
(define-enum GL_PIXEL_MAP_S_TO_S_SIZE #x0CB1)
(define-enum GL_PIXEL_MAP_I_TO_I_SIZE #x0CB0)
(define-enum GL_PIXEL_MAP_I_TO_R_SIZE #x0CB2)
(define-enum GL_PIXEL_MAP_I_TO_G_SIZE #x0CB3)
(define-enum GL_PIXEL_MAP_I_TO_B_SIZE #x0CB4)
(define-enum GL_PIXEL_MAP_I_TO_A_SIZE #x0CB5)
(define-enum GL_PIXEL_MAP_R_TO_R_SIZE #x0CB6)
(define-enum GL_PIXEL_MAP_G_TO_G_SIZE #x0CB7)
(define-enum GL_PIXEL_MAP_B_TO_B_SIZE #x0CB8)
(define-enum GL_PIXEL_MAP_A_TO_A_SIZE #x0CB9)
(define-enum GL_PIXEL_MAP_S_TO_S #x0C71)
(define-enum GL_PIXEL_MAP_I_TO_I #x0C70)
(define-enum GL_PIXEL_MAP_I_TO_R #x0C72)
(define-enum GL_PIXEL_MAP_I_TO_G #x0C73)
(define-enum GL_PIXEL_MAP_I_TO_B #x0C74)
(define-enum GL_PIXEL_MAP_I_TO_A #x0C75)
(define-enum GL_PIXEL_MAP_R_TO_R #x0C76)
(define-enum GL_PIXEL_MAP_G_TO_G #x0C77)
(define-enum GL_PIXEL_MAP_B_TO_B #x0C78)
(define-enum GL_PIXEL_MAP_A_TO_A #x0C79)
(define-enum GL_PACK_ALIGNMENT #x0D05)
(define-enum GL_PACK_LSB_FIRST #x0D01)
(define-enum GL_PACK_ROW_LENGTH #x0D02)
(define-enum GL_PACK_SKIP_PIXELS #x0D04)
(define-enum GL_PACK_SKIP_ROWS #x0D03)
(define-enum GL_PACK_SWAP_BYTES #x0D00)
(define-enum GL_UNPACK_ALIGNMENT #x0CF5)
(define-enum GL_UNPACK_LSB_FIRST #x0CF1)
(define-enum GL_UNPACK_ROW_LENGTH #x0CF2)
(define-enum GL_UNPACK_SKIP_PIXELS #x0CF4)
(define-enum GL_UNPACK_SKIP_ROWS #x0CF3)
(define-enum GL_UNPACK_SWAP_BYTES #x0CF0)
(define-enum GL_ZOOM_X #x0D16)
(define-enum GL_ZOOM_Y #x0D17)
(define-enum GL_TEXTURE_ENV #x2300)
(define-enum GL_TEXTURE_ENV_MODE #x2200)
(define-enum GL_TEXTURE_1D #x0DE0)
(define-enum GL_TEXTURE_2D #x0DE1)
(define-enum GL_TEXTURE_WRAP_S #x2802)
(define-enum GL_TEXTURE_WRAP_T #x2803)
(define-enum GL_TEXTURE_MAG_FILTER #x2800)
(define-enum GL_TEXTURE_MIN_FILTER #x2801)
(define-enum GL_TEXTURE_ENV_COLOR #x2201)
(define-enum GL_TEXTURE_GEN_S #x0C60)
(define-enum GL_TEXTURE_GEN_T #x0C61)
(define-enum GL_TEXTURE_GEN_MODE #x2500)
(define-enum GL_TEXTURE_BORDER_COLOR #x1004)
(define-enum GL_TEXTURE_WIDTH #x1000)
(define-enum GL_TEXTURE_HEIGHT #x1001)
(define-enum GL_TEXTURE_BORDER #x1005)
(define-enum GL_TEXTURE_COMPONENTS #x1003)
(define-enum GL_TEXTURE_RED_SIZE #x805C)
(define-enum GL_TEXTURE_GREEN_SIZE #x805D)
(define-enum GL_TEXTURE_BLUE_SIZE #x805E)
(define-enum GL_TEXTURE_ALPHA_SIZE #x805F)
(define-enum GL_TEXTURE_LUMINANCE_SIZE #x8060)
(define-enum GL_TEXTURE_INTENSITY_SIZE #x8061)
(define-enum GL_NEAREST_MIPMAP_NEAREST #x2700)
(define-enum GL_NEAREST_MIPMAP_LINEAR #x2702)
(define-enum GL_LINEAR_MIPMAP_NEAREST #x2701)
(define-enum GL_LINEAR_MIPMAP_LINEAR #x2703)
(define-enum GL_OBJECT_LINEAR #x2401)
(define-enum GL_OBJECT_PLANE #x2501)
(define-enum GL_EYE_LINEAR #x2400)
(define-enum GL_EYE_PLANE #x2502)
(define-enum GL_SPHERE_MAP #x2402)
(define-enum GL_DECAL #x2101)
(define-enum GL_MODULATE #x2100)
(define-enum GL_NEAREST #x2600)
(define-enum GL_REPEAT #x2901)
(define-enum GL_CLAMP #x2900)
(define-enum GL_S #x2000)
(define-enum GL_T #x2001)
(define-enum GL_R #x2002)
(define-enum GL_Q #x2003)
(define-enum GL_TEXTURE_GEN_R #x0C62)
(define-enum GL_TEXTURE_GEN_Q #x0C63)
(define-enum GL_VENDOR #x1F00)
(define-enum GL_RENDERER #x1F01)
(define-enum GL_VERSION #x1F02)
(define-enum GL_EXTENSIONS #x1F03)
(define-enum GL_NO_ERROR #x0)
(define-enum GL_INVALID_VALUE #x0501)
(define-enum GL_INVALID_ENUM #x0500)
(define-enum GL_INVALID_OPERATION #x0502)
(define-enum GL_STACK_OVERFLOW #x0503)
(define-enum GL_STACK_UNDERFLOW #x0504)
(define-enum GL_OUT_OF_MEMORY #x0505)
(define-enum GL_CURRENT_BIT #x00000001)
(define-enum GL_POINT_BIT #x00000002)
(define-enum GL_LINE_BIT #x00000004)
(define-enum GL_POLYGON_BIT #x00000008)
(define-enum GL_POLYGON_STIPPLE_BIT #x00000010)
(define-enum GL_PIXEL_MODE_BIT #x00000020)
(define-enum GL_LIGHTING_BIT #x00000040)
(define-enum GL_FOG_BIT #x00000080)
(define-enum GL_DEPTH_BUFFER_BIT #x00000100)
(define-enum GL_ACCUM_BUFFER_BIT #x00000200)
(define-enum GL_STENCIL_BUFFER_BIT #x00000400)
(define-enum GL_VIEWPORT_BIT #x00000800)
(define-enum GL_TRANSFORM_BIT #x00001000)
(define-enum GL_ENABLE_BIT #x00002000)
(define-enum GL_COLOR_BUFFER_BIT #x00004000)
(define-enum GL_HINT_BIT #x00008000)
(define-enum GL_EVAL_BIT #x00010000)
(define-enum GL_LIST_BIT #x00020000)
(define-enum GL_TEXTURE_BIT #x00040000)
(define-enum GL_SCISSOR_BIT #x00080000)
(define-enum GL_ALL_ATTRIB_BITS #x000FFFFF)
(define-enum GL_PROXY_TEXTURE_1D #x8063)
(define-enum GL_PROXY_TEXTURE_2D #x8064)
(define-enum GL_TEXTURE_PRIORITY #x8066)
(define-enum GL_TEXTURE_RESIDENT #x8067)
(define-enum GL_TEXTURE_BINDING_1D #x8068)
(define-enum GL_TEXTURE_BINDING_2D #x8069)
(define-enum GL_TEXTURE_INTERNAL_FORMAT #x1003)
(define-enum GL_ALPHA4 #x803B)
(define-enum GL_ALPHA8 #x803C)
(define-enum GL_ALPHA12 #x803D)
(define-enum GL_ALPHA16 #x803E)
(define-enum GL_LUMINANCE4 #x803F)
(define-enum GL_LUMINANCE8 #x8040)
(define-enum GL_LUMINANCE12 #x8041)
(define-enum GL_LUMINANCE16 #x8042)
(define-enum GL_LUMINANCE4_ALPHA4 #x8043)
(define-enum GL_LUMINANCE6_ALPHA2 #x8044)
(define-enum GL_LUMINANCE8_ALPHA8 #x8045)
(define-enum GL_LUMINANCE12_ALPHA4 #x8046)
(define-enum GL_LUMINANCE12_ALPHA12 #x8047)
(define-enum GL_LUMINANCE16_ALPHA16 #x8048)
(define-enum GL_INTENSITY #x8049)
(define-enum GL_INTENSITY4 #x804A)
(define-enum GL_INTENSITY8 #x804B)
(define-enum GL_INTENSITY12 #x804C)
(define-enum GL_INTENSITY16 #x804D)
(define-enum GL_R3_G3_B2 #x2A10)
(define-enum GL_RGB4 #x804F)
(define-enum GL_RGB5 #x8050)
(define-enum GL_RGB8 #x8051)
(define-enum GL_RGB10 #x8052)
(define-enum GL_RGB12 #x8053)
(define-enum GL_RGB16 #x8054)
(define-enum GL_RGBA2 #x8055)
(define-enum GL_RGBA4 #x8056)
(define-enum GL_RGB5_A1 #x8057)
(define-enum GL_RGBA8 #x8058)
(define-enum GL_RGB10_A2 #x8059)
(define-enum GL_RGBA12 #x805A)
(define-enum GL_RGBA16 #x805B)
(define-enum GL_CLIENT_PIXEL_STORE_BIT #x00000001)
(define-enum GL_CLIENT_VERTEX_ARRAY_BIT #x00000002)
(define-enum GL_ALL_CLIENT_ATTRIB_BITS #xFFFFFFFF)
(define-enum GL_CLIENT_ALL_ATTRIB_BITS #xFFFFFFFF)

;; These values are taken from SGI's glext.h.
(define-enum 12 GL_UNSIGNED_BYTE_3_3_2 #x8032)
(define-enum 12 GL_UNSIGNED_SHORT_4_4_4_4 #x8033)
(define-enum 12 GL_UNSIGNED_SHORT_5_5_5_1 #x8034)
(define-enum 12 GL_UNSIGNED_INT_8_8_8_8 #x8035)
(define-enum 12 GL_UNSIGNED_INT_10_10_10_2 #x8036)
(define-enum 12 GL_RESCALE_NORMAL #x803A)
(define-enum 12 GL_TEXTURE_BINDING_3D #x806A)
(define-enum 12 GL_PACK_SKIP_IMAGES #x806B)
(define-enum 12 GL_PACK_IMAGE_HEIGHT #x806C)
(define-enum 12 GL_UNPACK_SKIP_IMAGES #x806D)
(define-enum 12 GL_UNPACK_IMAGE_HEIGHT #x806E)
(define-enum 12 GL_TEXTURE_3D #x806F)
(define-enum 12 GL_PROXY_TEXTURE_3D #x8070)
(define-enum 12 GL_TEXTURE_DEPTH #x8071)
(define-enum 12 GL_TEXTURE_WRAP_R #x8072)
(define-enum 12 GL_MAX_3D_TEXTURE_SIZE #x8073)
(define-enum 12 GL_UNSIGNED_BYTE_2_3_3_REV #x8362)
(define-enum 12 GL_UNSIGNED_SHORT_5_6_5 #x8363)
(define-enum 12 GL_UNSIGNED_SHORT_5_6_5_REV #x8364)
(define-enum 12 GL_UNSIGNED_SHORT_4_4_4_4_REV #x8365)
(define-enum 12 GL_UNSIGNED_SHORT_1_5_5_5_REV #x8366)
(define-enum 12 GL_UNSIGNED_INT_8_8_8_8_REV #x8367)
(define-enum 12 GL_UNSIGNED_INT_2_10_10_10_REV #x8368)
(define-enum 12 GL_BGR #x80E0)
(define-enum 12 GL_BGRA #x80E1)
(define-enum 12 GL_MAX_ELEMENTS_VERTICES #x80E8)
(define-enum 12 GL_MAX_ELEMENTS_INDICES #x80E9)
(define-enum 12 GL_CLAMP_TO_EDGE #x812F)
(define-enum 12 GL_TEXTURE_MIN_LOD #x813A)
(define-enum 12 GL_TEXTURE_MAX_LOD #x813B)
(define-enum 12 GL_TEXTURE_BASE_LEVEL #x813C)
(define-enum 12 GL_TEXTURE_MAX_LEVEL #x813D)
(define-enum 12 GL_LIGHT_MODEL_COLOR_CONTROL #x81F8)
(define-enum 12 GL_SINGLE_COLOR #x81F9)
(define-enum 12 GL_SEPARATE_SPECULAR_COLOR #x81FA)
(define-enum 12 GL_SMOOTH_POINT_SIZE_RANGE #x0B12)
(define-enum 12 GL_SMOOTH_POINT_SIZE_GRANULARITY #x0B13)
(define-enum 12 GL_SMOOTH_LINE_WIDTH_RANGE #x0B22)
(define-enum 12 GL_SMOOTH_LINE_WIDTH_GRANULARITY #x0B23)
(define-enum 12 GL_ALIASED_POINT_SIZE_RANGE #x846D)
(define-enum 12 GL_ALIASED_LINE_WIDTH_RANGE #x846E)

(define-enum 'img GL_CONSTANT_COLOR #x8001)
(define-enum 'img GL_ONE_MINUS_CONSTANT_COLOR #x8002)
(define-enum 'img GL_CONSTANT_ALPHA #x8003)
(define-enum 'img GL_ONE_MINUS_CONSTANT_ALPHA #x8004)
(define-enum 'img GL_BLEND_COLOR #x8005)
(define-enum 'img GL_FUNC_ADD #x8006)
(define-enum 'img GL_MIN #x8007)
(define-enum 'img GL_MAX #x8008)
(define-enum 'img GL_BLEND_EQUATION #x8009)
(define-enum 'img GL_FUNC_SUBTRACT #x800A)
(define-enum 'img GL_FUNC_REVERSE_SUBTRACT #x800B)
(define-enum 'img GL_CONVOLUTION_1D #x8010)
(define-enum 'img GL_CONVOLUTION_2D #x8011)
(define-enum 'img GL_SEPARABLE_2D #x8012)
(define-enum 'img GL_CONVOLUTION_BORDER_MODE #x8013)
(define-enum 'img GL_CONVOLUTION_FILTER_SCALE #x8014)
(define-enum 'img GL_CONVOLUTION_FILTER_BIAS #x8015)
(define-enum 'img GL_REDUCE #x8016)
(define-enum 'img GL_CONVOLUTION_FORMAT #x8017)
(define-enum 'img GL_CONVOLUTION_WIDTH #x8018)
(define-enum 'img GL_CONVOLUTION_HEIGHT #x8019)
(define-enum 'img GL_MAX_CONVOLUTION_WIDTH #x801A)
(define-enum 'img GL_MAX_CONVOLUTION_HEIGHT #x801B)
(define-enum 'img GL_POST_CONVOLUTION_RED_SCALE #x801C)
(define-enum 'img GL_POST_CONVOLUTION_GREEN_SCALE #x801D)
(define-enum 'img GL_POST_CONVOLUTION_BLUE_SCALE #x801E)
(define-enum 'img GL_POST_CONVOLUTION_ALPHA_SCALE #x801F)
(define-enum 'img GL_POST_CONVOLUTION_RED_BIAS #x8020)
(define-enum 'img GL_POST_CONVOLUTION_GREEN_BIAS #x8021)
(define-enum 'img GL_POST_CONVOLUTION_BLUE_BIAS #x8022)
(define-enum 'img GL_POST_CONVOLUTION_ALPHA_BIAS #x8023)
(define-enum 'img GL_HISTOGRAM #x8024)
(define-enum 'img GL_PROXY_HISTOGRAM #x8025)
(define-enum 'img GL_HISTOGRAM_WIDTH #x8026)
(define-enum 'img GL_HISTOGRAM_FORMAT #x8027)
(define-enum 'img GL_HISTOGRAM_RED_SIZE #x8028)
(define-enum 'img GL_HISTOGRAM_GREEN_SIZE #x8029)
(define-enum 'img GL_HISTOGRAM_BLUE_SIZE #x802A)
(define-enum 'img GL_HISTOGRAM_ALPHA_SIZE #x802B)
(define-enum 'img GL_HISTOGRAM_LUMINANCE_SIZE #x802C)
(define-enum 'img GL_HISTOGRAM_SINK #x802D)
(define-enum 'img GL_MINMAX #x802E)
(define-enum 'img GL_MINMAX_FORMAT #x802F)
(define-enum 'img GL_MINMAX_SINK #x8030)
(define-enum 'img GL_TABLE_TOO_LARGE #x8031)
(define-enum 'img GL_COLOR_MATRIX #x80B1)
(define-enum 'img GL_COLOR_MATRIX_STACK_DEPTH #x80B2)
(define-enum 'img GL_MAX_COLOR_MATRIX_STACK_DEPTH #x80B3)
(define-enum 'img GL_POST_COLOR_MATRIX_RED_SCALE #x80B4)
(define-enum 'img GL_POST_COLOR_MATRIX_GREEN_SCALE #x80B5)
(define-enum 'img GL_POST_COLOR_MATRIX_BLUE_SCALE #x80B6)
(define-enum 'img GL_POST_COLOR_MATRIX_ALPHA_SCALE #x80B7)
(define-enum 'img GL_POST_COLOR_MATRIX_RED_BIAS #x80B8)
(define-enum 'img GL_POST_COLOR_MATRIX_GREEN_BIAS #x80B9)
(define-enum 'img GL_POST_COLOR_MATRIX_BLUE_BIAS #x80BA)
(define-enum 'img GL_POST_COLOR_MATRIX_ALPHA_BIAS #x80BB)
(define-enum 'img GL_COLOR_TABLE #x80D0)
(define-enum 'img GL_POST_CONVOLUTION_COLOR_TABLE #x80D1)
(define-enum 'img GL_POST_COLOR_MATRIX_COLOR_TABLE #x80D2)
(define-enum 'img GL_PROXY_COLOR_TABLE #x80D3)
(define-enum 'img GL_PROXY_POST_CONVOLUTION_COLOR_TABLE #x80D4)
(define-enum 'img GL_PROXY_POST_COLOR_MATRIX_COLOR_TABLE #x80D5)
(define-enum 'img GL_COLOR_TABLE_SCALE #x80D6)
(define-enum 'img GL_COLOR_TABLE_BIAS #x80D7)
(define-enum 'img GL_COLOR_TABLE_FORMAT #x80D8)
(define-enum 'img GL_COLOR_TABLE_WIDTH #x80D9)
(define-enum 'img GL_COLOR_TABLE_RED_SIZE #x80DA)
(define-enum 'img GL_COLOR_TABLE_GREEN_SIZE #x80DB)
(define-enum 'img GL_COLOR_TABLE_BLUE_SIZE #x80DC)
(define-enum 'img GL_COLOR_TABLE_ALPHA_SIZE #x80DD)
(define-enum 'img GL_COLOR_TABLE_LUMINANCE_SIZE #x80DE)
(define-enum 'img GL_COLOR_TABLE_INTENSITY_SIZE #x80DF)
(define-enum 'img GL_CONSTANT_BORDER #x8151)
(define-enum 'img GL_REPLICATE_BORDER #x8153)
(define-enum 'img GL_CONVOLUTION_BORDER_COLOR #x8154)

(define-enum 13 GL_TEXTURE0 #x84C0)
(define-enum 13 GL_TEXTURE1 #x84C1)
(define-enum 13 GL_TEXTURE2 #x84C2)
(define-enum 13 GL_TEXTURE3 #x84C3)
(define-enum 13 GL_TEXTURE4 #x84C4)
(define-enum 13 GL_TEXTURE5 #x84C5)
(define-enum 13 GL_TEXTURE6 #x84C6)
(define-enum 13 GL_TEXTURE7 #x84C7)
(define-enum 13 GL_TEXTURE8 #x84C8)
(define-enum 13 GL_TEXTURE9 #x84C9)
(define-enum 13 GL_TEXTURE10 #x84CA)
(define-enum 13 GL_TEXTURE11 #x84CB)
(define-enum 13 GL_TEXTURE12 #x84CC)
(define-enum 13 GL_TEXTURE13 #x84CD)
(define-enum 13 GL_TEXTURE14 #x84CE)
(define-enum 13 GL_TEXTURE15 #x84CF)
(define-enum 13 GL_TEXTURE16 #x84D0)
(define-enum 13 GL_TEXTURE17 #x84D1)
(define-enum 13 GL_TEXTURE18 #x84D2)
(define-enum 13 GL_TEXTURE19 #x84D3)
(define-enum 13 GL_TEXTURE20 #x84D4)
(define-enum 13 GL_TEXTURE21 #x84D5)
(define-enum 13 GL_TEXTURE22 #x84D6)
(define-enum 13 GL_TEXTURE23 #x84D7)
(define-enum 13 GL_TEXTURE24 #x84D8)
(define-enum 13 GL_TEXTURE25 #x84D9)
(define-enum 13 GL_TEXTURE26 #x84DA)
(define-enum 13 GL_TEXTURE27 #x84DB)
(define-enum 13 GL_TEXTURE28 #x84DC)
(define-enum 13 GL_TEXTURE29 #x84DD)
(define-enum 13 GL_TEXTURE30 #x84DE)
(define-enum 13 GL_TEXTURE31 #x84DF)
(define-enum 13 GL_ACTIVE_TEXTURE #x84E0)
(define-enum 13 GL_CLIENT_ACTIVE_TEXTURE #x84E1)
(define-enum 13 GL_MAX_TEXTURE_UNITS #x84E2)
(define-enum 13 GL_TRANSPOSE_MODELVIEW_MATRIX #x84E3)
(define-enum 13 GL_TRANSPOSE_PROJECTION_MATRIX #x84E4)
(define-enum 13 GL_TRANSPOSE_TEXTURE_MATRIX #x84E5)
(define-enum 13 GL_TRANSPOSE_COLOR_MATRIX #x84E6)
(define-enum 13 GL_MULTISAMPLE #x809D)
(define-enum 13 GL_SAMPLE_ALPHA_TO_COVERAGE #x809E)
(define-enum 13 GL_SAMPLE_ALPHA_TO_ONE #x809F)
(define-enum 13 GL_SAMPLE_COVERAGE #x80A0)
(define-enum 13 GL_SAMPLE_BUFFERS #x80A8)
(define-enum 13 GL_SAMPLES #x80A9)
(define-enum 13 GL_SAMPLE_COVERAGE_VALUE #x80AA)
(define-enum 13 GL_SAMPLE_COVERAGE_INVERT #x80AB)
(define-enum 13 GL_MULTISAMPLE_BIT #x20000000)
(define-enum 13 GL_NORMAL_MAP #x8511)
(define-enum 13 GL_REFLECTION_MAP #x8512)
(define-enum 13 GL_TEXTURE_CUBE_MAP #x8513)
(define-enum 13 GL_TEXTURE_BINDING_CUBE_MAP #x8514)
(define-enum 13 GL_TEXTURE_CUBE_MAP_POSITIVE_X #x8515)
(define-enum 13 GL_TEXTURE_CUBE_MAP_NEGATIVE_X #x8516)
(define-enum 13 GL_TEXTURE_CUBE_MAP_POSITIVE_Y #x8517)
(define-enum 13 GL_TEXTURE_CUBE_MAP_NEGATIVE_Y #x8518)
(define-enum 13 GL_TEXTURE_CUBE_MAP_POSITIVE_Z #x8519)
(define-enum 13 GL_TEXTURE_CUBE_MAP_NEGATIVE_Z #x851A)
(define-enum 13 GL_PROXY_TEXTURE_CUBE_MAP #x851B)
(define-enum 13 GL_MAX_CUBE_MAP_TEXTURE_SIZE #x851C)
(define-enum 13 GL_COMPRESSED_ALPHA #x84E9)
(define-enum 13 GL_COMPRESSED_LUMINANCE #x84EA)
(define-enum 13 GL_COMPRESSED_LUMINANCE_ALPHA #x84EB)
(define-enum 13 GL_COMPRESSED_INTENSITY #x84EC)
(define-enum 13 GL_COMPRESSED_RGB #x84ED)
(define-enum 13 GL_COMPRESSED_RGBA #x84EE)
(define-enum 13 GL_TEXTURE_COMPRESSION_HINT #x84EF)
(define-enum 13 GL_TEXTURE_COMPRESSED_IMAGE_SIZE #x86A0)
(define-enum 13 GL_TEXTURE_COMPRESSED #x86A1)
(define-enum 13 GL_NUM_COMPRESSED_TEXTURE_FORMATS #x86A2)
(define-enum 13 GL_COMPRESSED_TEXTURE_FORMATS #x86A3)
(define-enum 13 GL_CLAMP_TO_BORDER #x812D)
(define-enum 13 GL_COMBINE #x8570)
(define-enum 13 GL_COMBINE_RGB #x8571)
(define-enum 13 GL_COMBINE_ALPHA #x8572)
(define-enum 13 GL_SOURCE0_RGB #x8580)
(define-enum 13 GL_SOURCE1_RGB #x8581)
(define-enum 13 GL_SOURCE2_RGB #x8582)
(define-enum 13 GL_SOURCE0_ALPHA #x8588)
(define-enum 13 GL_SOURCE1_ALPHA #x8589)
(define-enum 13 GL_SOURCE2_ALPHA #x858A)
(define-enum 13 GL_OPERAND0_RGB #x8590)
(define-enum 13 GL_OPERAND1_RGB #x8591)
(define-enum 13 GL_OPERAND2_RGB #x8592)
(define-enum 13 GL_OPERAND0_ALPHA #x8598)
(define-enum 13 GL_OPERAND1_ALPHA #x8599)
(define-enum 13 GL_OPERAND2_ALPHA #x859A)
(define-enum 13 GL_RGB_SCALE #x8573)
(define-enum 13 GL_ADD_SIGNED #x8574)
(define-enum 13 GL_INTERPOLATE #x8575)
(define-enum 13 GL_SUBTRACT #x84E7)
(define-enum 13 GL_CONSTANT #x8576)
(define-enum 13 GL_PRIMARY_COLOR #x8577)
(define-enum 13 GL_PREVIOUS #x8578)
(define-enum 13 GL_DOT3_RGB #x86AE)
(define-enum 13 GL_DOT3_RGBA #x86AF)

(define-enum 14 GL_BLEND_DST_RGB #x80C8)
(define-enum 14 GL_BLEND_SRC_RGB #x80C9)
(define-enum 14 GL_BLEND_DST_ALPHA #x80CA)
(define-enum 14 GL_BLEND_SRC_ALPHA #x80CB)
(define-enum 14 GL_POINT_SIZE_MIN #x8126)
(define-enum 14 GL_POINT_SIZE_MAX #x8127)
(define-enum 14 GL_POINT_FADE_THRESHOLD_SIZE #x8128)
(define-enum 14 GL_POINT_DISTANCE_ATTENUATION #x8129)
(define-enum 14 GL_GENERATE_MIPMAP #x8191)
(define-enum 14 GL_GENERATE_MIPMAP_HINT #x8192)
(define-enum 14 GL_DEPTH_COMPONENT16 #x81A5)
(define-enum 14 GL_DEPTH_COMPONENT24 #x81A6)
(define-enum 14 GL_DEPTH_COMPONENT32 #x81A7)
(define-enum 14 GL_MIRRORED_REPEAT #x8370)
(define-enum 14 GL_FOG_COORDINATE_SOURCE #x8450)
(define-enum 14 GL_FOG_COORDINATE #x8451)
(define-enum 14 GL_FRAGMENT_DEPTH #x8452)
(define-enum 14 GL_CURRENT_FOG_COORDINATE #x8453)
(define-enum 14 GL_FOG_COORDINATE_ARRAY_TYPE #x8454)
(define-enum 14 GL_FOG_COORDINATE_ARRAY_STRIDE #x8455)
(define-enum 14 GL_FOG_COORDINATE_ARRAY_POINTER #x8456)
(define-enum 14 GL_FOG_COORDINATE_ARRAY #x8457)
(define-enum 14 GL_COLOR_SUM #x8458)
(define-enum 14 GL_CURRENT_SECONDARY_COLOR #x8459)
(define-enum 14 GL_SECONDARY_COLOR_ARRAY_SIZE #x845A)
(define-enum 14 GL_SECONDARY_COLOR_ARRAY_TYPE #x845B)
(define-enum 14 GL_SECONDARY_COLOR_ARRAY_STRIDE #x845C)
(define-enum 14 GL_SECONDARY_COLOR_ARRAY_POINTER #x845D)
(define-enum 14 GL_SECONDARY_COLOR_ARRAY #x845E)
(define-enum 14 GL_MAX_TEXTURE_LOD_BIAS #x84FD)
(define-enum 14 GL_TEXTURE_FILTER_CONTROL #x8500)
(define-enum 14 GL_TEXTURE_LOD_BIAS #x8501)
(define-enum 14 GL_INCR_WRAP #x8507)
(define-enum 14 GL_DECR_WRAP #x8508)
(define-enum 14 GL_TEXTURE_DEPTH_SIZE #x884A)
(define-enum 14 GL_DEPTH_TEXTURE_MODE #x884B)
(define-enum 14 GL_TEXTURE_COMPARE_MODE #x884C)
(define-enum 14 GL_TEXTURE_COMPARE_FUNC #x884D)
(define-enum 14 GL_COMPARE_R_TO_TEXTURE #x884E)

(define-enum 15 GL_BUFFER_SIZE #x8764)
(define-enum 15 GL_BUFFER_USAGE #x8765)
(define-enum 15 GL_QUERY_COUNTER_BITS #x8864)
(define-enum 15 GL_CURRENT_QUERY #x8865)
(define-enum 15 GL_QUERY_RESULT #x8866)
(define-enum 15 GL_QUERY_RESULT_AVAILABLE #x8867)
(define-enum 15 GL_ARRAY_BUFFER #x8892)
(define-enum 15 GL_ELEMENT_ARRAY_BUFFER #x8893)
(define-enum 15 GL_ARRAY_BUFFER_BINDING #x8894)
(define-enum 15 GL_ELEMENT_ARRAY_BUFFER_BINDING #x8895)
(define-enum 15 GL_VERTEX_ARRAY_BUFFER_BINDING #x8896)
(define-enum 15 GL_NORMAL_ARRAY_BUFFER_BINDING #x8897)
(define-enum 15 GL_COLOR_ARRAY_BUFFER_BINDING #x8898)
(define-enum 15 GL_INDEX_ARRAY_BUFFER_BINDING #x8899)
(define-enum 15 GL_TEXTURE_COORD_ARRAY_BUFFER_BINDING #x889A)
(define-enum 15 GL_EDGE_FLAG_ARRAY_BUFFER_BINDING #x889B)
(define-enum 15 GL_SECONDARY_COLOR_ARRAY_BUFFER_BINDING #x889C)
(define-enum 15 GL_FOG_COORDINATE_ARRAY_BUFFER_BINDING #x889D)
(define-enum 15 GL_WEIGHT_ARRAY_BUFFER_BINDING #x889E)
(define-enum 15 GL_VERTEX_ATTRIB_ARRAY_BUFFER_BINDING #x889F)
(define-enum 15 GL_READ_ONLY #x88B8)
(define-enum 15 GL_WRITE_ONLY #x88B9)
(define-enum 15 GL_READ_WRITE #x88BA)
(define-enum 15 GL_BUFFER_ACCESS #x88BB)
(define-enum 15 GL_BUFFER_MAPPED #x88BC)
(define-enum 15 GL_BUFFER_MAP_POINTER #x88BD)
(define-enum 15 GL_STREAM_DRAW #x88E0)
(define-enum 15 GL_STREAM_READ #x88E1)
(define-enum 15 GL_STREAM_COPY #x88E2)
(define-enum 15 GL_STATIC_DRAW #x88E4)
(define-enum 15 GL_STATIC_READ #x88E5)
(define-enum 15 GL_STATIC_COPY #x88E6)
(define-enum 15 GL_DYNAMIC_DRAW #x88E8)
(define-enum 15 GL_DYNAMIC_READ #x88E9)
(define-enum 15 GL_DYNAMIC_COPY #x88EA)
(define-enum 15 GL_SAMPLES_PASSED #x8914)
(define-enum 15 GL_FOG_COORD_SRC GL_FOG_COORDINATE_SOURCE)
(define-enum 15 GL_FOG_COORD GL_FOG_COORDINATE)
(define-enum 15 GL_CURRENT_FOG_COORD GL_CURRENT_FOG_COORDINATE)
(define-enum 15 GL_FOG_COORD_ARRAY_TYPE GL_FOG_COORDINATE_ARRAY_TYPE)
(define-enum 15 GL_FOG_COORD_ARRAY_STRIDE GL_FOG_COORDINATE_ARRAY_STRIDE)
(define-enum 15 GL_FOG_COORD_ARRAY_POINTER GL_FOG_COORDINATE_ARRAY_POINTER)
(define-enum 15 GL_FOG_COORD_ARRAY GL_FOG_COORDINATE_ARRAY)
(define-enum 15 GL_FOG_COORD_ARRAY_BUFFER_BINDING GL_FOG_COORDINATE_ARRAY_BUFFER_BINDING)
(define-enum 15 GL_SRC0_RGB GL_SOURCE0_RGB)
(define-enum 15 GL_SRC1_RGB GL_SOURCE1_RGB)
(define-enum 15 GL_SRC2_RGB GL_SOURCE2_RGB)
(define-enum 15 GL_SRC0_ALPHA GL_SOURCE0_ALPHA)
(define-enum 15 GL_SRC1_ALPHA GL_SOURCE1_ALPHA)
(define-enum 15 GL_SRC2_ALPHA GL_SOURCE2_ALPHA)

;; These values are taken from SGI's glu.h.
(define-enum GLU_FALSE 0)
(define-enum GLU_TRUE 1)
(define-enum GLU_VERSION 100800)
(define-enum GLU_EXTENSIONS 100801)
(define-enum GLU_INVALID_ENUM 100900)
(define-enum GLU_INVALID_VALUE 100901)
(define-enum GLU_OUT_OF_MEMORY 100902)
(define-enum GLU_INVALID_OPERATION 100904)
(define-enum GLU_OUTLINE_POLYGON 100240)
(define-enum GLU_OUTLINE_PATCH 100241)
(define-enum GLU_NURBS_ERROR 100103)
(define-enum GLU_ERROR 100103)
(define-enum GLU_NURBS_BEGIN 100164)
(define-enum GLU_NURBS_BEGIN_EXT 100164)
(define-enum GLU_NURBS_VERTEX 100165)
(define-enum GLU_NURBS_VERTEX_EXT 100165)
(define-enum GLU_NURBS_NORMAL 100166)
(define-enum GLU_NURBS_NORMAL_EXT 100166)
(define-enum GLU_NURBS_COLOR 100167)
(define-enum GLU_NURBS_COLOR_EXT 100167)
(define-enum GLU_NURBS_TEXTURE_COORD 100168)
(define-enum GLU_NURBS_TEX_COORD_EXT 100168)
(define-enum GLU_NURBS_END 100169)
(define-enum GLU_NURBS_END_EXT 100169)
(define-enum GLU_NURBS_BEGIN_DATA 100170)
(define-enum GLU_NURBS_BEGIN_DATA_EXT 100170)
(define-enum GLU_NURBS_VERTEX_DATA 100171)
(define-enum GLU_NURBS_VERTEX_DATA_EXT 100171)
(define-enum GLU_NURBS_NORMAL_DATA 100172)
(define-enum GLU_NURBS_NORMAL_DATA_EXT 100172)
(define-enum GLU_NURBS_COLOR_DATA 100173)
(define-enum GLU_NURBS_COLOR_DATA_EXT 100173)
(define-enum GLU_NURBS_TEXTURE_COORD_DATA 100174)
(define-enum GLU_NURBS_TEX_COORD_DATA_EXT 100174)
(define-enum GLU_NURBS_END_DATA 100175)
(define-enum GLU_NURBS_END_DATA_EXT 100175)
(define-enum GLU_NURBS_ERROR1 100251)
(define-enum GLU_NURBS_ERROR2 100252)
(define-enum GLU_NURBS_ERROR3 100253)
(define-enum GLU_NURBS_ERROR4 100254)
(define-enum GLU_NURBS_ERROR5 100255)
(define-enum GLU_NURBS_ERROR6 100256)
(define-enum GLU_NURBS_ERROR7 100257)
(define-enum GLU_NURBS_ERROR8 100258)
(define-enum GLU_NURBS_ERROR9 100259)
(define-enum GLU_NURBS_ERROR10 100260)
(define-enum GLU_NURBS_ERROR11 100261)
(define-enum GLU_NURBS_ERROR12 100262)
(define-enum GLU_NURBS_ERROR13 100263)
(define-enum GLU_NURBS_ERROR14 100264)
(define-enum GLU_NURBS_ERROR15 100265)
(define-enum GLU_NURBS_ERROR16 100266)
(define-enum GLU_NURBS_ERROR17 100267)
(define-enum GLU_NURBS_ERROR18 100268)
(define-enum GLU_NURBS_ERROR19 100269)
(define-enum GLU_NURBS_ERROR20 100270)
(define-enum GLU_NURBS_ERROR21 100271)
(define-enum GLU_NURBS_ERROR22 100272)
(define-enum GLU_NURBS_ERROR23 100273)
(define-enum GLU_NURBS_ERROR24 100274)
(define-enum GLU_NURBS_ERROR25 100275)
(define-enum GLU_NURBS_ERROR26 100276)
(define-enum GLU_NURBS_ERROR27 100277)
(define-enum GLU_NURBS_ERROR28 100278)
(define-enum GLU_NURBS_ERROR29 100279)
(define-enum GLU_NURBS_ERROR30 100280)
(define-enum GLU_NURBS_ERROR31 100281)
(define-enum GLU_NURBS_ERROR32 100282)
(define-enum GLU_NURBS_ERROR33 100283)
(define-enum GLU_NURBS_ERROR34 100284)
(define-enum GLU_NURBS_ERROR35 100285)
(define-enum GLU_NURBS_ERROR36 100286)
(define-enum GLU_NURBS_ERROR37 100287)
(define-enum GLU_AUTO_LOAD_MATRIX 100200)
(define-enum GLU_CULLING 100201)
(define-enum GLU_SAMPLING_TOLERANCE 100203)
(define-enum GLU_DISPLAY_MODE 100204)
(define-enum GLU_PARAMETRIC_TOLERANCE 100202)
(define-enum GLU_SAMPLING_METHOD 100205)
(define-enum GLU_U_STEP 100206)
(define-enum GLU_V_STEP 100207)
(define-enum GLU_NURBS_MODE 100160)
(define-enum GLU_NURBS_MODE_EXT 100160)
(define-enum GLU_NURBS_TESSELLATOR 100161)
(define-enum GLU_NURBS_TESSELLATOR_EXT 100161)
(define-enum GLU_NURBS_RENDERER 100162)
(define-enum GLU_NURBS_RENDERER_EXT 100162)
(define-enum GLU_OBJECT_PARAMETRIC_ERROR 100208)
(define-enum GLU_OBJECT_PARAMETRIC_ERROR_EXT 100208)
(define-enum GLU_OBJECT_PATH_LENGTH 100209)
(define-enum GLU_OBJECT_PATH_LENGTH_EXT 100209)
(define-enum GLU_PATH_LENGTH 100215)
(define-enum GLU_PARAMETRIC_ERROR 100216)
(define-enum GLU_DOMAIN_DISTANCE 100217)
(define-enum GLU_MAP1_TRIM_2 100210)
(define-enum GLU_MAP1_TRIM_3 100211)
(define-enum GLU_POINT 100010)
(define-enum GLU_LINE 100011)
(define-enum GLU_FILL 100012)
(define-enum GLU_SILHOUETTE 100013)
(define-enum GLU_SMOOTH 100000)
(define-enum GLU_FLAT 100001)
(define-enum GLU_NONE 100002)
(define-enum GLU_OUTSIDE 100020)
(define-enum GLU_INSIDE 100021)
(define-enum GLU_TESS_BEGIN 100100)
(define-enum GLU_BEGIN 100100)
(define-enum GLU_TESS_VERTEX 100101)
(define-enum GLU_VERTEX 100101)
(define-enum GLU_TESS_END 100102)
(define-enum GLU_END 100102)
(define-enum GLU_TESS_ERROR 100103)
(define-enum GLU_TESS_EDGE_FLAG 100104)
(define-enum GLU_EDGE_FLAG 100104)
(define-enum GLU_TESS_COMBINE 100105)
(define-enum GLU_TESS_BEGIN_DATA 100106)
(define-enum GLU_TESS_VERTEX_DATA 100107)
(define-enum GLU_TESS_END_DATA 100108)
(define-enum GLU_TESS_ERROR_DATA 100109)
(define-enum GLU_TESS_EDGE_FLAG_DATA 100110)
(define-enum GLU_TESS_COMBINE_DATA 100111)
(define-enum GLU_CW 100120)
(define-enum GLU_CCW 100121)
(define-enum GLU_INTERIOR 100122)
(define-enum GLU_EXTERIOR 100123)
(define-enum GLU_UNKNOWN 100124)
(define-enum GLU_TESS_WINDING_RULE 100140)
(define-enum GLU_TESS_BOUNDARY_ONLY 100141)
(define-enum GLU_TESS_TOLERANCE 100142)
(define-enum GLU_TESS_ERROR1 100151)
(define-enum GLU_TESS_ERROR2 100152)
(define-enum GLU_TESS_ERROR3 100153)
(define-enum GLU_TESS_ERROR4 100154)
(define-enum GLU_TESS_ERROR5 100155)
(define-enum GLU_TESS_ERROR6 100156)
(define-enum GLU_TESS_ERROR7 100157)
(define-enum GLU_TESS_ERROR8 100158)
(define-enum GLU_TESS_MISSING_BEGIN_POLYGON 100151)
(define-enum GLU_TESS_MISSING_BEGIN_CONTOUR 100152)
(define-enum GLU_TESS_MISSING_END_POLYGON 100153)
(define-enum GLU_TESS_MISSING_END_CONTOUR 100154)
(define-enum GLU_TESS_COORD_TOO_LARGE 100155)
(define-enum GLU_TESS_NEED_COMBINE_CALLBACK 100156)
(define-enum GLU_TESS_WINDING_ODD 100130)
(define-enum GLU_TESS_WINDING_NONZERO 100131)
(define-enum GLU_TESS_WINDING_POSITIVE 100132)
(define-enum GLU_TESS_WINDING_NEGATIVE 100133)
(define-enum GLU_TESS_WINDING_ABS_GEQ_TWO 100134)
(define-enum GLU_TESS_MAX_COORD 1.0e150)
