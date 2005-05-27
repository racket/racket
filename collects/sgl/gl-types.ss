(module gl-types mzscheme
  (require (lib "foreign.ss")
           "gl-info.ss")
  
  (provide (all-defined-except get-unsigned-type get-signed-type make-gl-vector-type))

  (define _float*
    (make-ctype _float 
                (lambda (n)
                  (if (exact? n)
                      (exact->inexact n)
                      n))
                #f))
  
  (define (get-unsigned-type size)
    (case size
      ((1) _uint8)
      ((2) _uint16)
      ((4) _uint32)
      ((8) _uint64)
      (else (error 'get-unsigned-type (format "no ~a byte unsigned type" size)))))

  (define (get-signed-type size)
    (case size
      ((1) _sint8)
      ((2) _sint16)
      ((4) _sint32)
      ((8) _sint64)
      (else (error 'get-signed-type (format "no ~a byte signed type" size)))))

  (define (make-gl-vector-type t)
    (make-ctype _cvector
                (lambda (sval)
                  (unless (cvector? sval)
                    (raise-type-error 'Scheme->C "cvector" sval))
                  (unless (eq? (cvector-type sval) t)
                    (error 'Scheme->C "wrong kind of cvector"))
                  sval)
                #f))

  (define _gl-byte (get-signed-type gl-byte-size))
  (define _gl-ubyte (get-unsigned-type gl-ubyte-size))
  (define _gl-short (get-signed-type gl-short-size))
  (define _gl-ushort (get-unsigned-type gl-ushort-size))
  (define _gl-int (get-signed-type gl-int-size))
  (define _gl-uint (get-unsigned-type gl-uint-size))
  (define _gl-boolean (make-ctype (get-unsigned-type gl-boolean-size)
                                  (lambda (x)
                                    (if x 1 0))
                                  (lambda (x) (not (= x 0)))))
  (define _gl-sizei (get-unsigned-type gl-sizei-size))
  (define _gl-enum (get-unsigned-type gl-enum-size))
  (define _gl-bitfield (get-unsigned-type gl-bitfield-size))
  (define _gl-float _float*)
  (define _gl-double _double*)
  (define _gl-clampf _float*)
  (define _gl-clampd _double*)

  (define _gl-bytev (make-gl-vector-type _gl-byte))
  (define _gl-ubytev (make-gl-vector-type _gl-ubyte))
  (define _gl-shortv (make-gl-vector-type _gl-short))
  (define _gl-ushortv (make-gl-vector-type _gl-ushort))
  (define _gl-intv (make-gl-vector-type _gl-int))
  (define _gl-uintv (make-gl-vector-type _gl-uint))
  (define _gl-booleanv (make-gl-vector-type _gl-boolean))
  (define _gl-floatv (make-gl-vector-type _gl-float))
  (define _gl-doublev (make-gl-vector-type _gl-double))
  (define _gl-voidv _cvector)

  ;; If GLfloat and GLdouble don't correspond to C's float and double, things
  ;; won't work.
  (unless (= gl-float-size (compiler-sizeof '(float)))
    (error 'gl-float "GLfloat does not correspond to C's float type"))
  (unless (= gl-double-size (compiler-sizeof '(double)))
    (error 'gl-double "GLdouble does not correspond to C's double type"))
  )