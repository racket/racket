;; Sometimes helpful for debugging: flip #t to #f to make
;; some unsafe primitives safe.
(meta-cond
 [#t (define-syntax-rule (unsafe-primitive id) #3%id)]
 [else (define-syntax-rule (unsafe-primitive id) id)])

(define unsafe-car (unsafe-primitive car))
(define unsafe-cdr (unsafe-primitive cdr))
(define unsafe-list-tail (unsafe-primitive list-tail))
(define unsafe-list-ref (unsafe-primitive list-ref))

(define unsafe-char=? (unsafe-primitive char=?))
(define unsafe-char<? (unsafe-primitive char<?))
(define unsafe-char>? (unsafe-primitive char>?))
(define unsafe-char>=? (unsafe-primitive char>=?))
(define unsafe-char<=? (unsafe-primitive char<=?))
(define unsafe-char->integer (unsafe-primitive char->integer))

(define unsafe-fx+ (unsafe-primitive fx+))
(define unsafe-fx- (unsafe-primitive fx-))
(define unsafe-fx* (unsafe-primitive fx*))
(define unsafe-fxquotient (unsafe-primitive fxquotient))
(define unsafe-fxremainder (unsafe-primitive fxremainder))
(define unsafe-fxmodulo (unsafe-primitive fxmodulo))
(define unsafe-fxabs (unsafe-primitive fxabs))
(define unsafe-fxand (unsafe-primitive fxand))
(define unsafe-fxior (unsafe-primitive fxior))
(define unsafe-fxxor (unsafe-primitive fxxor))
(define unsafe-fxnot (unsafe-primitive fxnot))
(define unsafe-fxrshift (unsafe-primitive fxarithmetic-shift-right))
(define unsafe-fxlshift (unsafe-primitive fxarithmetic-shift-left))

(define unsafe-fx= (unsafe-primitive fx=))
(define unsafe-fx< (unsafe-primitive fx<))
(define unsafe-fx> (unsafe-primitive fx>))
(define unsafe-fx>= (unsafe-primitive fx>=))
(define unsafe-fx<= (unsafe-primitive fx<=))
(define unsafe-fxmin (unsafe-primitive fxmin))
(define unsafe-fxmax (unsafe-primitive fxmax))

(define unsafe-fl+ (unsafe-primitive fl+))
(define unsafe-fl- (unsafe-primitive fl-))
(define unsafe-fl* (unsafe-primitive fl*))
(define unsafe-fl/ (unsafe-primitive fl/))
(define unsafe-flabs (unsafe-primitive flabs))

(define unsafe-fl= (unsafe-primitive fl=))
(define unsafe-fl< (unsafe-primitive fl<))
(define unsafe-fl> (unsafe-primitive fl>))
(define unsafe-fl>= (unsafe-primitive fl>=))
(define unsafe-fl<= (unsafe-primitive fl<=))
(define unsafe-flmin (unsafe-primitive flmin))
(define unsafe-flmax (unsafe-primitive flmax))

(define unsafe-fx->fl (unsafe-primitive fixnum->flonum))
(define unsafe-fl->fx (unsafe-primitive flonum->fixnum))

(define unsafe-flround (unsafe-primitive flround))
(define unsafe-flfloor (unsafe-primitive flfloor))
(define unsafe-flceiling (unsafe-primitive flceiling))
(define unsafe-fltruncate (unsafe-primitive fltruncate))

(define unsafe-flsin (unsafe-primitive flsin))
(define unsafe-flcos (unsafe-primitive flcos))
(define unsafe-fltan (unsafe-primitive fltan))
(define unsafe-flasin (unsafe-primitive flasin))
(define unsafe-flacos (unsafe-primitive flacos))
(define unsafe-flatan (unsafe-primitive flatan))
(define unsafe-fllog (unsafe-primitive fllog))
(define unsafe-flexp (unsafe-primitive flexp))
(define unsafe-flsqrt (unsafe-primitive flsqrt))
(define unsafe-flexpt (unsafe-primitive flexpt))

(define (unsafe-flrandom gen) (random gen))

(define unsafe-vector*-length (unsafe-primitive vector-length))
(define unsafe-vector*-ref (unsafe-primitive vector-ref))
(define unsafe-vector*-set! (unsafe-primitive vector-set!))
(define unsafe-vector*-cas! (unsafe-primitive vector-cas!))

(define (unsafe-struct*-cas! s k old new)
  (#3%$record-cas! s k old new))

(define unsafe-unbox* (unsafe-primitive unbox))
(define unsafe-set-box*! (unsafe-primitive set-box!))
(define unsafe-box*-cas! (unsafe-primitive box-cas!))

(define unsafe-bytes-length (unsafe-primitive bytevector-length))
(define unsafe-bytes-ref (unsafe-primitive bytevector-u8-ref))
(define unsafe-bytes-set! (unsafe-primitive bytevector-u8-set!))

(define unsafe-string-length (unsafe-primitive string-length))
(define unsafe-string-ref (unsafe-primitive string-ref))
(define unsafe-string-set! (unsafe-primitive string-set!))

(define unsafe-fxvector-length (unsafe-primitive fxvector-length))
(define unsafe-fxvector-ref (unsafe-primitive fxvector-ref))
(define unsafe-fxvector-set! (unsafe-primitive fxvector-set!))

(define (unsafe-s16vector-ref s16 k)
  (let* ([cptr (unsafe-struct*-ref s16 0)]
         [mem (cpointer-memory cptr)]
         [k (fx* k 2)])
    (if (bytes? mem)
        (bytevector-s16-native-ref mem k)
        (foreign-ref 'int16 mem k))))
(define (unsafe-s16vector-set! s16 k v)
  (let* ([cptr (unsafe-struct*-ref s16 0)]
         [mem (cpointer-memory cptr)]
         [k (fx* k 2)])
    (if (bytes? mem)
        (bytevector-s16-native-set! mem k v)
        (foreign-set! 'int16 mem k v))))

(define (unsafe-u16vector-ref u16 k)
  (let* ([cptr (unsafe-struct*-ref u16 0)]
         [mem (cpointer-memory cptr)]
         [k (fx* k 2)])
    (if (bytes? mem)
        (bytevector-u16-native-ref mem k)
        (foreign-ref 'uint16 mem k))))
(define (unsafe-u16vector-set! u16 k v)
  (let* ([cptr (unsafe-struct*-ref u16 0)]
         [mem (cpointer-memory cptr)]
         [k (fx* k 2)])
    (if (bytes? mem)
        (bytevector-u16-native-set! mem k v)
        (foreign-set! 'uint16 mem k v))))

(define (unsafe-f64vector-ref f64 k)
  (let* ([cptr (unsafe-struct*-ref f64 0)]
         [mem (cpointer-memory cptr)]
         [k (fx* k 8)])
    (if (bytes? mem)
        (bytevector-ieee-double-native-ref mem k)
        (foreign-ref 'double mem k))))
(define (unsafe-f64vector-set! f64 k v)
  (let* ([cptr (unsafe-struct*-ref f64 0)]
         [mem (cpointer-memory cptr)]
         [k (fx* k 8)])
    (if (bytes? mem)
        (bytevector-ieee-double-native-set! mem k v)
        (foreign-set! 'double mem k v))))

;; FIXME
(define (unsafe-f80vector-ref f80 k)
  (let* ([cptr (unsafe-struct*-ref f80 0)]
         [mem (cpointer-memory cptr)])
    (if (bytes? mem)
        (bytevector-ieee-double-native-ref mem k)
        (foreign-ref 'double mem k))))
(define (unsafe-f80vector-set! f80 k v)
  (let* ([cptr (unsafe-struct*-ref f80 0)]
         [mem (cpointer-memory cptr)])
    (if (bytes? mem)
        (bytevector-ieee-double-native-set! mem k v)
        (foreign-set! 'double mem k v))))

(define (unsafe-make-flrectangular r i)
  (#3%make-rectangular r i))
(define (unsafe-flreal-part c)
  (#3%real-part c))
(define (unsafe-flimag-part c)
  (#3%imag-part c))

(define unsafe-undefined (let ([p (make-record-type "undefined" '())])
                           ((record-constructor p))))

(define (check-not-unsafe-undefined v sym)
  (when (eq? v unsafe-undefined)
    (raise (|#%app|
            exn:fail:contract:variable
            (string-append (symbol->string sym)
                           ": undefined;\n cannot use before initialization")
            (current-continuation-marks)
            sym)))
  v)

(define (check-not-unsafe-undefined/assign v sym)
  (when (eq? v unsafe-undefined)
    (raise (|#%app|
            exn:fail:contract:variable
            (string-append (symbol->string sym)
                           ": assignment disallowed;\n cannot assign before initialization")
            (current-continuation-marks)
            sym)))
  v)
