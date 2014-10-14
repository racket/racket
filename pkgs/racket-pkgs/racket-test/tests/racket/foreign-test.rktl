
(load-relative "loadtest.rktl")

(Section 'foreign)

(require ffi/unsafe
         ffi/unsafe/cvector
         ffi/vector
         racket/extflonum
         racket/place)

(define test-async? (and (place-enabled?) (not (eq? 'windows (system-type)))))

(test #f malloc 0)
(test #f malloc 0 _int)
(test #f malloc _int 0)

(test 0 bytes-length (make-sized-byte-string #f 0))

;; Check integer-range checking:
(let ()
  (define (try-int-boundary N _int _uint)
    (test (- (expt 2 N)) cast (- (expt 2 N)) _int _int)
    (test (sub1 (expt 2 N)) cast (sub1 (expt 2 N)) _int _int)
    (test (expt 2 N) cast (expt 2 N) _uint _uint)
    (test (sub1 (expt 2 (add1 N))) cast (sub1 (expt 2 (add1 N))) _uint _uint)
    (err/rt-test (cast (expt 2 N) _int _int))
    (err/rt-test (cast (sub1 (- (expt 2 N))) _int _int))
    (err/rt-test (cast -1 _uint _uint))
    (err/rt-test (cast (- (expt 2 N)) _uint _uint)))
  (try-int-boundary 7 _int8 _uint8)
  (try-int-boundary 7 _sbyte _ubyte)
  (test 128 cast -128 _byte _byte)
  (test 239 cast -17 _byte _byte)
  (try-int-boundary 15 _int16 _uint16)
  (try-int-boundary 15 _sword _uword)
  (try-int-boundary 15 _short _ushort)
  (test (expt 2 15) cast (- (expt 2 15)) _word _word)
  (try-int-boundary 31 _int32 _uint32)
  (try-int-boundary 63 _int64 _uint64))

(let ([big/little (if (system-big-endian?) (lambda (x y) x) (lambda (x y) y))]
      [p (malloc _int32)])
  (ptr-set! p _int32 0)
  (test 0 ptr-ref p _int32)
  (ptr-set! p _int32 16909060)
  (test 16909060 ptr-ref p _int32)
  (test 16909060 ptr-ref p _int32 0)
  (test (big/little 1 4) ptr-ref p _int8 0)
  (test (big/little 2 3) ptr-ref p _int8 1)
  (test (big/little 3 2) ptr-ref p _int8 2)
  (test (big/little 4 1) ptr-ref p _int8 3))
(flush-output)

(err/rt-test (_array _byte 1024 1024 1024 1024 1024 1024 1200 2)
             (lambda (exn) (regexp-match? #rx"arithmetic overflow" (exn-message exn))))

(when (eq? 'windows (system-type))
  (define concat string-append)
  (define 64bit? (= 8 (compiler-sizeof '(* void))))
  (define (find-dir what . dirs)
    (or (for/or ([d (in-list dirs)]) (and (directory-exists? d) d))
        (error (format "Could not find a directory: ~a" what))))
  (define progfiles
    (find-dir "Program Files" "C:/Program Files (x86)" "C:/Program Files"))
  (define studio
    (and progfiles (concat progfiles "/Microsoft Visual Studio 10.0")))
  (when (and studio (directory-exists? studio))
    (define (paths-env var . ps)
      (define val
        (apply concat (for/list ([p (in-list ps)]
                                 #:when (and p (directory-exists? p)))
                        (concat p ";"))))
      (printf ">>> $~a = ~s\n" var val)
      (putenv var val))
    (define (vc p)     (concat studio "/VC/" p))
    (define (common p) (concat studio "/Common7/" p))
    (define (winsdk p) (concat progfiles "/Microsoft SDKs/Windows/v7.0A/" p))
    (paths-env "PATH"
               (getenv "PATH")
               (vc (if 64bit? "BIN/amd64" "BIN"))
               (vc "IDE") (vc "Tools") (vc "Tools/bin")
               (common "Tools") (common "IDE"))
    (paths-env "INCLUDE"
               (vc "INCLUDE") (vc "ATLMFC/INCLUDE") (vc "PlatformSDK/INCLUDE")
               (winsdk "include"))
    (paths-env "LIB"
               (vc (if 64bit? "LIB/amd64" "LIB"))
               (vc (if 64bit? "ATLMFC/LIB/amd64" "ATLMFC/LIB"))
               (vc "PlatformSDK/LIB")
               (winsdk (if 64bit? "Lib/x64" "Lib")))
    (putenv "LIBPATH" (getenv "LIB"))
    (define tmp (getenv "TEMP"))
    (unless (and tmp (directory-exists? tmp))
      (putenv "TEMP" (find-dir "Temporary directory" "C:/Temp" "C:/tmp")))
    (when 64bit? (putenv "Platform" "X64"))))

(require dynext/compile dynext/link mzlib/etc)
(define delete-test-files
  (let ([c  (build-path (this-expression-source-directory) "foreign-test.c")]
        [o  (build-path (current-directory)
                        (if (eq? 'windows (system-type))
                          "foreign-test.obj" "foreign-test.o"))]
        [so (build-path (current-directory)
                        (bytes->path (bytes-append #"foreign-test"
                                                   (system-type 'so-suffix))))])
    (when (file-exists? o) (delete-file o))
    (when (file-exists? so) (delete-file so))
    (parameterize ([current-standard-link-libraries '()]
                   [current-extension-compiler-flags
                    (if test-async?
                        (append '("-pthread" "-DUSE_THREAD_TEST") (current-extension-compiler-flags))
                        (current-extension-compiler-flags))]
                   [current-extension-linker-flags
                    (if test-async?
                        (append '("-pthread") (current-extension-linker-flags))
                        (current-extension-linker-flags))])
      (compile-extension #t c o '())
      (link-extension #t (list o) so))
    (lambda ()
      (when (file-exists? o) (delete-file o))
      (when (file-exists? so)
        (with-handlers ([exn:fail:filesystem?
                         (lambda (e)
                           (eprintf "warning: could not delete ~e\n" so))])
          (delete-file so))))))

;; Test arrays
(define _c7_list (_array/list _byte 7))
(test 7 ctype-sizeof _c7_list)
(test 1 ctype-alignof _c7_list)

(test 21 ctype-sizeof (_array _byte 3 7))

;; Test enum and bitmask
(define ([t (_enum '(a b c = 4 d))]
         [s (_bitmask '(a b c = 14 d))])
  (define (check t v n)
    (test n (cast v t _int)))
  (check t 'a 0)
  (check t 'b 1)
  (check t 'c 4)
  (check t 'd 5)
  (check t 'a 1)
  (check t 'b 2)
  (check t 'c 14)
  (check t 'd 16)
  (check t '(a b) 3))

(define-cstruct _ic7i ([i1 _int]
                       [c7 _c7_list]
                       [i2 _int]))

(define _borl (_union _byte _long))
(define _ic7iorl (_union _ic7i _long))

(define test-lib (ffi-lib "./foreign-test"))

(for ([n (in-range 5)])
  (define (ffi name type) (get-ffi-obj name test-lib type))
  (define (test* expected name type proc)
    (test expected name (proc (ffi name type))))
  (define (t expected name type . args)
    (test* expected name type (lambda (p) (apply p args))))
  (define (tc expected name type arg1 . args)
    ;; curry first argument
    (test* expected name type (lambda (p) (apply (p arg1) args))))
  (define (sqr x) (when (zero? (random 4)) (collect-garbage)) (* x x))
  (define b (box #f))
  ;; ---
  (t  2 'add1_int_int   (_fun _int  -> _int ) 1)
  (t  2 'add1_byte_int  (_fun _byte -> _int ) 1)
  (t  2 'add1_int_byte  (_fun _int  -> _byte) 1)
  (t  2 'add1_byte_byte (_fun _byte -> _byte) 1)
  (t  -1 'add1_int_int   (_fun _int  -> _int ) -2)
  (t  -1 'add1_int_int   (_fun _int  -> _fixint ) -2)
  (t 2.0 'add1_float_float (_fun _float -> _float) 1.0)
  (t -1.0 'add1_float_float (_fun _float -> _float) -2.0)
  (t 2.0 'add1_double_double (_fun _double -> _double) 1.0)
  (t -1.0 'add1_double_double (_fun _double -> _double) -2.0)
  ;; ---
  (t 12 'decimal_int_int_int    (_fun _int  _int  -> _int ) 1 2)
  (t 12 'decimal_byte_int_int   (_fun _byte _int  -> _int ) 1 2)
  (t 12 'decimal_int_byte_int   (_fun _int  _byte -> _int ) 1 2)
  (t 12 'decimal_byte_byte_int  (_fun _byte _byte -> _int ) 1 2)
  (t 12 'decimal_int_int_byte   (_fun _int  _int  -> _byte) 1 2)
  (t 12 'decimal_byte_int_byte  (_fun _byte _int  -> _byte) 1 2)
  (t 12 'decimal_int_byte_byte  (_fun _int  _byte -> _byte) 1 2)
  (t 12 'decimal_byte_byte_byte (_fun _byte _byte -> _byte) 1 2)
  ;; ---
  (t  9 'callback3_int_int_int     (_fun (_fun _int  -> _int ) -> _int ) sqr)
  (t  79 'callback3_int_int_int    (_fun (_fun _int  -> _int ) -> _int ) #f) ; NULL allowed as function pointer
  (t  9 'callback3_int_int_int     (_fun _pointer -> _int ) (function-ptr sqr (_fun _int  -> _int ))) ; callback allowed as pointer
  (t  9 'callback3_byte_int_int    (_fun (_fun _byte -> _int ) -> _int ) sqr)
  (t  9 'callback3_short_int_int   (_fun (_fun _short -> _int ) -> _int ) sqr)
  (t  9 'callback3_int_byte_int    (_fun (_fun _int  -> _byte) -> _int ) sqr)
  (t  9 'callback3_int_short_int   (_fun (_fun _int  -> _short) -> _int ) sqr)
  (t  9 'callback3_byte_byte_int   (_fun (_fun _byte -> _byte) -> _int ) sqr)
  (t  9 'callback3_short_short_int (_fun (_fun _short -> _short) -> _int ) sqr)
  (t  9 'callback3_int_int_byte    (_fun (_fun _int  -> _int ) -> _byte) sqr)
  (t  9 'callback3_int_int_short   (_fun (_fun _int  -> _int ) -> _short) sqr)
  (t  9 'callback3_byte_int_byte   (_fun (_fun _byte -> _int ) -> _byte) sqr)
  (t  9 'callback3_short_int_short (_fun (_fun _short -> _int ) -> _short) sqr)
  (t  9 'callback3_int_byte_byte   (_fun (_fun _int  -> _byte) -> _byte) sqr)
  (t  9 'callback3_int_short_short (_fun (_fun _int  -> _short) -> _short) sqr)
  (t  9 'callback3_byte_byte_byte  (_fun (_fun _byte -> _byte) -> _byte) sqr)
  (t  9 'callback3_short_short_short (_fun (_fun _short -> _short) -> _short) sqr)
  (t  9.0 'callback3_float_float_float (_fun (_fun _float -> _float) -> _float) sqr)
  (t  9.0 'callback3_double_double_double (_fun (_fun _double -> _double) -> _double) sqr)
  ;; ---
  (tc 3 'curry_int_int_int    (_fun _int  -> (_fun _int  -> _int )) 1 2)
  (tc 3 'curry_byte_int_int   (_fun _byte -> (_fun _int  -> _int )) 1 2)
  (tc 3 'curry_int_byte_int   (_fun _int  -> (_fun _byte -> _int )) 1 2)
  (tc 3 'curry_byte_byte_int  (_fun _byte -> (_fun _byte -> _int )) 1 2)
  (tc 3 'curry_int_int_byte   (_fun _int  -> (_fun _int  -> _byte)) 1 2)
  (tc 3 'curry_byte_int_byte  (_fun _byte -> (_fun _int  -> _byte)) 1 2)
  (tc 3 'curry_int_byte_byte  (_fun _int  -> (_fun _byte -> _byte)) 1 2)
  (tc 3 'curry_byte_byte_byte (_fun _byte -> (_fun _byte -> _byte)) 1 2)
  ;; ---
  (test* 14 'ho (_fun (_fun _int -> _int) _int -> (_fun _int -> _int))
         (lambda (p) ((p add1 3) 10)))
  (test* 14 'ho (_fun (_fun _int -> _int) _int -> (_fun _int -> _int))
         (lambda (p) ((p (ffi 'add1_int_int (_fun _int -> _int)) 3) 10)))
  (test* 14 'ho (_fun (_fun _int -> _int) _int -> (_fun _int -> _int))
         (lambda (p) ((p (ffi 'add1_byte_byte (_fun _byte -> _byte)) 3) 10)))
  ;; ---
  (test* 4 'g2 _int (lambda (p) p))
  ;; ---
  (set-ffi-obj! "g3" test-lib (_fun _int -> _int) add1)
  (t 4 'use_g3 (_fun _int -> _int) 3)
  (test* 4 'g3 _pointer (lambda (p) ((function-ptr p (_fun _int -> _int)) 3)))
  ;; Equivalentlly, 'g3 is a static variable that holds a function pointer. By
  ;; looking it up with _fpointer, we get its address, which then works
  ;; with ptr-ref to extract the function. (This pattern isn't a good idea, but
  ;; it's a useful extra check here.)
  (test* 7 'g3 _fpointer (lambda (p) ((ptr-ref (cast p _fpointer _pointer) (_fun _int -> _int)) 6)))
  ;; ---
  (test ((lambda (x f) ((f (+ x 1)) (- x 1)))
         3 (lambda (x) (lambda (y) (+ y (* x x)))))
        'hoho
        ((ffi 'hoho (_fun _int (_fun _int -> (_fun _int -> _int)) -> _int))
         3 (lambda (x) (lambda (y) (+ y (* x x))))))
  ;; ---
  (let ([qsort (get-ffi-obj 'qsort #f
                            (_fun (l    : (_list io _int len))
                                  (len  : _int = (length l))
                                  (size : _int = (ctype-sizeof _int))
                                  (compare : (_fun _pointer _pointer -> _int))
                                  -> _void -> l))])
    (test '(0 1 2 3 4 5 6 7 8 9)
          'qsort
          (qsort
           '(7 1 2 3 5 6 4 8 0 9)
           (lambda (x y)
             (let ([x (ptr-ref x _int)] [y (ptr-ref y _int)])
               (cond [(< x y) -1] [(> x y) +1] [else 0])))))
    (test '() 'qsort (qsort '() (lambda (x y) (error "bad!")))))
  ;; ---
  ;; test vectors
  (t 55 'grab7th (_fun _pointer -> _int ) #"012345678")
  (t 56 'grab7th (_fun _pointer -> _int ) (ptr-add #"012345678" 1))
  (t 52 'grab7th (_fun _pointer -> _int ) (ptr-add #"012345678" -3))
  (t 10 'vec4    (_fun (_list   i _int) -> _int) '(1 2 3 4))
  (t 10 'vec4    (_fun (_vector i _int) -> _int) '#(1 2 3 4))
  (t 10 'vec4    (_fun _cvector -> _int) (list->cvector '(1 2 3 4) _int))
  (t 10 'vec4    (_fun _pointer -> _int)
                 (cvector-ptr (list->cvector '(1 2 3 4) _int)))
  ;; ---
  ;; test passing and receiving structs
  (let ([_charint (_list-struct _byte _int)])
    (t 1212       'charint_to_int (_fun _charint -> _int)     '(12 1200))
    (t '(123 123) 'int_to_charint (_fun _int -> _charint)     123)
    (t '(255 1)   'charint_swap   (_fun _charint -> _charint) '(1 255)))
  ;; ---
  ;; test sending a callback for C to hold, preventing the callback from GCing
  (let ([with-keeper
         (lambda (k)
           (t (void) 'grab_callback
              (_fun (_fun #:keep k _int  -> _int) -> _void) sqr)
           (t 9      'use_grabbed_callback (_fun _int -> _int) 3)
           (collect-garbage) ; make sure it survives a GC
           (t 25     'use_grabbed_callback (_fun _int -> _int) 5)
           (collect-garbage)
           (t 81     'use_grabbed_callback (_fun _int -> _int) 9))])
    (with-keeper #t)
    (let ([b (box #f)])
      (with-keeper b)
      (set-box! b #f)))
  ;; ---
  ;; test exposing internal mzscheme functionality
  (test '(1 2)
        (get-ffi-obj 'scheme_make_pair #f (_fun _scheme _scheme -> _scheme))
        1 '(2))
  ;; ---
  ;; test arrays
  (let ([p (malloc _c7_list)]) ;; should allocate the right size
    (for ([i 7]) (ptr-set! p _byte i (+ i 10)))
    (test (for/list ([i 7]) (+ i 10)) cast p _pointer (_list o _byte 7))
    (t (for/list ([i 7]) (+ i 11)) 'increment_c_array (_fun _pointer -> (_list o _byte 7)) p)
    (let ([a (ptr-ref p (_array _byte 7))])
      (test 7 array-length a)
      (test 12 array-ref a 1)
      (ptr-set! p _byte 1 17)
      (test 17 array-ref a 1)))
  ;; Disable these tests on Windows/i386 where they fail (and crash the process
  ;; killing all other tests).  Matthew said: There's no consistent spec for
  ;; functions that return structures in i386 Windows.  Historically, gcc does
  ;; it one way, and MSVC another.  I think libffi expects the gcc protocol.
  ;; Newer versions of gcc may agree with msvc, so this may change in the
  ;; future.
  (unless (and (eq? 'windows (system-type)) (= 4 (compiler-sizeof '(* void))))
    (let ([v (for/list ([i 7]) i)])
      ;; pass array as pointer:
      ;; FIXME: these tests wrap the result pointer as non-GCable,
      ;; but _c7_list allocates the argument array as GCable.
      (t (for/list ([i 7]) (add1 i)) 'increment_c_array (_fun _c7_list -> (_list o _byte 7)) v)
      (t (for/list ([i 7]) (add1 i)) 'increment_c_array (_fun _c7_list -> _c7_list) v)
      (let ([r ((ffi 'increment_c_array (_fun _c7_list -> (_array _byte 7))) v)])
        (test 2 array-ref r 1))
      ;; Array within struct argument and result:
      (let* ([ic7i (make-ic7i 13 v 14)]
             [ic7i-2 ((ffi 'increment_ic7i (_fun _ic7i -> _ic7i)) ic7i)])
        (test v ptr-ref (cast ic7i _ic7i-pointer _pointer) _c7_list 'abs (ctype-sizeof _int))
        (test 13 ic7i-i1 ic7i)
        (test v ic7i-c7 ic7i)
        (test 14 ic7i-i2 ic7i)
        (test 14 ic7i-i1 ic7i-2)
        (test (map add1 v) ic7i-c7 ic7i-2)
        (test 15 ic7i-i2 ic7i-2)
        (let ([ic7i-3 ((ffi 'ic7i_cb (_fun _ic7i (_fun _ic7i -> _ic7i) -> _ic7i))
                       ic7i
                       (lambda (ic7i-4)
                         (test 12 ic7i-i1 ic7i-4)
                         (test (cons 255 (map sub1 (cdr v))) ic7i-c7 ic7i-4)
                         (test 13 ic7i-i2 ic7i-4)
                         (make-ic7i 2 (map (lambda (x) (- 252 x)) v) 9)))])
          (test 3 ic7i-i1 ic7i-3)
          (test (map add1 (map (lambda (x) (- 252 x)) v)) ic7i-c7 ic7i-3)
          (test 10 ic7i-i2 ic7i-3)))))
  ;; Two-dimensional array:
  ;; FIXME: same allocation bug for result as above
  (let ([v (for/list ([j 3]) (for/list ([i 7]) (+ i j)))]
        [v2 (for*/vector ([j 3] [i 7]) (+ i i j j))]
        [v3 (for/vector ([j 3]) (for/vector ([i 7]) (+ i i j j)))])
    (t v2 'increment_2d_array (_fun (_array/list _byte 3 7) -> (_vector o _byte 21)) v)
    (t v2 'increment_2d_array (_fun (_array/list (_array/list _byte 7) 3) -> (_vector o _byte 21)) v)
    (t v3 'increment_2d_array (_fun (_array/list (_array/list _byte 7) 3) -> (_array/vector _byte 3 7)) v))
  (t 0 'with_2d_array_cb (_fun (_fun (_array _byte 3 7) -> _void) -> _int)
     (lambda (a) (for ([i 3]) 
                   (for ([j 7]) 
                     (test (+ i (* 2 j)) array-ref a i j)
                     (array-set! a i j (+ (* 2 i) (* 2 j)))))))
  ;; ---
  ;; test union
  (let ([u (ptr-ref (malloc _borl) _borl)])
    (union-set! u 0 190)
    (test 190 union-ref u 0)
    (let ([u2 ((ffi 'increment_borl (_fun _int _borl -> _borl)) 0 u)])
      (test 191 union-ref u2 0))
    (union-set! u 1 (expt 2 19))
    (test (expt 2 19) union-ref u 1)
    (let ([u2 ((ffi 'increment_borl (_fun _int _borl -> _borl)) 1 u)])
      (test (add1 (expt 2 19)) union-ref u2 1)))
  (let ([u (ptr-ref (malloc _ic7iorl) _ic7iorl)])
    (union-set! u 1 190)
    (test 190 union-ref u 1)
    (let ([u2 ((ffi 'increment_ic7iorl (_fun _int _ic7iorl -> _ic7iorl)) 1 u)])
      (test 191 union-ref u2 1))
    (let ([v (for/list ([i 7]) i)])
      (union-set! u 0 (make-ic7i 3 v 88))
      (test 3 ic7i-i1 (union-ref u 0))
      (set-ic7i-i1! (union-ref u 0) 9)
      (test 9 ic7i-i1 (union-ref u 0))
      (let ([u2 ((ffi 'increment_ic7iorl (_fun _int _ic7iorl -> _ic7iorl)) 0 u)])
        (test 10 ic7i-i1 (union-ref u2 0))
        (test 89 ic7i-i2 (union-ref u2 0))
        (test (map add1 v) ic7i-c7 (union-ref u2 0)))))
  )

;; test setting vector elements
(let* ([x #b01010101]
       [l 20]
       [v (make-u8vector l x)])
  (do ([i 0 (add1 i)]) [(= i l)]
    (test x u8vector-ref v i)))

;; Test pointer arithmetic and memmove-like operations
(let ([p (malloc 10 _int)])
  (memset p 0 10 _int)
  (test 0 ptr-ref p _int)
  (test 0 ptr-ref (ptr-add p 3 _int) _int)
  (ptr-set! p _int 5)
  (test 5 ptr-ref p _int)
  (test 0 ptr-ref (ptr-add p 3 _int) _int)
  (memcpy p 3 p 0 1 _int)
  (test 5 ptr-ref (ptr-add p 3 _int) _int)

  ;; A Racket `int' is always 4 bytes.
  (memset p 1 17 9 _int)
  (test 5 ptr-ref p _int)
  (test #x11111111 ptr-ref (ptr-add p 4) _int)
  (memset p 2 18 (* 9 (ctype-sizeof _int)))
  (test #x12121212 ptr-ref (ptr-add p 4) _int)
  (test (if (system-big-endian?) #x00001212 #x12120005)
        ptr-ref p _int)

  (ptr-set! (ptr-add p 4 _int) _int 10)
  (ptr-set! (ptr-add p 5 _int) _int 11)
  (ptr-set! (ptr-add p 6 _int) _int 12)
  (memmove p 2 p 4 3 _int)
  (test 10 ptr-ref (ptr-add p 2 _int) _int)
  (test 11 ptr-ref (ptr-add p 3 _int) _int)
  (test 12 ptr-ref (ptr-add p 4 _int) _int)
  (memmove p (* 6 (ctype-sizeof _short)) p 8 12)
  (test 10 ptr-ref (ptr-add p 2 _int) _int)
  (test 10 ptr-ref (ptr-add p 3 _int) _int)
  (test 11 ptr-ref (ptr-add p 4 _int) _int)
  (test 12 ptr-ref (ptr-add p 5 _int) _int)
  (test 12 ptr-ref (ptr-add p 6 _int) _int)
  (memmove p p 8 4)
  (test 10 ptr-ref p _int)

  (test #f ptr-equal? p (ptr-add p 3))
  (test #t ptr-equal? p (ptr-add (ptr-add p 3) -3))
  (test #f ptr-equal? #f (ptr-add #f 8))
  (test #t ptr-equal? #f (ptr-add (ptr-add #f 8) -8))
  )

;; Test cstruct alignment
(let ()
  (define-cstruct _stuff ([a _int16]
                          [b _int32]
                          [c _int16])
    #:alignment 2)
  (define v (make-stuff 1 2 3))
  (test 8 ctype-sizeof _stuff)
  (test 3 stuff-c v)
  (test 1 ptr-ref v _int16 0)
  (test 3 ptr-ref v _int16 3))

;; Test intptr:
(let ([v (malloc _pointer)])
  (ptr-set! v _pointer (ptr-add #f 107))
  (test 107 ptr-ref v _intptr))

;; Test equality and hashing of c pointers:
(let ([seventeen1 (cast 17 _intptr _pointer)]
      [seventeen2 (cast 17 _intptr _pointer)]
      [seventeen3 (ptr-add (cast 13 _intptr _pointer) 4)]
      [sixteen (cast 16 _intptr _pointer)])
  (test #t equal? seventeen1 seventeen2)
  (test #t equal? seventeen1 seventeen3)
  (test #f equal? sixteen seventeen1)
  (test #t = (equal-hash-code seventeen1) (equal-hash-code seventeen2))
  (test #t = (equal-hash-code seventeen1) (equal-hash-code seventeen3))
  (let ([ht (make-hash)])
    (hash-set! ht seventeen1 'hello)
    (test 'hello hash-ref ht seventeen2 #f)
    (test 'hello hash-ref ht seventeen3 #f)))

;; Check proper handling of offsets:
(let ()
  (define scheme_make_sized_byte_string 
    (get-ffi-obj 'scheme_make_sized_byte_string #f (_fun _pointer _intptr _int -> _scheme)))
  ;; Non-gcable:
  (let ()
    (define p (cast (ptr-add #f 20) _pointer _pointer))
    (define d (scheme_make_sized_byte_string (ptr-add p 24)
                                             4
                                             0))
    (test 44 values (cast d _pointer _intptr)))
  ;; GCable:
  (let ()
    (define p (cast (ptr-add #f 20) _pointer _gcpointer))
    (define d (scheme_make_sized_byte_string (ptr-add p 24)
                                             4
                                             0))
    (test 44 values (cast d _gcpointer _intptr))))

;; test multi-dimension arrays
(let ([_t (_array _int 20 10 5)])
  (define ar (ptr-ref (malloc _t) _t))
  (define (t n)
    (test (void) array-set! ar 19 9 4 n)
    (test n      array-ref ar 19 9 4))
  (for-each t '(1 2 3)))

;; test casting of GCable and offset pointers:
(let ()
  (define _thing-pointer (_cpointer 'thing))
  (define _stuff-pointer (_cpointer 'stuff))
  
  (define p (cast (ptr-add (malloc 10) 5) _pointer _thing-pointer))
  (cpointer-gcable? p)
  (define q (cast p _thing-pointer _stuff-pointer))
  (test (cast p _pointer _intptr)
        cast q _pointer _intptr)
  (collect-garbage)
  (test (cast p _thing-pointer _intptr)
        cast q _stuff-pointer _intptr))

(let ()
  (struct foo (ptr)
    #:property prop:cpointer 0)
  
  (define a-foo (foo (malloc 16 'raw)))
  (free a-foo)
  (struct bar (ptr)
    #:property prop:cpointer (λ (s) (bar-ptr s)))
  
  (define a-bar (bar (malloc 16 'raw)))
  (free a-bar))

(delete-test-files)

(let ()
  (define _values (get-ffi-obj 'scheme_values #f (_fun _int (_list i _racket) -> _racket)))
  (test-values '(1 "b" three) (lambda () (_values 3 (list 1 "b" 'three)))))

(when (extflonum-available?)
  (define m (malloc _longdouble))
  (ptr-set! m _longdouble 13.57t0)
  (test 13.57t0 ptr-ref m _longdouble)

  (define v (extflvector 1.1t0 2.2t0 3.3t0))
  (test 3.3t0 extflvector-ref v 2)
  (test (void) extflvector-set! v 2 4.4t0)
  (test 4.4t0 extflvector-ref v 2)
  (test 2.2t0 ptr-ref (ptr-add (extflvector->cpointer v) (ctype-sizeof _longdouble)) _longdouble))

;; Check a corner of UTF-16 conversion:
(test "\U171D3" cast (cast "\U171D3" _string/utf-16 _gcpointer) _gcpointer _string/utf-16)

;; check async:
(when test-async?
  (define (check async like)
    (define foreign_thread_callback (get-ffi-obj 'foreign_thread_callback test-lib 
                                                 (_fun (_fun #:async-apply async
                                                             _intptr -> _intptr)
                                                       _intptr
                                                       (_fun -> _void)
                                                       -> _intptr)))
    (test (like 16) foreign_thread_callback (lambda (v) (add1 v)) 16 sleep))
  (check (lambda (f) (f)) add1)
  (check (box 20) (lambda (x) 20)))

;; check in-array
(let ()
  (define _t (_array _int 6))
  (define ar (ptr-ref (malloc _t) _t))
  (for ([i (array-length ar)])
    (array-set! ar i (+ 10 i)))

  (test '(10 11 12 13 14 15) values (for/list ([a (in-array ar)]) a))
  (test '(11 12 13 14 15) values (for/list ([a (in-array ar 1)]) a))
  (test '(11 12) values (for/list ([a (in-array ar 1 3)]) a))
  (test '(10 12 14) values (for/list ([a (in-array ar 0 #f 2)]) a))

  (define seq (in-array ar))
  (test '(10 11 12 13 14 15) values (for/list ([a seq]) a))

  (err/rt-test (in-array '(1 2 3)) exn:fail:contract?))


;; check cstruct serialization (define-serialize-cstruct must be at module level, can't use (let () ...))
(module mod-cstruct-serialize racket/base
  (require (for-syntax racket/base rackunit)
           rackunit
           racket/serialize
           racket/list
           ffi/unsafe
           ffi/serialize-cstruct)

  ;; malloc helper
  (define current-raw (make-parameter (make-hash)))
  (define-syntax-rule (with-free . body)
    (parameterize ([current-raw (make-hash)])
      (register-finalizer (current-raw) (lambda (h) (for ([(k v) h]) (free k))))
      (begin . body)))

  (define (malloc/register size/type)
    (define m (malloc (if (ctype? size/type)
                          (ctype-sizeof size/type)
                          size/type)
                      'raw))
    (hash-set! (current-raw) m #t)
    m)

  ;; run multiple times to better catching gc related errors
  (define num-runs 5)

  ;; --- syntax errors
  (begin-for-syntax
   (define-syntax-rule (check-exn+rx exn rx thunk)
     (begin
       (check-exn exn thunk)
       (check-exn rx thunk)))

   (check-exn+rx exn:fail:syntax? #rx"id must start with"
                 (lambda () (local-expand #'(define-serializable-cstruct F1a ([a _int])) 'module #f)))
   (check-exn+rx exn:fail:syntax? #rx"only allowed in module context"
                 (lambda () (local-expand #'(define-serializable-cstruct _F1b ([a _int])) 'expression #f)))
   (check-exn+rx exn:fail:syntax? #rx"#:property prop:serializable not allowed"
                 (lambda () (local-expand #'(define-serializable-cstruct _F1c ([a _int]) #:property prop:serializable #f)
                                          'module #f)))
   (check-exn+rx exn:fail:syntax? #rx"expected \\[field-id ctype\\]"
                 (lambda () (local-expand #'(define-serializable-cstruct _F1d ()) 'module #f))))


  ;; --- misc creation tests
  (define-values (prop:tmp prop:tmp? prop:tmp-ref)
    (make-struct-type-property 'testprop))

  (define-serializable-cstruct _M1 ([a _int]) #:alignment 8)
  (define-serializable-cstruct _M2 ([a _int]) #:property prop:tmp 'abc)
  (define-serializable-cstruct _M3 ([a _int]) #:malloc-mode 'atomic)
  (define-serializable-cstruct _M4 ([a _int]) #:malloc-mode 'raw)

  (let ()
    (check-not-exn (lambda () (make-M1 123)))
    (check-not-exn (lambda () (make-M2 123)))
    (check-not-exn (lambda () (make-M3/mode 123)))
    (check-not-exn (lambda ()
                     (define s (make-M4/mode 123))
                     (free s))))

  ;; --- test different types
  (define-serializable-cstruct _MISC ([d _double] [ad (_array _double 10)]
                                      [i _int] [ai (_array _int 10)])
    #:malloc-mode malloc/register)
  (define-serializable-cstruct _MISCPTR ([s _string] [as (_array _string 4)]
                                         [b _bytes] [ab (_array _bytes 4)])
    #:malloc-mode 'nonatomic)

  (for ([i num-runs])
    (with-free
     (define m (ptr-ref (malloc/register _MISC) _MISC))
     (set-MISC-d! m 1.234)
     (set-MISC-i! m 456)
     (for ([i 10])
       (array-set! (MISC-ad m) i (exact->inexact (+ i (* i .1))))
       (array-set! (MISC-ai m) i (+ i 5)))

     (check-not-exn (lambda () (deserialize (serialize m))))

     (define d  (deserialize (serialize m)))
     (collect-garbage)

     (check-equal? (MISC-d d) 1.234)
     (check-equal? (MISC-i d) 456)
     (for ([i 10])
       (check-equal? (array-ref (MISC-ad d) i) (exact->inexact (+ i (* i .1))))
       (check-equal? (array-ref (MISC-ai d) i) (+ i 5)))))

  (for ([i num-runs])
    (define m (ptr-ref (malloc _MISCPTR 'nonatomic) _MISCPTR))
    (set-MISCPTR-s! m "str")
    (set-MISCPTR-b! m #"bstr")
    (define s-list (list "abc" "def" "ghi" "jkl"))
    (define b-list (list #"mno" #"pqr" #"stu"))
    (for ([s s-list] [b b-list] [i (in-naturals)])
      (array-set! (MISCPTR-as m) i s)
      (array-set! (MISCPTR-ab m) i b))

    (check-not-exn (lambda () (deserialize (serialize m))))
    (define d  (deserialize (serialize m)))
    (collect-garbage)

    (check-equal? (MISCPTR-s d) "str")
    (check-equal? (MISCPTR-b d) #"bstr")

    (for ([s s-list] [b b-list] [i (in-naturals)])
      (check-equal? (array-ref (MISCPTR-as d) i) s)
      (check-equal? (array-ref (MISCPTR-ab d) i) b)))


  ;; --- simple failing tests
  (define-serializable-cstruct _F4 ([a _int]) #:malloc-mode 'abc)
  (define-serializable-cstruct _F40 ([a _fpointer]))
  (define-cstruct _F4E ([a _int]))
  (define-serializable-cstruct _F41 ([a _F4E]))
  (define-serializable-cstruct _F42 ([a _F4E-pointer]))
  (define-serializable-cstruct _F43 ([a (_array _F4E 3)]))
  (define-serializable-cstruct _F44 ([a (_array _F4E-pointer 3)]))
  (define-serializable-cstruct _F45 ([a _pointer]))
  (define-serializable-cstruct _F46 ([a (_array _pointer 3)]))
  (define-serializable-cstruct _F47 ([a _fpointer]))
  (define-serializable-cstruct _F48 ([a (_array _fpointer 3)]))

  (with-free
   (check-exn exn:fail? (lambda () (make-F4/mode 1)))
   (check-exn exn:fail? (lambda () (deserialize (serialize (make-F4 1)))))

   (define msg #rx"is not serializable")
   (check-exn msg (lambda () (serialize (ptr-ref (malloc _F40) _F40))))
   (check-exn msg (lambda () (serialize (ptr-ref (malloc _F40) _F40)))) ; same: test promise
   (check-exn msg (lambda () (serialize (ptr-ref (malloc _F41) _F41))))
   (check-exn msg (lambda () (serialize (ptr-ref (malloc _F42) _F42))))
   (check-exn msg (lambda () (serialize (ptr-ref (malloc _F43) _F43))))
   (check-exn msg (lambda () (serialize (ptr-ref (malloc _F44) _F44))))
   (check-exn msg (lambda () (serialize (ptr-ref (malloc _F45) _F45))))
   (check-exn msg (lambda () (serialize (ptr-ref (malloc _F46) _F46))))
   (check-exn msg (lambda () (serialize (ptr-ref (malloc _F47) _F47))))
   (check-exn msg (lambda () (serialize (ptr-ref (malloc _F48) _F48)))))

  ;; --- test shared pointers
  (define-serializable-cstruct _B ([a _int]) #:malloc-mode malloc/register)
  (define-serializable-cstruct _C ([b _B-pointer]) #:malloc-mode malloc/register)
  (define-serializable-cstruct _D ([b _B-pointer]) #:malloc-mode malloc/register)
  (define-serializable-cstruct _A ([c _C-pointer] [d _D-pointer]) #:malloc-mode malloc/register)

  (for ([i num-runs])
    (with-free
     (define b (make-B/mode 123))
     (define c (make-C/mode b))
     (define d (make-D/mode b))
     (define a (make-A/mode c d))

     (define s1 (serialize a))
     (define ds1 (deserialize s1))
     (check-true (ptr-equal? (C-b (A-c ds1)) (D-b (A-d ds1))))

     (collect-garbage)
     (check-equal? (C-b (A-c a)) (D-b (A-d a)))
     (check-equal? (C-b (A-c ds1)) (D-b (A-d ds1)))
     (check-equal? (B-a (C-b (A-c ds1))) 123)))


  ;; --- test cyclic pointers
  (define _CY1/fwd (_cpointer/null 'CY1 _pointer
                                   values
                                   (lambda (e) (cast e _pointer _CY1-pointer))))

  (define-serializable-cstruct _CY0 ([a _CY1/fwd]) #:malloc-mode malloc/register)
  (define-serializable-cstruct _CY1 ([a _CY0-pointer]) #:malloc-mode malloc/register)

  (for ([i num-runs])
    (with-free
     (define cy0 (make-CY0/mode #f))
     (define cy1 (make-CY1/mode cy0))
     (set-CY0-a! cy0 cy1)

     (define s2 (serialize cy1))
     (define ds2 (deserialize s2))

     (collect-garbage)
     (check-equal? cy1 (CY0-a cy0))
     (check-equal? cy0 (CY1-a (CY0-a cy0)))

     (check-equal? ds2 (CY0-a (CY1-a ds2)))))


  ;; --- self referencing struct
  (define-serializable-cstruct _SELF ([a _SELF-pointer/null]) #:malloc-mode malloc/register)

  (for ([i num-runs])
    (with-free
     (define self (make-SELF/mode #f))
     (set-SELF-a! self self)

     (cast self _pointer _uintptr)
     (cast (SELF-a self) _pointer _uintptr)

     (define s3 (serialize self))
     s3
     (define ds3 (deserialize s3))
     ds3

     (collect-garbage)
     (cast ds3 _pointer _uintptr)
     (cast (SELF-a ds3) _pointer _uintptr)
     (check-equal? ds3 (SELF-a ds3))

     ;; ---

     (define self2 (make-SELF/mode #f))
     (define ds4 (deserialize (serialize self2)))
     (collect-garbage)
     (check-equal? (SELF-a ds4) #f)))


  ;; --- struct pointer array and embedded struct array
  (define-serializable-cstruct _SINT ([a _int]) #:malloc-mode malloc/register)
  (define-serializable-cstruct _PTRAR ([a (_array _SINT-pointer/null 2 5)]) #:malloc-mode malloc/register)
  (define-serializable-cstruct _EMBAR ([a (_array _SINT 2 5)]) #:malloc-mode malloc/register)

  (for ([i num-runs])
    (with-free
     (define par (ptr-ref (malloc/register _PTRAR) _PTRAR))
     (for* ([i 2] [j 5])
       (array-set! (PTRAR-a par) i j (make-SINT/mode (+ 10 j (* i 5)))))

     (define ds (deserialize (serialize par)))
     (collect-garbage)

     (check-true
      (for*/and ([i 2] [j 5])
        (= (SINT-a (array-ref (PTRAR-a ds) i j)) (+ 10 j (* i 5)))))

     ;; --
     (define ear (ptr-ref (malloc/register _EMBAR) _EMBAR))
     (for* ([i 2] [j 5])
       (array-set! (EMBAR-a ear) i j (make-SINT/mode (+ 10 j (* i 5)))))

     (define ds2 (deserialize (serialize ear)))
     (collect-garbage)

     (check-true
      (for*/and ([i 2] [j 5])
        (= (SINT-a (array-ref (EMBAR-a ds2) i j)) (+ 10 j (* i 5)))))))

  ;; --- array with embedded struct with pointer
  (define-serializable-cstruct _TP ([a _int]) #:malloc-mode malloc/register)
  (define-serializable-cstruct _TB ([a _TP-pointer]) #:malloc-mode malloc/register)
  (define-serializable-cstruct _TS ([a (_array _TB 2)]) #:malloc-mode malloc/register)

  (with-free
   (define p (make-TP/mode 65))
   (define s (ptr-ref (malloc/register _TS) _TS))

   (set-TB-a! (array-ref (TS-a s) 0) p)
   (set-TB-a! (array-ref (TS-a s) 1) p)

   (define ds (deserialize (serialize s)))
   (memset p 0 1 _TP)

   (check-equal? (TB-a (array-ref (TS-a ds) 0))
                 (TB-a (array-ref (TS-a ds) 1)))
   (check-equal? 65 (TP-a (TB-a (array-ref (TS-a ds) 0)))))


  ;; --- inplace tests
  (define-serializable-cstruct _NOIN ([a _int]))

  (define-serializable-cstruct _INS ([a _int]) #:serialize-inplace)

  (define-serializable-cstruct _IND ([a _int]) #:deserialize-inplace)

  (define-serializable-cstruct _INSD ([a _int])
    #:serialize-inplace #:deserialize-inplace
    #:malloc-mode (lambda (_) (error "should not get here")))

  ;; non-inplace + modification
  (let ()
    (define noin (make-NOIN/mode 123))
    (define s (serialize noin))
    (set-NOIN-a! noin 0)

    (define ds (deserialize s))
    (check-equal? (NOIN-a ds) 123)

    (for ([e (flatten s)])
      (when (bytes? e)
        (memset e 0 (bytes-length e)  _byte)))
    (check-equal? (NOIN-a ds) 123))

  ;; inplace serialize and test modification
  (let ()
    (define ins (make-INS/mode 123))

    (check-not-exn (lambda () (serialize ins)))
    (define s (serialize ins))
    (check-not-exn (lambda () (deserialize s)))

    ;; unmodified
    (define ds1 (deserialize s))
    (check-equal? 123 (INS-a ds1))

    ;; modified
    (set-INS-a! ins 456)
    (define ds2 (deserialize s))
    (check-equal? 456 (INS-a ds2)))

  ;; inplace deser
  (let ()
    (define ind (ptr-ref (malloc _IND) _IND))
    (set-IND-a! ind 123)

    (check-not-exn (lambda () (serialize ind)))
    (define s (serialize ind))
    (check-not-exn (lambda () (deserialize s)))

    (define ds (deserialize s))
    (check-equal? 123 (IND-a ds)))

  ;; both inplace, should never malloc
  (let ()
    (define insd (ptr-ref (malloc _INSD) _INSD))
    (set-INSD-a! insd 123)

    (check-not-exn (lambda () (serialize insd)))
    (define s (serialize insd))
    (check-not-exn (lambda () (deserialize s)))
    (define ds (deserialize s))

    (check-equal? 123 (INSD-a ds))))

(require (only-in 'mod-cstruct-serialize))


;; ----------------------------------------

(report-errs)

#| --- ignore everything below ---

The following is some random Scheme and C code, with some things that should be
added.

-------------------------------------------------------------------------------
(define foo-struct1
  (get-ffi-obj "foo_struct1" "junk/foo.so" (_fun _foo -> _int)))
(define foo-struct2
  (get-ffi-obj "foo_struct2" "junk/foo.so" (_fun _foo -> _foo)))
(printf ">>> foo-struct1(32,23) = ~s\n" (foo-struct1 '(32 23)))
(printf ">>> foo-struct2(32,23) = ~s\n" (foo-struct2 '(32 23)))
(exit)

(define-cstruct _foo ((b _byte) (i _int)))
(printf ">>> struct-type: ~s\n" _foo)
(printf ">>>        size: ~s\n" (ctype-sizeof _foo))
(printf ">>>   alignment: ~s\n" (ctype-alignof _foo))
(define foo-struct1
  (get-ffi-obj "foo_struct1" "junk/foo.so" (_fun _foo -> _int)))
(define foo-struct2
  (get-ffi-obj "foo_struct2" "junk/foo.so" (_fun _foo -> _foo)))
(define foostruct (make-foo 32 23))
(printf ">>> foostruct = ~s = ~s,~s\n"
        foostruct (foo-b foostruct) (foo-i foostruct))
(set-foo-b! foostruct 34)
(set-foo-i! foostruct 43)
(printf ">>> foostruct = ~s = ~s,~s\n"
        foostruct (foo-b foostruct) (foo-i foostruct))
(printf ">>> foo-struct1(34,43) = ~s\n" (foo-struct1 foostruct))
(printf ">>> foo-struct2(34,43) = ~s\n"
        (let ([x (foo-struct2 foostruct)]) (list (foo-b x) (foo-i x))))
(exit)

(printf ">>> g3 = ~s\n"
        (get-ffi-obj "g3" "junk/foo.so" _string))
(printf ">>> g3 := \"blah\" -> ~s\n"
        (set-ffi-obj! "g3" "junk/foo.so" _string "blah"))
(printf ">>> g3 = ~s\n"
        (get-ffi-obj "g3" "junk/foo.so" _string))
(printf ">>> g3 := add1 -> ~s\n"
        (set-ffi-obj! "g3" "junk/foo.so" _scheme add1))
(printf ">>> g3 = ~s\n"
        (get-ffi-obj "g3" "junk/foo.so" _scheme))
(printf ">>> g3 := s->c[int->int](add1) -> ~s\n"
        (set-ffi-obj! "g3" "junk/foo.so" (_fun _int -> _int) add1))
(printf ">>> (ptr) g3 = ~s\n"
        (get-ffi-obj "g3" "junk/foo.so" _pointer))
(printf ">>> (int->int) g3 = ~s\n"
        (get-ffi-obj "g3" "junk/foo.so" (_fun _int -> _int)))
(printf ">>> use_g3(3) = ~s\n"
        ((get-ffi-obj "use_g3" "junk/foo.so" (_fun _int -> _int)) 3))
(printf ">>> g3(3) = ~s\n"
        ((ptr-ref (get-ffi-obj "g3" "junk/foo.so" _pointer)
                  (_fun _int -> _int))
         3))

(define block (malloc 4 _int))
(ptr-set! block _int 0 2)
(ptr-set! block _int 1 1)
(ptr-set! block _int 2 4)
(ptr-set! block _int 3 3)
(printf ">>> block= ~s\n" (list (ptr-ref block _int 0)
                                (ptr-ref block _int 1)
                                (ptr-ref block _int 2)
                                (ptr-ref block _int 3)))

(printf ">>> foo_none() = ~s\n"
        ((get-ffi-obj "foo_none" "./junk/foo.so" (_fun -> _void))))
(printf ">>> sleep(1) = ~s\n"
        ((get-ffi-obj "sleep" #f (_fun _int -> _void)) 1))
(printf ">>> fprintf(stderr,\"...\",3) = ~s\n"
        ((get-ffi-obj "fprintf" #f (_fun _pointer _string _int -> _void))
         (get-ffi-obj "stderr" #f _pointer)
         "using fprintf on stderr with an argument: %d\n"
         3))

(define (foo-test name args type)
  (define res (apply (get-ffi-obj name "junk/foo.so" type) args))
  (printf ">>> ~a~s -> ~s\n" name args res)
  res)
(foo-test "foo_int"    '(3)     (_fun _int -> _int))
(foo-test "foo_int"    '(3)     (_fun _fixint -> _fixint))
(foo-test "foo_int3"   '(1 2 3) (_fun _int _int _int -> _int))
(foo-test "foo_int3"   '(1 2 3) (_fun _fixint _fixint _fixint -> _fixint))
(foo-test "foo_long3"  '(1 2 3) (_fun _long _long _long -> _long))
(foo-test "foo_long3"  '(1 2 3) (_fun _fixnum _fixnum _fixnum -> _fixnum))
(foo-test "foo_char"   '(3)     (_fun _byte -> _byte))
(foo-test "foo_short"  '(22)    (_fun _word -> _word))
(foo-test "foo_ushort" '(22)    (_fun _ushort -> _ushort))
(foo-test "foo_byte"   '(22)    (_fun _byte -> _byte))
(foo-test "foo_ubyte"  '(22)    (_fun _ubyte -> _ubyte))
(foo-test "foo_byte"   '(156)   (_fun _byte -> _byte))
(foo-test "foo_ubyte"  '(156)   (_fun _ubyte -> _ubyte))
(foo-test "foo_double" '(81.0)  (_fun _double -> _double))
(foo-test "foo_double" '(81.0f0)  (_fun _double -> _double))
(foo-test "foo_float"  '(81.0)  (_fun _float -> _float))
(foo-test "foo_float"  '(81.0f0)  (_fun _float -> _float))

(exit) ;=======================================================================
(newline)
(define x1 "foo")
(define x2 (foo-test "foo_string" (list x1) '(string) 'string))
(printf ">>> now x1 = ~s\n" x1)
(string-set! x2 1 #\X)
(foo-test "foo_string" '(#f) '(string) 'string)

(newline)
(printf ">>> sizeof(int) = ~s\n" (ffi-size-of 'int))
'(let loop ((l '()))
  (eprintf ">>> ~s\n" (length l))
  (when (> (length l) 50)
    (set-cdr! (cddddr l) '())
    (collect-garbage))
  (sleep 1)
  ;; behavior with flags of this: atomic -> much faster, uncollectable -> crash
  (loop (cons (malloc 'byte 1000000) l)))
(define block1 (malloc 'int 4))
(printf ">>> block1 = ~s\n" block1)
(ffi-ptr-set! block1 'ulong 0 67305985)
(printf ">>> first uint of block1 = ~s\n" (ffi-ptr-ref block1 'uint))
(define block2 (malloc block1 'int 4))
(printf ">>> first 4 bytes of block2 = ~s\n"
        (list (ffi-ptr-ref block2 'ubyte 0) (ffi-ptr-ref block2 'ubyte 1)
              (ffi-ptr-ref block2 'ubyte 2) (ffi-ptr-ref block2 'ubyte 3)))
(ffi-ptr-set! block1 'ulong 0 11)
(ffi-ptr-set! block1 'ulong 1 22)
(ffi-ptr-set! block1 'ulong 2 33)
(ffi-ptr-set! block1 'ulong 3 44)
;(ffi-ptr-set! block1 'ulong 'abs 1 22)
(printf ">>> [0] -> ~s\n" (ffi-ptr-ref block1 'ulong 0))
(printf ">>> [1] -> ~s\n" (ffi-ptr-ref block1 'ulong 1))
(printf ">>> [abs 1] -> ~s\n" (ffi-ptr-ref block1 'ulong 'abs 1))
(printf ">>> [abs 4] -> ~s\n" (ffi-ptr-ref block1 'ulong 'abs 4))
(printf ">>> ptr-equal? b1 b2     -> ~s\n" (ffi-equal-ptr? block1 block2))
(printf ">>> ptr-equal? b1 NULL   -> ~s\n" (ffi-equal-ptr? block1 #f))
(printf ">>> ptr-equal? b1 b1     -> ~s\n" (ffi-equal-ptr? block1 block1))
(printf ">>> ptr-equal? NULL NULL -> ~s\n" (ffi-equal-ptr? #f #f))

(newline)
(define (final x) (printf "GC>>> ~s[0]=~s\n" x (ffi-ptr-ref x 'ulong)))
(printf ">>> set finalizer b1 -> ~s\n" (register-finalizer block1 final))
(printf ">>> set finalizer b1 -> ~s\n" (register-finalizer block1 final))
(printf ">>> set finalizer b2 -> ~s\n" (register-finalizer block2 final))
(printf ">>> set finalizer b2 -> ~s\n" (register-finalizer block2 #f))
(printf "Collecting garbage...\n")
(collect-garbage)
(printf "Clearing block1,2 and GCing again...\n")
(set! block1 #f) (set! block2 #f)
(collect-garbage)

(newline)
(define (foo x) (+ x x x))
(define a (malloc 'int 20))
(ffi-ptr-set! a 'scheme foo)
(define b (ffi-ptr-ref a 'pointer))
(ffi-ptr-set! a 'scheme 1 b)
(printf ">>> a[1] as a pointer object = ~s\n" (ffi-ptr-ref a 'scheme 1))
(ffi-ptr-set! a 'pointer 1 b)
(printf ">>> a[1] as the pointer value = ~s\n" (ffi-ptr-ref a 'scheme 1))
(printf ">>> (a[1] 12) -> ~s\n" ((ffi-ptr-ref a 'scheme 1) 12))

(newline)
(define c:int1 (make-ctype c:int sub1 add1))
(define c:foo
  (make-ctype
   c:foo c:int1
   (lambda (x)
     (unless (memq x '(foo1 foo2 bar1 bar2))
       (raise-type-error 'foo->int1 "foo" x))
     (cdr (assq x '((foo1 . 1) (foo2 . 2) (bar1 . 3) (bar2 . 4)))))
   (lambda (x)
     (cdr (or (assq x '((1 . foo1) (2 . foo2) (3 . bar1) (4 . bar2)))
              '(#f . #f))))))

(printf ">>> sizeof(fooo) = ~s\n" (ffi-size-of 'fooo))
(foo-test "foo_foo" (list 'foo1) '(fooo) 'fooo)
(foo-test "foo_foo" (list 'foo2) '(fooo) 'fooo)
(foo-test "foo_foo" (list 'bar1) '(fooo) 'fooo)
(foo-test "foo_foo" (list 'bar2) '(fooo) 'fooo)
(foo-test "foo_foo" (list 'fooo) '(fooo) 'fooo)
-------------------------------------------------------------------------------
void foo_none() {
  fprintf(stderr, "===>>> in foo_none()\n");
}

#define U unsigned

int     foo_int    (int x)     { return x*2; }
int     foo_int3   (int x, int y, int z) { return x*y*z; }
long    foo_long3  (long x, long y, long z) { return x*y*z; }
char    foo_char   (char x)    { return toupper(x); }
short   foo_short  (short x)   { return x + 40000; }
U short foo_ushort (U short x) { return x + 40000; }
char    foo_byte   (char x)    { return x + 200; }
U char  foo_ubyte  (U char x)  { return x + 200; }
double  foo_double (double x)  { return sqrt(x); }
float   foo_float  (float x)   { return sqrt(x); }

char*   foo_string (char* x) {
  if (x==NULL) {
    printf("===>>> Got NULL, HOME=\"%s\"\n", getenv("HOME"));
    return NULL;
  } else {
    x[0] = toupper(x[0]);
    return getenv("HOME");
  }
}

int foo_foo(int x) { return x^1; }

-------------------------------------------------------------------------------
|#
