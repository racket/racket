
(load-relative "loadtest.rktl")

(Section 'foreign)

(require mzlib/foreign)
(unsafe!)

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

(when (eq? 'windows (system-type))
  (let* ([concat string-append]
         [studio  "c:/Program Files/Microsoft Visual Studio 8"]
         [scommon (concat studio "/Common7")]
         [vc      (concat studio "/VC")])
    (putenv "PATH"    (concat (getenv "PATH")
                              ";" vc "/bin"
                              ";" scommon "/IDE"
                              ";" scommon "/Tools"
                              ";" scommon "/Tools/bin"))
    (putenv "INCLUDE" (concat ";" vc "/include"
                              ";" vc "/atlmfc/include"
                              ";" vc "/PlatformSDK/Include"))
    (putenv "LIB"     (concat ";" vc "/lib"
                              ";" vc "/atlmfc/lib"
                              ";" vc "/PlatformSDK/lib"))))

(require dynext/compile dynext/link mzlib/etc)
(define delete-test-files
  (let ([c  (build-path (this-expression-source-directory) "foreign-test.c")]
        [o  (build-path (current-directory) "foreign-test.o")]
        [so (build-path (current-directory)
                        (bytes->path (bytes-append #"foreign-test"
                                                   (system-type 'so-suffix))))])
    (when (file-exists? o) (delete-file o))
    (when (file-exists? so) (delete-file so))
    (parameterize ([current-standard-link-libraries '()])
      (compile-extension #t c o '())
      (link-extension #t (list o) so))
    (lambda ()
      (when (file-exists? o) (delete-file o))
      (when (file-exists? so) (delete-file so)))))

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
  (t  9 'callback3_int_int_int    (_fun (_fun _int  -> _int ) -> _int ) sqr)
  (t  79 'callback3_int_int_int   (_fun (_fun _int  -> _int ) -> _int ) #f) ; NULL allowed as function pointer
  (t  9 'callback3_int_int_int    (_fun _pointer -> _int ) (function-ptr sqr (_fun _int  -> _int ))) ; callback allowed as pointer
  (t  9 'callback3_byte_int_int   (_fun (_fun _byte -> _int ) -> _int ) sqr)
  (t  9 'callback3_int_byte_int   (_fun (_fun _int  -> _byte) -> _int ) sqr)
  (t  9 'callback3_byte_byte_int  (_fun (_fun _byte -> _byte) -> _int ) sqr)
  (t  9 'callback3_int_int_byte   (_fun (_fun _int  -> _int ) -> _byte) sqr)
  (t  9 'callback3_byte_int_byte  (_fun (_fun _byte -> _int ) -> _byte) sqr)
  (t  9 'callback3_int_byte_byte  (_fun (_fun _int  -> _byte) -> _byte) sqr)
  (t  9 'callback3_byte_byte_byte (_fun (_fun _byte -> _byte) -> _byte) sqr)
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
  ;; with ptr-ref to extract the function.
  (test* 7 'g3 _fpointer (lambda (p) ((ptr-ref p (_fun _int -> _int)) 6)))
  ;; ---
  (test ((lambda (x f) ((f (+ x 1)) (- x 1)))
         3 (lambda (x) (lambda (y) (+ y (* x x)))))
        'hoho
        ((ffi 'hoho (_fun _int (_fun _int -> (_fun _int -> _int)) -> _int))
         3 (lambda (x) (lambda (y) (+ y (* x x))))))
  ;; ---
  (test '(0 1 2 3 4 5 6 7 8 9)
        'qsort
        ((get-ffi-obj 'qsort #f
                      (_fun (l    : (_list io _int len))
                            (len  : _int = (length l))
                            (size : _int = (ctype-sizeof _int))
                            (compare : (_fun _pointer _pointer -> _int))
                            -> _void -> l))
         '(7 1 2 3 5 6 4 8 0 9)
         (lambda (x y)
           (let ([x (ptr-ref x _int)] [y (ptr-ref y _int)])
             (cond [(< x y) -1] [(> x y) +1] [else 0])))))
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

(delete-test-files)

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
(foo-test "foo_float"  '(81.0)  (_fun _float -> _float))

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
