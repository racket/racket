#lang scheme/base

(require rnrs/enums-6
         scheme/mpair)

(provide endianness
         native-endianness
         (rename-out [bytes? bytevector?]
                     [bytes-length bytevector-length]
                     [bytes=? bytevector=?]
                     [bytes-copy bytevector-copy]
                     [bytes-ref bytevector-u8-ref]
                     [bytes-set! bytevector-u8-set!])
         bytevector-copy!
         bytevector->u8-list
         u8-list->bytevector
         make-bytevector 
         bytevector-fill!
         bytevector-s8-ref
         bytevector-s8-set!

         bytevector-u16-ref
         bytevector-s16-ref
         bytevector-u16-native-ref
         bytevector-s16-native-ref
         bytevector-u16-set!
         bytevector-s16-set!
         bytevector-u16-native-set!
         bytevector-s16-native-set!

         bytevector-u32-ref
         bytevector-s32-ref
         bytevector-u32-native-ref
         bytevector-s32-native-ref
         bytevector-u32-set!
         bytevector-s32-set!
         bytevector-u32-native-set!
         bytevector-s32-native-set!

         bytevector-u64-ref
         bytevector-s64-ref
         bytevector-u64-native-ref
         bytevector-s64-native-ref
         bytevector-u64-set!
         bytevector-s64-set!
         bytevector-u64-native-set!
         bytevector-s64-native-set!

         bytevector-uint-ref
         bytevector-sint-ref
         bytevector-uint-set!
         bytevector-sint-set!

         bytevector-ieee-single-ref
         bytevector-ieee-single-native-ref
         bytevector-ieee-single-set!
         bytevector-ieee-single-native-set!
         bytevector-ieee-double-ref
         bytevector-ieee-double-native-ref
         bytevector-ieee-double-set!
         bytevector-ieee-double-native-set!

         bytevector->uint-list
         bytevector->sint-list
         uint-list->bytevector
         sint-list->bytevector

         string->utf8
         string->utf16
         string->utf32
         utf8->string
         utf16->string
         utf32->string)

(define-enumeration endianness (big little) endianness-set)

(define (native-endianness)
  (if (system-big-endian?)
      (endianness big)
      (endianness little)))

(define (bytevector-copy! src src-start target target-start k)
  (bytes-copy! target target-start src src-start (+ src-start k)))

(define (bytevector->u8-list bv)
  (list->mlist (bytes->list bv)))

(define (u8-list->bytevector l)
  (list->bytes (mlist->list l)))

(define (make-bytevector k [fill 0])
  (make-bytes k (convert-fill 'make-bytevector fill)))

(define (convert-fill who fill)
  (cond
   [(byte? fill) fill]
   [(and (exact-integer? fill)
         (<= -128 fill -1))
    (+ fill 256)]
   [else (raise-type-error who
                           "exact integer in [128, 255]"
                           fill)]))


(define (bytevector-fill! bytes [fill 0])
  (bytes-fill! bytes (convert-fill 'bytevector-fill! fill)))

;; ----------------------------------------

(define (bytevector-s8-ref bytes k)
  (let ([v (bytes-ref bytes k)])
    (if (v . > . 127)
        (- v 256)
        v)))

(define (bytevector-s8-set! bytes k v)
  (bytes-set! bytes k (convert-fill 'bytevector-s8-set! v)))

(define (check-endian endianness)
  (unless (or (eq? endianness 'little)
              (eq? endianness 'big))
    (raise-type-error 'bytevector-operation "'big or 'little" endianness)))

(define (make-integer-ops size)
  (values
   ;; uXX-ref
   (lambda (bytes k endianness)
     (check-endian endianness)
     (integer-bytes->integer bytes #f (eq? endianness 'big) k (+ k size)))
   ;; sXX-ref
   (lambda (bytes k endianness)
     (check-endian endianness)
     (integer-bytes->integer bytes #t (eq? endianness 'big) k (+ k size)))
   ;; uXX-native-ref
   (lambda (bytes k)
     (integer-bytes->integer bytes #f (system-big-endian?) k (+ k size)))
   ;; sXX-native-ref
   (lambda (bytes k)
     (integer-bytes->integer bytes #t (system-big-endian?) k (+ k size)))
   ;; uXX-set!
   (lambda (bytes k n endianness)
     (check-endian endianness)
     (integer->integer-bytes n size #f (eq? endianness 'big) bytes k)
     (void))
   ;; sXX-set!
   (lambda (bytes k n endianness)
     (check-endian endianness)
     (integer->integer-bytes n size #t (eq? endianness 'big) bytes k)
     (void))
   ;; uXX-native-set!
   (lambda (bytes k n)
     (integer->integer-bytes n size #f (system-big-endian?) bytes k)
     (void))
   ;; sXX-native-set!
   (lambda (bytes k n)
     (integer->integer-bytes n size #t (system-big-endian?) bytes k)
     (void))))

(define-values (bytevector-u16-ref
                bytevector-s16-ref
                bytevector-u16-native-ref
                bytevector-s16-native-ref
                bytevector-u16-set!
                bytevector-s16-set!
                bytevector-u16-native-set!
                bytevector-s16-native-set!)
  (make-integer-ops 2))

(define-values (bytevector-u32-ref
                bytevector-s32-ref
                bytevector-u32-native-ref
                bytevector-s32-native-ref
                bytevector-u32-set!
                bytevector-s32-set!
                bytevector-u32-native-set!
                bytevector-s32-native-set!)
  (make-integer-ops 4))
   
(define-values (bytevector-u64-ref
                bytevector-s64-ref
                bytevector-u64-native-ref
                bytevector-s64-native-ref
                bytevector-u64-set!
                bytevector-s64-set!
                bytevector-u64-native-set!
                bytevector-s64-native-set!)
  (make-integer-ops 8))

(define (check-offset size k)
  (unless (and (exact-nonnegative-integer? k)
               (zero? (bitwise-and (sub1 size) k)))
    (raise-type-error 'bytevector-operation 
                      (format "exact nonnegative integer multiple of ~a" size)
                      k)))

(define (make-ieee-ops size)
  (values
   ;; -ref
   (lambda (bytes k endianness)
     (check-endian endianness)
     (floating-point-bytes->real bytes (eq? endianness 'big) k (+ k size)))
   ;; -native-ref
   (lambda (bytes k)
     (check-offset size k)
     (floating-point-bytes->real bytes (system-big-endian?) k (+ k size)))
   ;; -set!
   (lambda (bytes k n endianness)
     (check-endian endianness)
     (real->floating-point-bytes n size (eq? endianness 'big) bytes k))
   ;; -native-set!
   (lambda (bytes k n)
     (check-offset size k)
     (real->floating-point-bytes n size (system-big-endian?) bytes k)
     (void))))

(define-values (bytevector-ieee-single-ref
                bytevector-ieee-single-native-ref
                bytevector-ieee-single-set!
                bytevector-ieee-single-native-set!)
  (make-ieee-ops 4))

(define-values (bytevector-ieee-double-ref
                bytevector-ieee-double-native-ref
                bytevector-ieee-double-set!
                bytevector-ieee-double-native-set!)
  (make-ieee-ops 8))

;; ----------------------------------------

(define (bytevector-int-ref who bstr k endianness size)
  (unless (bytes? bstr)
    (raise-type-error who "bytevector" bstr))
  (unless (exact-nonnegative-integer? k)
    (raise-type-error who "exact nonnegative integer" k))
  (check-endian endianness)
  (unless (exact-positive-integer? size)
    (raise-type-error who "exact positive integer" size))
  (unless (<= (+ k size) (bytes-length bstr))
    (error who "specified range [~a, ~a) beyond string range [0, ~a)"
           k (+ k size) (bytes-length bstr)))
  (for/fold ([r 0])
      ([i (in-range size)])
    (+ (arithmetic-shift r 8)
       (bytes-ref bstr (+ (if (eq? endianness 'big)
                              i 
                              (- size i 1))
                          k)))))

(define (bytevector-uint-ref bstr k endianness size)
  (bytevector-int-ref 'bytevector-uint-ref bstr k endianness size))
           
(define (bytevector-sint-ref bstr k endianness size)
  (let ([v (bytevector-int-ref 'bytevector-sint-ref bstr k endianness size)]
        [max (sub1 (arithmetic-shift 1 (sub1 (* size 8))))])
    (if (v . > . max)
        (- v (* 2 (add1 max)))
        v)))


(define (bytevector-int-set! who bstr k n orig-n endianness size bit-size)
  (unless (bytes? bstr)
    (raise-type-error who "bytevector" bstr))
  (unless (exact-nonnegative-integer? k)
    (raise-type-error who "exact nonnegative integer" k))
  (check-endian endianness)
  (unless (exact-positive-integer? size)
    (raise-type-error who "exact positive integer" size))
  (unless (<= (+ k size) (bytes-length bstr))
    (error who "specified target range [~a, ~a) beyond string range [0, ~a)"
           k (+ k size) (bytes-length bstr)))
  (unless ((integer-length orig-n) . <= . bit-size)
    (error who "integer does not fit into ~a bytes: ~e"
           size orig-n))
  (for/fold ([n n])
      ([i (in-range size)])
    (bytes-set! bstr (+ (if (eq? endianness 'little)
                            i 
                            (- size i 1))
                        k)
                (bitwise-and n 255))
    (arithmetic-shift n -8))
  (void))

(define (bytevector-uint-set! bstr k n endianness size)
  (unless (exact-nonnegative-integer? n)
    (raise-type-error 'bytevector-uint-set! "exact nonnegative integer" n))
  (bytevector-int-set! 'bytevector-uint-set! bstr k n n endianness size (* size 8)))

(define (bytevector-sint-set! bstr k n endianness size)
  (let ([pos-n (if (negative? n)
                   (+ n (arithmetic-shift 1 (* 8 size)))
                   n)])
    (bytevector-int-set! 'bytevector-sint-set! bstr k pos-n n endianness size (sub1 (* size 8)))))

(define (bytevector->int-list who ref bv endianness size)
  (unless (bytes? bv)
    (raise-type-error who "bytevector" bv))
  (check-endian endianness)
  (unless (exact-positive-integer? size)
    (raise-type-error who "exact positive integer" size))
  (unless (zero? (modulo (bytes-length bv) size))
    (raise-mismatch-error who "bytevector length is not a mulitple of given size: " size))
  (list->mlist
   (for/list ([k (in-range 0 (bytes-length bv) size)])
     (ref bv k endianness size))))

(define (bytevector->uint-list bv endianness size)
  (bytevector->int-list 'bytevector->uint-list bytevector-uint-ref bv endianness size))

(define (bytevector->sint-list bv endianness size)
  (bytevector->int-list 'bytevector->sint-list bytevector-sint-ref bv endianness size))

(define (int-list->bytevector who signed? set l endianness size)
  (unless (mlist? l)
    (raise-type-error who "list" l))
  (check-endian endianness)
  (unless (exact-positive-integer? size)
    (raise-type-error who "exact positive integer" size))
  (let* ([l (mlist->list l)]
         [len (length l)]
         [bv (make-bytes (* size len))])
    (for ([v (in-list l)]
          [k (in-naturals)])
      (set bv (* k size) v endianness size))
    bv))

(define (uint-list->bytevector l endianness size)
  (int-list->bytevector 'uint-list->bytevector #f bytevector-uint-set! l endianness size))

(define (sint-list->bytevector l endianness size)
  (int-list->bytevector 'sint-list->bytevector #f bytevector-sint-set! l endianness size))

;; ----------------------------------------

(define (string->utf8 str)
  (string->bytes/utf-8 str))

(define (string->utf16 str [endianness 'big])
  (check-endian endianness)
  (let ([big? (eq? endianness 'big)])
    (let loop ([pos (string-length str)]
               [accum null])
      (if (zero? pos)
          (list->bytes accum)
          (let* ([pos (sub1 pos)]
                 [c (string-ref str pos)]
                 [v (char->integer c)])
            (if (v . >= . #x10000)
                (let ([v2 (- v #x10000)])
                  (let-values ([(a b c d)
                                (values (bitwise-ior #xD8
                                                     (arithmetic-shift v2 -18))
                                        (bitwise-and #xFF
                                                     (arithmetic-shift v2 -10))
                                        (bitwise-ior #xDC
                                                     (bitwise-and #x3
                                                                  (arithmetic-shift v2 -8)))
                                        (bitwise-and v2 #xFF))])
                    (if big?
                        (loop pos (list* a b c d accum))
                        (loop pos (list* b a d c accum)))))
                (let-values ([(hi lo)
                              (values (arithmetic-shift v -8)
                                      (bitwise-and v 255))])
                  (if big?
                      (loop pos (list* hi lo accum))
                      (loop pos (list* lo hi accum))))))))))

(define (string->utf32 str [endianness 'big])
  (check-endian endianness)
  (let ([bstr (make-bytes (* 4 (string-length str)))]
        [big? (eq? endianness 'big)])
    (for ([c (in-string str)]
          [i (in-naturals)])
      (let* ([v (char->integer c)]
             [a (arithmetic-shift v -24)]
             [b (bitwise-and #xFF (arithmetic-shift v -16))]
             [c (bitwise-and #xFF (arithmetic-shift v -8))]
             [d (bitwise-and #xFF v)]
             [pos (* i 4)])
        (if big?
            (begin
              (bytes-set! bstr pos a)
              (bytes-set! bstr (+ 1 pos) b)
              (bytes-set! bstr (+ 2 pos) c)
              (bytes-set! bstr (+ 3 pos) d))
            (begin
              (bytes-set! bstr pos d)
              (bytes-set! bstr (+ 1 pos) c)
              (bytes-set! bstr (+ 2 pos) b)
              (bytes-set! bstr (+ 3 pos) a)))))
    bstr))

(define (utf8->string bstr)
  (bytes->string/utf-8 bstr #\uFFFD))

(define (utf16->string bstr endianness [skip-bom? #f])
  ;; This version skips a two bytes for decoding errors,
  ;; except those that correspond to a trailing single byte.
  (check-endian endianness)
  (let ([len (bytes-length bstr)])
    (let-values ([(big? offset)
                  (cond
                   [skip-bom?
                    (values (eq? endianness 'big) 0)]
                   [(len . >= . 2)
                    (cond
                     [(and (eq? #xFE (bytes-ref bstr 0))
                           (eq? #xFF (bytes-ref bstr 1)))
                      (values #t 2)]
                     [(and (eq? #xFF (bytes-ref bstr 0))
                           (eq? #xFE (bytes-ref bstr 1)))
                      (values #f 2)]
                     [else (values (eq? endianness 'big) 0)])]
                   [else (values (eq? endianness 'big) 0)])])
      (list->string
       (let loop ([pos offset])
         (cond
          [(= pos len) null]
          [(= (add1 pos) len)
           ;; decoding error
           '(#\uFFFD)]
          [else
           (let ([a (bytes-ref bstr pos)]
                 [b (bytes-ref bstr (add1 pos))])
             (let ([a (if big? a b)]
                   [b (if big? b a)])
               (cond
                [(= (bitwise-and a #xFC) #xD8)
                 (if (len . < . (+ pos 4))
                     ;; decoding error
                     (cons #\uFFFD (loop (+ pos 2)))
                     ;; Surrogate...
                     (let ([c (bytes-ref bstr (+ pos 2))]
                           [d (bytes-ref bstr (+ pos 3))])
                       (let ([c (if big? c d)]
                             [d (if big? d c)])
                         (cond
                          [(= (bitwise-and c #xFC) #xDC)
                           ;; A valid surrogate
                           (let ([v (+ #x10000
                                       (bitwise-ior
                                        (arithmetic-shift (bitwise-and #x3 a) 18)
                                        (arithmetic-shift b 10)
                                        (arithmetic-shift (bitwise-and #x3 c) 8)
                                        d))])
                             (cons (integer->char v) (loop (+ pos 4))))]
                          [else
                           ;; Invalid surrogate.
                           (cons #\uFFFD (loop (+ pos 2)))]))))]
                [(= (bitwise-and a #xFC) #xDC)
                 ;; invalid surrogate code
                 (cons #\uFFFD (loop (+ pos 2)))]
                [else
                 (let ([v (bitwise-ior (arithmetic-shift a 8)
                                       b)])
                   (cons (integer->char v)
                         (loop (+ pos 2))))])))]))))))

(define (utf32->string bstr endianness [skip-bom? #f])
  ;; Skips 4 bytes for each dcoding error (except too-few-bytes-at-end
  ;; errors, obviously).
  (check-endian endianness)
  (let ([len (bytes-length bstr)])
    (let-values ([(big? offset)
                  (cond
                   [skip-bom?
                    (values (eq? endianness 'big) 0)]
                   [(len . >= . 4)
                    (cond
                     [(and (eq? #x00 (bytes-ref bstr 0))
                           (eq? #x00 (bytes-ref bstr 1))
                           (eq? #xFE (bytes-ref bstr 2))
                           (eq? #xFF (bytes-ref bstr 3)))
                      (values #t 4)]
                     [(and (eq? #xFF (bytes-ref bstr 0))
                           (eq? #xFE (bytes-ref bstr 1))
                           (eq? #x00 (bytes-ref bstr 2))
                           (eq? #x00 (bytes-ref bstr 3)))
                      (values #f 4)]
                     [else (values (eq? endianness 'big) 0)])]
                   [else (values (eq? endianness 'big) 0)])])
      (list->string
       (let loop ([pos offset])
         (cond
          [(= pos len) null]
          [((+ pos 4) . > . len)
           ;; decoding error
           '(#\uFFFD)]
          [else
           (let ([a (bytes-ref bstr pos)]
                 [b (bytes-ref bstr (+ pos 1))]
                 [c (bytes-ref bstr (+ pos 2))]
                 [d (bytes-ref bstr (+ pos 3))])
             (let ([a (if big? a d)]
                   [b (if big? b c)]
                   [c (if big? c b)]
                   [d (if big? d a)])
               (let ([v (bitwise-ior
                         (arithmetic-shift a 24)
                         (arithmetic-shift b 16)
                         (arithmetic-shift c 8)
                         d)])
                 (if (or (and (v . >= . #xD800)
                              (v . <= . #xDFFF))
                         (v . > . #x10FFFF))
                     (cons #\uFFFD (loop (+ pos 4)))
                     (cons (integer->char v) (loop (+ pos 4)))))))]))))))
