#lang racket/base
(require '#%extfl
         (for-syntax racket/base)
         "private/truncate-path.rkt"
         "private/relative-path.rkt"
         (rename-in racket/base
                    [write-byte r:write-byte]
                    [write-bytes r:write-bytes]))

(provide s-exp->fasl
         fasl->s-exp)

;; ----------------------------------------

;; These wrappers are to make it harder to misuse write-byte[s]
;; (e.g. calling without the port)

(define (write-byte byte out)
  (r:write-byte byte out))

(define (write-bytes bstr out [start-pos 0] [end-pos (bytes-length bstr)])
  (r:write-bytes bstr out start-pos end-pos))

;; ----------------------------------------

(define-for-syntax constants (make-hasheq))

(define-syntax (define-constants stx)
  (syntax-case stx ()
    [(_ [id n] ...)
     (begin
       (for ([id (in-list (syntax->list #'(id ...)))]
             [n (in-list (syntax->list #'(n ...)))])
         (hash-set! constants (syntax-e id) (syntax-e n)))
       #'(begin
           (define id n) ...))]))

(define-syntax (constant-case stx)
  (syntax-case stx (else)
    [(_ e [(id ...) rhs ...] ... [else else-rhs ...])
     (with-syntax ([((n ...) ...)
                    (for/list ([ids (in-list (syntax->list #'((id ...) ...)))])
                      (for/list ([id (in-list (syntax->list ids))])
                        (hash-ref constants (syntax-e id))))])
       #'(case e [(n ...) rhs ...] ... [else else-rhs ...]))]))

;; ----------------------------------------

;; There is no versioning of the fasl format, so don't change the
;; numbers below --- only add to the set

(define-constants
  (fasl-graph-def-type 1)
  (fasl-graph-ref-type 2)

  (fasl-false-type 3)
  (fasl-true-type  4)
  (fasl-null-type  5)
  (fasl-void-type  6)
  (fasl-eof-type   7)

  (fasl-integer-type  8)
  (fasl-flonum-type   9)
  (fasl-single-flonum-type 10)
  (fasl-rational-type 11)
  (fasl-complex-type  12)
  (fasl-char-type     13)

  (fasl-symbol-type   14)
  (fasl-unreadable-symbol-type 15)
  (fasl-uninterned-symbol-type 16)
  (fasl-keyword-type  17)
  (fasl-string-type   18)
  (fasl-immutable-string-type  19)
  (fasl-bytes-type    20)
  (fasl-immutable-bytes-type   21)
  (fasl-path-type     22)
  (fasl-relative-path-type 23)

  (fasl-pregexp-type  24)
  (fasl-regexp-type   25)
  (fasl-byte-pregexp-type 26)
  (fasl-byte-regexp-type 27)

  (fasl-list-type     28)
  (fasl-list*-type    29)
  (fasl-pair-type     30)
  (fasl-vector-type   31)
  (fasl-immutable-vector-type 32)
  (fasl-box-type      33)
  (fasl-immutable-box-type 34)
  (fasl-prefab-type   35)
  (fasl-hash-type     36)
  (fasl-immutable-hash-type 37)

  (fasl-srcloc 38)

  (fasl-extflonum-type 39)

  ;; Unallocated numbers here are for future extensions

  ;; 100 to 255 is used for small integers:
  (fasl-small-integer-start 100))

(define fasl-lowest-small-integer -10)
(define fasl-highest-small-integer (- 255 (- fasl-small-integer-start fasl-lowest-small-integer) 1))
(define fasl-prefix #"racket/fasl:")
(define fasl-prefix-length (bytes-length fasl-prefix))

(define-constants
  (fasl-hash-eq-variant    0)
  (fasl-hash-equal-variant 1)
  (fasl-hash-eqv-variant   2))

;; ----------------------------------------

(define (s-exp->fasl v
                     [orig-o #f]
                     #:keep-mutable? [keep-mutable? #f])
  (when orig-o
    (unless (output-port? orig-o)
      (raise-argument-error 'fasl->s-exp "(or/c output-port? #f)" orig-o)))
  (define o (or orig-o (open-output-bytes)))
  (define shared (make-hasheq))
  (define shared-counter 0)
  ;; Find shared symbols and similar for compactness. We don't try to
  ;; save general graph structure, leaving that to `serialize`.
  (let loop ([v v])
    (cond
      [(or (symbol? v)
           (keyword? v)
           (string? v)
           (bytes? v)
           (path? v))
       (hash-update! shared v add1 0)]
      [(pair? v)
       (loop (car v))
       (loop (cdr v))]
      [(vector? v)
       (for ([e (in-vector v)])
         (loop e))]
      [(hash? v)
       (hash-for-each v
                      (lambda (k v)
                        (loop k)
                        (loop v))
                      #t)]
      [(box? v)
       (loop (unbox v))]
      [(prefab-struct-key v)
       => (lambda (k)
            (loop k)
            (for ([e (in-vector (struct->vector v) 1)])
              (loop e)))]
      [else (void)]))
  (define (treat-immutable? v) (or (not keep-mutable?) (immutable? v)))
  (define path->relative-path-elements (make-path->relative-path-elements))
  ;; The fasl formal prefix:
  (write-bytes fasl-prefix o)
  ;; Write content to a string, so we can measure it
  (define bstr
    (let ([o (open-output-bytes)])
      (let loop ([v v])
        (cond
          [(not (eq? (hash-ref shared v 1) 1))
           (define c (hash-ref shared v))
           (cond
             [(negative? c)
              (write-byte fasl-graph-ref-type o)
              (write-fasl-integer (sub1 (- c)) o)]
             [else
              (define pos shared-counter)
              (set! shared-counter (add1 shared-counter))
              (write-byte fasl-graph-def-type o)
              (write-fasl-integer pos o)
              (hash-remove! shared v)
              (loop v)
              (hash-set! shared v (- (add1 pos)))])]
          [(not v)
           (write-byte fasl-false-type o)]
          [(eq? v #t)
           (write-byte fasl-true-type o)]
          [(null? v)
           (write-byte fasl-null-type o)]
          [(void? v)
           (write-byte fasl-void-type o)]
          [(eof-object? v)
           (write-byte fasl-eof-type o)]
          [(exact-integer? v)
           (cond
             [(<= fasl-lowest-small-integer v fasl-highest-small-integer)
              (write-byte (+ fasl-small-integer-start (- v fasl-lowest-small-integer)) o)]
             [else
              (write-byte fasl-integer-type o)
              (write-fasl-integer v o)])]
          [(flonum? v)
           (write-byte fasl-flonum-type o)
           (write-bytes (if (eqv? v +nan.0)
                            ;; use a canonical NaN (0 mantissa)
                            #"\0\0\0\0\0\0\370\177"
                            (real->floating-point-bytes v 8 #f))
                        o)]
          [(single-flonum? v)
           (write-byte fasl-single-flonum-type o)
           (write-bytes (if (eqv? v +nan.f)
                            ;; use a canonical NaN (0 mantissa)
                            #"\0\0\300\177"
                            (real->floating-point-bytes v 4 #f))
                        o)]
          [(extflonum? v)
           (write-byte fasl-extflonum-type o)
           (define bstr (string->bytes/utf-8 (format "~a" v)))
           (write-fasl-integer (bytes-length bstr) o)
           (write-bytes bstr o)]
          [(rational? v)
           (write-byte fasl-rational-type o)
           (loop (numerator v))
           (loop (denominator v))]
          [(complex? v)
           (write-byte fasl-complex-type o)
           (loop (real-part v))
           (loop (imag-part v))]
          [(char? v)
           (write-byte fasl-char-type o)
           (write-fasl-integer (char->integer v) o)]
          [(symbol? v)
           (cond
             [(symbol-interned? v)
              (write-byte fasl-symbol-type o)]
             [(symbol-unreadable? v)
              (write-byte fasl-unreadable-symbol-type o)]
             [else
              (write-byte fasl-uninterned-symbol-type o)])
           (define bstr (string->bytes/utf-8 (symbol->string v)))
           (write-fasl-integer (bytes-length bstr) o)
           (write-bytes bstr o)]
          [(keyword? v)
           (write-byte fasl-keyword-type o)
           (define bstr (string->bytes/utf-8 (keyword->string v)))
           (write-fasl-integer (bytes-length bstr) o)
           (write-bytes bstr o)]
          [(string? v)
           (write-fasl-integer (if (treat-immutable? v) fasl-immutable-string-type fasl-string-type) o)
           (write-fasl-string v o)]
          [(bytes? v)
           (write-fasl-integer (if (treat-immutable? v) fasl-immutable-bytes-type fasl-bytes-type) o)
           (write-fasl-bytes v o)]
          [(path-for-some-system? v)
           (define rel-elems (path->relative-path-elements v))
           (cond
             [rel-elems
              (write-byte fasl-relative-path-type o)
              (loop rel-elems)]
             [else
              (write-byte fasl-path-type o)
              (write-fasl-bytes (path->bytes v) o)
              (loop (path-convention-type v))])]
          [(and (srcloc? v) (let ([src (srcloc-source v)])
                              (or (not src)
                                  (path-for-some-system? src)
                                  (string? src)
                                  (bytes? src)
                                  (symbol? src))))
           (define src (srcloc-source v))
           (define new-src
             (cond
               [(and (path? src)
                     (not (path->relative-path-elements src)))
                ;; Convert to a string
                (truncate-path src)]
               [else src]))
           (write-fasl-integer fasl-srcloc o)
           (loop new-src)
           (loop (srcloc-line v))
           (loop (srcloc-column v))
           (loop (srcloc-position v))
           (loop (srcloc-span v))]
          [(pair? v)
           (cond
             [(pair? (cdr v))
              (define-values (n normal-list?)
                (let loop ([v v] [len 0])
                  (cond
                    [(null? v) (values len #t)]
                    [(pair? v) (loop (cdr v) (add1 len))]
                    [else (values len #f)])))
              (write-byte (if normal-list? fasl-list-type fasl-list*-type) o)
              (write-fasl-integer n o)
              (let ploop ([v v])
                (cond
                  [(pair? v)
                   (loop (car v))
                   (ploop (cdr v))]
                  [else
                   (unless normal-list?
                     (loop v))]))]
             [else
              (write-byte fasl-pair-type o)
              (loop (car v))
              (loop (cdr v))])]
          [(vector? v)
           (write-byte (if (treat-immutable? v) fasl-immutable-vector-type fasl-vector-type) o)
           (write-fasl-integer (vector-length v) o)
           (for ([e (in-vector v)])
             (loop e))]
          [(box? v)
           (write-byte (if (treat-immutable? v) fasl-immutable-box-type fasl-box-type) o)
           (loop (unbox v))]
          [(prefab-struct-key v)
           => (lambda (k)
                (write-byte fasl-prefab-type o)
                (loop k)
                (define vec (struct->vector v))
                (write-fasl-integer (sub1 (vector-length vec)) o)
                (for ([e (in-vector vec 1)])
                  (loop e)))]
          [(hash? v)
           (write-byte (if (treat-immutable? v) fasl-immutable-hash-type fasl-hash-type) o)
           (write-byte (cond
                         [(hash-eq? v) fasl-hash-eq-variant]
                         [(hash-eqv? v) fasl-hash-eqv-variant]
                         [else fasl-hash-equal-variant])
                       o)
           (write-fasl-integer (hash-count v) o)
           (hash-for-each v (lambda (k v) (loop k) (loop v)) #t)]
          [(regexp? v)
           (write-byte (if (pregexp? v) fasl-pregexp-type fasl-regexp-type) o)
           (write-fasl-string (object-name v) o)]
          [(byte-regexp? v)
           (write-byte (if (byte-pregexp? v) fasl-byte-pregexp-type fasl-byte-regexp-type) o)
           (write-fasl-bytes (object-name v) o)]
          [else
           (raise-arguments-error 'fasl-write
                                  "cannot write value"
                                  "value" v)]))
      (get-output-bytes o #t)))
  ;; Record the number of entries in the shared-value table that is
  ;; used by `fasl-graph-ref-type` and `fasl-graph-ref-type`:
  (write-fasl-integer shared-counter o)
  ;; Record the byte-string size of the encoded data:
  (write-fasl-integer (bytes-length bstr) o)
  ;; Write the encoded data to `o`
  (write-bytes bstr o)
  (if orig-o
      (void)
      (get-output-bytes o)))

;; ----------------------------------------

;; For input parsing internally, in place of an input port, use a
;; mutable pair containing a byte string and position

(define (fasl->s-exp orig-i
                     #:datum-intern? [intern? #t])
  (define init-i (cond
                   [(bytes? orig-i) (mcons orig-i 0)]
                   [(input-port? orig-i) orig-i]
                   [else (raise-argument-error 'fasl->s-exp "(or/c bytes? input-port?)" orig-i)]))
  (unless (bytes=? (read-bytes/exactly fasl-prefix-length init-i) fasl-prefix)
    (read-error "unrecognized prefix"))
  (define shared-count (read-fasl-integer init-i))
  (define shared (make-vector shared-count))
  (define len (read-fasl-integer init-i))

  (define i (if (mpair? init-i)
                init-i
                ;; Faster to work with a byte string:
                (let ([bstr (read-bytes/exactly len init-i)])
                  (mcons bstr 0))))

  (define (intern v) (if intern? (datum-intern-literal v) v))
  (let loop ()
    (define type (read-byte/no-eof i))
    (constant-case
     type
     [(fasl-graph-def-type)
      (define pos (read-fasl-integer i))
      (define v (loop))
      (unless (pos . < . shared-count)
        (read-error "bad graph index"))
      (vector-set! shared pos v)
      v]
     [(fasl-graph-ref-type)
      (define pos (read-fasl-integer i))
      (unless (pos . < . shared-count)
        (read-error "bad graph index"))
      (vector-ref shared pos)]
     [(fasl-false-type) #f]
     [(fasl-true-type) #t]
     [(fasl-null-type) null]
     [(fasl-void-type) (void)]
     [(fasl-eof-type) eof]
     [(fasl-integer-type) (intern (read-fasl-integer i))]
     [(fasl-flonum-type) (floating-point-bytes->real (read-bytes/exactly 8 i) #f)]
     [(fasl-single-flonum-type) (real->single-flonum (floating-point-bytes->real (read-bytes/exactly 4 i) #f))]
     [(fasl-extflonum-type)
      (define bstr (read-bytes/exactly (read-fasl-integer i) i))
      (string->number (bytes->string/utf-8 bstr) 10 'read)]
     [(fasl-rational-type) (intern (/ (loop) (loop)))]
     [(fasl-complex-type) (intern (make-rectangular (loop) (loop)))]
     [(fasl-char-type) (intern (integer->char (read-fasl-integer i)))]
     [(fasl-symbol-type) (string->symbol (read-fasl-string i))]
     [(fasl-unreadable-symbol-type) (string->unreadable-symbol (read-fasl-string i))]
     [(fasl-uninterned-symbol-type) (string->uninterned-symbol (read-fasl-string i))]
     [(fasl-keyword-type) (string->keyword (read-fasl-string i))]
     [(fasl-string-type) (read-fasl-string i)]
     [(fasl-immutable-string-type) (intern (string->immutable-string (read-fasl-string i)))]
     [(fasl-bytes-type) (read-fasl-bytes i)]
     [(fasl-immutable-bytes-type) (intern (bytes->immutable-bytes (read-fasl-bytes i)))]
     [(fasl-path-type) (bytes->path (read-fasl-bytes i)
                                    (loop))]
     [(fasl-relative-path-type)
      (define wrt-dir (current-load-relative-directory))
      (define rel-elems (for/list ([p (in-list (loop))])
                          (if (bytes? p) (bytes->path-element p) p)))
      (cond
        [wrt-dir (apply build-path wrt-dir rel-elems)]
        [(null? rel-elems) (build-path 'same)]
        [else (apply build-path rel-elems)])]
     [(fasl-pregexp-type) (intern (pregexp (read-fasl-string i)))]
     [(fasl-regexp-type) (intern (regexp (read-fasl-string i)))]
     [(fasl-byte-pregexp-type) (intern (byte-pregexp (read-fasl-bytes i)))]
     [(fasl-byte-regexp-type) (intern (byte-regexp (read-fasl-bytes i)))]
     [(fasl-list-type)
      (define len (read-fasl-integer i))
      (for/list ([j (in-range len)])
        (loop))]
     [(fasl-pair-type)
      (cons (loop) (loop))]
     [(fasl-list*-type)
      (define len (read-fasl-integer i))
      (let ploop ([len len])
        (if (zero? len)
            (loop)
            (cons (loop) (ploop (sub1 len)))))]
     [(fasl-vector-type fasl-immutable-vector-type)
      (define len (read-fasl-integer i))
      (define vec (for/vector #:length len ([j (in-range len)])
                    (loop)))
      (if (eqv? type fasl-immutable-vector-type)
          (vector->immutable-vector vec)
          vec)]
     [(fasl-box-type) (box (loop))]
     [(fasl-immutable-box-type) (box-immutable (loop))]
     [(fasl-prefab-type)
      (define key (loop))
      (define len (read-fasl-integer i))
      (apply make-prefab-struct
             key
             (for/list ([j (in-range len)])
               (loop)))]
     [(fasl-hash-type)
      (define ht (constant-case
                  (read-byte/no-eof i)
                  [(fasl-hash-eq-variant) (make-hasheq)]
                  [(fasl-hash-eqv-variant) (make-hasheqv)]
                  [else (make-hash)]))
      (define len (read-fasl-integer i))
      (for ([j (in-range len)])
        (hash-set! ht (loop) (loop)))
      ht]
     [(fasl-immutable-hash-type)
      (define ht (constant-case
                  (read-byte/no-eof i)
                  [(fasl-hash-eq-variant) #hasheq()]
                  [(fasl-hash-eqv-variant) #hasheqv()]
                  [else #hash()]))
      (define len (read-fasl-integer i))
      (for/fold ([ht ht]) ([j (in-range len)])
        (hash-set ht (loop) (loop)))]
     [(fasl-srcloc)
      (srcloc (loop) (loop) (loop) (loop) (loop))]
     [else
      (cond
        [(type . >= . fasl-small-integer-start)
         (+ (- type fasl-small-integer-start) fasl-lowest-small-integer)]
        [else
         (read-error "unrecognized fasl tag" "tag" type)])])))

;; ----------------------------------------

;; Integer encoding:
;;  -124 to 127 = direct (as 2's complement)
;;  128 => 2-byte little-endian integer
;;  129 => 4-byte little-endian integer
;;  130 => 8-byte little-endian integer
;;  131 => # of ASCII hex digits followed by digits

(define (write-fasl-integer i o)
  (cond
    [(<= -124 i 127)
     (if (negative? i)
         (write-byte (+ i 256) o)
         (write-byte i o))]
    [(<= -32768 i 32767)
     (write-byte 128 o)
     (write-bytes (integer->integer-bytes i 2 #t #f) o)]
    [(<= -2147483648 i 2147483647)
     (write-byte 129 o)
     (write-bytes (integer->integer-bytes i 4 #t #f) o)]
    [(<= -9223372036854775808 i 9223372036854775807)
     (write-byte 130 o)
     (write-bytes (integer->integer-bytes i 8 #t #f) o)]
    [else
     (write-byte 131 o)
     (define s (format "~x" i)) ; always ASCII
     (write-fasl-integer (string-length s) o)
     (write-string s o)]))

(define (write-fasl-string v o)
  (define bstr (string->bytes/utf-8 v))
  (write-fasl-integer (bytes-length bstr) o)
  (write-bytes bstr o))

(define (write-fasl-bytes v o)
  (write-fasl-integer (bytes-length v) o)
  (write-bytes v o))

;; ----------------------------------------

(define (read-error s . args)
  (apply raise-arguments-error
         'fasl-read
         (string-append "error parsing fasl stream;\n"
                        " " s)
         args))

(define (read-byte/no-eof i)
  (cond
    [(mpair? i)
     (define pos (mcdr i))
     (unless (pos . < . (bytes-length (mcar i)))
       (read-error "truncated stream"))
     (set-mcdr! i (add1 pos))
     (bytes-ref (mcar i) pos)]
    [else
     (define b (read-byte i))
     (when (eof-object? b)
       (read-error "truncated stream"))
     b]))

(define (read-bytes/exactly n i)
  (cond
    [(mpair? i)
     (define pos (mcdr i))
     (unless ((+ pos n) . <= . (bytes-length (mcar i)))
       (read-error "truncated stream"))
     (set-mcdr! i (+ pos n))
     (subbytes (mcar i) pos (+ pos n))]
    [else
     (define bstr (read-bytes n i))
     (unless (and (bytes? bstr) (= n (bytes-length bstr)))
       (read-error "truncated stream"))
     bstr]))

(define (read-fasl-integer i)
  (define b (read-byte/no-eof i))
  (cond
    [(<= b 127) b]
    [(>= b 132) (- b 256)]
    [(eqv? b 128)
     (integer-bytes->integer (read-bytes/exactly 2 i) #t #f)]
    [(eqv? b 129)
     (integer-bytes->integer (read-bytes/exactly 4 i) #t #f)]
    [(eqv? b 130)
     (integer-bytes->integer (read-bytes/exactly 8 i) #t #f)]
    [(eqv? b 131)
     (define len (read-fasl-integer i))
     (define str (read-fasl-string i len))
     (unless (and (string? str) (= len (string-length str)))
       (read-error "truncated stream at number"))
     (string->number str 16)]
    [else
     (read-error "internal error on integer mode")]))

(define (read-fasl-string i [len (read-fasl-integer i)])
  (define bstr (read-bytes/exactly len i))
  (bytes->string/utf-8 bstr))

(define (read-fasl-bytes i)
  (define len (read-fasl-integer i))
  (read-bytes/exactly len i))
