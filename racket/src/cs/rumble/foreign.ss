
;; Externally, a cpointer can be #f or a byte string, in
;; addition to a cpointer record
(define (cpointer? v)
  (or (authentic-cpointer? v)
      (not v)
      (bytes? v)
      (has-cpointer-property? v)))

;; A cpointer record's `memory` is either a raw foreign address (i.e., a
;; number), bytevector, or flvector. A reference bytevector is used for
;; non-atomic memory.
(define-record-type (cpointer make-cpointer authentic-cpointer?)
  (fields memory (mutable tags)))
(define-record-type cpointer+offset
  (parent cpointer)
  (fields (mutable offset)))

(define-values (prop:cpointer has-cpointer-property? cpointer-property-ref)
  (make-struct-type-property 'cpointer
                             (lambda (v info)
                               (cond
                                [(exact-nonnegative-integer? v)
                                 (unless (< v (list-ref info 1))
                                   (raise-arguments-error 'prop:cpointer
                                                          "index is out of range"
                                                          "index" v))
                                 (unless (#%memv v (list-ref info 5))
                                   (raise-arguments-error 'prop:cpointer
                                                          "index does not refer to an immutable field"
                                                          "index" v))
                                 (+ v (let ([p (list-ref info 6)])
                                        (if p
                                            (struct-type-total*-field-count p)
                                            0)))]
                                [(and (procedure? v)
                                      (procedure-arity-includes? v 1))
                                 v]
                                [(cpointer? v) v]
                                [else
                                 (raise-argument-error 'prop:cpointer
                                                       (string-append
                                                        "(or/c exact-nonnegative-integer?\n"
                                                        "      (procedure-arity-includes/c 1)\n"
                                                        "      cpointer?)")
                                                       v)]))))

;; Gets a primitive cpointer type by following a `prop:evt` property
;; as needed. Call with function *before* disabling GC interrupts.
(define (unwrap-cpointer who p)
  (cond
   [(authentic-cpointer? p) p]
   [(not p) p]
   [(bytes? p) p]
   [(ffi-callback? p) p]
   [else (let ([v (cpointer-property-ref p)])
           (cond
            [(exact-nonnegative-integer? v)
             (let ([v (unsafe-struct-ref p v)])
               (if (cpointer? v)
                   (unwrap-cpointer who v)
                   #f))]
            [(procedure? v)
             (let ([p2 (v p)])
               (unless (cpointer? p2)
                 (raise-result-error 'prop:cpointer-accessor
                                     "cpointer?"
                                     p2))
               (unwrap-cpointer who p2))]
            [else
             (unwrap-cpointer who v)]))]))

;; Like `unwrap-cpointer*`, but also allows an integer as a raw
;; foreign address:
(define (unwrap-cpointer* who p)
  (if (integer? p)
      p
      (unwrap-cpointer who p)))

(define (offset-ptr? p)
  (unless (cpointer? p)
    (raise-argument-error 'offset-ptr? "cpointer?" p))
  (cpointer+offset? p))

(define/who (set-cpointer-tag! p t)
  (if (authentic-cpointer? p)
      (cpointer-tags-set! p t)
      (if (cpointer? p)
          (let ([q (unwrap-cpointer who p)])
            (if (authentic-cpointer? q)
                (set-cpointer-tag! q t)
                (raise-arguments-error who
                                       "cannot set tag on given cpointer"
                                       "given" p
                                       "tag" t)))
          (raise-argument-error who "cpointer?" p))))

(define/who (cpointer-tag p)
  (if (authentic-cpointer? p)
      (cpointer-tags p)
      (if (cpointer? p)
          (let ([q (unwrap-cpointer who p)])
            (if (authentic-cpointer? q)
                (cpointer-tag q)
                #f))
          (raise-argument-error who "cpointer?" p))))

;; Convert a `memory` --- typically a raw foreign address, but possibly
;; a bytevector or flvector --- to a cpointer, using #f for a NULL
;; address:
(define (memory->cpointer x)
  (cond
   [(or (not x) (authentic-cpointer? x))
    ;; This happens when a pointer is converted without going through
    ;; `cpointer-address` such as a `ptr-ref` on a struct or array type
    x]
   [(eqv? x 0) #f]
   [else (make-cpointer x #f)]))

;; Works on unwrapped cpointers:
(define (cpointer-nonatomic? p)
  (and (authentic-cpointer? p)
       (reference-bytevector? (cpointer-memory p))))

;; Works on unwrapped cpointers:
(define (cpointer->name proc-p)
  (and (ffi-obj? proc-p)
       (string->symbol (utf8->string (cpointer/ffi-obj-name proc-p)))))

;; ----------------------------------------

;; Convert a raw foreign address to a Scheme value on the
;; assumption that the address is the payload of a byte
;; string:
(define (addr->gcpointer-memory v) ; call with GC disabled
  (reference-address->object v))

;; Converts a primitive cpointer (normally the result of
;; `unwrap-cpointer`) to a memory plus offset
(define (cpointer-address+offset p)
  (cond
   [(not p) (values 0 0)]
   [(or (bytevector? p) (flvector? p)) (values p 0)]
   [(cpointer+offset? p)
    (values (cpointer-memory p) (cpointer+offset-offset p))]
   [(authentic-cpointer? p)
    (values (cpointer-memory p) 0)]
   [(ffi-callback? p)
    (values (foreign-callable-entry-point (callback-code p)) 0)]
   [else
    (raise-arguments-error 'internal-error "bad case extracting a cpointer address"
                           "value" p)]))

;; Convert a `memory` (as in a cpointer) to a raw foreign address.
(define (memory-address memory) ; call with GC disabled
  (cond
   [(integer? memory) memory]
   [else (object->reference-address memory)]))

;; Converts a primitive cpointer (normally the result of
;; `unwrap-cpointer`) to a raw foreign address. The
;; GC must be disabled while extracting an address,
;; which might be the address of a byte string that
;; could otherwise change due to a GC.
(define (cpointer-address p) ; call with GC disabled
  (let-values ([(memory offset) (cpointer-address+offset p)])
    (+ (memory-address memory) offset)))

(define (cpointer-needs-lock? p)
  (cond
   [(bytes? p) #t]
   [(authentic-cpointer? p) (not (integer? (cpointer-memory p)))]
   [else #f]))

;; Like `cpointer-address`, but allows a raw foreign
;; address to pass through:
(define (cpointer*-address p) ; call with GC disabled
  (if (number? p)
      p
      (cpointer-address p)))

;; ----------------------------------------

(define/who (ptr-equal? p1 p2)
  (let ([p1 (unwrap-cpointer who p1)]
        [p2 (unwrap-cpointer who p2)])
    (with-interrupts-disabled* ; disable GC while extracting addresses
     (= (cpointer-address p1) (cpointer-address p2)))))

(define/who (ptr-offset p)
  (let ([p (unwrap-cpointer who p)])
    (ptr-offset* p)))

(define (ptr-offset* p)
  (if (cpointer+offset? p)
      (cpointer+offset-offset p)
      0))

(define (set-ptr-offset! p n)
  (unless (cpointer+offset? p)
    (raise-argument-error 'ptr-offset "(and/c cpointer? ptr-offset?)" p))
  (unless (exact-integer? n)
    (raise-argument-error 'ptr-offset "exact-integer?" n))
  (cpointer+offset-offset-set! p n))

(define ptr-add
  (case-lambda
   [(p n type)
    (unless (cpointer? p)
      (raise-argument-error 'ptr-add "cpointer?" p))
    (unless (exact-integer? n)
      (raise-argument-error 'ptr-add "exact-integer?" n))
    (unless (ctype? type)
      (raise-argument-error 'ptr-add "ctype?" type))
    (do-ptr-add p (* n (ctype-sizeof type)) #t)]
   [(p n)
    (unless (cpointer? p)
      (raise-argument-error 'ptr-add "cpointer?" p))
    (unless (exact-integer? n)
      (raise-argument-error 'ptr-add "exact-integer?" n))
    (do-ptr-add p n #t)]))

(define (do-ptr-add p n save-tags?)
  (cond
   [(authentic-cpointer? p)
    (make-cpointer+offset (cpointer-memory p)
                          (and save-tags? (cpointer-tag p))
                          (+ n (ptr-offset* p)))]
   [(has-cpointer-property? p)
    (do-ptr-add (unwrap-cpointer 'do-ptr-add p) n save-tags?)]
   [else
    (make-cpointer+offset (or p 0) #f n)]))

(define ptr-add!
  (case-lambda
   [(p n type)
    (unless (cpointer+offset? p)
      (raise-argument-error 'ptr-add! "(and/c cpointer? offset-ptr?)" p))
    (unless (exact-integer? n)
      (raise-argument-error 'ptr-add! "exact-integer?" n))
    (unless (ctype? type)
      (raise-argument-error 'ptr-add! "ctype?" type))
    (do-ptr-add! p (* n (ctype-sizeof type)))]
   [(p n)
    (unless (cpointer+offset? p)
      (raise-argument-error 'ptr-add! "(and/c cpointer? offset-ptr?)" p))
    (unless (exact-integer? n)
      (raise-argument-error 'ptr-add! "exact-integer?" n))
    (do-ptr-add! p n)]))

(define (do-ptr-add! p n)
  (unless (cpointer+offset? p)
    (raise-arguments-error 'ptr-add!
                           "given cpointer does not have an offset"
                           "given" p))
  (cpointer+offset-offset-set! p (+ n (cpointer+offset-offset p))))

;; ----------------------------------------

;; In ctype-host-rep, we use 'uptr and 'void* (which are aliases for foreign-ref)
;; to reflect intent with respect to foreign versus Scheme addresses when reading:
;;  - 'uptr  => inferred as Scheme or foreign, read as memory instead of address
;;  - 'void* => ctype-out-rep ('pointer versus 'gcpointer) implies Scheme or foreign,
;;;             and ctype-out-rep is assumed to be correct in that regard

(define-record-type (ctype create-ctype ctype?)
  (fields host-rep    ; host-Scheme representation description, 'struct, 'union, or 'array
          our-rep     ; Racket representation description
          basetype    ; parent ctype or the same as `our-rep`
          scheme->c   ; converter of values to `basetype`
          c->scheme)) ; converter of values from `basetype`

;; A `compound-ctype` is used for structs, unions, and arrays
(define-record-type (compound-ctype create-compound-ctype compound-ctype?)
  (parent ctype)
  (fields get-decls
          size
          alignment
          malloc-mode))

(define/who (make-ctype type racket-to-c c-to-racket)
  (check who ctype? type)
  (check who (procedure-arity-includes/c 1) :or-false racket-to-c)
  (check who (procedure-arity-includes/c 1) :or-false c-to-racket)
  (cond
   [(compound-ctype? type)
    (create-compound-ctype (ctype-host-rep type)
                           (ctype-our-rep type)
                           type
                           (protect-racket-to-c racket-to-c)
                           c-to-racket
                           (compound-ctype-get-decls type)
                           (compound-ctype-size type)
                           (compound-ctype-alignment type)
                           (compound-ctype-malloc-mode type))]
   [else
    (create-ctype (ctype-host-rep type)
                  (ctype-our-rep type)
                  type
                  (protect-racket-to-c racket-to-c)
                  c-to-racket)]))

(define (protect-racket-to-c racket-to-c)
  ;; Make sure `racket-to-c` is not confused for an internal
  ;; variant that accepts a `who` argument:
  (if (and (#%procedure? racket-to-c)
           (chez:procedure-arity-includes? racket-to-c 2))
      (lambda (v) (racket-to-c v))
      racket-to-c))

;; Apply all the conversion wrappers of `type` to the Scheme value `v`
(define (s->c who type v)
  (let* ([racket-to-c (ctype-scheme->c type)]
         [v (if racket-to-c
                (if (and (#%procedure? racket-to-c)
                         (chez:procedure-arity-includes? racket-to-c 2))
                    (racket-to-c who v)
                    (|#%app| racket-to-c v))
                v)]
         [next (ctype-basetype type)])
    (if (ctype? next)
        (s->c who next v)
        v)))

;; Apply all the conversion wrapper of `type` to the C value `v`
(define (c->s type v)
  (let* ([next (ctype-basetype type)]
         [v (if (ctype? next)
                (c->s next v)
                v)]
         [c-to-racket (ctype-c->scheme type)])
    (if c-to-racket
        (|#%app| c-to-racket v)
        v)))

;; ----------------------------------------

(define-syntax define-ctype
  (syntax-rules ()
    [(_ id host-rep basetype)
     (define/who id (create-ctype host-rep basetype basetype #f #f))]
    [(_ id host-rep basetype s->c)
     (define/who id (create-ctype host-rep basetype basetype s->c #f))]
    [(_ id host-rep basetype s->c c->s)
     (define/who id (create-ctype host-rep basetype basetype s->c c->s))]))

;; We need `s->c` checks, even if they seem redundant, to make sure
;; that the checks happen early enough --- outside of atomic and
;; foreign-thread regions. Also, the integer checks built into Chez
;; Scheme are more permissive than Racket's.

(define-syntax-rule (checker who ?) (lambda (for-whom x) (if (? x) x (bad-ctype-value for-whom who x))))
(define-syntax integer-checker
  (syntax-rules (signed unsigned)
    [(_ who signed n int?) (checker who (lambda (x) (and (int? x) (<= (- (expt 2 (- n 1))) x  (- (expt 2 (- n 1)) 1)))))]
    [(_ who unsigned n int?) (checker who (lambda (x) (and (int? x) (<= 0 x  (- (expt 2 n) 1)))))]))

(define-ctype _bool 'boolean 'bool)
(define-ctype _double 'double 'double (checker who flonum?))
(define-ctype _fixnum 'fixnum 'fixnum (checker who fixnum?))
(define-ctype _float 'float 'float (checker who flonum?))
(define-ctype _int8 'integer-8 'int8 (integer-checker who signed 8 fixnum?))
(define-ctype _int16 'integer-16 'int16 (integer-checker who signed 16 fixnum?))
(define-ctype _int32 'integer-32 'int32 (integer-checker who signed 32 exact-integer?))
(define-ctype _int64 'integer-64 'int64 (integer-checker who signed 64 exact-integer?))
(define-ctype _uint8 'unsigned-8 'uint8 (integer-checker who unsigned 8 fixnum?))
(define-ctype _uint16 'unsigned-16 'uint16 (integer-checker who unsigned 16 fixnum?))
(define-ctype _uint32 'unsigned-32 'uint32 (integer-checker who unsigned 32 exact-integer?))
(define-ctype _uint64 'unsigned-64 'uint64 (integer-checker who unsigned 64 exact-integer?))
(define-ctype _scheme 'scheme-object 'scheme)
(define-ctype _void 'void 'void (checker who void))

(define (bad-ctype-value who type-name v)
  (raise-arguments-error who
                         "given value does not fit primitive C type"
                         "C type" (make-unquoted-printing-string (symbol->string type-name))
                         "value" v))

;; Unlike traditional Racket, copies when converting from C:
(define-ctype _bytes 'uptr 'bytes
  (checker who (lambda (x) (or (not x) (bytes? x))))
  (lambda (x)
    (cond
     [(not x) ; happens with non-atomic memory reference
      x]
     [(bytes? x) ; happens with non-atomic memory reference
      ;; For consistency, truncate byte string at any NUL byte
      (let ([len (bytes-length x)])
        (let loop ([i 0])
          (cond
           [(fx= i len) x]
           [(fx= 0 (bytes-ref x i))
            (subbytes x 0 i)]
           [else (loop (fx+ i 1))])))]
     [(eqv? x 0) #f]
     [else
      (let loop ([i 0])
        (if (fx= 0 (foreign-ref 'unsigned-8 x i))
            (let ([bstr (make-bytes i)])
              (memcpy* bstr 0 x 0 i #f)
              bstr)
            (loop (fx+ i 1))))])))

(define (uptr->bytevector/two-nuls x)
  (cond
    [(not x) #f]
    [else
     (let loop ([i 0])
       (if (fx= 0 (if (bytevector? x)
                      (bytevector-u16-native-ref x i)
                      (foreign-ref 'unsigned-16 x i)))
           (let ([bstr (make-bytes i)])
             (memcpy* bstr 0 x 0 i #f)
             bstr)
           (loop (+ i 2))))]))

(define-ctype _short_bytes 'uptr 'bytes
  (lambda (form-whom x) x)
  (lambda (x) (uptr->bytevector/two-nuls x)))

(define-ctype _string/utf-16 'uptr 'string/utf-16
  (lambda (for-whom x)
    (cond
      [(not x) #f]
      [(string? x) (string->utf16 (string-append x "\x0;") (if (system-big-endian?) 'big 'little))]
      [else (bad-ctype-value who for-whom x)]))
  (lambda (x) (and x
                   (not (eq? x 0))
                   (utf16->string (uptr->bytevector/two-nuls x)
                                  (if (system-big-endian?) 'big 'little)))))

(define (uptr->bytevector/four-nuls x)
  (cond
    [(not x) #f]
    [else
     (let loop ([i 0])
       (if (eqv? 0 (if (bytevector? x)
                       (bytevector-u32-native-ref x i)
                       (foreign-ref 'unsigned-32 x i)))
           (let ([bstr (make-bytes i)])
             (memcpy* bstr 0 x 0 i #f)
             bstr)
           (loop (+ i 4))))]))
  
(define-ctype _string/ucs-4 'uptr 'string/ucs-4
  (lambda (for-whom x)
    (cond
      [(not x) #f]
      [(string? x) (string->utf32 (string-append x "\x0;") (if (system-big-endian?) 'big 'little))]
      [else (bad-ctype-value who for-whom x)]))
  (lambda (x) (and x
                   (not (eq? x 0))
                   (utf32->string (uptr->bytevector/four-nuls x)
                                  (if (system-big-endian?) 'big 'little)))))

(define-ctype _double* 'double 'double
  (lambda (for-whom x) (if (real? x)
                           (exact->inexact x)
                           (bad-ctype-value for-whom who x))))

(define-ctype _ufixnum 'fixnum 'fixnum (checker who fixnum?)) ; historically, no sign check
(define-ctype _fixint 'integer-32 'fixint (checker who fixnum?))
(define-ctype _ufixint 'unsigned-32 'ufixint (checker who fixnum?)) ; historically, no sign check

(define-ctype _symbol 'string 'string
  (lambda (for-whom x) (if (symbol? x)
                           (symbol->string x)
                           (bad-ctype-value for-whom who x)))
  (lambda (s) (string->symbol s)))

(define-ctype _longdouble 'double 'double
  (lambda (for-whom x) (bad-ctype-value for-whom who x)))

(define-ctype _pointer 'void* 'pointer
  (lambda (for-whom v) (unwrap-cpointer for-whom v)) ; resolved to an address later (with the GC disabled)
  (lambda (x) (memory->cpointer x)))

;; Treated specially by `ptr-ref`
(define-ctype _fpointer 'void* 'fpointer
  (lambda (for-whom v) (unwrap-cpointer for-whom v)) ; resolved to an address later (with the GC disabled)
  (lambda (x)
    (if (ffi-obj? x) ; check for `ptr-ref` special case on `ffi-obj`s
        x
        (memory->cpointer x))))

(define-ctype _gcpointer 'void* 'gcpointer
  (lambda (for-whom v) (unwrap-cpointer for-whom v)) ; like `_pointer`: resolved later
  (lambda (x)
    ;; `x` must have been converted to a bytevector or vector before
    ;; the GC was re-enabled
    (memory->cpointer x)))

;; One-byte stdbool is correct on all currently supported platforms, at least:
(define-ctype _stdbool 'integer-8 'stdbool
  (lambda (for-whom x) (if x 1 0))
  (lambda (v) (not (zero? v))))

(define make-cstruct-type
  (case-lambda
   [(types) (make-cstruct-type types #f #f 'atomic)]
   [(types abi) (make-cstruct-type types abi #f 'atomic)]
   [(types abi alignment) (make-cstruct-type types abi alignment 'atomic)]
   [(types abi alignment malloc-mode)
    (let ([make-decls
           (lambda (id next!-id)
             (let-values ([(reps decls) (types->reps types next!-id)])
               (append decls
                       `((define-ftype ,id
                           (struct ,@(map (lambda (rep)
                                            `[,(next!-id) ,rep])
                                          reps)))))))])
      (let-values ([(size alignment) (ctypes-sizeof+alignof types alignment)])
        (create-compound-ctype 'struct
                               'struct
                               types
                               (lambda (s) (unwrap-cpointer '_struct s)) ; like `_pointer`: resolved later
                               (lambda (c) (memory->cpointer c))
                               make-decls
                               size
                               alignment
                               malloc-mode)))]))

(define/who (make-union-type . types)
  (for-each (lambda (type) (check who ctype? type))
            types)
  (let ([make-decls
         (lambda (id next!-id)
           (let-values ([(reps decls) (types->reps types next!-id)])
             (append decls
                     `((define-ftype ,id
                         (union ,@(map (lambda (rep)
                                         `[,(next!-id) ,rep])
                                       reps)))))))]
        [size (apply max (map ctype-sizeof types))]
        [alignment (apply max (map ctype-alignof types))])
    (create-compound-ctype 'union
                           'union
                           types
                           (lambda (s) (unwrap-cpointer '_union s)) ; like `_pointer`: resolved later
                           (lambda (c) (memory->cpointer c))
                           make-decls
                           size
                           alignment
                           'atomic)))

(define/who (make-array-type type count)
  (check who ctype? type)
  (check who exact-nonnegative-integer? count)
  (let ([make-decls
         (lambda (id next!-id)
           (let-values ([(reps decls) (types->reps (list type) next!-id)])
             (append decls
                     `((define-ftype ,id
                         (array ,count ,(car reps)))))))]
        [size (* count (ctype-sizeof type))]
        [alignment (ctype-alignof type)])
    (unless (fixnum? size)
      (raise-arguments-error who "arithmetic overflow for overlarge array type"
                             "size" size))
    (create-compound-ctype 'array
                           'array
                           (vector type count)
                           (lambda (s) (unwrap-cpointer '_array s)) ; like `_pointer`: resolved later
                           (lambda (c) (memory->cpointer c))
                           make-decls
                           size
                           alignment
                           #f)))

(define (compiler-sizeof sl)
  (let ([rest (lambda (sl) (if (pair? sl) (cdr sl) '()))])
    (unless (or (symbol? sl)
                (list? sl))
      (raise-argument-error 'compiler-sizeof
                            "(or/c ctype-symbol? (listof ctype-symbol?))"
                            sl))
    (let loop ([sl sl] [base-type #f] [star? #f] [size #f])
      (cond
       [(null? sl)
        (cond
         [(eq? base-type 'void)
          (when size
            (raise-arguments-error 'compiler-sizeof "cannot qualify 'void"))
          (if star?
              (foreign-sizeof 'void*)
              (raise-arguments-error 'compiler-sizeof "cannot use 'void without a '*"))]
         [(or (not size)
              (eq? base-type 'int)
              (not base-type))
          (if star?
              (foreign-sizeof 'void*)
              (foreign-sizeof (or size base-type 'int)))]
         [(eq? base-type 'double)
          (case size
            [(long)
             (if star?
                 (foreign-sizeof 'void*)
                 ;; FIXME:
                 (foreign-sizeof 'double))]
            [(#f)
             (if star?
                 (foreign-sizeof 'void*)
                 (foreign-sizeof 'double))]
            [else
             (raise-arguments-error 'compiler-sizeof "bad qualifiers for 'double")])]
         [(eq? base-type 'float)
          (case size
            [(#f)
             (if star?
                 (foreign-sizeof 'void*)
                 (foreign-sizeof 'float))]
            [else
             (raise-arguments-error 'compiler-sizeof "bad qualifiers for 'float")])]
         [size
          (raise-arguments-error 'compiler-sizeof (format "cannot qualify '~a" base-type))])]
       [else
        (let ([s (if (pair? sl) (car sl) sl)])
          (case s
            [(int char wchar float double void)
             (cond
              [base-type
               (raise-arguments-error 'compiler-sizeof
                                      (format "extraneous type: '~a" s))]
              [else
               (loop (rest sl) s star? size)])]
            [(short)
             (case size
               [(short)
                (raise-arguments-error 'compiler-sizeof
                                       "cannot handle more than one 'short")]
               [(long)
                (raise-arguments-error 'compiler-sizeof
                                       "cannot use both 'short and 'long")]
               [(#f) (loop (rest sl) base-type star? 'short)])]
            [(long)
             (case size
               [(short)
                (raise-arguments-error 'compiler-sizeof
                                       "cannot use both 'short and 'long")]
               [(long-long)
                (raise-arguments-error 'compiler-sizeof
                                       "cannot handle more than two 'long")]
               [(long)
                (loop (rest sl) base-type star? 'long-long)]
               [(#f)
                (loop (rest sl) base-type star? 'long)])]
            [(*)
             (if star?
                 (raise-arguments-error 'compiler-sizeof
                                        "cannot handle more than one '*")
                 (loop (rest sl) base-type #t size))]
            [else
             (raise-argument-error 'compiler-sizeof
                                   "(or/c ctype-symbol? (listof ctype-symbol?))"
                                   sl)]))]))))

(define (ctype-malloc-mode c)
  (let ([t (ctype-our-rep c)])
    (if (or (eq? t 'gcpointer)
            (eq? t 'bytes)
            (eq? t 'scheme)
            (eq? t 'string)
            (eq? t 'string/ucs-4)
            (eq? t 'string/utf-16))
        'nonatomic
        'atomic)))

(define/who (ctype-sizeof c)
  (check who ctype? c)
  (case (ctype-host-rep c)
    [(void) 0]
    [(boolean int) 4]
    [(double) 8]
    [(float) 4]
    [(integer-8 unsigned-8) 1]
    [(integer-16 unsigned-16) 2]
    [(integer-32 unsigned-32) 4]
    [(integer-64 unsigned-64) 8]
    [else
     (if (compound-ctype? c)
         (compound-ctype-size c)
         ;; Everything else is pointer-sized:
         (foreign-sizeof 'void*))]))

(define (ctypes-sizeof+alignof base alignment)
  (let ([align (lambda (size algn)
                 (let ([amt (modulo size (or alignment algn))])
                   (if (zero? amt)
                       size
                       (+ size (- algn amt)))))])
    (let loop ([types base] [size 0] [max-align 1])
      (cond
       [(null? types) (values (align size max-align)
                              max-align)]
       [else (let ([sz (ctype-sizeof (car types))]
                   [algn (ctype-alignof (car types))])
               (loop (cdr types)
                     (+ (align size algn)
                        sz)
                     (max algn max-align)))]))))

(define/who (ctype-alignof c)
  (check who ctype? c)
  (cond
   [(compound-ctype? c)
    (compound-ctype-alignment c)]
   [else
    (case (ctype-host-rep c)
      [(boolean int) (foreign-alignof 'int)]
      [(double) (foreign-alignof 'double)]
      [(float) (foreign-alignof 'float)]
      [(integer-8 unsigned-8) (foreign-alignof 'integer-8)]
      [(integer-16 unsigned-16) (foreign-alignof 'integer-16)]
      [(integer-32 unsigned-32) (foreign-alignof 'integer-32)]
      [(integer-64 unsigned-64) (foreign-alignof 'integer-64)]
      [else
       ;; Everything else is pointer-sized:
       (foreign-alignof 'void*)])]))

(define/who (cpointer-gcable? p)
  (let ([p (unwrap-cpointer who p)])
    (or (bytes? p)
        (and (authentic-cpointer? p)
             (let ([memory (cpointer-memory p)])
               (or (bytes? memory)
                   (flvector? memory)))))))

;; ----------------------------------------

(define-record-type (ffi-lib make-ffi-lib ffi-lib?)
  (fields handle name))

(define ffi-lib*
  (case-lambda
   [(name) (ffi-lib* name #f #f)]
   [(name fail-as-false?) (ffi-lib* name fail-as-false? #f)]
   [(name fail-as-false? as-global?)
    (let ([name (if (string? name)
                    (string->immutable-string name)
                    name)])
      (ffi-get-lib 'ffi-lib
                   name
                   as-global?
                   fail-as-false?
                   (lambda (h)
                     (make-ffi-lib h name))))]))

(define/who (ffi-lib-unload lib)
  (check who ffi-lib? lib)
  (ffi-unload-lib (ffi-lib-handle lib))) 

(define-record-type (cpointer/ffi-obj make-ffi-obj ffi-obj?)
  (parent cpointer)
  (fields lib name))

(define/who (ffi-obj name lib)
  (check who bytes? name)
  (check who ffi-lib? lib)
  (let ([name (bytes->immutable-bytes name)])
    (ffi-get-obj who
                 (ffi-lib-handle lib)
                 (ffi-lib-name lib)
                 name
                 (lambda (ptr)
                   (make-ffi-obj (ffi-ptr->address ptr) #f lib name)))))

(define (ffi-obj-name obj)
  (cpointer/ffi-obj-name obj))

(define (ffi-obj-lib obj)
  (cpointer/ffi-obj-lib obj))

(define ffi-get-lib
  ;; Placeholder implementation that either fails
  ;; or returns a dummy value:
  (lambda (who name as-global? fail-as-false? success-k)
    (if fail-as-false?
        #f
        (success-k #f))))

(define ffi-unload-lib
  ;; Placeholder implementation that does nothing:
  (lambda (lib)
    (void)))

(define ffi-get-obj
  ;; Placeholder implementation that always fails:
  (lambda (who lib lib-name name success-k)
    (raise
     (|#%app|
      exn:fail:filesystem
      (format "~a: not yet ready\n  name: ~a" who name)
      (current-continuation-marks)))))

(define ffi-ptr->address
  ;; Placeholder implementation
  (lambda (p) p))

(define (set-ffi-get-lib-and-obj! do-ffi-get-lib do-ffi-get-obj do-ffi-unload-lib do-ffi-ptr->address)
  (set! ffi-get-lib do-ffi-get-lib)
  (set! ffi-get-obj do-ffi-get-obj)
  (set! ffi-unload-lib do-ffi-unload-lib)
  (set! ffi-ptr->address do-ffi-ptr->address))

;; ----------------------------------------

(define/who ptr-ref
  (case-lambda
   [(p type)
    (check who cpointer? p)
    (check who ctype? type)
    (c->s type (foreign-ref* type p 0))]
   [(p type offset)
    (check who cpointer? p)
    (check who ctype? type)
    (check who exact-integer? offset)
    (c->s type (foreign-ref* type
                             p
                             (* (ctype-sizeof type) offset)))]
   [(p type abs-tag offset)
    (check who cpointer? p)
    (check who ctype? type)
    (check who (lambda (p) (eq? p 'abs)) :contract "'abs" abs-tag)
    (check who exact-integer? offset)
    (c->s type (foreign-ref* type p offset))]))

(define (foreign-ref* type orig-p offset)
  (cond
   [(and (ffi-obj? orig-p)
         (eq? 'fpointer (ctype-our-rep type)))
    ;; Special case for `ptr-ref` on a function-type ffi-object:
    ;; cancel a level of indirection and preserve `ffi-obj`ness
    ;; to keep its name
    orig-p]
   [else
    (cond
     [(compound-ctype? type)
      ;; Instead of copying, get a pointer within `p`:
      (do-ptr-add orig-p offset #f)]
     [else
      (let ([p (unwrap-cpointer 'foreign-ref* orig-p)]
            [host-rep (ctype-host-rep type)])
        (let-values ([(memory mem-offset) (cpointer-address+offset p)])
          (cond
            [(and (eq? 'scheme-object host-rep)
                  (reference-bytevector? memory))
             (bytevector-reference-ref memory (+ offset mem-offset))]
            [(and (eq? 'uptr host-rep)
                  (reference-bytevector? memory))
             ;; used for string conversions; allow Scheme or foreign pointer
             (bytevector-reference*-ref memory (+ offset mem-offset))]
            [(and (eq? 'void* host-rep)
                  (reference-bytevector? memory))
             ;; used for _pointer and _gcpointer
             (case (ctype-our-rep type)
               [(gcpointer)
                (bytevector-reference-ref memory (+ offset mem-offset))]
               [else
                ;; Although `bytevector-reference*-ref` would be sensible
                ;; here, since a non-GCable pointer that overlaps with the
                ;; GC pages is likely to go wrong with a GC, we return a
                ;; non-GC-pointer representation and don't automatically
                ;; fix up a GCable-pointer reference (if for no other reason
                ;; then consistency with BC)
                (if (fx= 8 (foreign-sizeof 'ptr))
                    (bytevector-u64-native-ref memory (+ mem-offset offset))
                    (bytevector-u32-native-ref memory (+ mem-offset offset)))])]
            [else
             ;; Disable interrupts to avoid a GC:
             (with-interrupts-disabled*
              ;; Special treatment is needed for 'scheme-object, since the
              ;; host Scheme rejects the use of 'scheme-object with
              ;; `foreign-ref`
              (let ([v (foreign-ref (if (eq? host-rep 'scheme-object)
                                        'uptr
                                        host-rep)
                                    (+ (memory-address memory) mem-offset)
                                    offset)])
                (case host-rep
                  [(scheme-object) (reference-address->object v)]
                  [else
                   (case (ctype-our-rep type)
                     [(gcpointer) (addr->gcpointer-memory v)]
                     [else v])])))])))])]))

(define/who ptr-set!
  (case-lambda
   [(p type v)
    (check who cpointer? p)
    (check who ctype? type)
    (foreign-set!* who
                   type
                   p
                   0
                   v)]
   [(p type offset v)
    (check who cpointer? p)
    (check who ctype? type)
    (check who exact-integer? offset)
    (foreign-set!* who
                   type
                   p
                   (* (ctype-sizeof type) offset)
                   v)]
   [(p type abs-tag offset v)
    (check who cpointer? p)
    (check who ctype? type)
    (check who (lambda (p) (eq? p 'abs)) :contract "'abs" abs-tag)
    (check who exact-integer? offset)
    (foreign-set!* who
                   type
                   p
                   offset
                   v)]))

(define-syntax-rule (define-fast-ptr-ops ref set _type ok-v? bytes-ref bytes-set foreign-type type-bits)
  (begin
    (define (ref p offset abs?)
      (let ([simple-p (if (bytevector? p)
                          p
                          (and (authentic-cpointer? p)
                               (let ([m (cpointer-memory p)])
                                 (and (or (bytevector? m)
                                          (exact-integer? m))
                                      m))))])
        (cond
         [(and simple-p
               (fixnum? offset)
               (or (not abs?) (fx= 0 (fxand offset (fx- (fxsll 1 type-bits) 1)))))
          (let ([offset (let ([offset (if abs? offset (fxsll offset type-bits))])
                          (if (cpointer+offset? p)
                              (+ offset (cpointer+offset-offset p))
                              offset))])
            (if (bytevector? simple-p)
                (bytes-ref simple-p offset)
                (foreign-ref 'foreign-type simple-p offset)))]
         [else
          (if abs?
              (ptr-ref p _type 'abs offset)
              (ptr-ref p _type offset))])))
    (define (set p offset v abs?)
      (let ([simple-p (if (bytevector? p)
                          p
                          (and (authentic-cpointer? p)
                               (let ([m (cpointer-memory p)])
                                 (and (or (bytevector? m)
                                          (exact-integer? m))
                                      m))))])
        (cond
         [(and simple-p
               (fixnum? offset)
               (or (not abs?) (fx= 0 (fxand offset (fx- (fxsll 1 type-bits) 1))))
               (ok-v? v))
          (let ([offset (let ([offset (if abs? offset (fxsll offset type-bits))])
                          (if (cpointer+offset? p)
                              (+ offset (cpointer+offset-offset p))
                              offset))])
            (if (bytevector? simple-p)
                (bytes-set simple-p offset v)
                (foreign-set! 'foreign-type simple-p offset v)))]
         [else
          (if abs?
              (ptr-set! p _type 'abs offset v)
              (ptr-set! p _type offset v))])))))

(define (fixnum-in-range? lo hi) (lambda (v) (and (fixnum? v) (fx>= v lo) (fx>= v hi))))
(define (in-range? lo hi) (lambda (v) (and (exact-integer? v) (>= v lo) (>= v hi))))

;; Schemify optimizes `(ptr-ref p _uint16 offset v)` to `(ptr-set!/uint16 p (fxlshift offset 1) v #f)`, etc.
(define-fast-ptr-ops ptr-ref/int8 ptr-set!/int8 _int8 (fixnum-in-range? -128 127) bytevector-s8-ref bytevector-s8-set! integer-8 0)
(define-fast-ptr-ops ptr-ref/uint8 ptr-set!/uint8 _uint8 byte? bytevector-u8-ref bytevector-u8-set! unsigned-8 0)
(define-fast-ptr-ops ptr-ref/int16 ptr-set!/int16 _int16 (fixnum-in-range? -32768 32767) bytevector-s16-native-ref bytevector-s16-native-set! integer-16 1)
(define-fast-ptr-ops ptr-ref/uint16 ptr-set!/uint16 _uint16 (fixnum-in-range? 0 65535) bytevector-u16-native-ref bytevector-u16-native-set! unsigned-16 1)
(define-fast-ptr-ops ptr-ref/int32 ptr-set!/int32 _int32 (in-range? -2147483648 2147483647) bytevector-s32-native-ref bytevector-s32-native-set! integer-32 2)
(define-fast-ptr-ops ptr-ref/uint32 ptr-set!/uint32 _uint32 (in-range? 0 4294967296) bytevector-u32-native-ref bytevector-u32-native-set! unsigned-32 2)
(define-fast-ptr-ops ptr-ref/int64 ptr-set!/int64 _int64 (in-range? -9223372036854775808 9223372036854775807) bytevector-s64-native-ref bytevector-s64-native-set! integer-64 3)
(define-fast-ptr-ops ptr-ref/uint64 ptr-set!/uint64 _uint64 (in-range? 0 18446744073709551616) bytevector-u64-native-ref bytevector-u64-native-set! unsigned-64 3)
(define-fast-ptr-ops ptr-ref/double ptr-set!/double _double flonum? bytevector-ieee-double-native-ref bytevector-ieee-double-native-set! double 3)
(define-fast-ptr-ops ptr-ref/float ptr-set!/float _float flonum? bytevector-ieee-single-native-ref bytevector-ieee-single-native-set! float 2)

(define (foreign-set!* who type orig-p offset orig-v)
  (let ([p (unwrap-cpointer 'foreign-set!* orig-p)])
    (cond
     [(compound-ctype? type)
      ;; Corresponds to a copy, since `v` is represented by a pointer
      (memcpy* p offset
               (s->c who type orig-v) 0
               (compound-ctype-size type)
               #f)]
     [else
      (let ([host-rep (ctype-host-rep type)]
            [v (s->c who type orig-v)])
        (let-values ([(memory mem-offset) (cpointer-address+offset p)])
          (cond
            [(and (eq? 'scheme-object host-rep)
                  (reference-bytevector? memory))
             (bytevector-reference-set! memory (+ mem-offset offset) v)]
            [(and (or (eq? 'void* host-rep)
                      (eq? 'uptr host-rep))
                  (reference-bytevector? memory))
             (let ([v (cond
                        [(not v) #f]
                        [(bytes? v) v]
                        [(flvector? v) v]
                        [(authentic-cpointer? v)
                         (let-values ([(memory offset) (cpointer-address+offset v)])
                           (cond
                             [(integer? memory) (+ memory offset)]
                             [(zero? offset) memory]
                             [else (raise-arguments-error 'ptr-set!
                                                          "cannot install value into non-atomic memory"
                                                          "value" orig-v
                                                          "destination" orig-p)]))])])
               (cond
                 [(integer? v)
                  (if (fx= 8 (foreign-sizeof 'ptr))
                      (bytevector-u64-native-set! memory (+ mem-offset offset) v)
                      (bytevector-u32-native-set! memory (+ mem-offset offset) v))]
                 [else
                  (bytevector-reference-set! memory (+ mem-offset offset) v)]))]
            [else
             ;; Disable interrupts to avoid a GC:
             (with-interrupts-disabled*
              ;; Special treatment is needed for 'scheme-object, since
              ;; the host Scheme rejects the use of 'scheme-object with
              ;; `foreign-set!`
              (foreign-set! (if (eq? host-rep 'scheme-object)
                                'uptr
                                host-rep)
                            (+ (memory-address memory) mem-offset)
                            offset
                            (case host-rep
                              [(scheme-object) (object->reference-address v)]
                              [(void* uptr) (cpointer-address v)]
                              [else v])))])))])))

(define (memcpy* to to-offset from from-offset len move?)
  (let ([to (unwrap-cpointer* 'memcpy to)]
        [from (unwrap-cpointer* 'memcpy from)])
    (with-interrupts-disabled*
     (let ([to (+ (cpointer*-address to) to-offset)]
           [from (+ (cpointer*-address from) from-offset)])
       (cond
         [(and move?
               ;; overlap?
               (or (<= to from (+ to len -1))
                   (<= from to (+ from len -1)))
               ;; shifting up?
               (< from to))
          ;; Copy from high to low to move in overlapping region
          (let loop ([len len])
            (unless (fx= len 0)
              (cond
                [(and (> (fixnum-width) 64)
                      (fx>= len 8))
                 (let ([len (fx- len 8)])
                   (foreign-set! 'integer-64 to len
                                 (foreign-ref 'integer-64 from len))
                   (loop len))]
                [(and (> (fixnum-width) 32)
                      (fx>= len 4))
                 (let ([len (fx- len 4)])
                   (foreign-set! 'integer-32 to len
                                 (foreign-ref 'integer-32 from len))
                   (loop len))]
                [(fx>= len 2)
                 (let ([len (fx- len 2)])
                   (foreign-set! 'integer-16 to len
                                 (foreign-ref 'integer-16 from len))
                   (loop len))]
                [else
                 (let ([len (fx- len 1)])
                   (foreign-set! 'integer-8 to len
                                 (foreign-ref 'integer-8 from len))
                   (loop len))])))]
         [else
          (let loop ([pos 0])
            (when (fx< pos len)
              (cond
                [(and (> (fixnum-width) 64)
                      (fx<= (fx+ pos 8) len))
                 (foreign-set! 'integer-64 to pos
                               (foreign-ref 'integer-64 from pos))
                 (loop (fx+ pos 8))]
                [(and (> (fixnum-width) 32)
                      (fx<= (fx+ pos 4) len))
                 (foreign-set! 'integer-32 to pos
                               (foreign-ref 'integer-32 from pos))
                 (loop (fx+ pos 4))]
                [(fx<= (fx+ pos 2) len)
                 (foreign-set! 'integer-16 to pos
                               (foreign-ref 'integer-16 from pos))
                 (loop (fx+ pos 2))]
                [else
                 (foreign-set! 'integer-8 to pos
                               (foreign-ref 'integer-8 from pos))
                 (loop (fx+ pos 1))])))])))))

(define memcpy/memmove
  (case-lambda
   [(who cptr src-cptr count)
    (check who cpointer? cptr)
    (check who cpointer? src-cptr)
    (check who exact-nonnegative-integer? count)
    (memcpy* cptr 0 src-cptr 0 count (eq? who 'memmove))]
   [(who cptr offset/src-cptr/src-cptr src-cptr/offset/count count/count/type)
    (check who cpointer? cptr)
    (cond
     [(cpointer? offset/src-cptr/src-cptr)
      ;; use y or z of x/y/z
      (cond
       [(ctype? count/count/type)
        ;; use z of x/y/z
        (check who exact-nonnegative-integer? src-cptr/offset/count)
        (memcpy* cptr 0 (unwrap-cpointer who offset/src-cptr/src-cptr) 0 (* src-cptr/offset/count (ctype-sizeof count/count/type)) (eq? who 'memmove))]
       [else
        ;; use y of x/y/z
        (check who exact-integer? src-cptr/offset/count)
        (check who exact-nonnegative-integer? count/count/type)
        (memcpy* cptr 0 (unwrap-cpointer who offset/src-cptr/src-cptr) src-cptr/offset/count src-cptr/offset/count (eq? who 'memmove))])]
     [else
      ;; use x of x/y/z
      (check who exact-integer? offset/src-cptr/src-cptr)
      (check who cpointer? src-cptr/offset/count)
      (check who exact-nonnegative-integer? count/count/type)
      (memcpy* cptr offset/src-cptr/src-cptr src-cptr/offset/count 0 count/count/type (eq? who 'memmove))])]
   [(who cptr offset src-cptr src-offset/count count/type)
    (check who cpointer? cptr)
    (check who exact-integer? offset)
    (check who cpointer? src-cptr)
    (cond
     [(ctype? count/type)
      ;; use y of x/y
      (check who exact-nonnegative-integer? src-offset/count)
      (let ([sz (ctype-sizeof count/type)])
        (memcpy* cptr (* sz offset) src-cptr 0 (* src-offset/count sz) (eq? who 'memmove)))]
     [else
      ;; use x of x/y
      (check who exact-integer? src-offset/count)
      (check who exact-nonnegative-integer? count/type)
      (memcpy* cptr offset src-cptr src-offset/count count/type (eq? who 'memmove))])]
   [(who cptr offset src-cptr src-offset count type)
    (check who cpointer? cptr)
    (check who exact-integer? offset)
    (check who cpointer? src-cptr)
    (check who exact-integer? src-offset)
    (check who ctype? type)
    (let ([sz (ctype-sizeof type)])
      (memcpy* cptr (* offset sz) src-cptr (* src-offset sz) (* count sz) (eq? who 'memmove)))]))

(define/who memcpy
  (case-lambda
   [(cptr src-cptr count)
    (memcpy/memmove who cptr src-cptr count)]
   [(cptr offset/src-cptr src-cptr/count count/type)
    (memcpy/memmove who cptr offset/src-cptr src-cptr/count count/type)]
   [(cptr offset src-cptr src-offset/count count/type)
    (memcpy/memmove who cptr offset src-cptr src-offset/count count/type)]
   [(cptr offset src-cptr src-offset count type)
    (memcpy/memmove who cptr offset src-cptr src-offset count type)]))

(define/who memmove
  (case-lambda
   [(cptr src-cptr count)
    (memcpy/memmove who cptr src-cptr count)]
   [(cptr offset/src-cptr src-cptr/count count/type)
    (memcpy/memmove who cptr offset/src-cptr src-cptr/count count/type)]
   [(cptr offset src-cptr src-offset/count count/type)
    (memcpy/memmove who cptr offset src-cptr src-offset/count count/type)]
   [(cptr offset src-cptr src-offset count type)
    (memcpy/memmove who cptr offset src-cptr src-offset count type)]))

;; ----------------------------------------

(define (memset* to to-offset byte len)
  (let ([to (unwrap-cpointer* 'memset to)])
    (with-interrupts-disabled*
     (let ([to (+ (cpointer*-address to) to-offset)])
       (let loop ([to to] [len len])
         (unless (fx= len 0)
           (foreign-set! 'unsigned-8 to 0 byte)
           (loop (+ to 1) (fx- len 1))))))))

(define/who memset
  (case-lambda
   [(cptr byte count)
    (check who cpointer? cptr)
    (check who byte? byte)
    (check who exact-nonnegative-integer? count)
    (memset* cptr 0 byte count)]
   [(cptr byte/offset count/byte type/count)
    (check who cpointer? cptr)
    (cond
     [(ctype? type/count)
      (check who byte? byte/offset)
      (check who exact-nonnegative-integer? count/byte)
      (memset* cptr 0 byte/offset (fx* count/byte (ctype-sizeof type/count)))]
     [else
      (check who exact-integer? byte/offset)
      (check who byte? count/byte)
      (check who exact-nonnegative-integer? type/count)
      (memset* cptr byte/offset count/byte type/count)])]
   [(cptr offset byte count type)
    (check who cpointer? cptr)
    (check who exact-integer? offset)
    (check who byte? byte)
    (check who exact-nonnegative-integer? count)
    (check who ctype? type)
    (memset* cptr (fx* offset (ctype-sizeof type)) byte (fx* count (ctype-sizeof type)))]))

;; ----------------------------------------

;; With finalization through an ordered guardian,
;; a "late" weak hash table is just a hash table.
(define (make-late-weak-hasheq)
  (make-weak-hasheq))

;; Same for late weak boxes:
(define (make-late-weak-box b)
  (make-weak-box b))

(define malloc
  ;; Recognize common ordering as fast cases, and dispatch to
  ;; a general handler to arbtrary argument order
  (case-lambda
   [(arg1)
    (cond
     [(nonnegative-fixnum? arg1)
      (normalized-malloc arg1 'atomic)]
     [(ctype? arg1)
      (normalized-malloc (ctype-sizeof arg1) (ctype-malloc-mode arg1))]
     [else
      (do-malloc (list arg1))])]
   [(arg1 arg2)
    (cond
     [(and (nonnegative-fixnum? arg1)
           (ctype? arg2))
      (normalized-malloc (* arg1 (ctype-sizeof arg2)) (ctype-malloc-mode arg2))]
     [(and (ctype? arg1)
           (nonnegative-fixnum? arg2))
      (normalized-malloc (* arg2 (ctype-sizeof arg1)) (ctype-malloc-mode arg1))]
     [(and (nonnegative-fixnum? arg1)
           (malloc-mode? arg2))
      (normalized-malloc arg1 arg2)]
     [else
      (do-malloc (list arg1 arg2))])]
   [(arg1 arg2 arg3) (do-malloc (list arg1 arg2 arg3))]
   [(arg1 arg2 arg3 arg4) (do-malloc (list arg1 arg2 arg3 arg4))]
   [(arg1 arg2 arg3 arg4 arg5) (do-malloc (list arg1 arg2 arg3 arg4 arg5))]))

(define (do-malloc args)
  (let ([duplicate-argument
         (lambda (what a1 a2)
           (raise-arguments-error 'malloc
                                  (string-append "multiple " what " arguments")
                                  "first" a1
                                  "second" a2))])
    (let loop ([args args] [count #f] [type #f] [copy-from #f] [mode #f] [fail-mode #f])
      (cond
       [(null? args)
        (let* ([len (* (or count 1) (if type (ctype-sizeof type) 1))]
               [p (normalized-malloc len
                                     (or mode (if type (ctype-malloc-mode type) 'atomic)))])
          (when copy-from
            (memcpy* p 0 copy-from 0 len #f))
          p)]
       [(nonnegative-fixnum? (car args))
        (if count
            (duplicate-argument "size" count (car args))
            (loop (cdr args) (car args) type copy-from mode fail-mode))]
       [(ctype? (car args))
        (if type
            (duplicate-argument "type" type (car args))
            (loop (cdr args) count (car args) copy-from mode fail-mode))]
       [(and (cpointer? (car args)) (car args))
        (if copy-from
            (duplicate-argument "source for copy" copy-from (car args))
            (loop (cdr args) count type (car args) mode fail-mode))]
       [(malloc-mode? (car args))
        (if mode
            (duplicate-argument "mode" mode (car args))
            (loop (cdr args) count type copy-from (car args) fail-mode))]
       [(eq? (car args) 'failok)
        (if fail-mode
            (duplicate-argument "failure mode" fail-mode (car args))
            (loop (cdr args) count type copy-from mode (car args)))]
       [else
        (raise-argument-error 'malloc
                              (string-append "(or/c (and/c exact-nonnegative-integer? fixnum?)\n"
                                             "      ctype? cpointer?\n"
                                             "      (or/c 'raw 'atomic 'nonatomic 'tagged\n"
                                             "            'atomic-interior 'interior\n"
                                             "            'stubborn 'uncollectable 'eternal)\n"
                                             "      'fail-ok)")
                              (car args))]))))

(define (normalized-malloc size mode)
  (unless (and (fixnum? size)
               (fx<? size 4096))
    (guard-large-allocation 'malloc "allocation" size 1))
  (cond
   [(eqv? size 0) #f]
   [(eq? mode 'raw)
    (make-cpointer (foreign-alloc size) #f)]
   [(eq? mode 'atomic)
    (make-cpointer (make-bytevector size) #f)]
   [(eq? mode 'nonatomic)
    (make-cpointer (make-reference-bytevector size) #f)]
   [(eq? mode 'atomic-interior)
    ;; This is not quite the same as Racket BC, because interior
    ;; pointers are not allowed as GCable pointers. So, "interior"
    ;; just means "doesn't move".
    (make-cpointer (make-immobile-bytevector size) #f)]
   [(eq? mode 'interior)
    ;; Ditto
    (make-cpointer (make-immobile-reference-bytevector size) #f)]
   [else
    (raise-unsupported-error 'malloc
                             (format "'~a mode is not supported" mode))]))

(define/who (free p)
  (let ([p (unwrap-cpointer who p)])
    (with-interrupts-disabled*
     (foreign-free (cpointer-address p)))))

(define (lock-cpointer p)
  (when (authentic-cpointer? p)
    (lock-object (cpointer-memory p))))

(define (unlock-cpointer p)
  (when (authentic-cpointer? p)
    (unlock-object (cpointer-memory p))))

(define-record-type (cpointer/cell make-cpointer/cell cpointer/cell?)
  (parent cpointer)
  (fields))

(define immobile-cells (make-eq-hashtable))

(define (malloc-immobile-cell v)
  (let ([vec (make-immobile-reference-bytevector (foreign-sizeof 'ptr))])
    (bytevector-reference-set! vec 0 v)
    (with-global-lock
     (eq-hashtable-set! immobile-cells vec #t))
    (make-cpointer vec #f)))

(define (free-immobile-cell b)
  (with-global-lock
   (eq-hashtable-delete! immobile-cells (cpointer-memory b))))

(define (immobile-cell-ref b)
  (bytevector-reference-ref (cpointer-memory b) 0))

(define (immobile-cell->address b)
  (object->reference-address (cpointer-memory b)))

(define (address->immobile-cell a)
  (make-cpointer (reference-address->object a) #f))

(define (malloc-mode? v)
  (#%memq v '(raw atomic nonatomic tagged
                  atomic-interior interior
                  stubborn uncollectable eternal)))

(define (end-stubborn-change p)
  (raise-unsupported-error 'end-stubborn-change))

(define/who (extflvector->cpointer extfl-vector)
  (raise-unsupported-error who))

(define/who (vector->cpointer vec)
  (raise-unsupported-error who))

(define (flvector->cpointer flvec)
  (make-cpointer flvec #f))

;; ----------------------------------------

(define the-foreign-guardian (make-guardian))

;; Can be called in any host thread, but all other
;; threads are stopped
(define (poll-foreign-guardian)
  (let ([v (the-foreign-guardian)])
    (when v
      (v)
      (poll-foreign-guardian))))

(define (unsafe-add-global-finalizer v proc)
  (with-global-lock (the-foreign-guardian v proc)))

;; ----------------------------------------

(define eval/foreign
  (lambda (expr mode)
    (call-with-system-wind (lambda () (eval expr)))))

(define (set-foreign-eval! proc)
  (set! eval/foreign proc))

;; Cache generated code for an underlying foreign call or callable shape:
(define-thread-local ffi-expr->code (make-weak-hash))         ; expr to weak cell of code
(define-thread-local ffi-code->expr (make-weak-eq-hashtable)) ; keep exprs alive as long as code lives

(define/who ffi-call
  (case-lambda
   [(p in-types out-type)
    (ffi-call p in-types out-type #f #f #f #f)]
   [(p in-types out-type abi)
    (ffi-call p in-types out-type abi #f #f #f #f)]
   [(p in-types out-type abi save-errno)
    (ffi-call p in-types out-type abi save-errno #f #f #f)]
   [(p in-types out-type abi save-errno orig-place?)
    (ffi-call p in-types out-type abi save-errno orig-place? #f #f #f)]
   [(p in-types out-type abi save-errno orig-place? lock-name)
    (ffi-call p in-types out-type abi save-errno orig-place? lock-name #f #f #f)]
   [(p in-types out-type abi save-errno orig-place? lock-name blocking?)
    (ffi-call p in-types out-type abi save-errno orig-place? lock-name blocking? #f #f)]
   [(p in-types out-type abi save-errno orig-place? lock-name blocking? varargs-after)
    (ffi-call p in-types out-type abi save-errno orig-place? lock-name blocking? varargs-after #f)]
   [(p in-types out-type abi save-errno orig-place? lock-name blocking? varargs-after exns?)
    (check who cpointer? p)
    (check-ffi-call who in-types out-type abi varargs-after save-errno lock-name)
    ((ffi-call/callable #t in-types out-type abi varargs-after
                        save-errno lock-name (and blocking? #t) (and orig-place? #t) #f (and exns? #t)
                        #f)
     p)]))

(define/who ffi-call-maker
  (case-lambda
   [(in-types out-type)
    (ffi-call-maker in-types out-type #f #f #f #f #f)]
   [(in-types out-type abi)
    (ffi-call-maker in-types out-type abi #f #f #f #f)]
   [(in-types out-type abi save-errno)
    (ffi-call-maker in-types out-type abi save-errno #f #f #f)]
   [(in-types out-type abi save-errno orig-place?)
    (ffi-call-maker in-types out-type abi save-errno orig-place? #f #f #f)]
   [(in-types out-type abi save-errno orig-place? lock-name)
    (ffi-call-maker in-types out-type abi save-errno orig-place? lock-name #f #f #f)]
   [(in-types out-type abi save-errno orig-place? lock-name blocking?)
    (ffi-call-maker in-types out-type abi save-errno orig-place? lock-name blocking? #f #f)]
   [(in-types out-type abi save-errno orig-place? lock-name blocking? varargs-after)
    (ffi-call-maker in-types out-type abi save-errno orig-place? lock-name blocking? varargs-after #f)]
   [(in-types out-type abi save-errno orig-place? lock-name blocking? varargs-after exns?)
    (check-ffi-call who in-types out-type abi varargs-after save-errno lock-name)
    (ffi-call/callable #t in-types out-type abi varargs-after
                       save-errno lock-name (and blocking? #t) (and orig-place? #t) #f (and exns? #t)
                       #f)]))

(define (check-ffi-call who in-types out-type abi varargs-after save-errno lock-name)
  (check-ffi who in-types out-type abi varargs-after)
  (check who (lambda (save-errno) (#%memq save-errno '(#f posix windows)))
         :contract "(or/c #f 'posix 'windows)"
         save-errno)
  (check who string? :or-false lock-name))

;; For sanity checking of callbacks during a blocking callout:
(define-virtual-register currently-blocking? #f)

(define-syntax-rule (retain v ... e)
  ;; Make sure that the `v ...` stay live until `e` produces a result,
  ;; so uses of the FFI can rely on passing an argument to a foreign
  ;; function as retaining the argument until the function returns.
  (let ([result e])
    (keep-live v) ...
    result))

(define call-locks (make-eq-hashtable))

(define (ffi-call/callable call? in-types out-type abi varargs-after
                           save-errno lock-name blocking? orig-place? atomic? exns?
                           async-apply)
  (let* ([conv* (let ([conv* (case abi
                               [(stdcall) '(__stdcall)]
                               [(sysv) '(__cdecl)]
                               [else '()])])
                  (if varargs-after
                      (cons `(__varargs_after ,varargs-after) conv*)
                      conv*))]
         [by-value? (lambda (type)
                      ;; An 'array rep is compound, but should be
                      ;; passed as a pointer, so only pass 'struct and
                      ;; 'union "by value":
                      (#%memq (ctype-host-rep type) '(struct union)))]
         [array-rep-to-pointer-rep (lambda (host-rep)
                                     (if (eq? host-rep 'array)
                                         'void*
                                         host-rep))]
         [next!-id (let ([counter 0])
                     ;; Like `gensym`, but deterministic --- and doesn't
                     ;; have to be totally unique, as long as it doesn't
                     ;; collide with other code that we generate
                     (lambda ()
                       (set! counter (add1 counter))
                       (string->symbol (string-append "type_" (number->string counter)))))]
         [ids (map (lambda (in-type)
                     (and (by-value? in-type)
                          (next!-id)))
                   in-types)]
         [ret-id (and (by-value? out-type)
                      (next!-id))]
         [decls (let loop ([in-types in-types] [ids ids] [decls '()])
                  (cond
                   [(null? in-types) decls]
                   [(car ids)
                    (let ([id-decls ((compound-ctype-get-decls (car in-types)) (car ids) next!-id)])
                      (loop (cdr in-types) (cdr ids) (append decls id-decls)))]
                   [else
                    (loop (cdr in-types) (cdr ids) decls)]))]
         [ret-decls (if ret-id
                        ((compound-ctype-get-decls out-type) ret-id next!-id)
                        '())]
         [ret-size (and ret-id (ctype-sizeof out-type))]
         [ret-malloc-mode (and ret-id (compound-ctype-malloc-mode out-type))]
         [gen-proc+ret-maker+arg-makers
          (let ([expr `(let ()
                         ,@decls
                         ,@ret-decls
                         (list
                          (lambda (to-wrap)
                            (,(if call? 'foreign-procedure 'foreign-callable)
                             ,@conv*
                             ,@(if (or blocking? async-apply) '(__collect_safe) '())
                             to-wrap
                             ,(map (lambda (in-type id)
                                     (if id
                                         `(& ,id)
                                         (array-rep-to-pointer-rep
                                          (ctype-host-rep in-type))))
                                   in-types ids)
                             ,(if ret-id
                                  `(& ,ret-id)
                                  (array-rep-to-pointer-rep
                                   (ctype-host-rep out-type)))))
                          ,(and call?
                                ret-id
                                `(lambda (p)
                                   (make-ftype-pointer ,ret-id p)))
                          ,@(if call?
                                (map (lambda (id)
                                       (and id
                                            `(lambda (p)
                                               (make-ftype-pointer ,id p))))
                                     ids)
                                '())))])
            (let* ([wb (with-interrupts-disabled*
                        (hash-ref ffi-expr->code expr #f))]
                   [code (if wb (car wb) #!bwp)])
              (if (eq? code #!bwp)
                  (let ([code (eval/foreign expr (if call? 'comp-ffi-call 'comp-ffi-back))])
                    (hashtable-set! ffi-code->expr (car code) expr)
                    (with-interrupts-disabled*
                     (hash-set! ffi-expr->code expr (weak-cons code #f)))
                    code)
                  code)))]
         [gen-proc (car gen-proc+ret-maker+arg-makers)]
         [ret-maker (cadr gen-proc+ret-maker+arg-makers)]
         [arg-makers (cddr gen-proc+ret-maker+arg-makers)]
         [async-callback-queue (and (procedure? async-apply) (current-async-callback-queue))]
         [lock (and lock-name
                    (with-global-lock
                     (or (eq-hashtable-ref call-locks (string->symbol lock-name) #f)
                         (let ([lock (make-mutex)])
                           (eq-hashtable-set! call-locks (string->symbol lock-name) lock)
                           lock))))])
    (cond
     [call?
      (cond
       [(and (not ret-id)
             (not blocking?)
             (not orig-place?)
             (not exns?)
             (not save-errno)
             (#%andmap (lambda (in-type)
                         (case (ctype-host-rep in-type)
                           [(scheme-object struct union) #f]
                           [else #t]))
                       in-types))
        (let ([arity-mask (bitwise-arithmetic-shift-left 1 (length in-types))])
          (lambda (to-wrap)
            (let* ([proc-p (unwrap-cpointer 'ffi-call to-wrap)]
                   [proc (and (not (cpointer-needs-lock? proc-p))
                              (gen-proc (cpointer-address proc-p)))]
                   [name (cpointer->name proc-p)]
                   [unwrap (lambda (arg in-type)
                             (let ([c (s->c name in-type arg)])
                               (if (cpointer? c)
                                   (unwrap-cpointer 'ffi-call c)
                                   c)))]
                   [unpack (lambda (arg in-type)
                             (case (array-rep-to-pointer-rep (ctype-host-rep in-type))
                               [(void* uptr) (cpointer-address arg)]
                               [else arg]))])
              (do-procedure-reduce-arity-mask
               (cond
                 [proc
                  (let-syntax ([gen (lambda (stx)
                                      (syntax-case stx ()
                                        [(_ id ...)
                                         (with-syntax ([(type ...) (generate-temporaries #'(id ...))]
                                                       [(orig ...) (generate-temporaries #'(id ...))])
                                           (let ([make-proc
                                                  (lambda (lock)
                                                    #`(lambda (orig ...)
                                                        (let ([id (unwrap orig type)] ...)
                                                          (when #,lock (mutex-acquire #,lock))
                                                          (let ([r (retain
                                                                    orig ...
                                                                    (with-interrupts-disabled*
                                                                     (proc (unpack id type) ...)))])
                                                            (when #,lock (mutex-release #,lock))
                                                            (c->s out-type r)))))])
                                             #`(let*-values ([(type in-types) (values (car in-types) (cdr in-types))]
                                                             ...)
                                                 (if lock
                                                     #,(make-proc #'lock)
                                                     #,(make-proc #'#f)))))]))])
                    (case arity-mask
                      [(1) (gen)]
                      [(2) (gen a)]
                      [(4) (gen a b)]
                      [(8) (gen a b c)]
                      [(16) (gen a b c d)]
                      [(32) (gen a b c d e)]
                      [(64) (gen a b c d e f)]
                      [(128) (gen a b c d e f g)]
                      [(256) (gen a b c d e f g h)]
                      [else
                       (lambda orig-args
                         (let ([args (map (lambda (a t) (unwrap a t)) orig-args in-types)])
                           (c->s out-type (with-interrupts-disabled*
                                           (retain
                                            orig-args
                                            (#%apply proc (map (lambda (a t) (unpack a t)) args in-types)))))))]))]
                 [else
                  (lambda orig-args
                    (let ([args (map (lambda (a t) (unwrap a t)) orig-args in-types)])
                      (when lock (mutex-acquire lock))
                      (let ([r (with-interrupts-disabled*
                                (retain
                                 orig-args
                                 (#%apply (gen-proc (cpointer-address proc-p))
                                          (map (lambda (a t) (unpack a t)) args in-types))))])
                        (when lock (mutex-release lock))
                        (c->s out-type r))))])
               arity-mask
               name
               default-realm))))]
       [else
        (lambda (to-wrap)
          (let* ([proc-p (unwrap-cpointer 'ffi-call to-wrap)]
                 [name (cpointer->name proc-p)])
            (do-procedure-reduce-arity-mask
             (lambda orig-args
               (let* ([args (map (lambda (orig-arg in-type)
                                   (let ([arg (s->c name in-type orig-arg)])
                                     (if (and (cpointer? arg)
                                              (not (eq? 'scheme-object (ctype-host-rep in-type))))
                                         (unwrap-cpointer 'ffi-call arg)
                                         arg)))
                                 orig-args in-types)]
                      [r (let ([ret-ptr (and ret-id
                                             ;; result is a struct type; need to allocate space for it
                                             (normalized-malloc ret-size ret-malloc-mode))])
                           (let ([go (lambda ()
                                       (when lock (mutex-acquire lock))
                                       (with-interrupts-disabled*
                                        (when blocking? (currently-blocking? #t))
                                        (retain
                                         orig-args
                                         (let ([r (let ([args (append
                                                               (if ret-ptr
                                                                   (begin
                                                                     (lock-cpointer ret-ptr)
                                                                     (list (ret-maker (cpointer-address ret-ptr))))
                                                                   '())
                                                               (map (lambda (arg in-type maker)
                                                                      (let ([host-rep (array-rep-to-pointer-rep
                                                                                       (ctype-host-rep in-type))])
                                                                        (case host-rep
                                                                          [(void* uptr) (cpointer-address arg)]
                                                                          [(struct union)
                                                                           (maker (cpointer-address arg))]
                                                                          [else arg])))
                                                                    args in-types arg-makers))]
                                                        [proc (gen-proc (cpointer-address proc-p))])
                                                    (cond
                                                      [(not exns?)
                                                       (#%apply proc args)]
                                                      [else
                                                       (call-guarding-foreign-escape
                                                        (lambda () (#%apply proc args))
                                                        (lambda ()
                                                          (when lock (mutex-release lock))
                                                          (when blocking? (currently-blocking? #f))))]))])
                                           (when lock (mutex-release lock))
                                           (when blocking? (currently-blocking? #f))
                                           (case save-errno
                                             [(posix) (thread-cell-set! errno-cell (get-errno))]
                                             [(windows) (thread-cell-set! errno-cell (get-last-error))])
                                           (cond
                                            [ret-ptr (unlock-cpointer ret-ptr) ret-ptr]
                                            [(eq? (ctype-our-rep out-type) 'gcpointer)
                                             (addr->gcpointer-memory r)]
                                            [else r])))))])
                             (if (and orig-place?
                                      (not (eqv? 0 (get-thread-id))))
                                 (async-callback-queue-call orig-place-async-callback-queue (lambda (th) (th)) (lambda () (go)) #f #t #t)
                                 (go))))])
                 (c->s out-type r)))
             (fxsll 1 (length in-types))
             name
             default-realm)))])]
     [else ; callable
      (lambda (to-wrap)
        (gen-proc (lambda args ; if ret-id, includes an extra initial argument to receive the result
                    (let ([v (call-as-atomic-callback
                              (lambda ()
                                (unless async-apply
                                  ;; Sanity check; if the check fails, things can go bad from here on,
                                  ;; but we try to continue, anyway
                                  (when (currently-blocking?)
                                    (#%printf "non-async in callback during blocking: ~s\n" to-wrap)))
                                (s->c
                                 'callback
                                 out-type
                                 (apply to-wrap
                                        (let loop ([args (if ret-id (cdr args) args)] [in-types in-types])
                                          (cond
                                           [(null? args) '()]
                                           [else
                                            (let* ([arg (car args)]
                                                   [type (car in-types)]
                                                   [arg (c->s type
                                                              (case (ctype-host-rep type)
                                                                [(struct union)
                                                                 ;; Like Racket BC, refer to argument on stack:
                                                                 (make-cpointer (ftype-pointer-address arg) #f)
                                                                 #;
                                                                 (let* ([size (compound-ctype-size type)]
                                                                        [addr (ftype-pointer-address arg)]
                                                                        [bstr (make-bytevector size)])
                                                                   (memcpy* bstr 0 addr 0 size #f)
                                                                   (make-cpointer bstr #f))]
                                                                [else
                                                                 (cond
                                                                  [(eq? (ctype-our-rep type) 'gcpointer)
                                                                   (addr->gcpointer-memory arg)]
                                                                  [else arg])]))])
                                              (cons arg (loop (cdr args) (cdr in-types))))])))))
                              (or #t atomic?) ; force all callbacks to be atomic
                              async-apply
                              async-callback-queue)])
                      (if ret-id
                          (let* ([size (compound-ctype-size out-type)]
                                 [addr (ftype-pointer-address (car args))])
                            (memcpy* addr 0 v 0 size #f))
                          (case (ctype-host-rep out-type)
                            [(void* uptr) (cpointer-address v)]
                            [else v]))))))])))

(define (types->reps types next!-id)
  (let loop ([types types] [reps '()] [decls '()])
    (cond
     [(null? types) (values (reverse reps) decls)]
     [else
      (let ([type (car types)])
        (if (compound-ctype? type)
            (let* ([id (next!-id)]
                   [id-decls ((compound-ctype-get-decls type) id next!-id)])
              (loop (cdr types) (cons id reps) (append id-decls decls)))
            (loop (cdr types) (cons (ctype-host-rep type) reps) decls)))])))

;; Rely on the fact that a virtual register defaults to 0 to detect a
;; thread that we didn't start.
(define PLACE-UNKNOWN-THREAD 0)
(define PLACE-KNOWN-THREAD 1)
(define PLACE-MAIN-THREAD 2)
(define-virtual-register place-thread-category PLACE-KNOWN-THREAD)
(define (register-as-place-main!)
  (place-thread-category PLACE-MAIN-THREAD))

(define orig-place-async-callback-queue #f)
(define (remember-original-place!)
  (set! orig-place-async-callback-queue (current-async-callback-queue)))

;; Can be called in any Scheme thread
(define (call-as-atomic-callback thunk atomic? async-apply async-callback-queue)
  (cond
   [(eqv? (place-thread-category) PLACE-MAIN-THREAD)
    ;; In the main thread of a place. We must have gotten here by a
    ;; foreign call that called back, so interrupts are currently
    ;; disabled.
    (cond
     [(not atomic?)
      ;; reenable interrupts
      (enable-interrupts)
      (let ([v (thunk)])
        (disable-interrupts)
        v)]
     [else
      ;; Inform the scheduler that it's in atomic mode
      (scheduler-start-atomic)
      ;; Now that the schedule is in atomic mode, reenable interrupts (for GC)
      (enable-interrupts)
      ;; See also `call-guarding-foreign-escape`, which will need to take
      ;; appropriate steps if `(thunk)` escapes, which currently means ending
      ;; the scheduler's atomic mode
      (let ([v (thunk)])
        (disable-interrupts)
        (scheduler-end-atomic)
        v)])]
   [(box? async-apply)
    ;; Not in a place's main thread; return the box's content
    (unbox async-apply)]
   [else
    ;; Not in a place's main thread; queue an async callback
    ;; and wait for the response
    (let ([known-thread? (eqv? (place-thread-category) PLACE-KNOWN-THREAD)])
      (unless known-thread? (ensure-virtual-registers))
      (async-callback-queue-call async-callback-queue
                                 async-apply
                                 thunk
                                 ;; If we created this thread by `fork-pthread`, we must
                                 ;; have gotten here by a foreign call, so interrupts are
                                 ;; currently disabled
                                 known-thread?
                                 ;; In a thread created by `fork-pthread`, we'll have to tell
                                 ;; the scheduler to be in atomic mode:
                                 known-thread?
                                 ;; Wait for result:
                                 #t))]))

(define (call-enabling-ffi-callbacks proc)
  (disable-interrupts)
  (let ([v (proc)])
    (enable-interrupts)
    v))

(define scheduler-start-atomic void)
(define scheduler-end-atomic void)
(define (set-scheduler-atomicity-callbacks! start-atomic end-atomic)
  (set! scheduler-start-atomic start-atomic)
  (set! scheduler-end-atomic end-atomic))

;; ----------------------------------------

;; Call `thunk` to enter a foreign call while wrapping it with a way
;; to escape with an exception from a foreign callback during the
;; call:
(define (call-guarding-foreign-escape thunk clean-up)
  ((call-with-c-return
    (lambda ()
      (call-with-current-continuation
       (lambda (esc)
         (call-with-exception-handler
          (lambda (x)
            ;; Deliver an exception re-raise after returning back
            ;; from `call-with-c-return`:
            (|#%app| esc (lambda ()
                           (scheduler-end-atomic) ; error in callback means during atomic mode
                           (clean-up)
                           (raise x))))
          (lambda ()
            (call-with-values thunk
              ;; Deliver successful values after returning back from
              ;; `call-with-c-return`:
              (case-lambda
               [(v) (lambda () v)]
               [args (lambda () (#%apply values args))]))))))))))

;; `call-with-c-return` looks like a foreign function, due to a "cast"
;; to and from a callback, so returning from `call-with-c-return` will
;; pop and C frame stacks (via longjmp internally) that were pushed
;; since `call-with-c-return` was called.
(define call-with-c-return
  (let ([call (lambda (thunk) (thunk))])
    (define-ftype ptr->ptr (function (ptr) ptr))
    (cond
      [(not (eq? (machine-type) (#%$target-machine)))
       (lambda (thunk) (#%error 'call-with-c-return "cannot use while cross-compiling"))]
      [else
       (let ([fptr (make-ftype-pointer ptr->ptr call)])
         (let ([v (ftype-ref ptr->ptr () fptr)])
           ;; must leave the callable code object locked
           v))])))

;; ----------------------------------------

(define-record-type (callback create-callback ffi-callback?)
  (fields code))

(define/who ffi-callback
  (case-lambda
   [(proc in-types out-type)
    (ffi-callback proc in-types out-type #f #f #f #f)]
   [(proc in-types out-type abi)
    (ffi-callback proc in-types out-type abi #f #f #f)]
   [(proc in-types out-type abi atomic?)
    (ffi-callback proc in-types out-type abi atomic? #f #f)]
   [(proc in-types out-type abi atomic? async-apply)
    (ffi-callback proc in-types out-type abi atomic? #f)]
   [(proc in-types out-type abi atomic? async-apply varargs-after)
    (check who procedure? proc)
    (check-ffi-callback who in-types out-type abi varargs-after async-apply)
    ((ffi-callback-maker* in-types out-type abi varargs-after atomic? async-apply) proc)]))

(define/who ffi-callback-maker
  (case-lambda
   [(in-types out-type)
    (ffi-callback-maker in-types out-type #f #f #f #f)]
   [(in-types out-type abi)
    (ffi-callback-maker in-types out-type abi #f #f #f)]
   [(in-types out-type abi atomic?)
    (ffi-callback-maker in-types out-type abi atomic? #f #f)]
   [(in-types out-type abi atomic? async-apply)
    (ffi-callback-maker in-types out-type abi atomic? async-apply #f)]
   [(in-types out-type abi atomic? async-apply varargs-after)
    (check-ffi-callback who in-types out-type abi varargs-after async-apply)
    (ffi-callback-maker* in-types out-type abi varargs-after atomic? async-apply)]))

(define (ffi-callback-maker* in-types out-type abi varargs-after atomic? async-apply)
  (let ([make-code (ffi-call/callable #f in-types out-type abi varargs-after
                                      #f #f #f #f (and atomic? #t) #f
                                      async-apply)])
    (lambda (proc)
      (check 'make-ffi-callback procedure? proc)
      (create-callback (make-code proc)))))

(define (check-ffi-callback who in-types out-type abi varargs-after async-apply)
  (check-ffi who in-types out-type abi varargs-after)
  (check who (lambda (async-apply)
               (or (not async-apply)
                   (box? async-apply)
                   (and (procedure? async-apply)
                        (unsafe-procedure-and-arity-includes? async-apply 1))))
         :contract "(or/c #f (procedure-arity-includes/c 1) box?)"
         async-apply))
  
(define (check-ffi who in-types out-type abi varargs-after)
  (check who (lambda (l)
               (and (list? l)
                    (andmap ctype? l)))
         :contract "(listof ctype?)"
         in-types)
  (check who ctype? out-type)
  (check who (lambda (a) (#%memq a '(#f default stdcall sysv)))
         :contract "(or/c #f 'default 'stdcall 'sysv)"
         abi)
  (check who (lambda (varargs-after) (or (not varargs-after)
                                         (and (exact-positive-integer? varargs-after))))
         :contract "(or/c #f exact-positive-integer?)"
         varargs-after)
  (when  varargs-after
    (let ([len (length in-types)])
      (when (> varargs-after len)
        (raise-arguments-error who
                               "varargs-after value is too large"
                               "given value" varargs-after
                               "argument count" len)))))

;; ----------------------------------------

(define/who (make-sized-byte-string cptr len)
  (check who cpointer? cptr)
  (check who exact-nonnegative-integer? len)
  (raise-unsupported-error who))

(define errno-cell (make-thread-cell 0))

(define/who saved-errno
  (case-lambda
   [() (thread-cell-ref errno-cell)]
   [(v)
    (check who exact-integer? v)
    (thread-cell-set! errno-cell v)]))

(define/who (lookup-errno sym)
  (check who symbol? sym)
  (let ([errno-alist
         (case (system-type 'os*)
           [(linux) (linux-errno-alist)]
           [(macosx darwin) (macosx-errno-alist)]
           [(windows) (windows-errno-alist)]
           [(freebsd) (freebsd-errno-alist)]
           [(openbsd) (openbsd-errno-alist)]
           [(netbsd) (netbsd-errno-alist)]
           [(solaris) (solaris-errno-alist)]
           [else (raise-unsupported-error who)])])
    (cond
     [(assq sym errno-alist) => cdr]
     [else #f])))

;; function is called with interrupts disabled
(define get-errno
  (cond
   [(not (#%memq (machine-type) '(a6nt ta6nt i3nt ti3nt arm64nt tarm64nt)))
    (foreign-procedure "(cs)s_errno" () int)]
   [else
    ;; On Windows, `errno` could be a different one from
    ;; `_errno` in MSVCRT. Therefore fallback to the foreign function.
    ;; See `save_errno_values` in `foreign.c` from Racket BC for more
    ;; information.
    (load-shared-object (if (#%memq (machine-type) '(arm64nt tarm64nt))
			    "API-MS-WIN-CRT-RUNTIME-L1-1-1.0.DLL"
			    "msvcrt.dll"))
    (let ([get-&errno (foreign-procedure "_errno" () void*)])
      (lambda ()
        (foreign-ref 'int (get-&errno) 0)))]))

;; function is called with interrupts disabled
(define get-last-error
  (case (machine-type)
    [(a6nt ta6nt i3nt ti3nt)
     (load-shared-object "kernel32.dll")
     (foreign-procedure "GetLastError" () int)]
    [else (lambda () 0)]))

;; ----------------------------------------

(define process-global-table (make-hashtable equal-hash-code equal?))

(define/who (unsafe-register-process-global key val)
  (check who bytes? key)
  (with-global-lock
   (cond
    [(not val)
     (hashtable-ref process-global-table key #f)]
    [else
     (let ([old-val (hashtable-ref process-global-table key #f)])
       (cond
        [(not old-val)
         (hashtable-set! process-global-table (bytes-copy key) val)
         #f]
        [else old-val]))])))

;; ----------------------------------------

(define (set-cpointer-hash!)
  (struct-set-equal+hash! (record-type-descriptor cpointer)
                          (lambda (a b eql?)
                            (ptr-equal? a b))
                          (lambda (a hc)
                            (if (number? (cpointer-memory a))
                                (hc (+ (cpointer-memory a)
                                       (ptr-offset* a)))
                                (eq-hash-code (cpointer-memory a)))))
  (inherit-equal+hash! (record-type-descriptor cpointer+offset)
                       (record-type-descriptor cpointer)))
