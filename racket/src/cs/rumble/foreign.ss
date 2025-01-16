
;; Externally, a cpointer can be #f or a byte string, in
;; addition to a cpointer record
(define (cpointer? v)
  (or (authentic-cpointer? v)
      (not v)
      (bytes? v)
      (has-cpointer-property? v)))

;; A Racket-level cpointer wraps a Scheme-level ftypoe pointer.
;; If there's a non-#f offset, it's also reflected by keeping an
;; fptr for the original address, so that the represented address
;; can be updated atomically in the case of `ptr-set!`. A `_gcpointer`
;; corresponds to a cpointer where the fptr answers #t for
;; `ftype-scheme-object-pointer?`.
(define-record-type (cpointer make-cpointer authentic-cpointer?)
  (fields (mutable fptr) (mutable tags)))
(define-record-type cpointer+offset
  (parent cpointer)
  (fields base-fptr))

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

(define null-fptr (make-ftype-pointer integer-8 0))

;; Follows `prop:cpointer` properties as needed, in two parts to
;; encourage inlining of the common case
(define (cptr->fptr who p)
  (cond
    [(authentic-cpointer? p) (cpointer-fptr p)]
    [else (#%$app/no-inline other-cptr->fptr who p)]))

(define (other-cptr->fptr who p)
  (cond
   [(not p) null-fptr]
   [(bytes? p) (make-ftype-scheme-object-pointer p)]
   [else (let ([v (cpointer-property-ref p none)])
           (cond
             [(eq? v none)
              (raise-argument-error who "cpointer?" p)]
             [(exact-nonnegative-integer? v)
              (let ([v (unsafe-struct-ref p v)])
                (if (cpointer? v)
                    (cptr->fptr who v)
                    #f))]
             [(procedure? v)
              (let ([p2 (v p)])
                (unless (cpointer? p2)
                  (raise-result-error 'prop:cpointer-accessor
                                      "cpointer?"
                                      p2))
                (cptr->fptr who p2))]
             [else
              (cptr->fptr who v)]))]))

;; Never fails, even if a `prop:cpointer` result is broken
(define (extract-authentic-cpointer p)
  (let ([v (cpointer-property-ref p #f)])
    (cond
      [(exact-nonnegative-integer? v)
       (let ([v (unsafe-struct-ref p v)])
         (if (authentic-cpointer? v)
             v
             (extract-authentic-cpointer v)))]
      [(procedure? v)
       (let ([p2 (v p)])
         (if (authentic-cpointer? p2)
             p2
             (extract-authentic-cpointer p2)))]
      [else #f])))

(define (offset-ptr? p)
  (unless (cpointer? p)
    (raise-argument-error 'offset-ptr? "cpointer?" p))
  (cpointer+offset? p))

(define/who (set-cpointer-tag! p t)
  (if (authentic-cpointer? p)
      (cpointer-tags-set! p t)
      (if (cpointer? p)
          (let ([q (extract-authentic-cpointer p)])
            (if q
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
          (let ([q (extract-authentic-cpointer p)])
            (if q
                (cpointer-tag q)
                #f))
          (raise-argument-error who "cpointer?" p))))

;; Convert an ftype pointer to a cpointer, using #f for a NULL address:
(define (fptr->cptr x)
  (cond
    [(ftype-pointer-null? x) #f]
    [else (make-cpointer x #f)]))

;; Works on an authentic cpointer:
(define (cpointer->name proc-p)
  (and (ffi-obj? proc-p)
       (string->symbol (utf8->string (cpointer/ffi-obj-name proc-p)))))

;; ----------------------------------------

(define/who (ptr-equal? p1 p2)
  (ftype-pointer=? (cptr->fptr who p1)
                   (cptr->fptr who p2)))

(define/who (ptr-offset p)
  (cond
    [(cpointer+offset? p)
     (let ([m (cpointer-fptr p)]
           [m2 (cpointer+offset-base-fptr p)])
       (cond
         [(ftype-scheme-object-pointer? m) (- (ftype-scheme-object-pointer-offset m)
                                              (ftype-scheme-object-pointer-offset m2))]
         [else (- (ftype-pointer-address m)
                  (ftype-pointer-address m2))]))]
    [(cpointer? p)
     (let ([p (extract-authentic-cpointer p)])
       (if (cpointer+offset? p)
           (ptr-offset p)
           0))]
    [else
     (raise-argument-error who "cpointer?" p)]))

(define/who (set-ptr-offset! p n)
  (unless (cpointer+offset? p)
    (raise-argument-error who "(and/c cpointer? ptr-offset?)" p))
  (unless (exact-integer? n)
    (raise-argument-error who "exact-integer?" n))
  (cpointer-fptr-set! p (let ([m (cpointer+offset-base-fptr p)])
                          (cond
                            [(ftype-scheme-object-pointer? m)
                             (make-ftype-scheme-object-pointer (ftype-scheme-object-pointer-object m)
                                                               n)]
                            [else
                             (make-ftype-pointer integer-8
                                                 (+ (ftype-pointer-address m) n))]))))

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

(define (fptr-add m n)
  (cond
    [(ftype-scheme-object-pointer? m)
     (make-ftype-scheme-object-pointer (ftype-scheme-object-pointer-object m)
                                       (+ (ftype-scheme-object-pointer-offset m) n))]
    [else
     (make-ftype-pointer integer-8
                         (+ (ftype-pointer-address m) n))]))

(define (do-ptr-add p n save-tags?)
  (cond
    [(cpointer+offset? p)
     (make-cpointer+offset (fptr-add (cpointer-fptr p) n)
                           (and save-tags? (cpointer-tags p))
                           (cpointer+offset-base-fptr p))]
    [(authentic-cpointer? p)
     (make-cpointer+offset (fptr-add (cpointer-fptr p) n)
                           (and save-tags? (cpointer-tags p))
                           (cpointer-fptr p))]
    [(has-cpointer-property? p)
     (do-ptr-add (extract-authentic-cpointer p) n save-tags?)]
    [(bytes? p)
     (make-cpointer+offset (make-ftype-scheme-object-pointer p n)
                           #f
                           (make-ftype-scheme-object-pointer p))]
    [(not p)
     (make-cpointer+offset (make-ftype-pointer integer-8 n)
                           #f
                           null-fptr)]))

(define ptr-add!
  (case-lambda
   [(p n type)
    (unless (cpointer+offset? p)
      (raise-argument-error 'ptr-add! "(and/c cpointer? offset-ptr?)" p))
    (unless (exact-integer? n)
      (raise-argument-error 'ptr-add! "exact-integer?" n))
    (unless (ctype? type)
      (raise-argument-error 'ptr-add! "ctype?" type))
    (cpointer-fptr-set! p (fptr-add (cpointer-fptr p) (* n (ctype-sizeof type))))]
   [(p n)
    (unless (cpointer+offset? p)
      (raise-argument-error 'ptr-add! "(and/c cpointer? offset-ptr?)" p))
    (unless (exact-integer? n)
      (raise-argument-error 'ptr-add! "exact-integer?" n))
    (cpointer-fptr-set! p (fptr-add (cpointer-fptr p) n))]))

;; ----------------------------------------

(define-record-type (ctype create-ctype ctype?)
  (fields host-rep    ; host-Scheme representation description, 'struct, 'union, or 'array
          in-host-rep ; usually  the same as `host-rep`, but may be generic instade of GCable
          our-rep     ; Racket representation description
          basetype    ; parent ctype or the same as `our-rep`
          c->scheme   ; immediate layer of c->s
          scheme->c   ; immediate later of s->c
          c->s        ; converter of values from `basetype`
          s->c        ; converter of values to `basetype`
          ref         ; pointer-referencing operation, includes `c->s` step
          set!))      ; pointer-update operation, includes `s->c` step

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
  (let ([new-c->s (let ([c->s (ctype-c->s type)])
                    (if c-to-racket
                        (lambda (p) (c-to-racket (c->s p)))
                        c->s))]
        [new-s->c (let ([s->c (ctype-s->c type)])
                    (if racket-to-c
                        (lambda (who v) (s->c who (racket-to-c v)))
                        s->c))]
        [new-ref (let ([ref (ctype-ref type)])
                   (if c-to-racket
                       (lambda (p offset) (c-to-racket (ref p offset)))
                       ref))]
        [new-set (let ([set (ctype-set! type)])
                   (if racket-to-c
                       (lambda (who p offset v) (set who p offset (racket-to-c v)))
                       set))])
    (cond
      [(compound-ctype? type)
       (create-compound-ctype (ctype-host-rep type)
                              (ctype-in-host-rep type)
                              (ctype-our-rep type)
                              type
                              c-to-racket
                              racket-to-c
                              new-c->s
                              new-s->c
                              new-ref
                              new-set
                              (compound-ctype-get-decls type)
                              (compound-ctype-size type)
                              (compound-ctype-alignment type)
                              (compound-ctype-malloc-mode type))]
      [else
       (create-ctype (ctype-host-rep type)
                     (ctype-in-host-rep type)
                     (ctype-our-rep type)
                     type
                     c-to-racket
                     racket-to-c
                     new-c->s
                     new-s->c
                     new-ref
                     new-set)])))

(define (identity-scheme->c v) v)
(define (identity-c->scheme v) v)

;; ----------------------------------------

(define-syntax define-ctype
  (syntax-rules (*)
    [(_ id host-rep basetype)
     (define-ctype id host-rep host-rep basetype * *)]
    [(_ id host-rep basetype c->s s->c/whom)
     (define-ctype id host-rep host-rep basetype c->s s->c/whom)]
    [(_ id host-rep in-host-rep basetype * *)
     (define/who id (create-ctype 'host-rep 'in-host-rep basetype basetype
                                  identity-c->scheme
                                  identity-scheme->c
                                  (lambda (c) c)
                                  (lambda (for-whom s) s)
                                  (lambda (c offset) (ftype-any-ref host-rep () (cptr->fptr who c) offset))
                                  (lambda (for-whom dest-c offset s)
                                    (ftype-any-set! host-rep () (cptr->fptr for-whom dest-c) offset s))))]
    [(_ id host-rep in-host-rep basetype * s->c/whom)
     (define/who id (create-ctype 'host-rep 'in-host-rep basetype basetype
                                  identity-c->scheme
                                  identity-scheme->c
                                  (lambda (c) c)
                                  s->c/whom
                                  (lambda (c offset) (ftype-any-ref host-rep () (cptr->fptr who c) offset))
                                  (lambda (for-whom dest-c offset s)
                                    (ftype-any-set! host-rep () (cptr->fptr for-whom dest-c) offset (s->c/whom for-whom s)))))]
    [(_ id host-rep in-host-rep basetype c->s s->c/whom)
     (define/who id (create-ctype 'host-rep 'in-host-rep basetype basetype
                                  identity-c->scheme
                                  identity-scheme->c
                                  c->s
                                  s->c/whom
                                  (lambda (c offset) (c->s (ftype-any-ref host-rep () (cptr->fptr who c) offset)))
                                  (lambda (for-whom dest-c offset s)
                                    (ftype-any-set! host-rep () (cptr->fptr for-whom dest-c) offset (s->c/whom for-whom s)))))]))

;; We need `s->c` checks, even if they seem redundant, to make sure
;; that the checks happen early enough --- outside of atomic and
;; foreign-thread regions. Also, the integer checks built into Chez
;; Scheme are more permissive than Racket's.

(define-syntax-rule (checker who ?) (lambda (for-whom x) (if (? x) x (bad-ctype-value for-whom who x))))
(define-syntax integer-checker
  (syntax-rules (signed unsigned)
    [(_ who signed n int?) (checker who (lambda (x) (and (int? x) (<= (- (#%expt 2 (- n 1))) x  (- (#%expt 2 (- n 1)) 1)))))]
    [(_ who unsigned n int?) (checker who (lambda (x) (and (int? x) (<= 0 x  (- (#%expt 2 n) 1)))))]))

(define-ctype _bool boolean 'bool)
(define-ctype _double double 'double * (checker who flonum?))
(define-ctype _fixnum fixnum 'fixnum * (checker who fixnum?))
(define-ctype _float float 'float * (checker who flonum?))
(define-ctype _int8 integer-8 'int8 * (integer-checker who signed 8 fixnum?))
(define-ctype _int16 integer-16 'int16 * (integer-checker who signed 16 fixnum?))
(define-ctype _int32 integer-32 'int32 * (integer-checker who signed 32 exact-integer?))
(define-ctype _int64 integer-64 'int64 * (integer-checker who signed 64 exact-integer?))
(define-ctype _uint8 unsigned-8 'uint8 * (integer-checker who unsigned 8 fixnum?))
(define-ctype _uint16 unsigned-16 'uint16 * (integer-checker who unsigned 16 fixnum?))
(define-ctype _uint32 unsigned-32 'uint32 * (integer-checker who unsigned 32 exact-integer?))
(define-ctype _uint64 unsigned-64 'uint64 * (integer-checker who unsigned 64 exact-integer?))

(define/who _void
  (create-ctype 'void 'void 'void 'void
                identity-c->scheme
                identity-scheme->c
                (lambda (c) c)
                (lambda (for-whom s) s)
                (lambda (c offset)
                  (raise-arguments-error who "cannnot dereference void"))
                (lambda (for-whom dest-c offset s)
                  (raise-arguments-error who "cannnot set void content"))))

(define/who _scheme
  (create-ctype 'scheme-object 'scheme-object 'scheme 'scheme
                identity-c->scheme
                identity-scheme->c
                (lambda (c) c)
                (lambda (for-whom s) s)
                (lambda (c offset)
                  ;; only sensible if c is nonatomic memory or the retrived object
                  ;; is immobile
                  (ftype-scheme-object-pointer-object
                   (ftype-any-ref ftype-scheme-object-pointer () (cptr->fptr who c) offset)))
                (lambda (for-whom dest-c offset s)
                  (let ([m (cptr->fptr for-whom dest-c)])
                    (if (and (ftype-scheme-object-pointer? m)
                             (reference-bytevector? (ftype-scheme-object-pointer-object m)))
                        ;; use `bytevector-reference-set!` to get write barrier
                        (let ([offset (+ offset (ftype-scheme-object-pointer-offset m))])
                          (bytevector-reference-set! (ftype-scheme-object-pointer-object m) offset s))
                        ;; only sensible if `s` is immobile
                        (ftype-any-set! ftype-scheme-object-pointer () (cptr->fptr for-whom dest-c) offset
                                        (make-ftype-scheme-object-pointer s)))))))

(define (bad-ctype-value who type-name v)
  (raise-arguments-error who
                         "given value does not fit primitive C type"
                         "C type" (make-unquoted-printing-string (symbol->string type-name))
                         "value" v))

(define-syntax define-ctype/promote-pointer
  (syntax-rules (*)
    [(_ id basetype c->s s->c/whom)
     (define/who id (create-ctype 'ftype-pointer 'ftype-pointer basetype basetype
                                  identity-c->scheme
                                  identity-scheme->c
                                  c->s
                                  s->c/whom
                                  (lambda (c offset) (let ([m (cptr->fptr who c)])
                                                       (c->s
                                                        ;; auto-promote to GCable when reading from nonatomic
                                                        (if (ftype-scheme-object-pointer? m)
                                                            (ftype-any-ref ftype-scheme-object-pointer () m offset)
                                                            (ftype-any-ref ftype-pointer () m offset)))))
                                  (lambda (for-whom dest-c offset s)
                                    (ftype-any-set! ftype-pointer () (cptr->fptr for-whom dest-c) offset (s->c/whom for-whom s)))))]))

(define (fptr->bytevector/nul m)
  (cond
    [(ftype-pointer-null? m) #f]
    [else
     (let loop ([i 0])
       (if (fx= 0 (ftype-ref unsigned-8 () m i))
           (let ([bstr (make-bytes i)])
             (memcpy* (cptr->fptr 'bv bstr) 0 m 0 i #f)
             bstr)
           (loop (fx+ i 1))))]))
    
;; Unlike Racket BC, copies when converting from C:
(define-ctype/promote-pointer _bytes 'bytes
  fptr->bytevector/nul
  (lambda (for-whom x)
    (cond
      [(not x) null-fptr]
      [(bytes? x) (make-ftype-scheme-object-pointer x)]
      [else (bad-ctype-value who for-whom x)])))

(define (fptr->bytevector/two-nuls m)
  (cond
    [(ftype-pointer-null? m) #f]
    [else
     (let loop ([i 0])
       (if (fx= 0 (ftype-any-ref unsigned-16 () m i))
           (let ([bstr (make-bytes i)])
             (memcpy* (cptr->fptr 'bv bstr) 0 m 0 i #f)
             bstr)
           (loop (+ i 2))))]))

(define-ctype/promote-pointer _short_bytes 'bytes
  (lambda (x) (fptr->bytevector/two-nuls x))
  (lambda (for-whom x)
    (cond
      [(not x) null-fptr]
      [(bytes? x) (make-ftype-scheme-object-pointer x)]
      [else (bad-ctype-value who for-whom x)])))

(define-ctype/promote-pointer _string/utf-16 'string/utf-16
  (lambda (m)
    (cond
      [(ftype-pointer-null? m) #f]
      [else (utf16->string (fptr->bytevector/two-nuls m)
                           (if (system-big-endian?) 'big 'little))]))
  (lambda (for-whom x)
    (cptr->fptr
     who
     (cond
       [(not x) #f]
       [(string? x) (string->utf16 (string-append x "\x0;") (if (system-big-endian?) 'big 'little))]
       [else (bad-ctype-value who for-whom x)]))))

(define (fptr->bytevector/four-nuls m)
  (cond
    [(ftype-pointer-null? m) #f]
    [else
     (let loop ([i 0])
       (if (fx= 0 (ftype-any-ref unsigned-32 () m i))
           (let ([bstr (make-bytes i)])
             (memcpy* (cptr->fptr 'bv bstr) 0 m 0 i #f)
             bstr)
           (loop (+ i 4))))]))

(define-ctype/promote-pointer _string/ucs-4 'string/ucs-4
  (lambda (m)
    (cond
      [(ftype-pointer-null? m) #f]
      [else (utf32->string (fptr->bytevector/four-nuls m)
                           (if (system-big-endian?) 'big 'little))]))
  (lambda (for-whom x)
    (cptr->fptr
     who
     (cond
       [(not x) #f]
       [(string? x) (string->utf32 (string-append x "\x0;") (if (system-big-endian?) 'big 'little))]
       [else (bad-ctype-value who for-whom x)]))))

(define-ctype _double* double 'double
  *
  (lambda (for-whom x) (if (real? x)
                           (exact->inexact x)
                           (bad-ctype-value for-whom who x))))

(define-ctype _ufixnum fixnum 'fixnum * (checker who fixnum?)) ; historically, no sign check
(define-ctype _fixint integer-32 'fixint * (checker who fixnum?))
(define-ctype _ufixint unsigned-32 'ufixint * (checker who fixnum?)) ; historically, no sign check

(define-ctype _symbol ftype-pointer 'string
  (lambda (m) (string->symbol (utf8->string (fptr->bytevector/nul m))))
  (lambda (for-whom x) (if (symbol? x)
                           (cptr->fptr who (string->utf8 (string-append (#%symbol->string x) (string #\nul))))
                           (bad-ctype-value for-whom who x))))

(define-ctype _longdouble double 'double
  *
  (lambda (for-whom x) (bad-ctype-value for-whom who x)))

(define pointer-set!
  ;; used for both `_pointer` or `_gcpointer`; this has the effect of
  ;; fixing up some uses of `_pointer` with GCable pointers by triggering
  ;; a write barrier as suitable for a nonatomic destination
  (lambda (for-whom dest-c offset s)
    (let ([m (cptr->fptr for-whom dest-c)]
          [v (cptr->fptr for-whom s)])
      (if (and (ftype-scheme-object-pointer? m)
               (ftype-scheme-object-pointer? v)
               (reference-bytevector? (ftype-scheme-object-pointer-object m))
               (eqv? (ftype-scheme-object-pointer-offset v) 0))
          ;; use `bytevector-reference-set!` to get write barrier
          (let ([offset (+ offset (ftype-scheme-object-pointer-offset m))])
            (bytevector-reference-set! (ftype-scheme-object-pointer-object m) offset (ftype-scheme-object-pointer-object v)))
          ;; maybe the caller knows that `s` is immobile
          (ftype-any-set! ftype-pointer () (cptr->fptr for-whom dest-c) offset v)))))

(define/who _pointer
  (create-ctype 'ftype-pointer 'ftype-pointer 'pointer 'pointer
                identity-c->scheme
                identity-scheme->c
                fptr->cptr
                cptr->fptr
                (lambda (c offset)
                  ;; It would make sense to promote to `_gcpointer` if
                  ;; `c` refers to GCable memory, as below, but we preserve
                  ;; old behavior/compatibility by not doing that
                  (fptr->cptr (ftype-any-ref ftype-pointer () (cptr->fptr who c) offset))
                  #;
                  (let ([m (cptr->fptr who c)])
                    (fptr->cptr
                     (if (and (ftype-scheme-object-pointer? m)
                              (reference-bytevector? (type-pointer-object m)))
                         ;; Access from non-atomic memory: promote to `_gcpointer`
                         (ftype-any-ref ftype-scheme-object-pointer () m offset)
                  (ftype-any-ref ftype-pointer () m offset)))))
                pointer-set!))

(define/who _fpointer
  (create-ctype 'ftype-pointer 'ftype-pointer 'fpointer 'fpointer
                identity-c->scheme
                identity-scheme->c
                fptr->cptr
                cptr->fptr
                (lambda (c offset)
                  (if (and (ffi-obj? c) (eqv? offset 0))
                      ;; Special case for `ptr-ref` on a function-type ffi-object:
                      ;; cancel a level of indirection and preserve `ffi-obj`ness
                      ;; to keep its name
                      c
                      (fptr->cptr (ftype-any-ref ftype-pointer () (cptr->fptr who c) offset))))
                (lambda (for-whom dest-c offset s)
                  (ftype-any-set! ftype-pointer () (cptr->fptr for-whom dest-c) offset (cptr->fptr for-whom s)))))

(define/who _gcpointer
  (create-ctype 'ftype-scheme-object-pointer 'ftype-scheme-object-pointer 'pointer 'pointer
                identity-c->scheme
                identity-scheme->c
                fptr->cptr
                cptr->fptr
                (lambda (c offset)
                  (fptr->cptr (ftype-any-ref ftype-scheme-object-pointer () (cptr->fptr who c) offset)))
                pointer-set!))

;; One-byte stdbool is correct on all currently supported platforms, at least:
(define-ctype _stdbool integer-8 'stdbool
  (lambda (c) (not (zero? c)))
  (lambda (for-whom v) (if v 1 0)))

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
        (create-compound-ctype 'struct 'struct
                               'struct
                               types
                               identity-scheme->c
                               identity-c->scheme
                               (lambda (c) (fptr->cptr c))
                               (lambda (for-whom s) (cptr->fptr for-whom s))
                               ;; `ref` just returns the pointer, which is maybe an
                               ;; offset into another structure
                               (lambda (c offset) (do-ptr-add c offset #f))
                               (lambda (for-whom dest-c offset s)
                                 ;; `set!` corresponds to a copy
                                 (memcpy* (cptr->fptr for-whom dest-c) offset (cptr->fptr for-whom s) 0 size #f))
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
    (create-compound-ctype 'union 'union
                           'union
                           types
                           identity-scheme->c
                           identity-c->scheme
                           ;; same implementation as `struct`:
                           (lambda (c) (fptr->cptr c))
                           (lambda (for-whom s) (cptr->fptr for-whom s))
                           (lambda (c offset) (do-ptr-add c offset #f))
                           (lambda (for-whom dest-c offset s) (memcpy* (cptr->fptr for-whom dest-c) offset (cptr->fptr for-whom s) 0 size #f))
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
    (create-compound-ctype 'array 'array
                           'array
                           (vector type count)
                           identity-scheme->c
                           identity-c->scheme
                           ;; same implementation as `struct`:
                           (lambda (c) (fptr->cptr c))
                           (lambda (for-whom s) (cptr->fptr for-whom s))
                           (lambda (c offset) (do-ptr-add c offset #f))
                           (lambda (for-whom dest-c offset s) (memcpy* (cptr->fptr for-whom dest-c) offset (cptr->fptr for-whom s) 0 size #f))
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
  (let ([p (cptr->fptr who p)])
    (ftype-scheme-object-pointer? p)))

(define (ctype-pointer-rep? type)
  (case (ctype-host-rep type)
    [(ftype-pointer ftype-scheme-object-pointer struct array union)
     #t]
    [else #f]))

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
                   (make-ffi-obj (make-ftype-pointer integer-8 (ffi-ptr->address ptr)) #f lib name)))))

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
    ((ctype-ref type) p 0)]
   [(p type offset)
    (check who cpointer? p)
    (check who ctype? type)
    (check who fixnum? offset)
    ((ctype-ref type) p (* (ctype-sizeof type) offset))]
   [(p type abs-tag offset)
    (check who cpointer? p)
    (check who ctype? type)
    (check who (lambda (p) (eq? p 'abs)) :contract "'abs" abs-tag)
    (check who exact-integer? offset)
    ((ctype-ref type) p offset)]))

(define/who ptr-set!
  (case-lambda
   [(p type v)
    (check who cpointer? p)
    (check who ctype? type)
    ((ctype-set! type) who p 0 v)]
   [(p type offset v)
    (check who cpointer? p)
    (check who ctype? type)
    (check who fixnum? offset)
    ((ctype-set! type) who p (* (ctype-sizeof type) offset) v)]
   [(p type abs-tag offset v)
    (check who cpointer? p)
    (check who ctype? type)
    (check who (lambda (p) (eq? p 'abs)) :contract "'abs" abs-tag)
    (check who exact-integer? offset)
    ((ctype-set! type) who p offset v)]))

(define (bad-ptr-ref-offset o)
  (raise-argument-error 'ptr-ref "fixnum?" o))
(define (bad-ptr-set!-offset o)
  (raise-argument-error 'ptr-set! "fixnum?" o))
(define (bad-ptr-set!-val type-name v)
  (bad-ctype-value 'ptr-set! type-name v))

(define-syntax-rule (define-fast-ptr-ops ref set _type ok-v? bytes-ref bytes-set ftype type-bits)
  (begin
    (define-syntax (ref stx)
      (syntax-case stx ()
        [(_ p offset #t)
         #'(let ([x p]
                 [o offset])
             (unless (fixnum? o) (bad-ptr-ref-offset o))
             (begin-unsafe
              (if (and (bytes? x) (fx= 0 (fxand o (fx- (fxsll 1 type-bits) 1))))
                  (bytes-ref x o)
                  (ftype-any-ref ftype () (cptr->fptr 'ptr-ref x) o))))]
        [(_ p offset #f)
         #'(let ([x p]
                 [o offset])
             (unless (fixnum? o) (bad-ptr-ref-offset o))
             (begin-unsafe
              (if (bytes? x)
                  (bytes-ref x (fxsll o type-bits))
                  (ftype-any-ref ftype () (cptr->fptr 'ptr-ref x) (fxsll o type-bits)))))]
        [(_ arg (...  ...)) #'(noninline-ref arg (... ...))]
        [_ #'noninline-ref]))
    (define-syntax (set stx)
      (syntax-case stx ()
        [(_ p offset val #t)
         #'(let ([x p]
                 [o offset]
                 [v val])
             (unless (fixnum? o) (bad-ptr-set!-offset o))
             (unless (ok-v? v) (bad-ptr-set!-val '_type v))
             (begin-unsafe
              (if (and (bytes? x) (fx= 0 (fxand offset (fx- (fxsll 1 type-bits) 1))))
                  (bytes-set x o v)
                  (ftype-any-set! ftype () (cptr->fptr 'ptr-ref x) o v))))]
        [(_ p offset val #f)
         #'(let ([x p]
                 [o offset]
                 [v val])
             (unless (fixnum? o) (bad-ptr-ref-offset o))
             (unless (ok-v? v) (bad-ptr-set!-val '_type v))
             (begin-unsafe
              (if (bytes? x)
                  (bytes-set x (fxsll o type-bits) v)
                  (ftype-any-set! ftype () (cptr->fptr 'ptr-ref x) (fxsll o type-bits) v))))]
        [(_ arg (... ...)) #'(noninline-set arg (... ...))]
        [_ #'noninline-set]))
    (define noninline-ref
      (let ([ref (lambda (p offset abs?)
                   (if abs?
                       (ptr-ref p _type 'abs offset)
                       (ptr-ref p _type offset)))])
        ref))
    (define noninline-set
      (let ([set (lambda (p offset val abs?)
                   (if abs?
                       (ptr-set! p _type 'abs offset val)
                       (ptr-set! p _type offset val)))])
        set))))

(define-syntax-rule (fixnum-in-range? lo hi) (lambda (v) (and (fixnum? v) (fx>= v lo) (fx<= v hi))))
(define-syntax-rule (in-range? lo hi) (lambda (v) (and (exact-integer? v) (>= v lo) (<= v hi))))

;; Schemify optimizes `(ptr-ref p _uint16 offset v)` to `(ptr-set!/uint16 p offset v #f)`, etc.
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

(define byte-copy (foreign-procedure "(cs)byte-copy" (ftype-pointer iptr ftype-pointer iptr iptr) void))
(define byte-copy/addr (foreign-procedure "(cs)byte-copy" (uptr iptr uptr iptr iptr) void))

(define (memcpy* to to-offset from from-offset len move?)
  (if move?
      (with-interrupts-disabled*
       (let ([to-addr (ftype-pointer-address to)])
         (byte-copy/addr to-addr (+ from-offset (- (ftype-pointer-address from) to-addr)) to-addr to-offset len)))
      (byte-copy from from-offset to to-offset len)))

(define memcpy/memmove
  (case-lambda
   [(who cptr src-cptr count)
    (check who cpointer? cptr)
    (check who cpointer? src-cptr)
    (check who exact-nonnegative-integer? count)
    (memcpy* (cptr->fptr who cptr) 0 (cptr->fptr who src-cptr) 0 count (eq? who 'memmove))]
   [(who cptr offset/src-cptr/src-cptr src-cptr/offset/count count/count/type)
    (check who cpointer? cptr)
    (cond
     [(cpointer? offset/src-cptr/src-cptr)
      ;; use y or z of x/y/z
      (cond
       [(ctype? count/count/type)
        ;; use z of x/y/z
        (check who exact-nonnegative-integer? src-cptr/offset/count)
        (memcpy* (cptr->fptr who cptr) 0 (cptr->fptr who offset/src-cptr/src-cptr) 0 (* src-cptr/offset/count (ctype-sizeof count/count/type)) (eq? who 'memmove))]
       [else
        ;; use y of x/y/z
        (check who exact-integer? src-cptr/offset/count)
        (check who exact-nonnegative-integer? count/count/type)
        (memcpy* (cptr->fptr who cptr) 0 (cptr->fptr who offset/src-cptr/src-cptr) src-cptr/offset/count src-cptr/offset/count (eq? who 'memmove))])]
     [else
      ;; use x of x/y/z
      (check who exact-integer? offset/src-cptr/src-cptr)
      (check who cpointer? src-cptr/offset/count)
      (check who exact-nonnegative-integer? count/count/type)
      (memcpy* (cptr->fptr who cptr) offset/src-cptr/src-cptr (cptr->fptr who src-cptr/offset/count) 0 count/count/type (eq? who 'memmove))])]
   [(who cptr offset src-cptr src-offset/count count/type)
    (check who cpointer? cptr)
    (check who exact-integer? offset)
    (check who cpointer? src-cptr)
    (cond
     [(ctype? count/type)
      ;; use y of x/y
      (check who exact-nonnegative-integer? src-offset/count)
      (let ([sz (ctype-sizeof count/type)])
        (memcpy* (cptr->fptr who cptr) (* sz offset) (cptr->fptr who src-cptr) 0 (* src-offset/count sz) (eq? who 'memmove)))]
     [else
      ;; use x of x/y
      (check who exact-integer? src-offset/count)
      (check who exact-nonnegative-integer? count/type)
      (memcpy* (cptr->fptr who cptr) offset (cptr->fptr who src-cptr) src-offset/count count/type (eq? who 'memmove))])]
   [(who cptr offset src-cptr src-offset count type)
    (check who cpointer? cptr)
    (check who exact-integer? offset)
    (check who cpointer? src-cptr)
    (check who exact-integer? src-offset)
    (check who ctype? type)
    (let ([sz (ctype-sizeof type)])
      (memcpy* (cptr->fptr who cptr) (* offset sz) (cptr->fptr who src-cptr) (* src-offset sz) (* count sz) (eq? who 'memmove)))]))

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
  (let ([to (cptr->fptr 'memset to)])
    (let loop ([i to-offset] [len len])
      (unless (fx= len 0)
        (ftype-any-set! unsigned-8 () to i byte)
        (loop (+ i 1) (fx- len 1))))))

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
        (unless (or count type)
          (raise-arguments-error 'malloc "no size given"))
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
                                             "            'zeroed-atomic 'zeroed-atomic-interior\n"
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
    (fptr->cptr (make-ftype-pointer integer-8 (foreign-alloc size)))]
   [(eq? mode 'atomic)
    (fptr->cptr (make-ftype-scheme-object-pointer (make-bytevector size)))]
   [(eq? mode 'nonatomic)
    (fptr->cptr (make-ftype-scheme-object-pointer (make-reference-bytevector size)))]
   [(eq? mode 'atomic-interior)
    ;; This is not quite the same as Racket BC, because interior
    ;; pointers are not allowed as GCable pointers. So, "interior"
    ;; just means "doesn't move".
    (fptr->cptr (make-ftype-scheme-object-pointer (make-immobile-bytevector size)))]
   [(eq? mode 'interior)
    ;; Ditto
    (fptr->cptr (make-ftype-scheme-object-pointer (make-immobile-reference-bytevector size)))]
   [(eq? mode 'zeroed-atomic)
    (fptr->cptr (make-ftype-scheme-object-pointer (make-bytevector size 0)))]
   [(eq? mode 'zeroed-atomic-interior)
    (fptr->cptr (make-ftype-scheme-object-pointer (make-immobile-bytevector size 0)))]
   [else
    (raise-unsupported-error 'malloc
                             (format "'~a mode is not supported" mode))]))

(define/who (free p)
  (let ([p (cptr->fptr who p)])
    (foreign-free (ftype-pointer-address p))))

(define/who (lock-cpointer p)
  (lock-object (ftype-scheme-object-pointer-object p)))

(define/who (unlock-cpointer p)
  (unlock-object (ftype-scheme-object-pointer-object p)))

(define-record-type (cpointer/cell make-cpointer/cell cpointer/cell?)
  (parent cpointer)
  (fields))

(define immobile-cells (make-eq-hashtable))

(define (malloc-immobile-cell v)
  (let ([vec (make-immobile-reference-bytevector (foreign-sizeof 'ptr))])
    (bytevector-reference-set! vec 0 v)
    (with-global-lock
     (eq-hashtable-set! immobile-cells vec #t))
    (fptr->cptr (make-ftype-scheme-object-pointer vec))))

(define/who (free-immobile-cell b)
  (with-global-lock
   (eq-hashtable-delete! immobile-cells (ftype-scheme-object-pointer-object (cptr->fptr who b)))))

(define/who (immobile-cell-ref b)
  (bytevector-reference-ref  (ftype-scheme-object-pointer-object (cptr->fptr who b)) 0))

(define (immobile-cell->address b)
  (ftype-pointer-address (cpointer-fptr b)))

(define (address->immobile-cell a)
  (fptr->cptr (make-ftype-scheme-object-pointer (reference-address->object a))))

(define (malloc-mode? v)
  (#%memq v '(raw atomic nonatomic tagged
                  atomic-interior interior
                  zeroed-atomic zeroed-atomic-interior
                  stubborn uncollectable eternal)))

(define (end-stubborn-change p)
  (raise-unsupported-error 'end-stubborn-change))

(define/who (extflvector->cpointer extfl-vector)
  (raise-unsupported-error who))

(define/who (vector->cpointer vec)
  (raise-unsupported-error who))

(define (flvector->cpointer flvec)
  (fptr->cptr (make-ftype-scheme-object-pointer flvec)))

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
                        #f
                        #f)
     p)]))

(define/who ffi-call-maker
  (case-lambda
   [(in-types out-type)
    (ffi-call-maker in-types out-type #f #f #f #f #f #f)]
   [(in-types out-type abi)
    (ffi-call-maker in-types out-type abi #f #f #f #f #f)]
   [(in-types out-type abi save-errno)
    (ffi-call-maker in-types out-type abi save-errno #f #f #f #f)]
   [(in-types out-type abi save-errno orig-place?)
    (ffi-call-maker in-types out-type abi save-errno orig-place? #f #f #f #f)]
   [(in-types out-type abi save-errno orig-place? lock-name)
    (ffi-call-maker in-types out-type abi save-errno orig-place? lock-name #f #f #f #f)]
   [(in-types out-type abi save-errno orig-place? lock-name blocking?)
    (ffi-call-maker in-types out-type abi save-errno orig-place? lock-name blocking? #f #f #f)]
   [(in-types out-type abi save-errno orig-place? lock-name blocking? varargs-after)
    (ffi-call-maker in-types out-type abi save-errno orig-place? lock-name blocking? varargs-after #f #f)]
   [(in-types out-type abi save-errno orig-place? lock-name blocking? varargs-after exns?)
    (ffi-call-maker in-types out-type abi save-errno orig-place? lock-name blocking? varargs-after exns? #f)]
   [(in-types out-type abi save-errno orig-place? lock-name blocking? varargs-after exns? core)
    (check-ffi-call who in-types out-type abi varargs-after save-errno lock-name)
    (ffi-call/callable #t in-types out-type abi varargs-after
                       save-errno lock-name (and blocking? #t) (and orig-place? #t) #f (and exns? #t)
                       #f
                       core)]))

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
                           async-apply
                           maybe-core)
  (let* ([by-value? (lambda (type)
                      ;; An 'array rep is compound, but should be
                      ;; passed as a pointer, so only pass 'struct and
                      ;; 'union "by value":
                      (#%memq (ctype-host-rep type) '(struct union)))]
         [array-rep-to-pointer-rep (lambda (host-rep)
                                     (if (eq? host-rep 'array)
                                         'ftype-pointer
                                         host-rep))]
         [core ; = (list call-proc callback-proc ret-maker arg-makers)
          (or maybe-core ; might be provided as statically generated
              ;; There's a second, compile-time copy of this core-creation code in
              ;; `ffi-static-call-and-callback-core`
              (let* ([conv* (let ([conv* (case abi
                                           [(stdcall) '(__stdcall)]
                                           [(sysv) '(__cdecl)]
                                           [else '()])])
                              (if varargs-after
                                  (cons `(__varargs_after ,varargs-after) conv*)
                                  conv*))]
                     [next!-id (let ([counter 0])
                                 ;; Like `gensym`, but deterministic --- and doesn't
                                 ;; have to be totally unique, as long as it doesn't
                                 ;; collide with other code that we generate
                                 (lambda ()
                                   (set! counter (add1 counter))
                                   (string->symbol (string-append "type_" (#%number->string counter)))))]
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
                                    '())])
                (let ([expr `(let ()
                               ,@decls
                               ,@ret-decls
                               (list
                                ,@(if call? '() '(#f))
                                (lambda (to-wrap)
                                  (,(if call? 'foreign-procedure 'foreign-callable)
                                   ,@conv*
                                   ,@(if (or blocking? async-apply) '(__collect_safe) '())
                                   to-wrap
                                   ,(map (lambda (in-type id)
                                           (if id
                                               `(& ,id ftype-pointer)
                                               (array-rep-to-pointer-rep
                                                (if call?
                                                    (ctype-in-host-rep in-type)
                                                    (ctype-host-rep in-type)))))
                                         in-types ids)
                                   ,(if ret-id
                                        `(& ,ret-id ftype-pointer)
                                        (array-rep-to-pointer-rep
                                         (if call?
                                             (ctype-host-rep out-type)
                                             (ctype-in-host-rep out-type))))))
                                ,@(if call? '(#f) '())))])
                  (let* ([wb (with-interrupts-disabled*
                              (hash-ref ffi-expr->code expr #f))]
                         [code (if wb (car wb) #!bwp)])
                    (if (eq? code #!bwp)
                        (let ([code (eval/foreign expr (if call? 'comp-ffi-call 'comp-ffi-back))])
                          (hashtable-set! ffi-code->expr (car code) expr)
                          (with-interrupts-disabled*
                           (hash-set! ffi-expr->code expr (weak-cons code #f)))
                          code)
                        code)))))]
         [ret-size (and (by-value? out-type) (ctype-sizeof out-type))]
         [ret-malloc-mode (and ret-size (compound-ctype-malloc-mode out-type))]
         [gen-proc (if call? (car core) (cadr core))]
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
       [(and (not ret-size)
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
            (let* ([proc-p (cptr->fptr 'ffi-call to-wrap)]
                   [proc (and (not (ftype-scheme-object-pointer? proc-p))
                              (gen-proc (ftype-pointer-address proc-p)))]
                   [name (cpointer->name to-wrap)])
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
                                                        (let ([id ((ctype-s->c type) name orig)] ...)
                                                          (when #,lock (mutex-acquire #,lock))
                                                          (let ([r (retain
                                                                    orig ...
                                                                    (with-interrupts-disabled*
                                                                     (proc id ...)))])
                                                            (when #,lock (mutex-release #,lock))
                                                            ((ctype-c->s out-type) r)))))])
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
                         (let ([args (map (lambda (a t) ((ctype-s->c t) name a)) orig-args in-types)])
                           ((ctype-c->s out-type) (retain
                                                   orig-args
                                                   (with-interrupts-disabled*
                                                    (#%apply proc args))))))]))]
                 [else
                  (lambda orig-args
                    (let ([args (map (lambda (a t) ((ctype-s->c t) name a)) orig-args in-types)])
                      (when lock (mutex-acquire lock))
                      (let ([r (retain
                                orig-args
                                (with-interrupts-disabled*
                                 (#%apply (gen-proc (ftype-pointer-address proc-p))
                                          args)))])
                        (when lock (mutex-release lock))
                        ((ctype-c->s out-type) r))))])
               arity-mask
               name
               default-realm))))]
       [else
        (lambda (to-wrap)
          (let* ([proc-p (cptr->fptr 'ffi-call to-wrap)]
                 [name (cpointer->name to-wrap)])
            (do-procedure-reduce-arity-mask
             (lambda orig-args
               (let* ([args (map (lambda (orig-arg in-type)
                                   ((ctype-s->c in-type) name orig-arg))
                                 orig-args in-types)]
                      [r (let ([ret-ptr (and ret-size
                                             ;; result is a struct type; need to allocate space for it
                                             (cptr->fptr
                                              'ret
                                              (normalized-malloc ret-size ret-malloc-mode)))])
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
                                                                     (list ret-ptr))
                                                                   '())
                                                               args)]
                                                        [proc (gen-proc (ftype-pointer-address proc-p))])
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
                                             [else r])))))])
                             (if (and orig-place?
                                      (not (eqv? 0 (get-thread-id))))
                                 (async-callback-queue-call orig-place-async-callback-queue (lambda (th) (th)) (lambda () (go)) #f #t #t)
                                 (go))))])
                 ((ctype-c->s out-type) r)))
             (fxsll 1 (length in-types))
             name
             default-realm)))])]
     [else ; callable
      (lambda (to-wrap)
        (gen-proc (lambda args ; if ret-size, includes an extra initial argument to receive the result
                    (let ([v (call-as-atomic-callback
                              (lambda ()
                                (unless async-apply
                                  ;; Sanity check; if the check fails, things can go bad from here on,
                                  ;; but we try to continue, anyway
                                  (when (currently-blocking?)
                                    (#%printf "non-async in callback during blocking: ~s\n" to-wrap)))
                                ((ctype-s->c out-type)
                                 'callback
                                 (apply to-wrap
                                        (let loop ([args (if ret-size (cdr args) args)] [in-types in-types])
                                          (cond
                                           [(null? args) '()]
                                           [else
                                            (let* ([arg (car args)]
                                                   [type (car in-types)]
                                                   [arg ((ctype-c->s type) arg)])
                                              (cons arg (loop (cdr args) (cdr in-types))))])))))
                              (or #t atomic?) ; force all callbacks to be atomic
                              async-apply
                              async-callback-queue)])
                      (if ret-size
                          (let* ([size (compound-ctype-size out-type)])
                            (memcpy* (car args) 0 v 0 size #f))
                          v)))))])))

(define-syntax (ffi-static-call-and-callback-core stx)
  (syntax-case stx ()
    [(_ (in-type ...) out-type abi varargs-after collect-safe?)
     ;; There's a second, run-time copy of this core-creation code in `ffi-call/callable`
     (let* ([conv* (let ([conv* (case (#%syntax->datum #'abi)
                                  [(stdcall) '(__stdcall)]
                                  [(sysv) '(__cdecl)]
                                  [else '()])])
                     (if (#%syntax->datum #'varargs-after)
                         (cons `(__varargs_after ,(#%syntax->datum #'varargs-after)) conv*)
                         conv*))]
            [by-value? (lambda (type-stx)
                         (syntax-case type-stx ()
                           [(type . _)
                            (#%memq (#%syntax->datum #'type) '(struct union))]
                           [_ #false]))]
            [in-types (syntax->list #'(in-type ...))]
            [out-type #'out-type]
            [array-rep-to-pointer-rep (lambda (host-rep-stx)
                                        (syntax-case host-rep-stx (array)
                                          [(array . _) #'ftype-pointer]
                                          [_ host-rep-stx]))]
            [next!-id (let ([counter 0])
                        (lambda ()
                          (set! counter (add1 counter))
                          (string->symbol (string-append "type_" (#%number->string counter)))))]
            [get-decls (lambda (type-stx id)
                         (#%error 'get-decls "not ready"))]
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
                        (let ([id-decls (get-decls (car in-types) (car ids))])
                          (loop (cdr in-types) (cdr ids) (append decls id-decls)))]
                       [else
                        (loop (cdr in-types) (cdr ids) decls)]))]
            [ret-decls (if ret-id
                           (get-decls out-type ret-id)
                           '())]
            [mk-proc (lambda (call?)
                       `(lambda (to-wrap)
                          (,(if call? 'foreign-procedure 'foreign-callable)
                           ,@conv*
                           ,@(if (#%syntax->datum #'collect-safe?) '(__collect_safe) '())
                           to-wrap
                           ,(map (lambda (in-type id)
                                   (if id
                                       `(& ,id ftype-pointer)
                                       (array-rep-to-pointer-rep
                                        in-type)))
                                 in-types ids)
                           ,(if ret-id
                                `(& ,ret-id ftype-pointer)
                                (array-rep-to-pointer-rep
                                 out-type)))))])
       (#%datum->syntax
        #'here
        `(begin-unsafe
          (let ()
            ,@decls
            ,@ret-decls
            (list
             ,(mk-proc #t)
             ,(mk-proc #f))))))]))

(define (ffi-maybe-call-and-callback-core must? abi varags-after blocking? async-apply? out . ins)
  (values #f ins out abi varags-after blocking? async-apply?))

(define (assert-ctype-representation ctype1 ctype2)
  (meta-cond
   [(< (optimize-level) 3)
    (unless (and (ctype? ctype1)
                 (ctype? ctype2)
                 (#%equal? (ctype-host-rep ctype1) (ctype-host-rep ctype2)))
      (#%error 'assert-ctype-representation "mismatch between ~s vs. ~s" ctype1 ctype2))])
  ctype2)

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
(define scheduler-end-atomic void) ; doesn't run end-atomic callbacks, which means that breaks may be delayed
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
  (parent cpointer)
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
    ((ffi-callback-maker* in-types out-type abi varargs-after atomic? async-apply #f) proc)]))

(define/who ffi-callback-maker
  (case-lambda
   [(in-types out-type)
    (ffi-callback-maker in-types out-type #f #f #f #f #f)]
   [(in-types out-type abi)
    (ffi-callback-maker in-types out-type abi #f #f #f #f)]
   [(in-types out-type abi atomic?)
    (ffi-callback-maker in-types out-type abi atomic? #f #f #f)]
   [(in-types out-type abi atomic? async-apply)
    (ffi-callback-maker in-types out-type abi atomic? async-apply #f #f)]
   [(in-types out-type abi atomic? async-apply varargs-after)
    (ffi-callback-maker in-types out-type abi atomic? async-apply varargs-after #f)]
   [(in-types out-type abi atomic? async-apply varargs-after core)
    (check-ffi-callback who in-types out-type abi varargs-after async-apply)
    (ffi-callback-maker* in-types out-type abi varargs-after atomic? async-apply core)]))

(define (ffi-callback-maker* in-types out-type abi varargs-after atomic? async-apply core)
  (let ([make-code (ffi-call/callable #f in-types out-type abi varargs-after
                                      #f #f #f #f (and atomic? #t) #f
                                      async-apply
                                      core)])
    (lambda (proc)
      (check 'make-ffi-callback procedure? proc)
      (let ([code (make-code proc)])
        (create-callback (make-ftype-pointer
                          integer-8
                          ;; this address will stay in place as long as `code` is reachable
                          (foreign-callable-entry-point code))
                         #f
                         ;; as long as the resulting cpointer stays reachable, `code` is reachable
                         code)))))

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
                            (let ([m (cpointer-fptr a)])
                              (if (ftype-scheme-object-pointer? m)
                                  (+ (eq-hash-code (ftype-scheme-object-pointer-object m))
                                     (ftype-scheme-object-pointer-offset m))
                                  (ftype-pointer-address m)))))
  (inherit-equal+hash! (record-type-descriptor cpointer+offset)
                       (record-type-descriptor cpointer)))
