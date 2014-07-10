#lang racket/base
(require racket/function
         racket/match
         racket/list
         unstable/struct
         compiler/zo-structs
         racket/dict
         racket/set)

(provide zo-parse)
(provide (all-from-out compiler/zo-structs))

#| Unresolved Issues

  The order of indirect-et-provides, indirect-syntax-provides, indirect-provides was changed, is that okay?
 
  orig-port of cport struct is never used, is it needed?

  Lines 628, 630 seem to be only for debugging and should probably throw errors

  vector and pair cases of decode-wraps seem to do different things from the corresponding C code

  Line 816: This should be an eqv placeholder (but they don't exist)

  Line 634: Export registry is always matched as false, but might not be

  What are the real differences between the module-binding cases?

  I think parse-module-path-index was only used for debugging, so it is short-circuited now

|#
;; ----------------------------------------
;; Bytecode unmarshalers for various forms

(define (read-toplevel v)
  (define SCHEME_TOPLEVEL_CONST #x02)
  (define SCHEME_TOPLEVEL_READY #x01)
  (match v
    [(cons depth (cons pos flags))
     ;; In the VM, the two flag bits are actually interpreted
     ;; as a number when the toplevel is a reference, but we
     ;; interpret the bits as flags here for backward compatibility.
     (make-toplevel depth pos 
                    (positive? (bitwise-and flags SCHEME_TOPLEVEL_CONST))
                    (positive? (bitwise-and flags SCHEME_TOPLEVEL_READY)))]
    [(cons depth pos)
     (make-toplevel depth pos #f #f)]))

(define (read-topsyntax v)
  (match v
    [`(,depth ,pos . ,midpt)
     (make-topsyntax depth pos midpt)]))

(define (read-variable v)
  (if (symbol? v)
      (make-global-bucket v)
      (error "expected a symbol")))

(define (do-not-read-variable v)
  (error "should not get here"))

(define (read-compilation-top v)
  (match v
    [`(,ld ,prefix . ,code)
     (unless (prefix? prefix)
       (error 'bad "not prefix ~a" prefix))
     (make-compilation-top ld prefix code)]))

(define (read-resolve-prefix v)
  (match v
    [`(,i ,tv . ,sv)
     ;; XXX Why not leave them as vectors and change the contract?
     (make-prefix i (vector->list tv) (vector->list sv))]))

(define read-free-id-info
  (match-lambda
    [(vector mpi0 symbol0 mpi1 symbol1 num0 num1 num2 bool0) ; I have no idea what these mean
     (make-free-id-info mpi0 symbol0 mpi1 symbol1 num0 num1 num2 bool0)]))

(define (read-unclosed-procedure v)
  (define CLOS_HAS_REST 1)
  (define CLOS_HAS_REF_ARGS 2)
  (define CLOS_PRESERVES_MARKS 4)
  (define CLOS_NEED_REST_CLEAR 8)
  (define CLOS_IS_METHOD 16)
  (define CLOS_SINGLE_RESULT 32)
  (define BITS_PER_MZSHORT 32)
  (define BITS_PER_ARG 4)
  (match v
    [`(,flags ,num-params ,max-let-depth ,tl-map ,name ,v . ,rest)
     (let ([rest? (positive? (bitwise-and flags CLOS_HAS_REST))])
       (let*-values ([(closure-size closed-over body)
                      (if (zero? (bitwise-and flags CLOS_HAS_REF_ARGS))
                          (values (vector-length v) v rest)
                          (values v (car rest) (cdr rest)))]
                     [(get-flags) (lambda (i)
                                    (if (zero? (bitwise-and flags CLOS_HAS_REF_ARGS))
                                        0
                                        (let ([byte (vector-ref closed-over
                                                                (+ closure-size (quotient (* BITS_PER_ARG i) BITS_PER_MZSHORT)))])
                                          (bitwise-and (arithmetic-shift byte (- (remainder (* BITS_PER_ARG i) BITS_PER_MZSHORT)))
                                                       (sub1 (arithmetic-shift 1 BITS_PER_ARG))))))]
                     [(num->type) (lambda (n)
                                    (case n
                                      [(2) 'flonum]
                                      [(3) 'fixnum]
                                      [(4) 'extflonum]
                                      [else (error "invaid type flag")]))]
                     [(arg-types) (let ([num-params ((if rest? sub1 values) num-params)])
                                    (for/list ([i (in-range num-params)]) 
                                      (define v (get-flags i))
                                      (case v
                                        [(0) 'val]
                                        [(1) 'ref]
                                        [else (num->type v)])))]
                     [(closure-types) (for/list ([i (in-range closure-size)]
                                                 [j (in-naturals num-params)])
                                        (define v (get-flags j))
                                        (case v
                                          [(0) 'val/ref]
                                          [(1) (error "invalid 'ref closure variable")]
                                          [else (num->type v)]))])
         (make-lam name
                   (append
                    (if (zero? (bitwise-and flags flags CLOS_PRESERVES_MARKS)) null '(preserves-marks))
                    (if (zero? (bitwise-and flags flags CLOS_IS_METHOD)) null '(is-method))
                    (if (zero? (bitwise-and flags flags CLOS_SINGLE_RESULT)) null '(single-result))
                    (if (zero? (bitwise-and flags flags CLOS_NEED_REST_CLEAR)) null '(sfs-clear-rest-args))
                    (if (and rest? (zero? num-params)) '(only-rest-arg-not-used) null))
                   (if (and rest? (num-params . > . 0))
                       (sub1 num-params)
                       num-params)
                   arg-types
                   rest?
                   (if (= closure-size (vector-length closed-over))
                       closed-over
                       (let ([v2 (make-vector closure-size)])
                         (vector-copy! v2 0 closed-over 0 closure-size)
                         v2))
                   closure-types
                   (and tl-map
                        (let* ([bits (if (exact-integer? tl-map)
                                         tl-map
                                         (for/fold ([i 0]) ([v (in-vector tl-map)]
                                                            [s (in-naturals)])
                                           (bitwise-ior i (arithmetic-shift v (* s 16)))))]
                               [len (integer-length bits)])
                          (list->set
                           (let loop ([bit 0])
                             (cond
                              [(bit . >= . len) null]
                              [(bitwise-bit-set? bits bit)
                               (cons bit (loop (add1 bit)))]
                              [else (loop (add1 bit))])))))
                   max-let-depth
                   body)))]))

(define (read-let-value v)
  (match v
    [`(,count ,pos ,boxes? ,rhs . ,body)
     (make-install-value count pos boxes? rhs body)]))

(define (read-let-void v)
  (match v
    [`(,count ,boxes? . ,body)
     (make-let-void count boxes? body)]))

(define (read-letrec v)
  (match v
    [`(,count ,body . ,procs)
     (make-let-rec procs body)]))

(define (read-with-cont-mark v)
  (match v
    [`(,key ,val . ,body)
     (make-with-cont-mark key val body)]))

(define (read-sequence v) 
  (make-seq v))

; XXX Allocates unnessary list
(define (read-define-values v)
  (make-def-values
   (cdr (vector->list v))
   (vector-ref v 0)))

(define (read-define-syntax v)
  (make-def-syntaxes (list-tail (vector->list v) 4)
                     (vector-ref v 0)
                     (vector-ref v 1)
                     (vector-ref v 2)
                     (vector-ref v 3)))

(define (read-begin-for-syntax v)
  (make-seq-for-syntax 
   (vector-ref v 0)
   (vector-ref v 1)
   (vector-ref v 2)
   (vector-ref v 3)))

(define (read-set! v)
  (make-assign (cadr v) (cddr v) (car v)))

(define (read-case-lambda v)
  (make-case-lam (car v) (cdr v)))

(define (read-begin0 v) 
  (make-beg0 v))

(define (read-boxenv v)
  (make-boxenv (car v) (cdr v)))
(define (read-require v)
  (make-req (cdr v) (car v)))
(define (read-#%variable-ref v)
  (make-varref (car v) (cdr v)))
(define (read-apply-values v)
  (make-apply-values (car v) (cdr v)))
(define (read-splice v)
  (make-splice v))

(define (in-list* l n)
  (make-do-sequence
   (lambda ()
     (values (lambda (l) (apply values (take l n)))
             (lambda (l) (drop l n))
             l
             (lambda (l) (>= (length l) n))
             (lambda _ #t)
             (lambda _ #t)))))

(define (split-phase-data rest n)
  (let loop ([n n] [rest rest] [phase-accum null])
    (cond
     [(zero? n)
      (values (reverse phase-accum) rest)]
     [else
      (let ([maybe-indirect (list-ref rest 1)])
        (if (void? maybe-indirect) 
            ;; no indirect or protect info:
            (loop (sub1 n)
                  (list-tail rest 9)
                  (cons (take rest 9) phase-accum))
            ;; has indirect or protect info:
            (loop (sub1 n)
                  (list-tail rest (+ 5 8))
                  (cons (take rest (+ 5 8)) phase-accum))))])))
      
(define (read-module v)
  (match v
    [`(,submod-path 
       ,name ,srcname ,self-modidx ,cross-phase?
       ,pre-submods ,post-submods
       ,lang-info ,functional? ,et-functional?
       ,rename ,max-let-depth ,dummy
       ,prefix ,num-phases
       ,provide-phase-count . ,rest)
     (let*-values ([(phase-data rest-module) (split-phase-data rest provide-phase-count)]
                   [(bodies rest-module) (values (take rest-module num-phases)
                                                 (drop rest-module num-phases))])
       (match rest-module
         [`(,requires ,syntax-requires ,template-requires ,label-requires
                      ,more-requires-count . ,more-requires)
          (make-mod (if (null? submod-path)
                        name 
                        (if (symbol? name)
                            (cons name submod-path)
                            (cons (car name) submod-path)))
                    srcname self-modidx
                    prefix
                    ;; provides:
                    (for/list ([l (in-list phase-data)])
                      (let* ([phase (list-ref l 0)]
                             [has-info? (not (void? (list-ref l 1)))]
                             [delta (if has-info? 5 1)]
                             [num-vars (list-ref l (+ delta 6))]
                             [num-all (list-ref l (+ delta 7))]
                             [ps (for/list ([name (in-vector (list-ref l (+ delta 5)))]
                                            [src (in-vector (list-ref l (+ delta 4)))]
                                            [src-name (in-vector (list-ref l (+ delta 3)))]
                                            [nom-src (or (list-ref l (+ delta 2))
                                                         (in-cycle (in-value #f)))]
                                            [src-phase (or (list-ref l (+ delta 1))
                                                           (in-cycle (in-value 0)))]
                                            [protected? (cond
                                                         [(or (not has-info?)
                                                              (not (list-ref l 5)))
                                                          (in-cycle (in-value #f))]
                                                         [else (list-ref l 5)])])
                                   (make-provided name src src-name 
                                                  (or nom-src src)
                                                  src-phase
                                                  protected?))])
                        (list
                         phase
                         (take ps num-vars)
                         (drop ps num-vars))))
                    ;; requires:
                    (list*
                     (cons 0 requires)
                     (cons 1 syntax-requires)
                     (cons -1 template-requires)
                     (cons #f label-requires)
                     (for/list ([(phase reqs) (in-list* more-requires 2)])
                       (cons phase reqs)))
                    ;; body:
                    (vector->list (last bodies))
                    ;; syntax-bodies: add phase to each list, break apart
                    (for/list ([b (cdr (reverse bodies))]
                               [i (in-naturals 1)])
                      (cons i
                            (for/list ([sb (in-vector b)])
                              (match sb
                                [`#(,ids ,expr ,max-let-depth ,prefix ,for-stx?)
                                 (if for-stx?
                                     (make-seq-for-syntax (list expr) prefix max-let-depth #f)
                                     (make-def-syntaxes
                                      (if (list? ids) ids (list ids)) expr prefix max-let-depth #f))]
                                [else (error 'zo-parse "bad phase ~a body element: ~e" i sb)]))))
                    ;; unexported:
                    (for/list ([l (in-list phase-data)]
                               #:unless (void? (list-ref l 1)))
                      (let* ([phase (list-ref l 0)]
                             [indirect-syntax 
                              ;; could check: (list-ref l 2) should be size of vector:
                              (list-ref l 1)]
                             [indirect 
                              ;; could check: (list-ref l 4) should be size of vector:
                              (list-ref l 3)])
                        (list
                         phase
                         (vector->list indirect)
                         (vector->list indirect-syntax))))
                    max-let-depth
                    dummy
                    lang-info
                    rename
                    (if cross-phase? '(cross-phase) '())
                    (map read-module pre-submods)
                    (map read-module post-submods))]))]))
(define (read-module-wrap v)
  v)

(define (read-inline-variant v)
  (make-inline-variant (car v) (cdr v)))

;; ----------------------------------------
;; Unmarshal dispatch for various types

;; Type mappings from "stypes.h":
(define (int->type i)
  (case i
    [(0) 'toplevel-type]
    [(6) 'sequence-type]
    [(8) 'unclosed-procedure-type]
    [(9) 'let-value-type]
    [(10) 'let-void-type]
    [(11) 'letrec-type]
    [(13) 'with-cont-mark-type]
    [(14) 'quote-syntax-type]
    [(15) 'define-values-type]
    [(16) 'define-syntaxes-type]
    [(17) 'begin-for-syntax-type]
    [(18) 'set-bang-type]
    [(19) 'boxenv-type]
    [(20) 'begin0-sequence-type]
    [(21) 'splice-sequence-type]
    [(22) 'require-form-type]
    [(23) 'varref-form-type]
    [(24) 'apply-values-type]
    [(25) 'case-lambda-sequence-type]
    [(26) 'module-type]
    [(27) 'inline-variant-type]
    [(35) 'variable-type]
    [(36) 'module-variable-type]
    [(114) 'resolve-prefix-type]
    [(164) 'free-id-info-type]
    [else (error 'int->type "unknown type: ~e" i)]))

(define type-readers
  (make-immutable-hash
   (list
    (cons 'toplevel-type read-toplevel)
    (cons 'sequence-type read-sequence)
    (cons 'unclosed-procedure-type read-unclosed-procedure)
    (cons 'let-value-type read-let-value)
    (cons 'let-void-type read-let-void)
    (cons 'letrec-type read-letrec)
    (cons 'with-cont-mark-type read-with-cont-mark)
    (cons 'quote-syntax-type read-topsyntax)
    (cons 'variable-type read-variable)
    (cons 'module-variable-type do-not-read-variable)
    (cons 'compilation-top-type read-compilation-top)
    (cons 'case-lambda-sequence-type read-case-lambda)
    (cons 'begin0-sequence-type read-begin0)
    (cons 'module-type read-module)
    (cons 'inline-variant-type read-inline-variant)
    (cons 'resolve-prefix-type read-resolve-prefix)
    (cons 'free-id-info-type read-free-id-info)
    (cons 'define-values-type read-define-values)
    (cons 'define-syntaxes-type read-define-syntax)
    (cons 'begin-for-syntax-type read-begin-for-syntax)
    (cons 'set-bang-type read-set!)
    (cons 'boxenv-type read-boxenv)
    (cons 'require-form-type read-require)
    (cons 'varref-form-type read-#%variable-ref)
    (cons 'apply-values-type read-apply-values)
    (cons 'splice-sequence-type read-splice))))

(define (get-reader type)
  (hash-ref type-readers type
            (λ ()
              (error 'read-marshalled "reader for ~a not implemented" type))))

;; ----------------------------------------
;; Lowest layer of bytecode parsing

(define (split-so all-short so)
  (define n (if (zero? all-short) 4 2))
  (let loop ([so so])
    (if (zero? (bytes-length so))
        null
        (cons (integer-bytes->integer (subbytes so 0 n) #f #f)
              (loop (subbytes so n))))))

(define (read-simple-number p)
  (integer-bytes->integer (read-bytes 4 p) #f #f))

(define-struct cport ([pos #:mutable] shared-start orig-port size bytes-start symtab shared-offsets decoded rns mpis))
(define (cport-get-bytes cp len)
  (define port (cport-orig-port cp))
  (define pos (cport-pos cp))
  (file-position port (+ (cport-bytes-start cp) pos))
  (read-bytes len port))
(define (cport-get-byte cp pos)
  (define port (cport-orig-port cp))
  (file-position port (+ (cport-bytes-start cp) pos))
  (read-byte port))

(define (cport-rpos cp)
  (+ (cport-pos cp) (cport-shared-start cp)))

(define (cp-getc cp)
  (when ((cport-pos cp) . >= . (cport-size cp))
    (error "off the end"))
  (define r (cport-get-byte cp (cport-pos cp)))
  (set-cport-pos! cp (add1 (cport-pos cp)))
  r)

(define small-list-max 50)
(define raw-cpt-table
  ;; The "schcpt.h" mapping, earlier entries override later ones
  `([0  escape]
    [1  symbol]
    [2  symref]
    [3  weird-symbol]
    [4  keyword]
    [5  byte-string]
    [6  string]
    [7  char]
    [8  int]
    [9  null]
    [10 true]
    [11 false]
    [12 void]
    [13 box]
    [14 pair]
    [15 list]
    [16 vector]
    [17 hash-table]
    [18 stx]
    [19 let-one-typed]
    [20 marshalled]
    [21 quote]
    [22 reference]
    [23 local]
    [24 local-unbox]
    [25 svector]
    [26 application]
    [27 let-one]
    [28 branch]
    [29 module-index]
    [30 module-var]
    [31 path]
    [32 closure]
    [33 delayed]
    [34 prefab]
    [35 let-one-unused]
    [36 60 small-number]
    [60 80 small-symbol]
    [80 92 small-marshalled]
    [92 ,(+ 92 small-list-max) small-proper-list]
    [,(+ 92 small-list-max) 192 small-list]
    [192 207 small-local]
    [207 222 small-local-unbox]
    [222 247 small-svector]
    [248 small-application2]
    [249 small-application3]
    [247 255 small-application]))

;; To accelerate cpt-table lookup, we flatten out the above
;; list into a vector:
(define cpt-table (make-vector 256 #f))
(for ([ent (in-list (reverse raw-cpt-table))])
  ;; reverse order so that early entries override later ones.
  (match ent
    [(list k sym)    (vector-set! cpt-table k (cons k sym))]
    [(list k k* sym) (for ([i (in-range k k*)])
                       (vector-set! cpt-table i (cons k sym)))]))

(define (read-compact-bytes port c)
  (begin0
    (cport-get-bytes port c)
    (set-cport-pos! port (+ c (cport-pos port)))))

(define (read-compact-chars port c)
  (bytes->string/utf-8 (read-compact-bytes port c)))

(define (read-compact-list c proper port)
  (cond [(= 0 c)
         (if proper null (read-compact port))]
        [else (cons (read-compact port) (read-compact-list (sub1 c) proper port))]))

(define (read-compact-number port)
  (define flag (cp-getc port))
  (cond [(< flag 128)
         flag]
        [(zero? (bitwise-and flag #x40))
         (let ([a (cp-getc port)])
           (+ (a . << . 6) (bitwise-and flag 63)))]
        [(zero? (bitwise-and flag #x20))
         (- (bitwise-and flag #x1F))]
        [else
         (let ([a (cp-getc port)]
               [b (cp-getc port)]
               [c (cp-getc port)]
               [d (cp-getc port)])
           (let ([n (integer-bytes->integer (bytes a b c d) #f #f)])
             (if (zero? (bitwise-and flag #x10))
                 (- n)
                 n)))]))

(define (read-compact-svector port n)
  (define v (make-vector n))
  (for ([i (in-range n)])
    (vector-set! v (sub1 (- n i)) (read-compact-number port)))
  v)

(define (read-marshalled type port)
  (let* ([type (if (number? type) (int->type type) type)]
         [l (read-compact port)]
         [reader (get-reader type)])
    (reader l)))

(define SCHEME_LOCAL_TYPE_FLONUM 1)
(define SCHEME_LOCAL_TYPE_FIXNUM 2)
(define SCHEME_LOCAL_TYPE_EXTFLONUM 3)

(define (make-local unbox? pos flags)
  (define SCHEME_LOCAL_CLEAR_ON_READ 1)
  (define SCHEME_LOCAL_OTHER_CLEARS 2)
  (define SCHEME_LOCAL_TYPE_OFFSET 2)
  (make-localref unbox? pos 
                 (= flags SCHEME_LOCAL_CLEAR_ON_READ)
                 (= flags SCHEME_LOCAL_OTHER_CLEARS)
                 (let ([t (- flags SCHEME_LOCAL_TYPE_OFFSET)])
                   (cond
                    [(= t SCHEME_LOCAL_TYPE_FLONUM) 'flonum]
                    [(= t SCHEME_LOCAL_TYPE_EXTFLONUM) 'extflonum]
                    [(= t SCHEME_LOCAL_TYPE_FIXNUM) 'fixnum]
                    [else #f]))))

(define (a . << . b)
  (arithmetic-shift a b))

(define-struct not-ready ())

;; ----------------------------------------
;; Syntax unmarshaling
(define (make-memo) (make-weak-hash))
(define (with-memo* mt arg thnk)
  (hash-ref! mt arg thnk))
(define-syntax-rule (with-memo mt arg body ...)
  (with-memo* mt arg (λ () body ...)))

(define (decode-mark-map alist)
  alist)

(define stx-memo (make-memo))
; XXX More memo use
(define (decode-stx cp v)
  (with-memo stx-memo v
    (if (integer? v)
        (unmarshal-stx-get/decode cp v decode-stx) 
        (let loop ([v v])
          (let-values ([(tamper-status v encoded-wraps)
                        (match v
                          [`#((,datum . ,wraps)) (values 'tainted datum wraps)]
                          [`#((,datum . ,wraps) #f) (values 'armed datum wraps)]
                          [`(,datum . ,wraps) (values 'clean datum wraps)]
                          [else (error 'decode-wraps "bad datum+wrap: ~.s" v)])])
            (let* ([wraps (decode-wraps cp encoded-wraps)]
                   [wrapped-memo (make-memo)]
                   [add-wrap (lambda (v) (with-memo wrapped-memo v (make-wrapped v wraps tamper-status)))])
              (cond
                [(pair? v)
                 (if (eq? #t (car v))
                     ;; Share decoded wraps with all nested parts.
                     (let iloop ([v (cdr v)])
                       (cond
                         [(pair? v) 
                          (let ploop ([v v])
                            (cond
                              [(null? v) null]
                              [(pair? v) (add-wrap (cons (iloop (car v)) (ploop (cdr v))))]
                              [else (iloop v)]))]
                         [(box? v) (add-wrap (box (iloop (unbox v))))]
                         [(vector? v)
                          (add-wrap (list->vector (map iloop (vector->list v))))]
                         [(hash? v)
                          (add-wrap (for/hash ([(k v) (in-hash v)])
                                      (values k (iloop v))))]
                         [(prefab-struct-key v)
                          => (lambda (k)
                               (add-wrap
                                (apply
                                 make-prefab-struct 
                                 k
                                 (map iloop (struct->list v)))))]
                         [else (add-wrap v)]))
                     ;; Decode sub-elements that have their own wraps:
                     (let-values ([(v counter) (if (exact-integer? (car v))
                                                   (values (cdr v) (car v))
                                                   (values v -1))])
                       (add-wrap
                        (let ploop ([v v][counter counter])
                          (cond
                            [(null? v) null]
                            [(or (not (pair? v)) (zero? counter)) (loop v)]
                            [(pair? v) (cons (loop (car v))
                                             (ploop (cdr v) (sub1 counter)))])))))]
                [(box? v) (add-wrap (box (loop (unbox v))))]
                [(vector? v)
                 (add-wrap (list->vector (map loop (vector->list v))))]
                [(hash? v)
                 (add-wrap (for/hash ([(k v) (in-hash v)])
                             (values k (loop v))))]
                [(prefab-struct-key v)
                 => (lambda (k)
                      (add-wrap
                       (apply
                        make-prefab-struct 
                        k
                        (map loop (struct->list v)))))]
                [else (add-wrap v)])))))))

(define wrape-memo (make-memo))
(define (decode-wrape cp a)
  (define (aloop a) (decode-wrape cp a))
  (with-memo wrape-memo a
    ; A wrap-elem is either
    (cond
      ; A reference 
      [(integer? a) 
       (unmarshal-stx-get/decode cp a (lambda (cp v) (aloop v)))]
      ; A mark wraped in a list
      [(and (pair? a) (number? (car a)) (null? (cdr a)))
       (make-wrap-mark (car a))]
      
      [(vector? a) 
       (make-lexical-rename (vector-ref a 0) (vector-ref a 1)
                            (let ([top (+ (/ (- (vector-length a) 2) 2) 2)])
                              (let loop ([i 2])
                                (if (= i top)
                                    null
                                    (cons (cons (vector-ref a i)
                                                (vector-ref a (+ (- top 2) i)))
                                          (loop (+ i 1)))))))]
      [(pair? a)
       (let-values ([(plus-kern? a) (if (eq? (car a) #t)
                                        (values #t (cdr a))
                                        (values #f a))])
         (match a
           [`(,phase ,kind ,set-id ,maybe-unmarshals . ,renames)
            (let-values ([(unmarshals renames mark-renames)
                          (if (vector? maybe-unmarshals)
                              (values null maybe-unmarshals renames)
                              (values maybe-unmarshals
                                      (car renames)
                                      (cdr renames)))])
              (make-module-rename phase 
                                  (if kind 'marked 'normal)
                                  set-id
                                  (map (curry decode-all-from-module cp) unmarshals)
                                  (decode-renames renames)
                                  mark-renames
                                  (and plus-kern? 'plus-kern)))]
           [else (error "bad module rename: ~e" a)]))]
      [(boolean? a)
       (make-top-level-rename a)]
      [(symbol? a)
       (make-mark-barrier a)]
      [(box? a)
       (match (unbox a)
         [(list (? symbol?) ...) (make-prune (unbox a))]
         [`#(,amt ,src ,dest #f #f ,cancel-id) 
          (make-phase-shift amt 
                            (parse-module-path-index cp src)
                            (parse-module-path-index cp dest)
                            cancel-id)]
         [else (error 'parse "bad phase shift: ~e" a)])]
      [else (error 'decode-wraps "bad wrap element: ~e" a)])))

(define (afm-context? v)
  (or (and (list? v) (andmap exact-integer? v))
      (and (vector? v) 
           (= 2 (vector-length v))
           (list? (vector-ref v 0))
           (andmap exact-integer? (vector-ref v 0)))))

(define all-from-module-memo (make-memo))
(define (decode-all-from-module cp afm)
  (define (phase? v)
    (or (number? v) (not v)))
  (with-memo all-from-module-memo afm
    (match afm
      [(list* path (? phase? phase) (? phase? src-phase) (list exn ...) prefix)
       (make-all-from-module
        (parse-module-path-index cp path)
        phase src-phase exn prefix null)]
      [(list* path (? phase? phase) (? afm-context? context) (? phase? src-phase))
       (make-all-from-module
        (parse-module-path-index cp path)
        phase src-phase null #f context)]
      [(list* path (? phase? phase) (? phase? src-phase))
       (make-all-from-module
        (parse-module-path-index cp path)
        phase src-phase null #f null)])))

(define wraps-memo (make-memo))
(define (decode-wraps cp w)
  (with-memo wraps-memo w
    ; A wraps is either a indirect reference or a list of wrap-elems (from stxobj.c:252)
    (if (integer? w)
        (unmarshal-stx-get/decode cp w decode-wraps)
        (map (curry decode-wrape cp) w))))

(define (in-vector* v n)
  (make-do-sequence
   (λ ()
     (values (λ (i) (vector->values v i (+ i n)))
             (λ (i) (+ i n))
             0
             (λ (i) (>= (vector-length v) (+ i n)))
             (λ _ #t)
             (λ _ #t)))))

(define nominal-path-memo (make-memo))
(define (decode-nominal-path np)
  (with-memo nominal-path-memo np
    (match np
      [(cons nominal-path (cons import-phase nominal-phase))
       (make-phased-nominal-path nominal-path import-phase nominal-phase)]
      [(cons nominal-path import-phase)
       (make-imported-nominal-path nominal-path import-phase)]
      [nominal-path
       (make-simple-nominal-path nominal-path)])))

; XXX Weird test copied from C code. Matthew?
(define (nom_mod_p p)
  (and (pair? p) (not (pair? (cdr p))) (not (symbol? (cdr p))))) 

(define rename-v-memo (make-memo))
(define (decode-rename-v v)
  (with-memo rename-v-memo v
    (match v
      [(list-rest path phase export-name nominal-path nominal-export-name)
       (make-phased-module-binding path
                                   phase
                                   export-name
                                   (decode-nominal-path nominal-path) 
                                   nominal-export-name)]
      [(list-rest path export-name nominal-path nominal-export-name)
       (make-exported-nominal-module-binding path
                                             export-name 
                                             (decode-nominal-path nominal-path)
                                             nominal-export-name)]
      [(cons module-path-index (? nom_mod_p nominal-path))
       (make-nominal-module-binding module-path-index (decode-nominal-path nominal-path))]
      [(cons module-path-index export-name)
       (make-exported-module-binding module-path-index export-name)]
      [module-path-index 
       (make-simple-module-binding module-path-index)])))

(define renames-memo (make-memo))
(define (decode-renames renames)
  (with-memo renames-memo renames
    (for/list ([(k v) (in-vector* renames 2)])
      (cons k (decode-rename-v v)))))

(define (parse-module-path-index cp s)
  s)

;; ----------------------------------------
;; Main parsing loop

(define (read-compact cp)
  (let loop ([need-car 0] [proper #f])
    (define ch (cp-getc cp))
    (define-values (cpt-start cpt-tag)
      (let ([x (vector-ref cpt-table ch)])
        (unless x (error 'read-compact "unknown code : ~a" ch))
        (values (car x) (cdr x))))
    (define v
      (case cpt-tag
        [(delayed)
         (let ([pos (read-compact-number cp)])
           (read-sym cp pos))]
        [(escape)
         (let* ([len (read-compact-number cp)]
                [s (cport-get-bytes cp len)])
           (set-cport-pos! cp (+ (cport-pos cp) len))
           (parameterize ([read-accept-compiled #t]
                          [read-accept-bar-quote #t]
                          [read-accept-box #t]
                          [read-accept-graph #t]
                          [read-case-sensitive #t]
                          [read-square-bracket-as-paren #t]
                          [read-curly-brace-as-paren #t]
                          [read-decimal-as-inexact #t]
                          [read-accept-dot #t]
                          [read-accept-infix-dot #t]
                          [read-accept-quasiquote #t]
                          [current-readtable
                           (make-readtable 
                            #f
                            #\^
                            'dispatch-macro
                            (lambda (char port src line col pos)
                              (let ([b (read port)])
                                (unless (bytes? b)
                                  (error 'read-escaped-path
                                         "expected a byte string after #^"))
                                (let ([p (bytes->path b)])
                                  (if (and (relative-path? p)
                                           (current-load-relative-directory))
                                    (build-path (current-load-relative-directory) p)
                                    p)))))])
             (read/recursive (open-input-bytes s))))]
        [(reference)
         (make-primval (read-compact-number cp))]
        [(small-list small-proper-list)
         (let* ([l (- ch cpt-start)]
                [ppr (eq? cpt-tag 'small-proper-list)])
           (if (positive? need-car)
             (if (= l 1)
               (cons (read-compact cp)
                     (if ppr null (read-compact cp)))
               (read-compact-list l ppr cp))
             (loop l ppr)))]
        [(let-one let-one-typed let-one-unused)
         (make-let-one (read-compact cp) (read-compact cp)
                       (and (eq? cpt-tag 'let-one-typed)
                            (case (read-compact-number cp)
                              [(1) 'flonum]
                              [(2) 'fixnum]
                              [(3) 'extflonum]
                              [else #f]))
                       (eq? cpt-tag 'let-one-unused))]
        [(branch)
         (make-branch (read-compact cp) (read-compact cp) (read-compact cp))]
        [(module-index) 
         (define name (read-compact cp))
         (define base (read-compact cp))
         (if (or name base)
             (module-path-index-join name base)
             (module-path-index-join #f #f (read-compact cp)))]
        [(module-var)
         (let ([mod (read-compact cp)]
               [var (read-compact cp)]
               [shape (read-compact cp)]
               [pos (read-compact-number cp)])
           (let-values ([(flags mod-phase pos)
                         (let loop ([pos pos])
                           (cond
                            [(pos . < . -3)
                             (let ([real-pos (read-compact-number cp)])
                               (define-values (_ m p) (loop real-pos))
                               (values (- (+ pos 3)) m p))]
                            [(= pos -2)
                             (values 0 (read-compact-number cp) (read-compact-number cp))]
                            [else (values 0 0 pos)]))])
             (make-module-variable mod var pos mod-phase
                                   (cond
                                    [shape
                                     (cond
                                      [(number? shape) 
                                       (define n (arithmetic-shift shape -1))
                                       (make-function-shape (if (negative? n)
                                                                (make-arity-at-least (sub1 (- n)))
                                                                n)
                                                            (odd? shape))]
                                      [(and (symbol? shape)
                                            (regexp-match? #rx"^struct" (symbol->string shape)))
                                       (define n (string->number (substring (symbol->string shape) 6)))
                                       (case (bitwise-and n #x7)
                                         [(0) (make-struct-type-shape (arithmetic-shift n -3))]
                                         [(1) (make-constructor-shape (arithmetic-shift n -3))]
                                         [(2) (make-predicate-shape)]
                                         [(3) (make-accessor-shape (arithmetic-shift n -3))]
                                         [(4) (make-mutator-shape (arithmetic-shift n -3))]
                                         [else (make-struct-other-shape)])]
                                      [else
                                       ;; parse symbol as ":"-separated sequence of arities
                                       (make-function-shape
                                        (for/list ([s (regexp-split #rx":" (symbol->string shape))])
                                          (define i (string->number s))
                                          (if (negative? i)
                                              (make-arity-at-least (sub1 (- i)))
                                              i))
                                        #f)])]
                                    [(not (zero? (bitwise-and #x1 flags))) 'constant]
                                    [(not (zero? (bitwise-and #x2 flags))) 'fixed]
                                    [else #f]))))]
        [(local-unbox)
         (let* ([p* (read-compact-number cp)]
                [p (if (< p* 0) (- (add1 p*)) p*)]
                [flags (if (< p* 0) (read-compact-number cp) 0)])
           (make-local #t p flags))]
        [(path)
         (let* ([p (bytes->path (read-compact-bytes cp (read-compact-number cp)))])
           (if (relative-path? p)
             (path->complete-path p (or (current-load-relative-directory)
                                        (current-directory)))
             p))]
        [(small-number)
         (let ([l (- ch cpt-start)])
           l)]
        [(int)
         (read-compact-number cp)]
        [(false) #f]
        [(true) #t]
        [(null) null]
        [(void) (void)]
        [(vector)
         ; XXX We should provide build-immutable-vector and write this as:
         #;(build-immutable-vector (read-compact-number cp)
                                   (lambda (i) (read-compact cp)))
         ; XXX Now it allocates an unnessary list AND vector
         (let* ([n (read-compact-number cp)]
                [lst (for/list ([i (in-range n)]) (read-compact cp))])
           (vector->immutable-vector (list->vector lst)))]
        [(pair)
         (let* ([a (read-compact cp)]
                [d (read-compact cp)])
           (cons a d))]
        [(list)
         (let ([len (read-compact-number cp)])
           (let loop ([i len])
             (if (zero? i)
               (read-compact cp)
               (list* (read-compact cp)
                      (loop (sub1 i))))))]
        [(prefab)
         (let ([v (read-compact cp)])
           ; XXX This is faster than apply+->list, but can we avoid allocating the vector?
           (call-with-values (lambda () (vector->values v))
                             make-prefab-struct))]
        [(hash-table)
         ; XXX Allocates an unnessary list (maybe use for/hash(eq))
         (let ([eq (read-compact-number cp)]
               [len (read-compact-number cp)])
           ((case eq
              [(0) make-hasheq-placeholder]
              [(1) make-hash-placeholder]
              [(2) make-hasheqv-placeholder])
            (for/list ([i (in-range len)])
              (cons (read-compact cp)
                    (read-compact cp)))))]
        [(marshalled) (read-marshalled (read-compact-number cp) cp)]
        [(stx)
         (let ([v (make-reader-graph (read-compact cp))])
           (make-stx (decode-stx cp v)))]
        [(local local-unbox)
         (let ([c (read-compact-number cp)]
               [unbox? (eq? cpt-tag 'local-unbox)])
           (if (negative? c)
             (make-local unbox? (- (add1 c)) (read-compact-number cp))
             (make-local unbox? c 0)))]
        [(small-local)
         (make-local #f (- ch cpt-start) 0)]
        [(small-local-unbox)
         (make-local #t (- ch cpt-start) 0)]
        [(small-symbol)
         (let ([l (- ch cpt-start)])
           (string->symbol (read-compact-chars cp l)))]
        [(symbol)
         (let ([l (read-compact-number cp)])
           (string->symbol (read-compact-chars cp l)))]
        [(keyword)
         (let ([l (read-compact-number cp)])
           (string->keyword (read-compact-chars cp l)))]
        [(byte-string)
         (let ([l (read-compact-number cp)])
           (read-compact-bytes cp l))]
        [(string)
         (let ([l (read-compact-number cp)]
               [cl (read-compact-number cp)])
           (read-compact-chars cp l))]
        [(char)
         (integer->char (read-compact-number cp))]
        [(box)
         (box (read-compact cp))]
        [(quote)
         (make-reader-graph 
          ;; Nested escapes need to share graph references. So get inside the
          ;;  read where `read/recursive' can be used:
          (let ([rt (current-readtable)])
            (parameterize ([current-readtable (make-readtable
                                               #f
                                               #\x 'terminating-macro
                                               (lambda args
                                                 (parameterize ([current-readtable rt])
                                                   (read-compact cp))))])
              (read (open-input-bytes #"x")))))]
        [(symref)
         (let* ([l (read-compact-number cp)])
           (read-sym cp l))]
        [(weird-symbol)
         (let ([uninterned (read-compact-number cp)]
               [str (read-compact-chars cp (read-compact-number cp))])
           (if (= 1 uninterned)
             ; uninterned is equivalent to weird in the C implementation 
             (string->uninterned-symbol str)
             ; unreadable is equivalent to parallel in the C implementation
             (string->unreadable-symbol str)))]
        [(small-marshalled)
         (read-marshalled (- ch cpt-start) cp)]
        [(small-application2)
         (make-application (read-compact cp)
                           (list (read-compact cp)))]
        [(small-application3)
         (make-application (read-compact cp)
                           (list (read-compact cp)
                                 (read-compact cp)))]
        [(small-application)
         (let ([c (add1 (- ch cpt-start))])
           (make-application (read-compact cp)
                             (for/list ([i (in-range (sub1 c))])
                               (read-compact cp))))]
        [(application)
         (let ([c (read-compact-number cp)])
           (make-application (read-compact cp)
                             (for/list ([i (in-range c)])
                               (read-compact cp))))]
        [(closure)
         (read-compact-number cp) ; symbol table pos. our marshaler will generate this
         (let ([v (read-compact cp)])
           (make-closure
            v
            (gensym
             (let ([s (lam-name v)])
               (cond
                 [(symbol? s) s]
                 [(vector? s) (vector-ref s 0)]
                 [else 'closure])))))]
        [(svector)
         (read-compact-svector cp (read-compact-number cp))]
        [(small-svector)
         (read-compact-svector cp (- ch cpt-start))]
        [else (error 'read-compact "unknown tag ~a" cpt-tag)]))
    (cond
      [(zero? need-car) v]
      [(and proper (= need-car 1))
       (cons v null)]
      [else
       (cons v (loop (sub1 need-car) proper))])))

(define (unmarshal-stx-get/decode cp pos decode-stx)
  (define v2 (read-sym cp pos))
  (define decoded? (vector-ref (cport-decoded cp) pos))
  (if decoded?
      v2
      (let ([dv2 (decode-stx cp v2)])
        (symtab-write! cp pos dv2)
        (vector-set! (cport-decoded cp) pos #t)
        dv2)))

(define (symtab-write! cp i v)
  (placeholder-set! (vector-ref (cport-symtab cp) i) v))

(define (symtab-lookup cp i)
  (vector-ref (cport-symtab cp) i))

(require unstable/markparam)
(define read-sym-mark (mark-parameter))
(define (read-sym cp i)
  (define ph (symtab-lookup cp i))
  ; We are reading this already, so return the placeholder
  (if (memq i (mark-parameter-all read-sym-mark))
      ph
      ; Otherwise, try to read it and return the real thing
      (let ([vv (placeholder-get ph)])
        (when (not-ready? vv)
          (let ([save-pos (cport-pos cp)])
            (set-cport-pos! cp (vector-ref (cport-shared-offsets cp) (sub1 i)))
            (mark-parameterize
             ([read-sym-mark i])
             (let ([v (read-compact cp)])
               (placeholder-set! ph v)))
            (set-cport-pos! cp save-pos)))
        (placeholder-get ph))))

(define (read-prefix port)
  ;; skip the "#~"
  (unless (equal? #"#~" (read-bytes 2 port))
    (error 'zo-parse "not a bytecode stream"))

  (define version (read-bytes (min 63 (read-byte port)) port))

  (read-char port))

;; path -> bytes
;; implementes read.c:read_compiled
(define (zo-parse [port (current-input-port)])
  (define init-pos (file-position port))

  (define mode (read-prefix port))

  (case mode
    [(#\T) (zo-parse-top port)]
    [(#\D)
     (struct mod-info (name start len))
     (define mod-infos
       (sort
        (for/list ([i (in-range (read-simple-number port))])
          (define size (read-simple-number port))
          (define name (read-bytes size port))
          (define start (read-simple-number port))
          (define len (read-simple-number port))
          (define left (read-simple-number port))
          (define right (read-simple-number port))
          (define name-p (open-input-bytes name))
          (mod-info (let loop ()
                      (define c (read-byte name-p))
                      (if (eof-object? c)
                          null
                          (cons (string->symbol
                                 (bytes->string/utf-8 (read-bytes (if (= c 255)
                                                                      (read-simple-number port)
                                                                      c)
                                                                  name-p)))
                                (loop))))
                    start
                    len))
        <
        #:key mod-info-start))
     (define tops
       (for/list ([mod-info (in-list mod-infos)])
         (define pos (file-position port))
         (unless (= (- pos init-pos) (mod-info-start mod-info))
           (error 'zo-parse 
                  "next module expected at ~a, currently at ~a"
                  (+ init-pos (mod-info-start mod-info)) pos))
         (unless (eq? (read-prefix port) #\T)
           (error 'zo-parse "expected a module"))
         (define top (zo-parse-top port #f))
         (define m (compilation-top-code top))
         (unless (mod? m)
           (error 'zo-parse "expected a module"))
         (unless (equal? (mod-info-name mod-info)
                         (if (symbol? (mod-name m))
                             '()
                             (cdr (mod-name m))))
           (error 'zo-parse "module name mismatch"))
         top))
     (define avail (for/hash ([mod-info (in-list mod-infos)]
                              [top (in-list tops)])
                     (values (mod-info-name mod-info) top)))
     (unless (hash-ref avail '() #f)
       (error 'zo-parse "no root module in directory"))
     (define-values (pre-subs post-subs seen)
       (for/fold ([pre-subs (hash)] [post-subs (hash)] [seen (hash)]) ([mod-info (in-list mod-infos)])
         (if (null? (mod-info-name mod-info))
             (values pre-subs post-subs (hash-set seen '() #t))
             (let ()
               (define name (mod-info-name mod-info))
               (define prefix (take name (sub1 (length name))))
               (unless (hash-ref avail prefix #f)
                 (error 'zo-parse "no parent module for ~s" name))
               (define (add subs)
                 (hash-set subs prefix (cons name (hash-ref subs prefix '()))))
               (define new-seen (hash-set seen name #t))
               (if (hash-ref seen prefix #f)
                   (values pre-subs (add post-subs) new-seen)
                   (values (add pre-subs) post-subs new-seen))))))
     (define (get-all prefix)
       (struct-copy mod 
                    (compilation-top-code (hash-ref avail prefix))
                    [pre-submodules (map get-all (reverse (hash-ref pre-subs prefix '())))]
                    [post-submodules (map get-all (reverse (hash-ref post-subs prefix '())))]))
     (struct-copy compilation-top (hash-ref avail '())
                  [code (get-all '())])]
    [else
     (error 'zo-parse "bad file format specifier")]))

(define (zo-parse-top [port (current-input-port)] [check-end? #t])

  ;; Skip module hash code
  (read-bytes 20 port)

  (define symtabsize (read-simple-number port))

  (define all-short (read-byte port))

  (define cnt (* (if (not (zero? all-short)) 2 4)
                 (sub1 symtabsize)))

  (define so (read-bytes cnt port))

  (define so* (list->vector (split-so all-short so)))

  (define shared-size (read-simple-number port))
  (define size* (read-simple-number port))

  (when (shared-size . >= . size*) 
    (error 'zo-parse "Non-shared data segment start is not after shared data segment (according to offsets)"))

  (define rst-start (file-position port))

  (file-position port (+ rst-start size*))
 
  (when check-end?
    (unless (eof-object? (read-byte port))
      (error 'zo-parse "File too big")))

  (define nr (make-not-ready))
  (define symtab
    (build-vector symtabsize (λ (i) (make-placeholder nr))))

  (define cp
    (make-cport 0 shared-size port size* rst-start symtab so*
                (make-vector symtabsize #f) (make-hash) (make-hash)))

  (for ([i (in-range 1 symtabsize)])
    (read-sym cp i))

  #;(printf "Parsed table:\n")
  #;(for ([(i v) (in-dict (cport-symtab cp))])
      (printf "~a = ~a\n" i (placeholder-get v)))
  (set-cport-pos! cp shared-size)
  (make-reader-graph (read-marshalled 'compilation-top-type cp)))

;; ----------------------------------------

#;
(begin
  (define (compile/write sexp)
    (define s (open-output-bytes))
    (write (parameterize ([current-namespace (make-base-namespace)])
             (eval '(require (for-syntax scheme/base)))
             (compile sexp))
           s)
    (get-output-bytes s))

  (define (compile/parse sexp)
    (let* ([bs (compile/write sexp)]
           [p (open-input-bytes bs)])
      (zo-parse p)))

  #;(compile/parse #s(foo 10 13))
  (zo-parse (open-input-file "/home/mflatt/proj/plt/collects/scheme/private/compiled/more-scheme_ss.zo"))
  )
