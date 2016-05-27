#lang racket/base
(require racket/function
         racket/match
         racket/list
         racket/struct
         compiler/zo-structs
         racket/dict
         racket/set)

(provide zo-parse
         decode-module-binding)
(provide (all-from-out compiler/zo-structs))

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
    [`(,ld ,binding-namess ,prefix . ,code)
     (unless (prefix? prefix)
       (error 'bad "not prefix ~a" prefix))
     (make-compilation-top ld
                           (binding-namess-list->hash binding-namess)
                           prefix
                           code)]))

(define (binding-namess-list->hash binding-namess)
  (for/hash ([e (in-list binding-namess)])
    (values (car e)
            (let ([vec (cdr e)])
              (for/hash ([i (in-range 0 (vector-length vec) 2)])
                (values (vector-ref vec i)
                        (vector-ref vec (add1 i))))))))

(define (read-resolve-prefix v)
  (match v
    [`(,src-insp-desc ,i ,tv . ,sv)
     ;; XXX Why not leave them as vectors and change the contract?
     (make-prefix i (vector->list tv) (vector->list sv) src-insp-desc)]))

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
(define (read-with-immed-mark v)
  (make-with-immed-mark (vector-ref v 0) (vector-ref v 1) (vector-ref v 2)))
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
       ,name ,srcname ,self-modidx
       ,rt-binding-names ,et-binding-names ,other-binding-names
       ,cross-phase?
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
                    (assemble-binding-names rt-binding-names
                                            et-binding-names
                                            other-binding-names)
                    (if cross-phase? '(cross-phase) '())
                    (map read-module pre-submods)
                    (map read-module post-submods))]))]))
(define (read-module-wrap v)
  v)


(define (read-inline-variant v)
  (make-inline-variant (car v) (cdr v)))

(define (assemble-binding-names rt-binding-names
                                et-binding-names
                                other-binding-names)
  (define (vector-to-ht vec)
    (define sz (vector-length vec))
    (let loop ([i 0] [ht #hasheq()])
      (cond
       [(= i sz) ht]
       [else (loop (+ i 2)
                   (hash-set ht (vector-ref vec i) (vector-ref vec (add1 i))))])))
  (for/hash ([(phase vec) (let* ([ht (if other-binding-names
                                         (vector-to-ht other-binding-names)
                                         #hash())]
                                 [ht (if rt-binding-names
                                         (hash-set ht 0 rt-binding-names)
                                         ht)]
                                 [ht (if et-binding-names
                                         (hash-set ht 0 et-binding-names)
                                         ht)])
                            ht)])
    (values phase (vector-to-ht vec))))

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
    [(25) 'with-immed-mark-type]
    [(26) 'case-lambda-sequence-type]
    [(27) 'module-type]
    [(28) 'inline-variant-type]
    [(37) 'variable-type]
    [(38) 'module-variable-type]
    [(122) 'resolve-prefix-type]
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
    (cons 'define-values-type read-define-values)
    (cons 'define-syntaxes-type read-define-syntax)
    (cons 'begin-for-syntax-type read-begin-for-syntax)
    (cons 'set-bang-type read-set!)
    (cons 'boxenv-type read-boxenv)
    (cons 'require-form-type read-require)
    (cons 'varref-form-type read-#%variable-ref)
    (cons 'apply-values-type read-apply-values)
    (cons 'with-immed-mark-type read-with-immed-mark)
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
    [36 scope]
    [37 root-scope]
    [38 shared]
    [39 62 small-number]
    [62 80 small-symbol]
    [80 92 small-marshalled]
    [92 ,(+ 92 small-list-max) small-proper-list]
    [,(+ 92 small-list-max) 192 small-list]
    [192 207 small-local]
    [207 222 small-local-unbox]
    [222 247 small-svector]
    [248 small-application2]
    [249 small-application3]
    [247 255 small-application]))

(define root-scope (scope 'root 'module null null #f))

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
(define-struct in-progress ())

;; ----------------------------------------
;; Syntax unmarshaling
(define (make-memo) (make-weak-hash))
(define (with-memo* mt arg thnk)
  (hash-ref! mt arg thnk))
(define-syntax-rule (with-memo mt arg body ...)
  (with-memo* mt arg (λ () body ...)))

;; placeholder for a `scope` decoded in a second pass:
(struct encoded-scope (relative-id content) #:prefab)

(define (decode-wrapped cp v)
  (let loop ([v v])
    (let-values ([(tamper-status v encoded-wraps esrcloc)
                  (match v
                    [`#(,datum ,wraps 1) (values 'tainted datum wraps #f)]
                    [`#(,datum ,wraps 2) (values 'armed datum wraps #f)]
                    [`#(,datum ,wraps ,esrcloc 1) (values 'tainted datum wraps esrcloc)]
                    [`#(,datum ,wraps ,esrcloc 2) (values 'armed datum wraps esrcloc)]
                    [`#(,datum ,wraps ,esrcloc) (values 'clean datum wraps esrcloc)]
                    [`(,datum . ,wraps) (values 'clean datum wraps #f)]
                    [else (error 'decode-wraps "bad datum+wrap: ~.s" v)])])
      (let* ([wrapped-memo (make-memo)]
             [add-wrap (lambda (v) (with-memo wrapped-memo v (make-stx-obj v encoded-wraps esrcloc #hasheq() tamper-status)))])
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
         [else (add-wrap v)])))))

(define (in-vector* v n)
  (make-do-sequence
   (λ ()
     (values (λ (i) (vector->values v i (+ i n)))
             (λ (i) (+ i n))
             0
             (λ (i) (>= (vector-length v) (+ i n)))
             (λ _ #t)
             (λ _ #t)))))

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
           (read-symref cp pos #t 'delayed))]
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
         (let ([len (read-compact-number cp)])
           (if (zero? len)
               ;; Read a list of byte strings as relative path elements:
               (let ([p (or (current-load-relative-directory)
                            (current-directory))])
                 (for/fold ([p p]) ([e (in-list (read-compact cp))])
                   (build-path p (if (bytes? e) (bytes->path-element e) e))))
               ;; Read a path:
               (bytes->path (read-compact-bytes cp len))))]
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
         (let ([v (read-compact cp)])
           (make-stx (decode-wrapped cp v)))]
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
           (read-symref cp l #t 'symref))]
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
         (define pos (read-compact-number cp))
         (define ph (make-placeholder 'closure))
         (symtab-write! cp pos ph)
         (define v (read-compact cp))
         (define r
           (make-closure
            v
            (gensym
             (let ([s (lam-name v)])
               (cond
                 [(symbol? s) s]
                 [(vector? s) (vector-ref s 0)]
                 [else 'closure])))))
         (placeholder-set! ph r)
         r]
        [(svector)
         (read-compact-svector cp (read-compact-number cp))]
        [(small-svector)
         (read-compact-svector cp (- ch cpt-start))]
        [(scope)
         (let ([pos (read-compact-number cp)]
               [relative-id (read-compact-number cp)])
           (if (zero? pos)
               (encoded-scope relative-id (read-compact cp))
               (read-cyclic cp pos 'scope (lambda (v)
                                            (encoded-scope relative-id 
                                                           v)))))]
        [(root-scope)
         root-scope]
        [(shared)
         (let ([pos (read-compact-number cp)])
           (read-cyclic cp pos 'shared))]
        [else (error 'read-compact "unknown tag ~a" cpt-tag)]))
    (cond
      [(zero? need-car) v]
      [(and proper (= need-car 1))
       (cons v null)]
      [else
       (cons v (loop (sub1 need-car) proper))])))

(define (symtab-write! cp i v)
  (vector-set! (cport-symtab cp) i v))

(define (symtab-lookup cp i)
  (vector-ref (cport-symtab cp) i))

(define (read-cyclic cp i who [wrap values])
  (define ph (make-placeholder (not-ready)))
  (symtab-write! cp i ph)
  (define r (wrap (read-compact cp)))
  (when (eq? r ph) (error who "unresolvable cyclic data"))
  (placeholder-set! ph r)
  ph)

(define (read-symref cp i mark-in-progress? who)
  (define v (symtab-lookup cp i))
  (cond
   [(not-ready? v)
    (when mark-in-progress?
      (symtab-write! cp i (in-progress)))
    (define save-pos (cport-pos cp))
    (set-cport-pos! cp (vector-ref (cport-shared-offsets cp) (sub1 i)))
    (define v (read-compact cp))
    (symtab-write! cp i v)
    (set-cport-pos! cp save-pos)
    v]
   [(in-progress? v)
    (error who "unexpected cycle in input")]
   [else v]))

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

  (define symtab (make-vector symtabsize (not-ready)))

  (define cp
    (make-cport 0 shared-size port size* rst-start symtab so*
                (make-vector symtabsize (not-ready)) (make-hash) (make-hash)))

  (for ([i (in-range 1 symtabsize)])
    (read-symref cp i #f 'table))

  #;(printf "Parsed table:\n")
  #;(for ([(i v) (in-dict (cport-symtab cp))])
      (printf "~a = ~a\n" i (placeholder-get v)))
  (set-cport-pos! cp shared-size)
  
  (define decoded-except-for-stx
    (make-reader-graph (read-marshalled 'compilation-top-type cp)))
  
  (decode-stxes decoded-except-for-stx))

;; ----------------------------------------

(define (decode-stxes v)
  ;; Walk `v` to find `stx-obj` instances and decode the `wrap` field.
  ;; We do this after building a graph from the input, and `decode-wrap`
  ;; preserves graph structure.
  (define decode-ht (make-hasheq))
  (define srcloc-ht (make-hasheq))
  (let walk ([p v])
    (match p
      [(compilation-top _ binding-namess pfx c)
       (struct-copy compilation-top p
                    [binding-namess (walk binding-namess)]
                    [prefix (walk pfx)]
                    [code (walk c)])]
      [(prefix _ _ s _)
       (struct-copy prefix p [stxs (map walk s)])]
      [(req rs _)
       (struct-copy req p
                    [reqs (walk rs)])]
      [(? mod?)
       (struct-copy mod p
                    [prefix (walk (mod-prefix p))]
                    [syntax-bodies
                     (for/list ([e (in-list (mod-syntax-bodies p))])
                       (cons (car e)
                             (map walk (cdr e))))]
                    [internal-context
                     (walk (mod-internal-context p))]
                    [binding-names
                     (for/hash ([(p ht) (in-hash (mod-binding-names p))])
                       (values p
                               (for/hash ([(k v) (in-hash ht)])
                                 (values k (walk v)))))]
                    [pre-submodules
                     (map walk (mod-pre-submodules p))]
                    [post-submodules
                     (map walk (mod-post-submodules p))])]
      [(stx c)
       (struct-copy stx p [content (walk c)])]
      [(def-syntaxes _ _ pfx _ _)
       (struct-copy def-syntaxes p
                    [prefix (walk pfx)])]
      [(seq-for-syntax _ pfx _ _)
       (struct-copy seq-for-syntax p
                    [prefix (walk pfx)])]
      [(stx-obj d w esrcloc _ _)
       (define-values (srcloc props) (decode-srcloc+props esrcloc srcloc-ht))
       (struct-copy stx-obj p
                    [datum (walk d)]
                    [wrap (decode-wrap w decode-ht)]
                    [srcloc srcloc]
                    [props props])]
      [(? zo?) p]
      ;; Generic constructors happen inside the `datum` of `stx-obj`,
      ;; for example (with no cycles):
      [(cons a d)
       (cons (walk a) (walk d))]
      [(? vector?)
       (vector->immutable-vector
        (for/vector #:length (vector-length p) ([e (in-vector p)])
                    (walk e)))]
      [(box v)
       (box-immutable (walk v))]
      [(? prefab-struct-key)
       (apply make-prefab-struct
              (prefab-struct-key p)
              (cdr (for/list ([e (in-vector (struct->vector p))])
                     (walk e))))]
      [(? hash?)
       (cond
        [(hash-eq? p)
         (for/hasheq ([(k v) (in-hash p)])
           (values k (walk v)))]
        [(hash-eqv? p)
         (for/hasheqv ([(k v) (in-hash p)])
           (values k (walk v)))]
        [else
         (for/hash ([(k v) (in-hash p)])
           (values k (walk v)))])]
      [_ p])))

;; ----------------------------------------

(define (decode-srcloc+props esrcloc ht)
  (define (norm v) (if (v . < . 0) #f v))
  (define p
    (hash-ref! ht
               esrcloc
               (lambda ()
                 (cons (and esrcloc
                            ;; We could reduce this srcloc to #f if
                            ;; there's no source, line, column, or position
                            ;; information, but we want to expose the actual
                            ;; content of a bytecode stream:
                            (srcloc (vector-ref esrcloc 0)
                                    (norm (vector-ref esrcloc 1))
                                    (norm (vector-ref esrcloc 2))
                                    (norm (vector-ref esrcloc 3))
                                    (norm (vector-ref esrcloc 4))))
                       (let ([props
                              (if (and esrcloc ((vector-length esrcloc) . > . 5))
                                  (case (vector-ref esrcloc 5)
                                    [(#\[) #hasheq((paren-shape . #\[))]
                                    [(#\{) #hasheq((paren-shape . #\{))]
                                    [else #hasheq()])
                                  #hasheq())])
                         (if (and esrcloc ((vector-length esrcloc) . > . 6))
                             (for/fold ([props props]) ([p (in-list (vector-ref esrcloc 6))])
                               (hash-set props (car p) (cdr p)))
                             props))))))
  (values (car p) (cdr p)))

;; ----------------------------------------

(define (decode-wrap encoded-wrap ht)
  (hash-ref! ht
             encoded-wrap
             (lambda ()
               (match encoded-wrap
                 [(vector shifts simple-scopes multi-scopes)
                  (make-wrap (decode-map decode-shift shifts ht)
                             (decode-map decode-scope simple-scopes ht)
                             (decode-map decode-shifted-multi-scope multi-scopes ht))]
                 [_ (error 'decode-wrap "bad wrap")]))))

(define (decode-map decode-one l ht)
  (cond
   [(null? l) l]
   [(not (pair? l))
    (error 'decode-wrap "bad list")]
   [else (hash-ref! ht l
                    (lambda ()
                      (cons (decode-one (car l) ht)
                            (decode-map decode-one (cdr l) ht))))]))

(define (decode-shift s ht)
  (hash-ref! ht s
             (lambda ()
               (match s
                 [(vector to from)
                  (module-shift to from #f #f)]
                 [(vector to from i-to i-from)
                  (module-shift to from i-to i-from)]
                 [_ (error 'decode-wrap "bad shift")]))))

(define (decode-scope s ht)
  (or
   (and (eq? s root-scope)
        s)
   (hash-ref ht s
             (lambda ()
               (unless (encoded-scope? s)
                 (error 'decode-wrap "bad scope: ~e" s))
               (define v (encoded-scope-content s))
               (define kind
                 (match v
                   [(? number?) v]
                   [(cons (? number?) _)
                    (car v)]
                   [else (error 'decode-wrap "bad scope")]))
               (define sc (scope (encoded-scope-relative-id s)
                                 (case kind
                                   [(0 1) 'module]
                                   [(2) 'macro]
                                   [(3) 'local]
                                   [(4) 'intdef]
                                   [else 'use-site])
                                 null
                                 null
                                 #f))
               (hash-set! ht s sc)
               (unless (number? v)
                 (define-values (bulk-bindings end)
                   (let loop ([l (cdr v)] [bulk-bindings null])
                     (cond
                      [(pair? l)
                       (loop (cdr l) (cons (list (decode-scope-set (caar l) ht)
                                                 (decode-bulk-import (cdar l) ht))
                                           bulk-bindings))]
                      [else (values (reverse bulk-bindings) l)])))
                 (set-scope-bulk-bindings! sc bulk-bindings)
                 (unless (and (vector? end)
                              (even? (vector-length end)))
                   (error 'decode-wrap "bad scope"))
                 (define bindings
                   (let loop ([i 0])
                     (cond
                      [(= i (vector-length end)) null]
                      [else
                       (append (for/list ([p (in-list (vector-ref end (add1 i)))])
                                 (list (vector-ref end i)
                                       (decode-scope-set (car p) ht)
                                       (decode-binding (cdr p) ht)))
                               (loop (+ i 2)))])))
                 (set-scope-bindings! sc bindings))
               sc))))

(define (decode-scope-set l ht)
  (decode-map decode-scope l ht))

(define (decode-binding b ht)
  (hash-ref! ht b
             (lambda ()
               (match b
                 [(box (cons base-b (cons (cons sym wraps) phase)))
                  (free-id=?-binding
                   (decode-binding base-b ht)
                   (stx-obj sym (decode-wrap wraps ht) #f #hasheq() 'clean)
                   phase)]
                 [(? symbol?)
                  (local-binding b)]
                 [else
                  ;; Leave it encoded, so that the compactness (or not)
                  ;; of the encoding is visible; clients decode further
                  ;; with `decode-module-binding`
                  (module-binding b)]))))

(define (decode-module-binding b name)
  (define-values (insp-desc rest-b)
    (match b
      [(cons (? symbol?) _)
       (values (car b) (cdr b))]
      [else
       (values #f b)]))
  (define (decode-nominal-modidx-plus-phase n mod-phase)
    (match n
      [(? module-path-index?)
       (values n mod-phase 0)]
      [(cons nom-modix (cons import-phase nom-phase))
       (values nom-modix nom-phase import-phase)]
      [(cons nom-modix import-phase)
       (values nom-modix mod-phase import-phase)]
      [_
       (error 'decode-module-binding "bad encoding")]))
  (match rest-b
    [(and modidx (? module-path-index?))
     (decoded-module-binding modidx name 0
                             modidx name 0
                             0 insp-desc)]
    [(cons (and modidx (? module-path-index?))
           (and name (? symbol?)))
     (decoded-module-binding modidx name 0
                             modidx name 0
                             0 insp-desc)]
    [(cons (and modidx (? module-path-index?))
           (and nom-modidx (? module-path-index?)))
     (decoded-module-binding modidx name 0
                             nom-modidx name 0
                             0 insp-desc)]
    [(list* modidx (and name (? symbol?))
            nominal-modidx-plus-phase nom-name)
     (define-values (nom-modidx nom-phase import-phase)
       (decode-nominal-modidx-plus-phase nominal-modidx-plus-phase 0))
     (decoded-module-binding modidx name 0
                             nom-modidx nom-name nom-phase
                             import-phase insp-desc)]
    [(list* modidx mod-phase (and name (? symbol?))
            nominal-modidx-plus-phase nom-name)
     (define-values (nom-modidx nom-phase import-phase)
       (decode-nominal-modidx-plus-phase nominal-modidx-plus-phase mod-phase))
     (decoded-module-binding modidx name mod-phase
                             nom-modidx nom-name nom-phase
                             import-phase insp-desc)]
    [_ (error 'decode-module-binding "bad encoding")]))

(define (decode-bulk-import l ht)
  (hash-ref! ht l
             (lambda ()
               (match l
                 [(vector (and modidx (? module-path-index?))
                          src-phase
                          info
                          (and insp-desc (or #f (? symbol?))))
                  (define-values (phase prefix excepts)
                    (match info
                      [(or #f (? exact-integer?))
                       (values info #f '#())]
                      [(cons phase (and prefix (? symbol?)))
                       (values phase prefix '#())]
                      [(cons phase (cons excepts prefix))
                       (values phase prefix excepts)]
                      [(cons phase excepts)
                       (values phase #f excepts)]
                      [_ (error 'decode-wrap "bad bulk import info")]))
                  (all-from-module modidx
                                   phase
                                   src-phase
                                   insp-desc
                                   (if excepts
                                       (vector->list excepts)
                                       null)
                                   prefix)]
                 [_ (error 'decode-wrap "bad bulk import")]))))

(define (decode-shifted-multi-scope sms ht)
  (unless (pair? sms)
    (error 'decode-wrap "bad multi-scope pair"))
  (list (decode-multi-scope (car sms) ht)
        (cdr sms)))

(define (decode-multi-scope ms ht)
  (unless (and (vector? ms)
               (odd? (vector-length ms)))
    (error 'decode-wrap "bad multi scope"))
  (hash-ref ht ms
            (lambda ()
              (define multi (multi-scope (hash-count ht)
                                         (vector-ref ms (sub1 (vector-length ms)))
                                         null))
              (hash-set! ht ms multi)
              (define scopes
                (let loop ([i 0])
                  (cond
                   [(= (add1 i) (vector-length ms)) null]
                   [else
                    (define s (decode-scope (vector-ref ms (add1 i)) ht))
                    (when (scope-multi-owner s)
                      (error 'decode-wrap "bad scope owner: ~e while reading ~e"
                             (scope-multi-owner s)
                             multi))
                    (set-scope-multi-owner! s multi)
                    (cons (list (vector-ref ms i)
                                s)
                          (loop (+ i 2)))])))
              (set-multi-scope-scopes! multi scopes)
              multi)))

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
