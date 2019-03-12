#lang racket/base
(require racket/function
         racket/match
         racket/list
         racket/struct
         compiler/zo-structs
         racket/dict
         racket/set
         racket/fasl)

(provide zo-parse)
(provide (all-from-out compiler/zo-structs))

;; ----------------------------------------
;; Bytecode unmarshalers for various forms

(define (read-toplevel flags pos depth)
  (define SCHEME_TOPLEVEL_CONST #x02)
  (define SCHEME_TOPLEVEL_READY #x01)
  ;; In the VM, the two flag bits are actually interpreted
  ;; as a number when the toplevel is a reference, but we
  ;; interpret the bits as flags here for backward compatibility.
  (make-toplevel depth pos 
                 (positive? (bitwise-and flags SCHEME_TOPLEVEL_CONST))
                 (positive? (bitwise-and flags SCHEME_TOPLEVEL_READY))))

(define (read-unclosed-procedure flags maybe-closure-size num-params max-let-depth
                                 name body closed-over tl-map)
  (define CLOS_HAS_REST 1)
  (define CLOS_HAS_REF_ARGS 2)
  (define CLOS_PRESERVES_MARKS 4)
  (define CLOS_NEED_REST_CLEAR 8)
  (define CLOS_IS_METHOD 16)
  (define CLOS_SINGLE_RESULT 32)
  (define BITS_PER_MZSHORT 32)
  (define BITS_PER_ARG 4)
  (let ([rest? (positive? (bitwise-and flags CLOS_HAS_REST))])
    (let*-values ([(closure-size)
                   (if (zero? (bitwise-and flags CLOS_HAS_REF_ARGS))
                       (vector-length closed-over)
                       maybe-closure-size)]
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
                body))))

(define (read-define-values v)
  (make-def-values
   (cdr (vector->list v))
   (vector-ref v 0)))

(define (in-list* l n)
  (make-do-sequence
   (lambda ()
     (values (lambda (l) (apply values (take l n)))
             (lambda (l) (drop l n))
             l
             (lambda (l) (>= (length l) n))
             (lambda _ #t)
             (lambda _ #t)))))
      
(define (read-linklet v)
  (match v
    [`(,name ,need-instance-access? ,max-let-depth ,num-lifts ,num-exports
       ,body
       ,source-names ,defns-vec ,imports-vec ,shapes-vec)
     (define defns (vector->list defns-vec))
     (linkl name
            (map vector->list (vector->list imports-vec))
            (if (not shapes-vec)
                (for/list ([imports (in-vector imports-vec)])
                  (for/list ([i (in-vector imports)])
                    #f))
                (let ([pos 0])
                  (for/list ([imports (in-vector imports-vec)])
                    (for/list ([i (in-vector imports)])
                      (begin0
                       (parse-shape (vector-ref shapes-vec pos))
                       (set! pos (add1 pos)))))))
            (take defns num-exports)
            (take (list-tail defns num-exports) (- (length defns) num-exports num-lifts))
            (drop defns (- (length defns) num-lifts))
            (for/hasheq ([i (in-range 0 (vector-length source-names) 2)])
              (values (vector-ref source-names i)
                      (vector-ref source-names (add1 i))))
            (vector->list body)
            max-let-depth
            need-instance-access?)]))

(define (parse-shape shape)
  (cond
   [(not shape) #f]
   [(eq? shape #t) 'constant]
   [(eq? shape (void)) 'fixed]
   [(number? shape) 
    (define n (arithmetic-shift shape -1))
    (make-function-shape (if (negative? n)
                             (make-arity-at-least (sub1 (- n)))
                             n)
                         (odd? shape))]
   [(and (symbol? shape)
         (regexp-match? #rx"^struct" (symbol->string shape)))
    (define n (string->number (substring (symbol->string shape) 6)))
    (define (authentic-shape? n) (bitwise-bit-set? n 4))
    (define (shape-count-shift n) (arithmetic-shift n -5))
    (case (bitwise-and n #x7)
      [(0) (make-struct-type-shape (shape-count-shift n) (authentic-shape? n))]
      [(1) (make-constructor-shape (shape-count-shift n))]
      [(2) (make-predicate-shape (authentic-shape? n))]
      [(3) (make-accessor-shape (shape-count-shift n) (authentic-shape? n))]
      [(4) (make-mutator-shape (shape-count-shift n) (authentic-shape? n))]
      [else (make-struct-other-shape)])]
   [(and (symbol? shape)
         (regexp-match? #rx"^prop" (symbol->string shape)))
    (define n (string->number (substring (symbol->string shape) 4)))
    (case n
      [(0 1) (make-struct-type-property-shape (= n 1))]
      [(2) (make-property-predicate-shape)]
      [else (make-property-accessor-shape)])]
   [else
    ;; parse symbol as ":"-separated sequence of arities
    (make-function-shape
     (for/list ([s (regexp-split #rx":" (symbol->string shape))])
       (define i (string->number s))
       (if (negative? i)
           (make-arity-at-least (sub1 (- i)))
           i))
     #f)]))

;; ----------------------------------------
;; Unmarshal dispatch for various types

;; Type mappings from "stypes.h":
(define (int->type i)
  (case i
    [(0) 'toplevel-type]
    [(1) 'static-toplevel-type]
    [(7) 'sequence-type]
    [(9) 'unclosed-procedure-type]
    [(10) 'let-value-type]
    [(11) 'let-void-type]
    [(12) 'letrec-type]
    [(14) 'with-cont-mark-type]
    [(15) 'define-values-type]
    [(16) 'set-bang-type]
    [(17) 'boxenv-type]
    [(18) 'begin0-sequence-type]
    [(19) 'varref-form-type]
    [(20) 'apply-values-type]
    [(21) 'with-immed-mark-type]
    [(22) 'case-lambda-sequence-type]
    [(23) 'inline-variant-type]
    [(25) 'linklet-type]
    [(88) 'prefix-type]
    [else (error 'int->type "unknown type: ~e" i)]))

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

(define-struct cport ([pos #:mutable] shared-start orig-port size bytes-start symtab shared-offsets))
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
    [18 let-one-typed]
    [19 linklet]
    [20 quote]
    [21 reference]
    [22 local]
    [23 local-unbox]
    [24 svector]
    [25 application]
    [26 let-one]
    [27 branch]
    [28 path]
    [29 closure]
    [30 delayed]
    [31 prefab]
    [32 let-one-unused]
    [33 shared]
    [34 toplevel]
    [35 begin]
    [36 begin0]
    [37 let-value]
    [38 let-void]
    [39 letrec]
    [40 wcm]
    [41 define-values]
    [42 set-bang]
    [43 varref]
    [44 apply-values]
    [45 other-form]
    [46 srcloc]
    [47 74 small-number]
    [74 92 small-symbol]
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
        [(srcloc)
         (srcloc (read-compact cp)
                 (read-compact cp)
                 (read-compact cp)
                 (read-compact cp)
                 (read-compact cp))]
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
        [(linklet)
         (unless (zero? (read-compact-number cp))
           ;; read and ignore the static-prefix placeholder
           (read-compact cp))
         (read-linklet (read-compact cp))]
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
        [(shared)
         (let ([pos (read-compact-number cp)])
           (read-cyclic cp pos 'shared))]
        [(toplevel)
         (read-toplevel (read-compact-number cp) (read-compact-number cp) (read-compact-number cp))]
        [(begin begin0)
         (define len  (read-compact-number cp))
         (define l (for/list ([i (in-range len)]) (read-compact cp)))
         (if (eq? cpt-tag 'begin)
             (make-seq l)
             (make-beg0 l))]
        [(let-value)
         (define count (read-compact-number cp))
         (define pos (read-compact-number cp))
         (define boxes? (not (zero? (read-compact-number cp))))
         (define rhs (read-compact cp))
         (define body (read-compact cp))
         (make-install-value count pos boxes? rhs body)]
        [(let-void)
         (define count (read-compact-number cp))
         (define boxes? (not (zero? (read-compact-number cp))))
         (define body (read-compact cp))
         (make-let-void count boxes? body)]
        [(letrec)
         (define len  (read-compact-number cp))
         (define procs (for/list ([i (in-range len)]) (read-compact cp)))
         (define body (read-compact cp))
         (make-let-rec procs body)]
        [(wcm)
         (make-with-cont-mark (read-compact cp) (read-compact cp) (read-compact cp))]
        [(define-values)
         (define v (read-compact cp))
         (make-def-values
          (cdr (vector->list v))
          (vector-ref v 0))]
        [(set-bang)
         (define undef-ok? (not (zero? (read-compact-number cp))))
         (make-assign (read-compact cp) (read-compact cp) undef-ok?)]
        [(varref)
         (define flags (read-compact-number cp))
         (make-varref (read-compact cp) (read-compact cp)
                      (bitwise-bit-set? flags 1)
                      (bitwise-bit-set? flags 2))]
        [(apply-values)
         (make-apply-values (read-compact cp) (read-compact cp))]
        [(other-form)
         (define type (read-compact-number cp))
         (case (int->type type)
           [(static-toplevel-type)
            (begin0
              (read-toplevel (read-compact-number cp) (read-compact-number cp) 0)
              ;; read and discard the prefix identity:
              (read-compact cp))]
           [(prefix-type)
            (read-compact-number cp)]
           [(boxenv-type)
            (make-boxenv (read-compact cp) (read-compact cp))]
           [(with-immed-mark-type)
            (make-with-immed-mark (read-compact cp) (read-compact cp) (read-compact cp))]
           [(inline-variant-type)
            (make-inline-variant (read-compact cp) (read-compact cp))]
           [(case-lambda-sequence-type)
            (define count (read-compact-number cp))
            (define name (read-compact cp))
            (define l (for/list ([i (in-range count)]) (read-compact cp)))
            (make-case-lam name l)]
           [(unclosed-procedure-type)
            (define flags (read-compact-number cp))
            (define CLOS_HAS_TYPED_ARGS 2)
            (define maybe-closure-size (if (positive? (bitwise-and flags CLOS_HAS_TYPED_ARGS))
                                           (read-compact-number cp)
                                           -1))
            (define num-params (read-compact-number cp))
            (define max-let-depth (read-compact-number cp))
            (define name (read-compact cp))
            (define body (read-compact cp))
            (define closure-map (read-compact cp))
            (define tl-map (read-compact cp))
            (read-unclosed-procedure flags maybe-closure-size num-params max-let-depth
                                     name body closure-map tl-map)]
           [else
            (error 'read-compact "unknown other-form type ~a" type)])]
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

(define (read-prefix port can-be-false?)
  ;; skip the "#~"
  (define tag (read-bytes 2 port))
  (unless (or (equal? #"#~" tag)
              (and can-be-false? (equal? #"#f" tag)))
    (error 'zo-parse "not a bytecode stream"))

  (cond
   [(equal? #"#f" tag) (values #f #f)]
   [else
    (define version (read-bytes (min 63 (read-byte port)) port))
    (define vm (read-bytes (min 63 (read-byte port)) port))
    (values vm (read-char port))]))

;; path -> bytes
;; implementes read.c:read_compiled
(define (zo-parse [port (current-input-port)])
  (define init-pos (file-position port))

  (define-values (vm mode) (read-prefix port #f))

  (case mode
    [(#\B) (linkl-bundle (hash-set (zo-parse-top port vm) 'vm vm))]
    [(#\D)
     (struct sub-info (name start len))
     (define sub-infos
       (sort
        (for/list ([i (in-range (read-simple-number port))])
          (define size (read-simple-number port))
          (define name (read-bytes size port))
          (define start (read-simple-number port))
          (define len (read-simple-number port))
          (define left (read-simple-number port))
          (define right (read-simple-number port))
          (define name-p (open-input-bytes name))
          (sub-info (let loop ()
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
        #:key sub-info-start))
     (define (remove-empty-root ht)
       ;; A linklet for top-level forms will have '() mapped to #f
       (if (hash-ref ht '() #f)
           ht
           (hash-remove ht '())))
     (linkl-directory
      (remove-empty-root
       (for/hash ([sub-info (in-list sub-infos)])
         (define pos (file-position port))
         (unless (= (- pos init-pos) (sub-info-start sub-info))
           (error 'zo-parse 
                  "next bundle expected at ~a, currently at ~a"
                  (+ init-pos (sub-info-start sub-info)) pos))
         (define-values (vm tag) (read-prefix port #t))
         (define sub
           (cond
             [(not tag) #f]
             [else
              (unless (eq? tag #\B)
                (error 'zo-parse "expected a bundle"))
              (define sub (zo-parse-top port vm #f))
              (unless (hash? sub)
                (error 'zo-parse "expected a bundle hash"))
              (linkl-bundle (hash-set sub 'vm vm))]))
         (values (sub-info-name sub-info) sub))))]
    [else
     (error 'zo-parse "bad file format specifier")]))

(define (zo-parse-top port vm [check-end? #t])

  ;; Skip module hash code
  (read-bytes 20 port)

  (define (check-end)
    (when check-end?
      (unless (eof-object? (read-byte port))
        (error 'zo-parse "file too big"))))

  (cond
    [(equal? vm #"linklet")
     (define s (fasl->s-exp port))
     (check-end)
     s]
    [(equal? vm #"racket")
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
     
     (check-end)

     (define symtab (make-vector symtabsize (not-ready)))

     (define cp
       (make-cport 0 shared-size port size* rst-start symtab so*))

     (for ([i (in-range 1 symtabsize)])
       (read-symref cp i #f 'table))

     #;(printf "Parsed table:\n")
     #;(for ([(i v) (in-dict (cport-symtab cp))])
         (printf "~a = ~a\n" i (placeholder-get v)))
     (set-cport-pos! cp shared-size)
     
     (make-reader-graph (read-compact cp))]
    [(equal? vm #"chez-scheme")
     (hash
      'opaque
      (read-bytes (read-simple-number port) port))]
    [else
     (error 'zo-parse "cannot parse for virtual machine: ~s" vm)]))

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
