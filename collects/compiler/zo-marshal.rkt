#lang racket/base
(require compiler/zo-structs
         unstable/struct
         racket/port
         racket/vector
         racket/match
         racket/contract
         racket/local
         racket/list
         racket/dict
         racket/function
         racket/pretty
         racket/path
         racket/set)

(provide/contract
 [zo-marshal (compilation-top? . -> . bytes?)]
 [zo-marshal-to (compilation-top? output-port? . -> . void?)])

(struct not-ready ())

(define (zo-marshal top)
  (define bs (open-output-bytes))
  (zo-marshal-to top bs)
  (get-output-bytes bs))

; ((obj -> (or pos #f)) output-port -> number) -> vector
; calculates what values show up in the compilation top more than once
; closures are always included even if they only show up once
(define (create-symbol-table out-compilation-top)
  (define encountered (make-hash))
  (define shared (make-hash))
  (define (encountered? v)
    (hash-ref encountered v #f))
  (define (encounter! v)
    (hash-set! encountered v #t) 
    #f)
  (define (shared-obj-pos v)
    (hash-ref shared v #f))
  (define (share! v)
    (or (shared-obj-pos v)
        (let ([pos (add1 (hash-count shared))])
          (hash-set! shared v pos) 
          pos)))
  
  (out-compilation-top 
   (λ (v)
     (if (or (closure? v)
             (encountered? v))
         (share! v)
         (encounter! v)))
   (open-output-nowhere))
  
  (define symbol-table (make-vector (hash-count shared) (not-ready)))
  (hash-map shared (λ (k v) (vector-set! symbol-table (sub1 v) k)))
  (values symbol-table shared-obj-pos))

(define (zo-marshal-to top outp) 
  
  ; XXX: wraps were encoded in traverse, now needs to be handled when writing
  (define wrapped (make-hash))
  
  ; (obj -> (or pos #f)) output-port -> number
  ; writes top to outp using shared-obj-pos to determine symref
  ; returns the file position at the end of the compilation top
  (define (out-compilation-top shared-obj-pos outp)
    (define ct
      (match top
        [(compilation-top max-let-depth prefix form)
         (list* max-let-depth prefix (protect-quote form))]))
    (out-anything ct (make-out outp shared-obj-pos wrapped))
    (file-position outp))
  
  (define-values (symbol-table shared-obj-pos) (create-symbol-table out-compilation-top))
  ; vector output-port -> (listof number) number
  ; writes symbol-table to outp
  ; returns the file positions of each value in the symbol table and the end of the symbol table
  (define (out-symbol-table symbol-table outp)
    (define (shared-obj-pos/modulo-v v)
      (define skip? #t)
      (λ (v2)
        (if (and skip? (eq? v v2) (not (closure? v2)))
            (begin
              (set! skip? #f)
              #f)
            (shared-obj-pos v2))))
    (values
     (for/list ([v (in-vector symbol-table)])
       (begin0
         (file-position outp)
         (out-anything v (make-out outp (shared-obj-pos/modulo-v v) wrapped))))
     (file-position outp)))
  
  ; Calculate file positions
  (define counting-port (open-output-nowhere))
  (define-values (offsets post-shared) (out-symbol-table symbol-table counting-port))
  (define all-forms-length (out-compilation-top shared-obj-pos counting-port))
  ; Write the compiled form header
  (write-bytes #"#~" outp)
  
  ; Write the version (notice that it isn't the same as out-string)
  (define version-bs (string->bytes/latin-1 (version)))
  (write-bytes (bytes (bytes-length version-bs)) outp)
  (write-bytes version-bs outp)

  
  ; Write the symbol table information (size, offsets)
  (define symtabsize (add1 (vector-length symbol-table)))
  (write-bytes (int->bytes symtabsize) outp)
  (define all-short? (post-shared . < . #xFFFF))
  (write-bytes (bytes (if all-short? 1 0)) outp)
  (for ([o (in-list offsets)])
    (write-bytes (integer->integer-bytes o (if all-short? 2 4) #f #f) outp))
  ; Post-shared is where the ctop actually starts
  (write-bytes (int->bytes post-shared) outp)
  ; This is where the file should end
  (write-bytes (int->bytes all-forms-length) outp)
  
  ; Actually write the zo
  (out-symbol-table symbol-table outp)
  (out-compilation-top shared-obj-pos outp)
  (void))

;; ----------------------------------------

(define toplevel-type-num 0)
(define syntax-type-num 3)
(define sequence-type-num 7)
(define unclosed-procedure-type-num 9)
(define let-value-type-num 10)
(define let-void-type-num 11)
(define letrec-type-num 12)
(define wcm-type-num 14)
(define quote-syntax-type-num 15)
(define variable-type-num 24)
(define top-type-num 89)
(define case-lambda-sequence-type-num 99)
(define begin0-sequence-type-num 100)
(define module-type-num 103)
(define prefix-type-num 105)
(define free-id-info-type-num 154)

(define-syntax define-enum
  (syntax-rules ()
    [(_ n) (begin)]
    [(_ n id . rest)
     (begin
       (define id n)
       (define-enum (add1 n) . rest))])) 

(define-enum
  0
  CPT_ESCAPE
  CPT_SYMBOL
  CPT_SYMREF
  CPT_WEIRD_SYMBOL
  CPT_KEYWORD
  CPT_BYTE_STRING
  CPT_CHAR_STRING
  CPT_CHAR
  CPT_INT
  CPT_NULL
  CPT_TRUE
  CPT_FALSE
  CPT_VOID
  CPT_BOX
  CPT_PAIR
  CPT_LIST
  CPT_VECTOR
  CPT_HASH_TABLE
  CPT_STX
  CPT_LET_ONE_FLONUM
  CPT_MARSHALLED
  CPT_QUOTE
  CPT_REFERENCE
  CPT_LOCAL
  CPT_LOCAL_UNBOX
  CPT_SVECTOR
  CPT_APPLICATION
  CPT_LET_ONE
  CPT_BRANCH
  CPT_MODULE_INDEX
  CPT_MODULE_VAR
  CPT_PATH
  CPT_CLOSURE
  CPT_DELAY_REF ; XXX unused, but appears to be same as CPT_SYMREF
  CPT_PREFAB
  CPT_LET_ONE_UNUSED)

(define-enum
  0
  DEFINE_VALUES_EXPD
  DEFINE_SYNTAX_EXPD
  SET_EXPD
  CASE_LAMBDA_EXPD
  BEGIN0_EXPD
  BOXENV_EXPD
  MODULE_EXPD
  REQUIRE_EXPD
  DEFINE_FOR_SYNTAX_EXPD
  REF_EXPD
  APPVALS_EXPD
  SPLICE_EXPD)

(define CPT_SMALL_NUMBER_START 36)
(define CPT_SMALL_NUMBER_END 60)

(define CPT_SMALL_SYMBOL_START 60)
(define CPT_SMALL_SYMBOL_END 80)

(define CPT_SMALL_MARSHALLED_START 80)
(define CPT_SMALL_MARSHALLED_END 92)

(define CPT_SMALL_LIST_MAX 65)
(define CPT_SMALL_PROPER_LIST_START 92)
(define CPT_SMALL_PROPER_LIST_END (+ CPT_SMALL_PROPER_LIST_START CPT_SMALL_LIST_MAX))

(define CPT_SMALL_LIST_START CPT_SMALL_PROPER_LIST_END)
(define CPT_SMALL_LIST_END 192)

(define CPT_SMALL_LOCAL_START 192)
(define CPT_SMALL_LOCAL_END 207)
(define CPT_SMALL_LOCAL_UNBOX_START 207)
(define CPT_SMALL_LOCAL_UNBOX_END 222)

(define CPT_SMALL_SVECTOR_START 222)
(define CPT_SMALL_SVECTOR_END 247)

(define CPT_SMALL_APPLICATION_START 247)
(define CPT_SMALL_APPLICATION_END 255)

(define CLOS_HAS_REST 1)
(define CLOS_HAS_REF_ARGS 2)
(define CLOS_PRESERVES_MARKS 4)
(define CLOS_IS_METHOD 16)
(define CLOS_SINGLE_RESULT 32)

(define BITS_PER_MZSHORT 32)

(define *dummy* #f)

(define (int->bytes x)
  (integer->integer-bytes x
                          4
                          #f
                          #f))

(define-struct case-seq (name lams))
(define-struct (seq0 seq) ())


(define (encode-module-bindings module-bindings)
  (define encode-nominal-path
    (match-lambda
      [(struct simple-nominal-path (value))
       value]
      [(struct imported-nominal-path (value import-phase))
       (cons value import-phase)]
      [(struct phased-nominal-path (value import-phase phase))
       (cons value (cons import-phase phase))]))
  (define encoded-bindings (make-vector (* (length module-bindings) 2)))
  (for ([i (in-naturals)]
        [(k v) (in-dict module-bindings)])
    (vector-set! encoded-bindings (* i 2) k)
    (vector-set! encoded-bindings (add1 (* i 2))
                 (match v
                   [(struct simple-module-binding (path))
                    path]
                   [(struct exported-module-binding (path export-name))
                    (cons path export-name)]
                   [(struct nominal-module-binding (path nominal-path))
                    (cons path (encode-nominal-path nominal-path))]
                   [(struct exported-nominal-module-binding (path export-name nominal-path nominal-export-name))
                    (list* path export-name (encode-nominal-path nominal-path) nominal-export-name)]
                   [(struct phased-module-binding (path phase export-name nominal-path nominal-export-name))
                    (list* path phase export-name (encode-nominal-path nominal-path) nominal-export-name)])))
  encoded-bindings)

(define (encode-all-from-module afm)
  (match afm
    [(struct all-from-module (path phase src-phase #f #f))
     (list* path phase src-phase)]
    [(struct all-from-module (path phase src-phase exns #f))
     (list* path phase exns src-phase)]
    [(struct all-from-module (path phase src-phase exns (vector prefix)))
     (list* path phase src-phase exns prefix)]))

(define (encode-wraps wraps)
  (for/list ([wrap (in-list wraps)])
    (match wrap
      [(struct phase-shift (amt src dest))
       (box (vector amt src dest #f))]
      [(struct module-rename (phase kind set-id unmarshals renames mark-renames plus-kern?))
       (define encoded-kind (eq? kind 'marked))
       (define encoded-unmarshals (map encode-all-from-module unmarshals))
       (define encoded-renames (encode-module-bindings renames))
       (define-values (maybe-unmarshals maybe-renames) (if (null? encoded-unmarshals) 
                                                           (values encoded-renames mark-renames)
                                                           (values encoded-unmarshals (cons encoded-renames mark-renames))))
       (define mod-rename (list* phase encoded-kind set-id maybe-unmarshals maybe-renames))
       (if plus-kern?
           (cons #t mod-rename)
           mod-rename)]
      [(struct lexical-rename (bool1 bool2 alist))
       (define len (length alist))
       (define vec (make-vector (+ (* 2 len) 2))) ; + 2 for booleans at the beginning
       (vector-set! vec 0 bool1)
       (vector-set! vec 1 bool2)
       (for ([(k v) (in-dict alist)]
             [i (in-naturals)])
         (vector-set! vec (+ 2 i) k)
         (vector-set! vec (+ 2 i len) v))
       vec]
      [(struct top-level-rename (flag))
       flag]
      [(struct mark-barrier (value)) 
       value]
      [(struct prune (syms))
       (box syms)]
      [(struct wrap-mark (val))
       (list val)])))

(define (encode-mark-map mm)
  mm
  #;(for/fold ([l empty])
      ([(k v) (in-hash ht)])
      (list* k v l)))

(define-struct protected-symref (val))

(define encode-certs
  (match-lambda
    [(struct certificate:nest (m1 m2))
     (list* (encode-mark-map m1) (encode-mark-map m2))]
    [(struct certificate:ref (val m))
     (list* #f (make-protected-symref val) (encode-mark-map m))]))

(define (encode-wrapped w)
  (match w
    [(struct wrapped (datum wraps certs))
     (let* ([enc-datum
             (match datum
               [(cons a b) 
                (let ([p (cons (encode-wrapped a)
                               (let bloop ([b b])
                                 (match b
                                   ['() null]
                                   [(cons b1 b2)
                                    (cons (encode-wrapped b1)
                                          (bloop b2))]
                                   [else
                                    (encode-wrapped b)])))]
                      ; XXX Cylic list error possible
                      [len (let loop ([datum datum][len 0])
                             (cond
                               [(null? datum) #f]
                               [(pair? datum) (loop (cdr datum) (add1 len))]
                               [else len]))])
                  ;; for improper lists, we need to include the length so the 
                  ;; parser knows where the end of the improper list is
                  (if len
                      (cons len p) 
                      p))]
               [(box x)
                (box (encode-wrapped x))]
               [(? vector? v)
                (vector-map encode-wrapped v)]
               [(? prefab-struct-key)
                (define l (vector->list (struct->vector datum)))
                (apply
                 make-prefab-struct
                 (car l)
                 (map encode-wrapped (cdr l)))]
               [_ datum])]
            [p (cons enc-datum
                     (encode-wraps wraps))])
       (if certs
           (vector p (encode-certs certs))
           p))]))

(define-struct out (s shared-index encoded-wraps))
(define (out-shared v out k)
  (if (shareable? v)
      (let ([v ((out-shared-index out) v)])
        (if v
            (begin
              (out-byte CPT_SYMREF out)
              (out-number v out))
            (k)))
      (k)))

(define (out-byte v out)
  (write-byte v (out-s out)))

(define (out-bytes b out)
  (write-bytes b (out-s out)))

(define (out-number n out)
  (cond
    [(n . < . 0)
     (if (n . > . -32)
         (out-byte (bitwise-ior #xC0 (- n)) out)
         (begin
           (out-byte #xE0 out)
           (out-bytes (int->bytes (- n)) out)))]
    [(n . < . 128)
     (out-byte n out)]
    [(n . < . #x4000)
     (out-byte (bitwise-ior #x80 (bitwise-and n #x3F)) out)
     (out-byte (bitwise-and #xFF (arithmetic-shift n -6)) out)]
    [else
     (out-byte #xF0 out)
     (out-bytes (int->bytes n) out)]))

(define (out-syntax key val out)
  (out-marshaled syntax-type-num (list* key val) out))

(define (out-marshaled type-num val out)
  (if (type-num . < . (- CPT_SMALL_MARSHALLED_END CPT_SMALL_MARSHALLED_START))
      (out-byte (+ CPT_SMALL_MARSHALLED_START type-num) out)
      (begin
        (out-byte CPT_MARSHALLED out)
        (out-number type-num out)))
  (out-anything val out))

(define (or-pred? v . ps)
  (ormap (lambda (?) (? v)) ps))


(define quoting? (make-parameter #f))

(define (shareable? v)
  (not (or quoting? (or-pred? v char? maybe-same-as-fixnum? empty? boolean? void?))))

(define (maybe-same-as-fixnum? v)
  (and (exact-integer? v)
       (and (v . >= . -1073741824) (v . <= . 1073741823))))


(define (out-anything v out)
  (out-shared 
   v out
   (λ ()
     (match v
       [(? char?)
        (out-byte CPT_CHAR out)
        (out-number (char->integer v) out)]
       [(? maybe-same-as-fixnum?) ;XXX not sure if it's okay to use fixnum? instead of exact range check
        (if (and (v . >= . 0)
                 (v . < . (- CPT_SMALL_NUMBER_END CPT_SMALL_NUMBER_START)))
            (out-byte (+ CPT_SMALL_NUMBER_START v) out)
            (begin
              (out-byte CPT_INT out)
              (out-number v out)))]
       [(list)
        (out-byte CPT_NULL out)]
       [#t
        (out-byte CPT_TRUE out)]
       [#f
        (out-byte CPT_FALSE out)]
       [(? void?)
        (out-byte CPT_VOID out)]
       [(struct module-variable (modidx sym pos phase))
        (out-byte CPT_MODULE_VAR out)
        (out-anything modidx out)
        (out-anything sym out)
        (unless (zero? phase)
          (out-number -2 out))
        (out-number pos out)]
       [(struct indirect (val)) (out-anything val out)]
       [(struct closure (lam gen-id))
        (out-byte CPT_CLOSURE out)
        (out-number ((out-shared-index out) v) out)
        (out-anything lam out)]
       [(struct prefix (num-lifts toplevels stxs))
        (out-marshaled
         prefix-type-num
         (cons num-lifts
               (cons (list->vector toplevels)
                     (list->vector stxs)))
         out)]
       [(struct global-bucket (name)) 
        (out-marshaled variable-type-num name out)]
       [(struct free-id-info (mpi0 s0 mpi1 s1 p0 p1 p2 insp?))
        (out-marshaled
         free-id-info-type-num
         (vector mpi0 s0 mpi1 s1 p0 p1 p2 insp?)
         out)]
       [(? mod?)
        (out-module v out)]
       [(struct def-values (ids rhs))
        (out-syntax DEFINE_VALUES_EXPD
                    (list->vector (cons (protect-quote rhs) ids))
                    out)]
       [(struct def-syntaxes (ids rhs prefix max-let-depth))
        (out-syntax DEFINE_SYNTAX_EXPD
                    (list->vector (list* (protect-quote rhs)
                                         prefix
                                         max-let-depth
                                         *dummy*
                                         ids))
                    out)]
       [(struct def-for-syntax (ids rhs prefix max-let-depth))
        (out-syntax DEFINE_FOR_SYNTAX_EXPD
                    (list->vector (list* (protect-quote rhs)
                                         prefix
                                         max-let-depth
                                         *dummy*
                                         ids))
                    out)]
       [(struct seq0 (forms))
        (out-marshaled begin0-sequence-type-num (map protect-quote forms) out)]
       [(struct seq (forms))
        (out-marshaled sequence-type-num (map protect-quote forms) out)]
       [(struct splice (forms))
        (out-syntax SPLICE_EXPD (make-seq forms) out)]
       [(struct req (reqs dummy))
        (error "cannot handle top-level `require', yet")
        (out-syntax REQUIRE_EXPD (cons dummy reqs) out)]
       [(struct toplevel (depth pos const? ready?))
        (out-marshaled toplevel-type-num
                       (cons
                        depth
                        (if (or const? ready?)
                            (cons pos
                                  (bitwise-ior 
                                   (if const? #x1 0)
                                   (if ready? #x2 0)))
                            pos))
                       out)]
       [(struct topsyntax (depth pos midpt))
        (out-marshaled quote-syntax-type-num
                       (cons depth
                             (cons pos midpt))
                       out)]
       [(struct primval (id))
        (out-byte CPT_REFERENCE out)
        (out-number id out)]
       [(struct assign (id rhs undef-ok?))
        (out-syntax SET_EXPD
                    (cons undef-ok? (cons id rhs))
                    out)]
       [(struct localref (unbox? offset clear? other-clears? flonum?))
        (if (and (not clear?) (not other-clears?) (not flonum?)
                 (offset . < . (- CPT_SMALL_LOCAL_END CPT_SMALL_LOCAL_START)))
            (out-byte (+ (if unbox?
                             CPT_SMALL_LOCAL_UNBOX_START
                             CPT_SMALL_LOCAL_START)
                         offset)
                      out)
            (begin
              (out-byte (if unbox? CPT_LOCAL_UNBOX CPT_LOCAL) out)
              (if (not (or clear? other-clears? flonum?))
                  (out-number offset out)
                  (begin
                    (out-number (- (add1 offset)) out)
                    (out-number (if clear?
                                    #x1
                                    (if other-clears? 
                                        #x2
                                        (if flonum?
                                            #x3
                                            0)))
                                out)))))]
       [(? lam?)
        (out-lam v out)]
       [(struct case-lam (name lams))
        (let ([seq (make-case-seq name lams)])
          ;; XXX: This seems like an optimization, which should probably happen somewhere else
          ;; If all closures are empty, generate a case sequence directly
          (if (andmap (lambda (lam)
                        (or (closure? lam)
                            (and (lam? lam)
                                 (equal? (lam-closure-map lam) #()))))
                      lams)
              (out-anything seq out)
              (out-syntax CASE_LAMBDA_EXPD
                          seq
                          out)))]
       [(struct case-seq (name lams))
        (out-marshaled case-lambda-sequence-type-num
                       (cons (or name null)
                             lams)
                       out)]
       [(struct let-one (rhs body flonum? unused?))
        (out-byte (cond
                    [flonum? CPT_LET_ONE_FLONUM]
                    [unused? CPT_LET_ONE_UNUSED]
                    [else CPT_LET_ONE])
                  out)
        (out-anything (protect-quote rhs) out)
        (out-anything (protect-quote body) out)]
       [(struct let-void (count boxes? body))
        (out-marshaled let-void-type-num
                       (list*
                        count
                        boxes?
                        (protect-quote body))
                       out)]
       [(struct let-rec (procs body))
        (out-marshaled letrec-type-num
                       (list*
                        (length procs)
                        (protect-quote body)
                        procs)
                       out)]
       [(struct install-value (count pos boxes? rhs body))
        (out-marshaled let-value-type-num
                       (list*
                        count
                        pos
                        boxes?
                        (protect-quote rhs)
                        (protect-quote body))
                       out)]
       [(struct boxenv (pos body))
        (out-syntax BOXENV_EXPD
                    (cons
                     pos
                     (protect-quote body))
                    out)]
       [(struct branch (test then else))
        (out-byte CPT_BRANCH out)
        (out-anything (protect-quote test) out)
        (out-anything (protect-quote then) out)
        (out-anything (protect-quote else) out)]
       [(struct application (rator rands))
        (let ([len (length rands)]) 
          (if (len . < . (- CPT_SMALL_APPLICATION_END CPT_SMALL_APPLICATION_START))
              (out-byte (+ CPT_SMALL_APPLICATION_START (length rands)) out)
              (begin
                (out-byte CPT_APPLICATION out)
                (out-number len out)))
          (for-each (lambda (e) (out-anything (protect-quote e) out))
                    (cons rator rands)))]
       [(struct apply-values (proc args-expr))
        (out-syntax APPVALS_EXPD
                    (cons (protect-quote proc)
                          (protect-quote args-expr))
                    out)]
       [(struct beg0 (exprs))
        (out-syntax BEGIN0_EXPD
                    (make-seq0 exprs)
                    out)]
       [(struct with-cont-mark (key val body))
        (out-marshaled wcm-type-num
                       (list*
                        (protect-quote key)
                        (protect-quote val)
                        (protect-quote body))
                       out)]
       [(struct varref (expr))
        (out-syntax REF_EXPD
                    expr
                    out)]
       [(protected-symref v)
        (out-anything ((out-shared-index out) v) out)]
       [(and (? symbol?) (not (? symbol-interned?)))
        (out-as-bytes v 
                      #:before-length (if (symbol-unreadable? v) 0 1)
                      (compose string->bytes/utf-8 symbol->string) 
                      CPT_WEIRD_SYMBOL
                      #f
                      out)]     
       [(? symbol?)
        (define bs (string->bytes/utf-8 (symbol->string v)))
        (define len (bytes-length bs))
        (if (len . < . (- CPT_SMALL_SYMBOL_END CPT_SMALL_SYMBOL_START))
            (out-byte (+ CPT_SMALL_SYMBOL_START len) out)
            (begin (out-byte CPT_SYMBOL out)
                   (out-number len out)))
        (out-bytes bs out)]
       [(? keyword?)
        (out-as-bytes v 
                      (compose string->bytes/utf-8 keyword->string) 
                      CPT_KEYWORD
                      #f
                      out)]
       [(? string?)
        (out-as-bytes v 
                      string->bytes/utf-8
                      CPT_CHAR_STRING
                      (string-length v)
                      out)]
       [(? bytes?)
        (out-as-bytes v 
                      values
                      CPT_BYTE_STRING
                      #f
                      out)]
       [(? box?)
        (out-byte CPT_BOX out)
        (out-anything (unbox v) out)]
       [(? pair?)
        (define (list-length-before-cycle/improper-end l)
          (let loop ([len 1] [l (cdr l)] [seen (set)])
            (cond 
              [(set-member? seen l)
               (values len #f)]
              [(null? l)
               (values len #t)]
              [(pair? l)
               (loop (add1 len) (cdr l) (set-add seen l))]
              [else
               (values len #f)])))
        (define-values (len proper?) (list-length-before-cycle/improper-end v))
        (define (print-contents-as-proper)
          (for ([e (in-list v)])
            (out-anything e out)))
        (define (print-contents-as-improper)
          (let loop ([l v] [i len])
            (cond
              [(zero? i)
               (out-anything l out)]
              [else
               (out-anything (car l) out)
               (loop (cdr l) (sub1 i))])))
        (if proper?
            (if (len . < . (- CPT_SMALL_PROPER_LIST_END CPT_SMALL_PROPER_LIST_START))
                (begin (out-byte (+ CPT_SMALL_PROPER_LIST_START len) out)
                       (print-contents-as-proper))
                (begin (out-byte CPT_LIST out)
                       (out-number len out)
                       (print-contents-as-proper)
                       (out-anything null out)))
            (if (len . < . (- CPT_SMALL_LIST_END CPT_SMALL_LIST_START))
                ; XXX If len = 1 (or maybe = 2?) then this could by CPT_PAIR
                (begin (out-byte (+ CPT_SMALL_LIST_START len) out)
                       (print-contents-as-improper))
                (begin (out-byte CPT_LIST out)
                       (out-number len out)
                       (print-contents-as-improper))))]
       [(? vector?)
        (out-byte CPT_VECTOR out)
        (out-number (vector-length v) out)
        (for ([v (in-vector v)])
          (out-anything v out))]
       [(? hash?)
        (out-byte CPT_HASH_TABLE out)
        (out-number (cond
                      [(hash-eqv? v) 2]
                      [(hash-eq? v) 0]
                      [else 1])
                    out)
        (out-number (hash-count v) out)
        (for ([(k v) (in-hash v)])
          (out-anything k out)
          (out-anything v out))]
       [(svector vec)
        (let* ([len (vector-length vec)])
          (if (len . < . (- CPT_SMALL_SVECTOR_END CPT_SMALL_SVECTOR_START))
              (out-byte (+ CPT_SMALL_SVECTOR_START len) out)
              (begin (out-byte CPT_SVECTOR out)
                     (out-number len out)))
          (for ([n (in-range (sub1 len) -1 -1)])
            (out-number (vector-ref vec n) out)))]
       [(? module-path-index?)
        (out-byte CPT_MODULE_INDEX out)
        (let-values ([(name base) (module-path-index-split v)])
          (out-anything name out)
          (out-anything base out))]
       [(module-decl content)
        (out-marshaled module-type-num
                       content
                       out)]
       [(stx encoded)
        (out-byte CPT_STX out)
        (out-anything encoded out)]
       [(? wrapped?)
        (out-anything (lookup-encoded-wrapped v out) out)]
       [(? prefab-struct-key)
        (define pre-v (struct->vector v))
        (vector-set! pre-v 0 (prefab-struct-key v))
        (out-byte CPT_PREFAB out)
        (out-anything pre-v out)]
       [else
        (out-byte CPT_QUOTE out)
        (if (quoted? v)
            (parameterize ([quoting? #t])
              (out-anything (quoted-v v) out))
            (let ([s (open-output-bytes)])
              (parameterize ([pretty-print-size-hook
                              (lambda (v mode port)
                                (and (path? v)
                                     (let ([v (make-relative v)])
                                       (+ 2 (let ([p (open-output-bytes)])
                                              (write (path->bytes v) p)
                                              (bytes-length (get-output-bytes p)))))))]
                             [pretty-print-print-hook
                              (lambda (v mode port)
                                (display "#^" port)
                                (write (path->bytes (make-relative v)) port))])
                (pretty-write v s))
              (out-byte CPT_ESCAPE out)
              (let ([bstr (get-output-bytes s)])
                (out-number (bytes-length bstr) out)
                (out-bytes bstr out))))]))))

(define-struct module-decl (content))

(define (out-module mod-form out)
  (match mod-form
    [(struct mod (name srcname self-modidx prefix provides requires body syntax-body unexported 
                       max-let-depth dummy lang-info internal-context))
     (out-syntax MODULE_EXPD
                 (let* ([lookup-req (lambda (phase)
                                      (let ([a (assq phase requires)])
                                        (if a
                                            (cdr a)
                                            null)))]
                        [other-requires (filter (lambda (l)
                                                  (not (memq (car l) '(#f -1 0 1))))
                                                requires)]
                        [extract-protects
                         (lambda (phase)
                           (let ([a (assq phase provides)])
                             (and a
                                  (let ([p (map provided-protected? (append (cadr a)
                                                                            (caddr a)))])
                                    (if (ormap values p)
                                        (list->vector p)
                                        #f)))))]
                        [list->vector/#f (lambda (default l)
                                           (if (andmap (lambda (x) (equal? x default)) l)
                                               #f
                                               (list->vector l)))]
                        [l 
                         (let loop ([l other-requires])
                           (match l
                             [(list)
                              empty]
                             [(list-rest (cons phase reqs) rst)
                              (list* phase reqs (loop rst))]))]
                        [l (cons (length other-requires) l)]
                        [l (cons (lookup-req #f) l)] ; dt-requires
                        [l (cons (lookup-req -1) l)] ; tt-requires
                        [l (cons (lookup-req 1) l)] ; et-requires
                        [l (cons (lookup-req 0) l)] ; requires
                        [l (cons (list->vector body) l)]
                        [l (cons (list->vector
                                  (for/list ([i (in-list syntax-body)])
                                    (define (maybe-one l) ;; a single symbol is ok
                                      (if (and (pair? l) (null? (cdr l)))
                                          (car l)
                                          l))
                                    (match i
                                      [(struct def-syntaxes (ids rhs prefix max-let-depth))
                                       (vector (maybe-one ids) rhs max-let-depth prefix #f)]
                                      [(struct def-for-syntax (ids rhs prefix max-let-depth))
                                       (vector (maybe-one ids) rhs max-let-depth prefix #t)])))
                                 l)]
                        [l (append (apply
                                    append
                                    (map (lambda (l)
                                           (let ([phase (car l)]
                                                 [all (append (cadr l) (caddr l))])
                                             (list phase
                                                   (list->vector/#f #f (map provided-insp all))
                                                   (list->vector/#f 0 (map (lambda (p) (= 1 (provided-src-phase p))) 
                                                                           all))
                                                   (list->vector/#f #f (map (lambda (p)
                                                                              (if (eq? (provided-nom-src p)
                                                                                       (provided-src p))
                                                                                  #f ; #f means "same as src"
                                                                                  (provided-nom-src p)))
                                                                            all))
                                                   (list->vector (map provided-src-name all))
                                                   (list->vector (map provided-src all))
                                                   (list->vector (map provided-name all))
                                                   (length (cadr l))
                                                   (length all))))
                                         provides))
                                   l)]
                        [l (cons (length provides) l)] ; number of provide sets
                        [l (cons (extract-protects 0) l)] ; protects
                        [l (cons (extract-protects 1) l)] ; et protects
                        [l (list* (list->vector (car unexported)) (length (car unexported)) l)] ; indirect-provides
                        [l (list* (list->vector (cadr unexported)) (length (cadr unexported)) l)] ; indirect-syntax-provides
                        [l (list* (list->vector (caddr unexported)) (length (caddr unexported)) l)] ; indirect-et-provides
                        [l (cons prefix l)]
                        [l (cons dummy l)]
                        [l (cons max-let-depth l)]
                        [l (cons internal-context l)] ; module->namespace syntax
                        [l (list* #f #f l)] ; obsolete `functional?' info
                        [l (cons lang-info l)] ; lang-info
                        [l (cons self-modidx l)]
                        [l (cons srcname l)]
                        [l (cons name l)])
                   (make-module-decl l))
                 out)]))


(define (lookup-encoded-wrapped w out)
  (hash-ref (out-encoded-wraps out) w
            (λ ()
              (encode-wrapped w))))


(define (out-lam expr out)  
  (match expr
    [(struct lam (name flags num-params param-types rest? closure-map closure-types max-let-depth body))
     (let* ([l (protect-quote body)]
            [any-refs? (or (ormap (lambda (t) (memq t '(ref flonum))) param-types)
                           (ormap (lambda (t) (memq t '(flonum))) closure-types))]
            [num-all-params ((if rest? add1 values) num-params)]
            [l (cons (make-svector (if any-refs?
                                       (list->vector
                                        (append
                                         (vector->list closure-map)
                                         (let* ([v (make-vector (ceiling 
                                                                 (/ (* 2 (+ num-params (vector-length closure-map)))
                                                                    BITS_PER_MZSHORT)))]
                                                [set-bit! (lambda (i bit)
                                                            (let ([pos (quotient (* 2 i) BITS_PER_MZSHORT)])
                                                              (vector-set! v pos
                                                                           (bitwise-ior (vector-ref v pos)
                                                                                        (arithmetic-shift 
                                                                                         bit
                                                                                         (modulo (* 2 i) BITS_PER_MZSHORT))))))])
                                           (for ([t (in-list param-types)]
                                                 [i (in-naturals)])
                                             (when (eq? t 'ref) (set-bit! i 1))
                                             (when (eq? t 'flonum) (set-bit! i 2)))
                                           (for ([t (in-list closure-types)]
                                                 [i (in-naturals num-all-params)])
                                             (when (eq? t 'flonum) (set-bit! i 2)))
                                           (vector->list v))))
                                       closure-map))
                     l)]
            [l (if any-refs?
                   (cons (vector-length closure-map) l)
                   l)])
       (out-marshaled unclosed-procedure-type-num
                      (list*
                       (+ (if rest? CLOS_HAS_REST 0)
                          (if any-refs? CLOS_HAS_REF_ARGS 0)
                          (if (memq 'preserves-marks flags) CLOS_PRESERVES_MARKS 0)
                          (if (memq 'is-method flags) CLOS_IS_METHOD 0)
                          (if (memq 'single-result flags) CLOS_SINGLE_RESULT 0))
                       num-all-params
                       max-let-depth
                       name
                       l)
                      out))]))

(define (out-as-bytes expr ->bytes CPT len2 out #:before-length [before-length #f])
  (define s (->bytes expr))
  (out-byte CPT out)
  (when before-length
    (out-number before-length out))
  (out-number (bytes-length s) out)
  (when len2 (out-number len2 out))
  (out-bytes s out))

(define-struct quoted (v))

(define (protect-quote v)
  (if (or (pair? v) (vector? v) (prefab-struct-key v) (box? v) (hash? v) (svector? v))
      (make-quoted v)
      v))


(define-struct svector (vec))

(define (make-relative v)
  (let ([r (current-write-relative-directory)])
    (if r
        (find-relative-path r v)
        v)))


;; ----------------------------------------

