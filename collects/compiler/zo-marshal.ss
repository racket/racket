#lang scheme/base
(require compiler/zo-structs
         scheme/match
         scheme/list
         scheme/dict)

(provide zo-marshal)

#| Unresolved Issues
  
  Less sharing occurs than in the C implementation, creating much larger files

  encode-all-from-module only handles one case

  What is the purpose of protect-quote? It was making it so certain things (like paths) weren't being encoded correctly.

|#

;; Doesn't write as compactly as MzScheme, since list and pair sequences
;; are not compacted, and symbols are not written in short form
(define current-wrapped-ht (make-parameter #f))
(define (zo-marshal top)
  (match top
    [(struct compilation-top (max-let-depth prefix form))
     (let ([encountered (make-hasheq)]
           [shared (make-hasheq)]
           [wrapped (make-hasheq)])
       (let ([visit (lambda (v) 
                      (if (hash-ref shared v #f)
                          #f
                          (if (hash-ref encountered v #f)
                              (begin
                                (hash-set! shared v (add1 (hash-count shared)))
                                #f)
                              (begin
                                (hash-set! encountered v #t)
                                (when (closure? v)
                                  (hash-set! shared v (add1 (hash-count shared))))
                                #t))))])
         (parameterize ([current-wrapped-ht wrapped])
           (traverse-prefix prefix visit)
           (traverse-form form visit)))
       (let* ([s (open-output-bytes)]
              [out (make-out s (lambda (v) (hash-ref shared v #f)) wrapped)]
              [offsets
               (map (lambda (v)
                      (let ([v (cdr v)])
                        (begin0
                          (file-position s)
                          (out-anything v (make-out
                                           s
                                           (let ([skip? #t])
                                             (lambda (v2)
                                               (if (and skip? (eq? v v2))
                                                   (begin
                                                     (set! skip? #f)
                                                     #f)
                                                   (hash-ref shared v2 #f))))
                                           wrapped)))))
                    (sort (hash-map shared (lambda (k v) (cons v k)))
                          <
                          #:key car))]
              [post-shared (file-position s)]
              [all-short? (post-shared . < . #xFFFF)])
         (out-data (list* max-let-depth prefix (protect-quote form)) out)
         (let ([res (get-output-bytes s)]
               [version-bs (string->bytes/latin-1 (version))])
           (bytes-append #"#~"
                         (bytes (bytes-length version-bs))
                         version-bs
                         (int->bytes (add1 (hash-count shared)))
                         (bytes (if all-short?
                                    1
                                    0))
                         (apply
                          bytes-append
                          (map (lambda (o)
                                 (integer->integer-bytes o
                                                         (if all-short? 2 4)
                                                         #f
                                                         #f))
                               offsets))
                         (int->bytes post-shared)
                         (int->bytes (bytes-length res))
                         res))))]))

;; ----------------------------------------

(define (traverse-prefix a-prefix visit)
  (match a-prefix
    [(struct prefix (num-lifts toplevels stxs))
     (for-each (lambda (stx) (traverse-toplevel stx visit)) toplevels)
     (for-each (lambda (stx) (traverse-stx stx visit)) stxs)]))

(define (traverse-module mod-form visit)
  (match mod-form
    [(struct mod (name self-modidx prefix provides requires body syntax-body unexported 
                       max-let-depth dummy lang-info internal-context))
     (traverse-data name visit)
     (traverse-data self-modidx visit)
     (traverse-prefix prefix visit)
     (for-each (lambda (f) (map (lambda (v) (traverse-data v visit)) (cdr f))) requires)
     (for-each (lambda (f) (traverse-form f visit)) body)
     (for-each (lambda (f) (traverse-form f visit)) syntax-body)
     (traverse-data lang-info visit)
     (traverse-data internal-context visit)]))

(define (traverse-toplevel tl visit)
  (match tl
    [#f (void)]
    [(? symbol?) (traverse-data tl visit)]
    [(struct global-bucket (name)) 
     (void)]
    [(struct module-variable (modidx sym pos phase))
     (visit tl)
     (let-values ([(p b) (module-path-index-split modidx)])
       (if (symbol? p)
           (traverse-data p visit)
           (traverse-data modidx visit)))
     (traverse-data sym visit)]))

(define (traverse-wrapped w visit)
  (define ew (hash-ref! (current-wrapped-ht) w (lambda () (encode-wrapped w))))
  (traverse-data ew visit))

(define (traverse-stx s visit)
  (when s
    (traverse-wrapped (stx-encoded s) visit)))


(define (traverse-form form visit)
  (match form
    [(? mod?)
     (traverse-module form visit)]
    [(struct def-values (ids rhs))
     (traverse-expr rhs visit)]
    [(struct def-syntaxes (ids rhs prefix max-let-depth))
     (traverse-prefix prefix visit)
     (traverse-expr rhs visit)]
    [(struct def-for-syntax (ids rhs prefix max-let-depth))
     (traverse-prefix prefix visit)
     (traverse-expr rhs visit)]
    [(struct seq (forms))
     (for-each (lambda (f) (traverse-form f visit)) forms)]
    [(struct splice (forms))
     (for-each (lambda (f) (traverse-form f visit)) forms)]
    [else
     (traverse-expr form visit)]))

(define (traverse-expr expr visit)
  (match expr
    [(struct toplevel (depth pos const? ready?))
     (void)]
    [(struct topsyntax (depth pos midpt))
     (void)]
    [(struct primval (id))
     (void)]
    [(struct assign (id rhs undef-ok?))
     (traverse-expr rhs visit)]
    [(struct localref (unbox? offset clear? other-clears? flonum?))
     (void)]
    [(? lam?)
     (traverse-lam expr visit)]
    [(struct case-lam (name lams))
     (traverse-data name visit)
     (for-each (lambda (lam) (traverse-lam lam visit)) lams)]
    [(struct let-one (rhs body flonum?))
     (traverse-expr rhs visit)
     (traverse-expr body visit)]
    [(struct let-void (count boxes? body))
     (traverse-expr body visit)]
    [(struct let-rec (procs body))
     (for-each (lambda (lam) (traverse-lam lam visit)) procs)
     (traverse-expr body visit)]
    [(struct install-value (count pos boxes? rhs body))
     (traverse-expr rhs visit)
     (traverse-expr body visit)]
    [(struct boxenv (pos body))
     (traverse-expr body visit)]
    [(struct branch (test then else))
     (traverse-expr test visit)
     (traverse-expr then visit)
     (traverse-expr else visit)]
    [(struct application (rator rands))
     (traverse-expr rator visit)
     (for-each (lambda (rand) (traverse-expr rand visit)) rands)]
    [(struct apply-values (proc args-expr))
     (traverse-expr proc visit)
     (traverse-expr args-expr visit)]
    [(struct seq (exprs))
     (for-each (lambda (expr) (traverse-form expr visit)) exprs)]
    [(struct beg0 (exprs))
     (for-each (lambda (expr) (traverse-expr expr visit)) exprs)]
    [(struct with-cont-mark (key val body))
     (traverse-expr key visit)
     (traverse-expr val visit)
     (traverse-expr body visit)]
    [(struct closure (lam gen-id))
     (traverse-lam expr visit)]
    [(struct indirect (val))
     (traverse-expr val visit)]
    [else (traverse-data expr visit)]))

(define (traverse-data expr visit)
  (cond
    [(or (symbol? expr)
         (keyword? expr)
         (string? expr)
         (bytes? expr)
         (path? expr))
     (visit expr)]
    [(module-path-index? expr)
     (visit expr)
     (let-values ([(name base) (module-path-index-split expr)])
       (traverse-data name visit)
       (traverse-data base visit))]
    [(pair? expr)
     (traverse-data (car expr) visit)
     (traverse-data (cdr expr) visit)]
    [(vector? expr)
     (for ([e (in-vector expr)])
       (traverse-data e visit))]
    [(box? expr)
     (traverse-data (unbox expr) visit)]
    [(stx? expr)
     (traverse-stx expr visit)]
    [(wrapped? expr)
     (traverse-wrapped expr visit)]
    [else
     (void)]))

(define (traverse-lam expr visit)
  (match expr
    [(struct indirect (val)) (traverse-lam val visit)]
    [(struct closure (lam gen-id))
     (when (visit expr)
       (traverse-lam lam visit))]
    [(struct lam (name flags num-params param-types rest? closure-map closure-types max-let-depth body))
     (traverse-data name visit)
     (traverse-expr body visit)]))

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
(define top-type-num 87)
(define case-lambda-sequence-type-num 96)
(define begin0-sequence-type-num 97)
(define module-type-num 100)
(define prefix-type-num 102)

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
  CPT_DELAY_REF
  CPT_PREFAB)

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

(define CPT_SMALL_LOCAL_START 192)
(define CPT_SMALL_LOCAL_END 207)
(define CPT_SMALL_LOCAL_UNBOX_START 207)
(define CPT_SMALL_LOCAL_UNBOX_END 222)

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

(define-struct out (s shared-index encoded-wraps))
(define (out-shared v out k)
  (let ([v ((out-shared-index out) v)])
    (if v
        (begin
          (out-byte CPT_SYMREF out)
          (out-number v out))
        (k))))
(define (display-byte b)
  (if (b . <= . #xf)
      (printf "0~x" b)
      (printf "~x" b)))

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
  (out-byte CPT_MARSHALLED out)
  (out-number type-num out)
  (out-data val out))

(define (out-anything v out)
  (cond
    [(module-variable? v)
     (out-toplevel v out)]
    [(closure? v)
     (out-expr v out)]
    [else
     (out-data v out)]))

(define (out-prefix a-prefix out)
  (match a-prefix
    [(struct prefix (num-lifts toplevels stxs))
     (out-marshaled
      prefix-type-num
      (cons num-lifts
            (cons (list->vector toplevels)
                  (list->vector stxs)))
      out)]))

(define-struct module-decl (content))

(define (out-module mod-form out)
  (match mod-form
    [(struct mod (name self-modidx prefix provides requires body syntax-body unexported 
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
                        [l (cons (list->vector syntax-body) l)]
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
                        [l (cons name l)])
                   (make-module-decl l))
                 out)]))

(define (out-toplevel tl out)
  (match tl
    [#f (out-data tl out)]
    [(? symbol?) (out-data tl out)]
    [(struct global-bucket (name)) 
     (out-marshaled variable-type-num name out)]
    [(struct module-variable (modidx sym pos phase))
     (out-shared
      tl
      out
      (lambda ()
        (out-byte CPT_MODULE_VAR out)
        (out-data modidx out)
        (out-data sym out)
        (unless (zero? phase)
          (out-number -2 out))
        (out-number pos out)))]))

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

(define (encode-all-from-module all)
  (match all
    [(struct all-from-module (path phase src-phase exceptions prefix))
     (list* path phase src-phase)]))

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
      [(struct prune (syms))
       (box syms)]
      [(struct wrap-mark (val))
       (list val)])))

(define (encode-wrapped w)
  (match w
    [(struct wrapped (datum wraps certs))
     (vector
      (cons
       datum
       (encode-wraps wraps))
      certs)]))

(define (lookup-encoded-wrapped w out)
  (hash-ref (out-encoded-wraps out) w))

(define (out-wrapped w out)
  (out-data (lookup-encoded-wrapped w out) out))

(define (out-stx s out)
  (out-shared s out 
              (lambda ()
                (match s
                  [(struct stx (encoded))
                   (out-byte CPT_STX out)
                   (out-wrapped encoded out)]))))
              
(define (out-form form out)
  (match form
    [(? mod?)
     (out-module form out)]
    [(struct def-values (ids rhs))
     (out-syntax DEFINE_VALUES_EXPD
                 (list->vector (cons rhs ids))
                 out)]
    [(struct def-syntaxes (ids rhs prefix max-let-depth))
     (out-syntax DEFINE_SYNTAX_EXPD
                 (list->vector (list* rhs
                                      prefix
                                      max-let-depth
                                      *dummy*
                                      ids))
                 out)]
    [(struct def-for-syntax (ids rhs prefix max-let-depth))
     (out-syntax DEFINE_FOR_SYNTAX_EXPD
                 (list->vector (list* rhs
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
    [else
     (out-expr form out)]))

(define (out-expr expr out)
  (match expr
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
     (out-lam expr out)]
    [(struct case-lam (name lams))
     (let ([seq (make-case-seq name lams)])
       ;; If all closures are empy, generate a case sequence directly
       (if (andmap (lambda (lam)
                     (or (closure? lam)
                         (and (lam? lam)
                              (equal? (lam-closure-map lam) #()))))
                   lams)
           (out-data seq out)
           (out-syntax CASE_LAMBDA_EXPD
                       seq
                       out)))]
    [(struct case-seq (name lams))
     (out-marshaled case-lambda-sequence-type-num
                    (cons (or name null)
                          lams)
                    out)]
    [(struct let-one (rhs body flonum?))
     (out-byte (if flonum? CPT_LET_ONE_FLONUM CPT_LET_ONE) out)
     (out-expr (protect-quote rhs) out)
     (out-expr (protect-quote body) out)]
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
     (out-expr (protect-quote test) out)
     (out-expr (protect-quote then) out)
     (out-expr (protect-quote else) out)]
    [(struct application (rator rands))
     (if ((length rands) . < . (- CPT_SMALL_APPLICATION_END CPT_SMALL_APPLICATION_START))
         (out-byte (+ CPT_SMALL_APPLICATION_START (length rands)) out)
         (begin
           (out-byte CPT_APPLICATION out)
           (out-number (length rands) out)))
     (for-each (lambda (e) (out-expr (protect-quote e) out))
               (cons rator rands))]
    [(struct apply-values (proc args-expr))
     (out-syntax APPVALS_EXPD
                 (cons (protect-quote proc)
                       (protect-quote args-expr))
                 out)]
    [(struct seq (exprs))
     (out-form expr out)]
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
    [(struct closure (lam gen-id))
     (out-lam expr out)]
    [(struct indirect (val))
     (out-expr val out)]
    [(struct varref (expr))
     (out-syntax REF_EXPD
                 expr
                 out)]
    [else (out-value expr out)]))

(define (out-lam expr out)  
  (match expr
    [(struct indirect (val)) (out-lam val out)]
    [(struct closure (lam gen-id))
     (out-shared
      expr
      out
      (lambda ()
        (out-byte CPT_CLOSURE out)
        (out-number ((out-shared-index out) expr) out)
        (out-lam lam out)))]
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
  (out-shared expr out (lambda ()
                         (let ([s (->bytes expr)])
                           (out-byte CPT out)
                           (when before-length
                             (out-number before-length out))
                           (out-number (bytes-length s) out)
                           (when len2 (out-number len2 out))
                           (out-bytes s out)))))

(define (out-data expr out)
  (cond
    [(prefix? expr) (out-prefix expr out)]
    [(global-bucket? expr) (out-toplevel expr out)]
    [(module-variable? expr) (out-toplevel expr out)]
    [else (out-form expr out)]))

(define (out-value expr out)
  (cond
    [(and (symbol? expr) (not (symbol-interned? expr)))
     (out-as-bytes expr 
                   #:before-length (if (symbol-unreadable? expr) 0 1)
                   (compose string->bytes/utf-8 symbol->string) 
                   CPT_WEIRD_SYMBOL
                   #f
                   out)]     
    [(symbol? expr)
     (out-as-bytes expr 
                   (compose string->bytes/utf-8 symbol->string) 
                   CPT_SYMBOL
                   #f
                   out)]
    [(keyword? expr)
     (out-as-bytes expr 
                   (compose string->bytes/utf-8 keyword->string) 
                   CPT_KEYWORD
                   #f
                   out)]
    [(string? expr)
     (out-as-bytes expr 
                   string->bytes/utf-8
                   CPT_CHAR_STRING
                   (string-length expr)
                   out)]
    [(bytes? expr)
     (out-as-bytes expr 
                   values
                   CPT_BYTE_STRING
                   #f
                   out)]
    [(path? expr)
     (out-as-bytes expr 
                   path->bytes
                   CPT_PATH
                   #f
                   out)]
    [(char? expr)
     (out-byte CPT_CHAR out)
     (out-number (char->integer expr) out)]
    [(and (exact-integer? expr)
          (and (expr . >= . -1073741824) (expr . <= . 1073741823)))
     (out-byte CPT_INT out)
     (out-number expr out)]
    [(null? expr)
     (out-byte CPT_NULL out)]
    [(eq? expr #t)
     (out-byte CPT_TRUE out)]
    [(eq? expr #f)
     (out-byte CPT_FALSE out)]
    [(void? expr)
     (out-byte CPT_VOID out)]
    [(box? expr)
     (out-byte CPT_BOX out)
     (out-data (unbox expr) out)]
    [(pair? expr)
     (out-byte CPT_LIST out)
     (out-number 1 out)
     (out-data (car expr) out)
     (out-data (cdr expr) out)]
    [(vector? expr)
     (out-byte CPT_VECTOR out)
     (out-number (vector-length expr) out)
     (for ([v (in-vector expr)])
       (out-data v out))]
    [(hash? expr)
     (out-byte CPT_HASH_TABLE out)
     (out-number (cond
                   [(hash-eqv? expr) 2]
                   [(hash-eq? expr) 0]
                   [else 1])
                 out)
     (out-number (hash-count expr) out)
     (for ([(k v) (in-hash expr)])
       (out-data k out)
       (out-data v out))]
    [(svector? expr)
     (out-byte CPT_SVECTOR out)
     (out-number (vector-length (svector-vec expr)) out)
     (let ([vec (svector-vec expr)])
       (for ([n (in-range (sub1 (vector-length vec)) -1 -1)])
         (out-number (vector-ref vec n) out)))]
    [(module-path-index? expr)
     (out-shared expr out 
                 (lambda ()
                   (out-byte CPT_MODULE_INDEX out)
                   (let-values ([(name base) (module-path-index-split expr)])
                     (out-data name out)
                     (out-data base out))))]
    [(module-decl? expr)
     (out-marshaled module-type-num
                    (module-decl-content expr)
                    out)]
    [(stx? expr)
     (out-stx expr out)]
    [(wrapped? expr)
     (out-wrapped expr out)]      
    [else
     (out-byte CPT_QUOTE out)
     (let ([s (open-output-bytes)])
       (write (if (quoted? expr) 
                  (quoted-v expr)
                  expr) s)
       (out-byte CPT_ESCAPE out)
       (let ([bstr (get-output-bytes s)])
         (out-number (bytes-length bstr) out)
         (out-bytes bstr out)))]))


(define-struct quoted (v) #:prefab)

(define (protect-quote v)
  v
  #;(if (or (list? v) (vector? v) (box? v) (hash? v))
      (make-quoted v)
      v))


(define-struct svector (vec))

;; ----------------------------------------

