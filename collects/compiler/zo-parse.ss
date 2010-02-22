#lang scheme/base
(require mzlib/etc 
         scheme/match
         scheme/list
         compiler/zo-structs)

(provide zo-parse)
(provide (all-from-out compiler/zo-structs))

#| Unresolved Issues

  The order of indirect-et-provides, indirect-syntax-provides, indirect-provides was changed, is that okay?
 
  orig-port of cport struct is never used, is it needed?

  Lines 628, 630 seem to be only for debugging and should probably throw errors

  unmarshal-stx-get also seems to be for debugging and should probably throw an error

  vector and pair cases of decode-wraps seem to do different things from the corresponding C code

  Line 816: This should be an eqv placeholder (but they don't exist)

  Line 634: Export registry is always matched as false, but might not be

  What are the real differences between the module-binding cases?

  I think parse-module-path-index was only used for debugging, so it is short-circuited now

 collects/browser/compiled/browser_scrbl.zo (eg) contains a all-from-module that looks like: (#<module-path-index> 0 (1363072) . #f) --- that doesn't seem to match the spec

  We seem to leave placeholders for hash-tables in the structs

|#
;; ----------------------------------------
;; Bytecode unmarshalers for various forms

(define (read-toplevel v)
  (define SCHEME_TOPLEVEL_CONST #x01)
  (define SCHEME_TOPLEVEL_READY #x02)
  (match v
    [(cons depth (cons pos flags))
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
  (let-values ([(v unsafe?) (if (integer? (car v))
                                (values v #f)
                                (values (cdr v) #t))])
    (match v
      [`(,i ,tv . ,sv)
       ; XXX Why not leave them as vectors and change the contract?
       (make-prefix i (vector->list tv) (vector->list sv))])))

(define (read-unclosed-procedure v)
  (define CLOS_HAS_REST 1)
  (define CLOS_HAS_REF_ARGS 2)
  (define CLOS_PRESERVES_MARKS 4)
  (define CLOS_IS_METHOD 16)
  (define CLOS_SINGLE_RESULT 32)
  (define BITS_PER_MZSHORT 32)
  (match v
    [`(,flags ,num-params ,max-let-depth ,name ,v . ,rest)
     (let ([rest? (positive? (bitwise-and flags CLOS_HAS_REST))])
       (let*-values ([(closure-size closed-over body)
                      (if (zero? (bitwise-and flags CLOS_HAS_REF_ARGS))
                          (values (vector-length v) v rest)
                          (values v (car rest) (cdr rest)))]
                     [(check-bit) (lambda (i)
                                    (if (zero? (bitwise-and flags CLOS_HAS_REF_ARGS))
                                        0
                                        (let ([byte (vector-ref closed-over
                                                                (+ closure-size (quotient (* 2 i) BITS_PER_MZSHORT)))])
                                          (+ (if (bitwise-bit-set? byte (remainder (* 2 i) BITS_PER_MZSHORT))
                                                 1
                                                 0)
                                             (if (bitwise-bit-set? byte (add1 (remainder (* 2 i) BITS_PER_MZSHORT)))
                                                 2
                                                 0)))))]
                     [(arg-types) (let ([num-params ((if rest? sub1 values) num-params)])
                                    (for/list ([i (in-range num-params)]) 
                                      (case (check-bit i)
                                        [(0) 'val]
                                        [(1) 'ref]
                                        [(2) 'flonum])))]
                     [(closure-types) (for/list ([i (in-range closure-size)]
                                                 [j (in-naturals num-params)])
                                        (case (check-bit j)
                                          [(0) 'val/ref]
                                          [(2) 'flonum]))])
         (make-lam name
                   (append
                    (if (zero? (bitwise-and flags flags CLOS_PRESERVES_MARKS)) null '(preserves-marks))
                    (if (zero? (bitwise-and flags flags CLOS_IS_METHOD)) null '(is-method))
                    (if (zero? (bitwise-and flags flags CLOS_SINGLE_RESULT)) null '(single-result)))
                   ((if rest? sub1 values) num-params)
                   arg-types
                   rest?
                   (if (= closure-size (vector-length closed-over))
                       closed-over
                       (let ([v2 (make-vector closure-size)])
                         (vector-copy! v2 0 closed-over 0 closure-size)
                         v2))
                   closure-types
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

; XXX Allocates unnessary list
(define (read-define-syntaxes mk v)
  (mk (list-tail (vector->list v) 4)
      (vector-ref v 0)
      (vector-ref v 1)
      (vector-ref v 2)
      #;(vector-ref v 3)))

(define (read-define-syntax v)
  (read-define-syntaxes make-def-syntaxes v))

(define (read-define-for-syntax v)
  (read-define-syntaxes make-def-for-syntax v))

(define (read-set! v)
  (make-assign (cadr v) (cddr v) (car v)))

(define (read-case-lambda v)
  (make-case-lam (car v) (cdr v)))

(define (read-begin0 v) 
  (match v
    [(struct seq (exprs))
     (make-beg0 exprs)]))

(define (read-boxenv v)
  (make-boxenv (car v) (cdr v)))
(define (read-require v)
  (make-req (cdr v) (car v)))
(define (read-#%variable-ref v)
  (make-varref v))
(define (read-apply-values v)
  (make-apply-values (car v) (cdr v)))
(define (read-splice v)
  (make-splice (seq-forms v)))

(define (in-list* l n)
  (make-do-sequence
   (lambda ()
     (values (lambda (l) (apply values (take l n)))
             (lambda (l) (drop l n))
             l
             (lambda (l) (>= (length l) n))
             (lambda _ #t)
             (lambda _ #t)))))

(define (read-module v)
  (match v
    [`(,name ,self-modidx ,lang-info ,functional? ,et-functional?
             ,rename ,max-let-depth ,dummy
             ,prefix
             ,indirect-et-provides ,num-indirect-et-provides 
             ,indirect-syntax-provides ,num-indirect-syntax-provides 
             ,indirect-provides ,num-indirect-provides 
             ,protects ,et-protects
             ,provide-phase-count . ,rest)
     (let ([phase-data (take rest (* 9 provide-phase-count))])
       (match (list-tail rest (* 9 provide-phase-count))
         [`(,syntax-body ,body
                         ,requires ,syntax-requires ,template-requires ,label-requires
                         ,more-requires-count . ,more-requires)
          (make-mod name self-modidx
                    prefix (let loop ([l phase-data])
                             (if (null? l)
                                 null
                                 (let ([num-vars (list-ref l 7)]
                                       [ps (for/list ([name (in-vector (list-ref l 6))]
                                                      [src (in-vector (list-ref l 5))]
                                                      [src-name (in-vector (list-ref l 4))]
                                                      [nom-src (or (list-ref l 3)
                                                                   (in-cycle (in-value #f)))]
                                                      [src-phase (or (list-ref l 2)
                                                                     (in-cycle (in-value #f)))]
                                                      [protected? (or (case (car l)
                                                                        [(0) protects]
                                                                        [(1) et-protects]
                                                                        [else #f])
                                                                      (in-cycle (in-value #f)))]
                                                      [insp (or (list-ref l 1)
                                                                (in-cycle (in-value #f)))])
                                             (make-provided name src src-name 
                                                            (or nom-src src)
                                                            (if src-phase 1 0)
                                                            protected?
                                                            insp))])
                                   (if (null? ps)
                                       (loop (list-tail l 9))
                                       (cons
                                        (list
                                         (car l)
                                         (take ps num-vars)
                                         (drop ps num-vars))
                                        (loop (list-tail l 9)))))))
                    (list*
                     (cons 0 requires)
                     (cons 1 syntax-requires)
                     (cons -1 template-requires)
                     (cons #f label-requires)
                     (for/list ([(phase reqs) (in-list* more-requires 2)])
                       (cons phase reqs)))
                    (vector->list body)
                    (map (lambda (sb)
                           (match sb
                             [(? def-syntaxes?) sb]
                             [(? def-for-syntax?) sb]
                             [`#(,ids ,expr ,max-let-depth ,prefix ,for-stx?)
                              ((if for-stx?
                                   make-def-for-syntax
                                   make-def-syntaxes)
                               (if (list? ids) ids (list ids)) expr prefix max-let-depth)]))
                         (vector->list syntax-body))
                    (list (vector->list indirect-provides)
                          (vector->list indirect-syntax-provides)
                          (vector->list indirect-et-provides))
                    max-let-depth
                    dummy
                    lang-info
                    rename)]))]))
(define (read-module-wrap v)
  v)

;; ----------------------------------------
;; Unmarshal dispatch for various types

(define (read-more-syntax v)
  (let ([id (car v)]
        [v (cdr v)])
    ;; This is the ..._EXPD mapping from "schpriv.h":
    (case id
      [(0) (read-define-values v)]
      [(1) (read-define-syntax v)]
      [(2) (read-set! v)]
      [(3) v] ; a case-lam already
      [(4) (read-begin0 v)]
      [(5) (read-boxenv v)]
      [(6) (read-module-wrap v)]
      [(7) (read-require v)]
      [(8) (read-define-for-syntax v)]
      [(9) (read-#%variable-ref v)]
      [(10) (read-apply-values v)]
      [(11) (read-splice v)]
      [else (error 'read-mode-unsyntax "unknown id: ~e" id)])))

;; Type mappings from "stypes.h":
(define (int->type i)
  (case i
    [(0) 'toplevel-type]
    [(3) 'syntax-type]
    [(7) 'sequence-type]
    [(9) 'unclosed-procedure-type]
    [(10) 'let-value-type]
    [(11) 'let-void-type]
    [(12) 'letrec-type]
    [(14) 'with-cont-mark-type]
    [(15) 'quote-syntax-type]
    [(24) 'variable-type]
    [(25) 'module-variable-type]
    [(96) 'case-lambda-sequence-type]
    [(97) 'begin0-sequence-type]
    [(100) 'module-type]
    [(102) 'resolve-prefix-type]
    [else (error 'int->type "unknown type: ~e" i)]))

(define type-readers
  (make-immutable-hash
   (list
    (cons 'toplevel-type read-toplevel)
    (cons 'syntax-type read-more-syntax)
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
    (cons 'begin0-sequence-type read-sequence)
    (cons 'module-type read-module)
    (cons 'resolve-prefix-type read-resolve-prefix))))

(define (get-reader type)
  (or (hash-ref type-readers type #f)
      (lambda (v)
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


(define-struct cport ([pos #:mutable] shared-start orig-port size bytes symtab shared-offsets decoded rns mpis))

(define (cport-rpos cp)
  (+ (cport-pos cp) (cport-shared-start cp)))

(define (cp-getc cp)
  (begin-with-definitions
    (when ((cport-pos cp) . >= . (cport-size cp))
      (error "off the end"))
    (define r
      (bytes-ref (cport-bytes cp) (cport-pos cp)))
    (set-cport-pos! cp (add1 (cport-pos cp)))
    r))

(define small-list-max 65)
(define cpt-table
  ;; The "schcpt.h" mapping
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
    [19 let-one-flonum]
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
    [35 60 small-number]
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

(define (cpt-table-lookup i)
  (for/or ([ent cpt-table])
    (match ent
      [(list k sym) (and (= k i) (cons k sym))]
      [(list k k* sym)
       (and (<= k i)
            (< i k*)
            (cons k sym))])))

(define (read-compact-bytes port c)
  (begin0
    (subbytes (cport-bytes port) (cport-pos port) (+ (cport-pos port) c))
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

(define (make-local unbox? pos flags)
  (define SCHEME_LOCAL_CLEAR_ON_READ #x01)
  (define SCHEME_LOCAL_OTHER_CLEARS #x02)
  (define SCHEME_LOCAL_FLONUM #x03)
  (make-localref unbox? pos 
                 (= flags SCHEME_LOCAL_CLEAR_ON_READ)
                 (= flags SCHEME_LOCAL_OTHER_CLEARS)
                 (= flags SCHEME_LOCAL_FLONUM)))

(define (a . << . b)
  (arithmetic-shift a b))

(define-struct not-ready ())

;; ----------------------------------------
;; Syntax unmarshaling

(define (decode-stx cp v)
  (if (integer? v)
      (let-values ([(v2 decoded?) (unmarshal-stx-get cp v)])
        (if decoded?
            v2
            (let ([v2 (decode-stx cp v2)])
              (unmarshal-stx-set! cp v v2)
              v2)))
      (let loop ([v v])
        (let-values ([(cert-marks v encoded-wraps)
                      (match v
                        [`#((,datum . ,wraps) ,cert-marks) (values cert-marks datum wraps)]
                        [`(,datum . ,wraps) (values #f datum wraps)]
                        [else (error 'decode-wraps "bad datum+wrap: ~e" v)])])
          (let* ([wraps (decode-wraps cp encoded-wraps)]
                 [add-wrap (lambda (v) (make-wrapped v wraps cert-marks))])
            (cond
              [(pair? v)
               (if (eq? #t (car v))
                   ;; Share decoded wraps with all nested parts.
                   (let loop ([v (cdr v)])
                     (cond
                       [(pair? v) 
                        (let ploop ([v v])
                          (cond
                            [(null? v) null]
                            [(pair? v) (add-wrap (cons (loop (car v)) (ploop (cdr v))))]
                            [else (loop v)]))]
                       [(box? v) (add-wrap (box (loop (unbox v))))]
                       [(vector? v)
                        (add-wrap (list->vector (map loop (vector->list v))))]
                       [(prefab-struct-key v)
                        => (lambda (k)
                             (add-wrap
                              (apply
                               make-prefab-struct 
                               k
                               (map loop (cdr (vector->list (struct->vector v)))))))]
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
              [(prefab-struct-key v)
               => (lambda (k)
                    (add-wrap
                     (apply
                      make-prefab-struct 
                      k
                      (map loop (cdr (vector->list (struct->vector v)))))))]
              [else (add-wrap v)]))))))



(define (decode-wraps cp w)
  ; A wraps is either a indirect reference or a list of wrap-elems (from stxobj.c:252)
  (if (integer? w)
      (let-values ([(w2 decoded?) (unmarshal-stx-get cp w)])
        (if decoded?
            w2
            (let ([w2 (decode-wraps cp w2)])
              (unmarshal-stx-set! cp w w2)
              w2)))
      (map (lambda (a)
             (let aloop ([a a])
               ; A wrap-elem is either
               (cond
                 ; A reference 
                 [(integer? a) 
                  (let-values ([(a2 decoded?) (unmarshal-stx-get cp a)])
                    (if decoded?
                        a2
                        (let ([a2 (aloop a2)])
                          (unmarshal-stx-set! cp a a2)
                          a2)))]
                 ; A mark (not actually a number as the C says, but a (list <num>)
                 [(and (pair? a) (null? (cdr a)) (number? (car a)))
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
                                             (let ([results (map (lambda (u)
                                                                   ; u = (list path phase . src-phase)
                                                                   ; or u = (list path phase src-phase exn ... . prefix)
                                                                   (let ([just-phase? (let ([v (cddr u)])
                                                                                        (or (number? v) (not v)))])
                                                                     (let-values ([(exns prefix)
                                                                                   (if just-phase?
                                                                                       (values null #f)
                                                                                       (let loop ([u (if just-phase? null (cdddr u))]
                                                                                                  [a null])
                                                                                         (if (pair? u)
                                                                                             (loop (cdr u) (cons (car u) a))
                                                                                             (values (reverse a) u))))])
                                                                       (make-all-from-module
                                                                        (parse-module-path-index cp (car u))
                                                                        (cadr u)
                                                                        (if just-phase?
                                                                            (cddr u)
                                                                            (caddr u))
                                                                        exns
                                                                        prefix))))
                                                                 unmarshals)])
                                               #;(printf "~nunmarshals: ~S~n" unmarshals)
                                               #;(printf "~nunmarshal results: ~S~n" results)
                                               results)
                                             (decode-renames renames)
                                             mark-renames
                                             (and plus-kern? 'plus-kern)))]
                      [else (error "bad module rename: ~e" a)]))]
                 [(boolean? a)
                  `(#%top-level-rename ,a)]
                 [(symbol? a)
                  '(#%mark-barrier)]
                 [(box? a)
                  (match (unbox a)
                    [(list (? symbol?) ...) (make-prune (unbox a))]
                    [`#(,amt ,src ,dest #f) 
                     (make-phase-shift amt 
                                       (parse-module-path-index cp src)
                                       (parse-module-path-index cp dest))]
                    [else (error 'parse "bad phase shift: ~e" a)])]
                 [else (error 'decode-wraps "bad wrap element: ~e" a)])))
           w)))

(define (in-vector* v n)
  (make-do-sequence
   (λ ()
     (values (λ (i) (vector->values v i (+ i n)))
             (λ (i) (+ i n))
             0
             (λ (i) (>= (vector-length v) (+ i n)))
             (λ _ #t)
             (λ _ #t)))))

(define (decode-renames renames)
  (define decode-nominal-path
    (match-lambda
      [(cons nominal-path (cons import-phase nominal-phase))
       (make-phased-nominal-path nominal-path import-phase nominal-phase)]
      [(cons nominal-path import-phase)
       (make-imported-nominal-path nominal-path import-phase)]
      [nominal-path
       (make-simple-nominal-path nominal-path)]))
  
  ; XXX Weird test copied from C code. Matthew?
  (define (nom_mod_p p)
    (and (pair? p) (not (pair? (cdr p))) (not (symbol? (cdr p))))) 
  
  (for/list ([(k v) (in-vector* renames 2)])
    (cons k 
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
             (make-simple-module-binding module-path-index)]))))

(define (unmarshal-stx-get cp pos)
  (if (pos . >= . (vector-length (cport-symtab cp)))
      (values `(#%bad-index ,pos) #t)
      (let ([v (vector-ref (cport-symtab cp) pos)])
        (if (not-ready? v)
            (let ([save-pos (cport-pos cp)])
              (set-cport-pos! cp (vector-ref (cport-shared-offsets cp) (sub1 pos)))
              (let ([v (read-compact cp)])
                (vector-set! (cport-symtab cp) pos v)
                (set-cport-pos! cp save-pos)
                (values v #f)))
            (values v (vector-ref (cport-decoded cp) pos))))))

(define (unmarshal-stx-set! cp pos v)
  (vector-set! (cport-symtab cp) pos v)
  (vector-set! (cport-decoded cp) pos #t))

(define (parse-module-path-index cp s)
  s)
;; ----------------------------------------
;; Main parsing loop

(define (read-compact cp)
  (let loop ([need-car 0] [proper #f])
    (begin-with-definitions
      (define ch (cp-getc cp))
      (define-values (cpt-start cpt-tag) (let ([x (cpt-table-lookup ch)])
                                           (unless x
                                             (error 'read-compact "unknown code : ~a" ch))
                                           (values (car x) (cdr x))))
      (define v 
        (case cpt-tag
          [(delayed)
           (let ([pos (read-compact-number cp)])
             (let ([v (vector-ref (cport-symtab cp) pos)])
               (if (not-ready? v)
                   (let ([save-pos (cport-pos cp)])
                     (set-cport-pos! cp (vector-ref (cport-shared-offsets cp) (sub1 pos)))
                     (let ([v (read-compact cp)])
                       (vector-set! (cport-symtab cp) pos v)
                       (set-cport-pos! cp save-pos)
                       v))
                   v)))]
          [(escape)
           (let* ([len (read-compact-number cp)]
                  [s (subbytes (cport-bytes cp) (cport-pos cp) (+ (cport-pos cp) len))])
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
                            [read-accept-quasiquote #t])
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
          [(let-one let-one-flonum)
           (make-let-one (read-compact cp) (read-compact cp)
                         (eq? cpt-tag 'let-one-flonum))]
          [(branch)
           (make-branch (read-compact cp) (read-compact cp) (read-compact cp))]
          [(module-index) (module-path-index-join (read-compact cp) (read-compact cp))]
          [(module-var)
           (let ([mod (read-compact cp)]
                 [var (read-compact cp)]
                 [pos (read-compact-number cp)])
             (let-values ([(mod-phase pos)
                           (if (= pos -2)
                               (values 1 (read-compact-number cp))
                               (values 0 pos))])
               (make-module-variable mod var pos mod-phase)))]
          [(local-unbox)
           (let* ([p* (read-compact-number cp)]
                  [p (if (< p* 0)
                         (- (add1 p*))
                         p*)]
                  [flags (if (< p* 0)
                             (read-compact-number cp)
                             0)])
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
                  [lst (for/list ([i (in-range n)])
                         (read-compact cp))])
             (vector->immutable-vector (list->vector lst)))]
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
                ; XXX One of these should be eqv
                [(1) make-hash-placeholder]
                [(2) make-hash-placeholder])
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
           (let* ([l (read-compact-number cp)]
                  [v (vector-ref (cport-symtab cp) l)])
             (if (not-ready? v)
                 (let ([pos (cport-pos cp)])
                   (set-cport-pos! cp (vector-ref (cport-shared-offsets cp) (sub1 l)))
                   (let ([v (read-compact cp)])
                     (set-cport-pos! cp pos)
                     (vector-set! (cport-symtab cp) l v)
                     v))
                 v))]                           
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
           (let* ([l (read-compact-number cp)]
                  [ind (make-indirect #f)])
             (vector-set! (cport-symtab cp) l ind)
             (let* ([v (read-compact cp)]
                    [cl (make-closure v (gensym
                                         (let ([s (lam-name v)])
                                           (cond
                                             [(symbol? s) s]
                                             [(vector? s) (vector-ref s 0)]
                                             [else 'closure]))))])
               (set-indirect-v! ind cl)
               ind))]
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
         (cons v (loop (sub1 need-car) proper))]))))

;; path -> bytes
;; implementes read.c:read_compiled
(define (zo-parse port)
  (begin-with-definitions    
    ;; skip the "#~"
    (unless (equal? #"#~" (read-bytes 2 port))
      (error 'zo-parse "not a bytecode stream"))
    
    (define version (read-bytes (min 63 (read-byte port)) port))
    
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
    
    (define rst (read-bytes size* port))
    
    (unless (eof-object? (read-byte port))
      (error 'not-end))
    
    (unless (= size* (bytes-length rst))
      (error "wrong number of bytes"))
    
    (define symtab (make-vector symtabsize (make-not-ready)))
    
    (define cp (make-cport 0 shared-size port size* rst symtab so* (make-vector symtabsize #f) (make-hash) (make-hash)))
    
    (for/list ([i (in-range 1 symtabsize)])
      (define vv (vector-ref symtab i))
      (when (not-ready? vv)
        (set-cport-pos! cp (vector-ref so* (sub1 i)))
        (let ([v (read-compact cp)])
          (vector-set! symtab i v))))
    (set-cport-pos! cp shared-size)
    (read-marshalled 'compilation-top-type cp)))

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
