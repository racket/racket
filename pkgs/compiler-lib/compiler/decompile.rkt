#lang racket/base
(require racket/linklet
         compiler/zo-parse
         compiler/zo-marshal
         syntax/modcollapse
         racket/port
         racket/match
         racket/list
         racket/set
         racket/path
         (only-in '#%linklet compiled-position->primitive)
         "private/deserialize.rkt")

(provide decompile)

;; ----------------------------------------

(define primitive-table
  (let ([value-names (let ([ns (make-base-empty-namespace)])
                       (parameterize ([current-namespace ns])
                         (namespace-require ''#%kernel)
                         (namespace-require ''#%unsafe)
                         (namespace-require ''#%flfxnum)
                         (namespace-require ''#%extfl)
                         (namespace-require ''#%futures)
                         (namespace-require ''#%foreign)
                         (namespace-require ''#%paramz)
                         (namespace-require ''#%linklet)
                         (for/hasheq ([name (in-list (namespace-mapped-symbols))])
                           (values (namespace-variable-value name #t (lambda () #f))
                                   name))))])
    (for/hash ([i (in-naturals)]
               #:break (not (compiled-position->primitive i)))
      (define v (compiled-position->primitive i))
      (values i (or (hash-ref value-names v #f) `',v)))))

(define (list-ref/protect l pos who)
  (list-ref l pos)
  #;
  (if (pos . < . (length l))
      (list-ref l pos)
      `(OUT-OF-BOUNDS ,who ,pos ,(length l) ,l)))

;; ----------------------------------------

(define-struct glob-desc (vars))

;; Main entry:
(define (decompile top #:to-linklets? [to-linklets? #f])
  (cond
    [(linkl-directory? top)
     (cond
       [to-linklets?
        (cons
         'linklet-directory
         (apply
          append
          (for/list ([(k v) (in-hash (linkl-directory-table top))])
            (list '#:name k '#:bundle (decompile v #:to-linklets? to-linklets?)))))]
       [else
        (define main (hash-ref (linkl-directory-table top) '() #f))
        (cond
          [(and main
                (hash-ref (linkl-bundle-table main) 'decl #f))
           (decompile-module-with-submodules top '() main)]
          [main
           (decompile-single-top main)]
          [else
           (decompile-multi-top top)])])]
    [(linkl-bundle? top)
     (cond
       [to-linklets?
        (cons
         'linklet-bundle
         (apply
          append
          (for/list ([(k v) (in-hash (linkl-bundle-table top))])
            (case (and (not to-linklets?) k)
              [(stx-data)
               (list '#:stx-data (decompile-data-linklet v))]
              [else
               (list '#:key k '#:value (decompile v #:to-linklets? to-linklets?))]))))]
       [else
        (decompile-module top)])]
    [(linkl? top)
     (decompile-linklet top)]
    [(faslable-correlated-linklet? top)
     (strip-correlated (faslable-correlated-linklet-expr top))]
    [else `(quote ,top)]))

(define (decompile-module-with-submodules l-dir name-list main-l)
  (decompile-module main-l
                    (lambda ()
                      (for/list ([(k l) (in-hash (linkl-directory-table l-dir))]
                                 #:when  (and (list? k)
                                              (= (length k) (add1 (length name-list)))
                                              (for/and ([s1 (in-list name-list)]
                                                        [s2 (in-list k)])
                                                (eq? s1 s2))))
                        (decompile-module-with-submodules l-dir k l)))))

(define (decompile-module l [get-nested (lambda () '())])
  (define ht (linkl-bundle-table l))
  (define phases (sort (for/list ([k (in-hash-keys ht)]
                                  #:when (exact-integer? k))
                         k)
                       <))
  (define-values (mpi-vector requires provides)
    (let ([data-l (hash-ref ht 'data #f)]
          [decl-l (hash-ref ht 'decl #f)])
      (define (zo->linklet l)
        (cond
          [(faslable-correlated-linklet? l)
           (compile-linklet (strip-correlated (faslable-correlated-linklet-expr l))
                            (faslable-correlated-linklet-name l))]
          [else
           (let ([o (open-output-bytes)])
             (zo-marshal-to (linkl-bundle (hasheq 'data l)) o)
             (parameterize ([read-accept-compiled #t])
               (define b (read (open-input-bytes (get-output-bytes o))))
               (hash-ref (linklet-bundle->hash b) 'data)))]))
      (cond
        [(and data-l
              decl-l)
         (define data-i (instantiate-linklet (zo->linklet data-l)
                                             (list deserialize-instance)))
         (define decl-i (instantiate-linklet (zo->linklet decl-l)
                                             (list deserialize-instance
                                                   data-i)))
         (values (instance-variable-value data-i '.mpi-vector)
                 (instance-variable-value decl-i 'requires)
                 (instance-variable-value decl-i 'provides))]
        [else (values '#() '() '#hasheqv())])))
  (define (phase-wrap phase l)
    (case phase
      [(0) l]
      [(1) `((for-syntax ,@l))]
      [(-1) `((for-template ,@l))]
      [(#f) `((for-label ,@l))]
      [else `((for-meta ,phase ,@l))]))
  `(module ,(hash-ref ht 'name 'unknown) ....
     (require ,@(apply
                 append
                 (for/list ([phase+mpis (in-list requires)])
                   (phase-wrap (car phase+mpis)
                               (map collapse-module-path-index (cdr phase+mpis))))))
     (provide ,@(apply
                 append
                 (for/list ([(phase ht) (in-hash provides)])
                   (phase-wrap phase (hash-keys ht)))))
     ,@(let loop ([phases phases] [depth 0])
         (cond
           [(null? phases) '()]
           [(= depth (car phases))
            (append
             (decompile-linklet (hash-ref ht (car phases)) #:just-body? #t)
             (loop (cdr phases) depth))]
           [else
            (define l (loop phases (add1 depth)))
            (define (convert-syntax-definition s wrap)
              (match s
                [`(let ,bindings ,body)
                 (convert-syntax-definition body
                                            (lambda (rhs)
                                              `(let ,bindings
                                                 ,rhs)))]
                [`(begin (.set-transformer! ',id ,rhs) ',(? void?))
                 `(define-syntaxes ,id ,(wrap rhs))]
                [`(begin (.set-transformer! ',ids ,rhss) ... ',(? void?))
                 `(define-syntaxes ,ids ,(wrap `(values . ,rhss)))]
                [_ #f]))
            (let loop ([l l] [accum '()])
              (cond
                [(null? l) (if (null? accum)
                               '()
                               `((begin-for-syntax ,@(reverse accum))))]
                [(convert-syntax-definition (car l) values)
                 => (lambda (s)
                      (append (loop null accum)
                              (cons s (loop (cdr l) null))))]
                [else
                 (loop (cdr l) (cons (car l) accum))]))]))
     ,@(get-nested)
     ,@(let ([l (hash-ref ht 'stx-data #f)])
         (if l
             `((begin-for-all
                 (define (.get-syntax-literal! pos)
                   ....
                   ,(decompile-data-linklet l)
                   ....)))
             null))))

(define (decompile-single-top b)
  (define forms (let ([l (hash-ref (linkl-bundle-table b) 0 #f)])
                  (if l
                      (decompile-linklet l #:just-body? #t)
                      '(<opaque-compiled-linklet>))))
  (if (= (length forms) 1)
      (car forms)
      `(begin ,@forms)))

(define (decompile-multi-top ld)
  `(begin
     ,@(let loop ([i 0])
         (define b (hash-ref (linkl-directory-table ld) (list (string->symbol (format "~a" i))) #f))
         (define l (and b (hash-ref (linkl-bundle-table b) 0 #f)))
         (cond
           [l (append (decompile-linklet l #:just-body? #t)
                      (loop (add1 i)))]
           [else null]))))

(define (decompile-linklet l #:just-body? [just-body? #f])
  (match l
    [(struct linkl (name importss import-shapess exports internals lifts source-names body max-let-depth needs-instance?))
     (define closed (make-hasheq))
     (define globs (glob-desc
                    (append
                     (list 'root)
                     (apply append importss)
                     exports
                     internals
                     lifts)))
     (define body-l
       (for/list ([form (in-list body)])
         (decompile-form form globs '(#%globals) closed)))
     (if just-body?
         body-l
         `(linklet
           ,importss
           ,exports
           '(import-shapes: ,@(for/list ([imports (in-list importss)]
                                         [import-shapes (in-list import-shapess)]
                                         #:when #t
                                         [import (in-list imports)]
                                         [import-shape (in-list import-shapes)]
                                         #:when import-shape)
                                `[,import ,import-shape]))
           '(source-names: ,source-names)
           ,@body-l))]
    [(struct faslable-correlated-linklet (expr name))
     (match (strip-correlated expr)
       [`(linklet ,imports ,exports ,body-l ...)
        body-l])]))

(define (decompile-data-linklet l)
  (match l
    [(struct linkl (_ _ _ _ _ _ _ (list vec-def (struct def-values (_ deser-lam))) _ _))
     (match deser-lam
       [(struct lam (_ _ _ _ _ _ _ _ _ (struct seq ((list vec-copy! _)))))
        (match vec-copy!
          [(struct application (_ (list _ _ (struct application (_ (list mpi-vector inspector bulk-binding-registry
                                                                         num-mutables mutable-vec
                                                                         num-shares share-vec
                                                                         mutable-fill-vec
                                                                         result-vec))))))
           (decompile-deserialize '.mpi-vector '.inspector '.bulk-binding-registry
                                  num-mutables mutable-vec
                                  num-shares share-vec
                                  mutable-fill-vec
                                  result-vec)]
           [else
            (decompile-linklet l)])]
       [else
        (decompile-linklet l)])]
    [(struct faslable-correlated-linklet (expr name))
     (match (strip-correlated expr)
       [`(linklet ,_ ,_
           ,_
           (define-values ,_
             (lambda ,_
               (begin
                 (vector-copy! ,_ ,_ (let-values ([(.inspector) #f])
                                       (let-values ([(data)
                                                     '#(,mutable-vec ,share-vec ,mutable-fill-vec ,result-vec)])
                                         (deserialize .mpi-vector .inspector .bulk-binding-registry
                                                      ',num-mutables (,_ data 0)
                                                      ',num-shares (,_ data 1)
                                                      (,_ data 2)
                                                      (,_ data 3)))))
                 ,_))))
        (decompile-deserialize '.mpi-vector '.inspector '.bulk-binding-registry
                               num-mutables mutable-vec
                               num-shares share-vec
                               mutable-fill-vec
                               result-vec)]
       [else
        (decompile-linklet l)])]
    [else
     (decompile-linklet l)]))
     
(define (decompile-form form globs stack closed)
  (match form
    [(struct def-values (ids rhs))
     `(define-values ,(map (lambda (tl)
                             (match tl
                               [(struct toplevel (depth pos const? set-const?))
                                (list-ref/protect (glob-desc-vars globs) pos 'def-vals)]))
                           ids)
        ,(if (inline-variant? rhs)
             `(begin
                ,(list 'quote '%%inline-variant%%)
                ,(decompile-expr (inline-variant-inline rhs) globs stack closed)
                ,(decompile-expr (inline-variant-direct rhs) globs stack closed))
             (decompile-expr rhs globs stack closed)))]
    [(struct seq (forms))
     `(begin ,@(map (lambda (form)
                      (decompile-form form globs stack closed))
                    forms))]
    [else
     (decompile-expr form globs stack closed)]))

(define (extract-name name)
  (if (symbol? name)
      (gensym name)
      (if (vector? name)
          (gensym (vector-ref name 0))
          #f)))

(define (extract-id expr)
  (match expr
    [(struct lam (name flags num-params arg-types rest? closure-map closure-types tl-map max-let-depth body))
     (extract-name name)]
    [(struct case-lam (name lams))
     (extract-name name)]
    [(struct closure (lam gen-id))
     (extract-id lam)]
    [else #f]))

(define (extract-ids! body ids)
  (match body
    [(struct let-rec (procs body))
     (for ([proc (in-list procs)]
           [delta (in-naturals)])
       (when (< -1 delta (vector-length ids))
         (vector-set! ids delta (extract-id proc))))
     (extract-ids! body ids)]
    [(struct install-value (val-count pos boxes? rhs body))
     (extract-ids! body ids)]
    [(struct boxenv (pos body))
     (extract-ids! body ids)]
    [else #f]))

(define (decompile-tl expr globs stack closed no-check?)
  (match expr
    [(struct toplevel (depth pos const? ready?))
     (let ([id (list-ref/protect (glob-desc-vars globs) pos 'toplevel)])
       (cond
        [no-check? id]
        [(and (not const?) (not ready?))
         `(#%checked ,id)]
        #;[(and const? ready?) `(#%const ,id)]
        #;[const? `(#%iconst ,id)]
        [else id]))]))

(define (decompile-expr expr globs stack closed)
  (match expr
    [(struct toplevel (depth pos const? ready?))
     (decompile-tl expr globs stack closed #f)]
    [(struct varref (tl dummy constant? from-unsafe?))
     `(#%variable-reference . ,(cond
                                 [(not tl) '()]
                                 [(eq? tl #t) '(<constant-local>)]
                                 [(symbol? tl) (list tl)] ; primitive
                                 [else (list (decompile-tl tl globs stack closed #t))]))]
    [(struct primval (id))
     (hash-ref primitive-table id (lambda () (error "unknown primitive: " id)))]
    [(struct assign (id rhs undef-ok?))
     `(set! ,(decompile-expr id globs stack closed)
            ,(decompile-expr rhs globs stack closed))]
    [(struct localref (unbox? offset clear? other-clears? type))
     (let ([id (list-ref/protect stack offset 'localref)])
       (let ([e (if unbox?
                    `(#%unbox ,id)
                    id)])
         (if clear?
             `(#%sfs-clear ,e)
             e)))]
    [(? lam?)
     `(lambda . ,(decompile-lam expr globs stack closed))]
    [(struct case-lam (name lams))
     `(case-lambda
       ,@(map (lambda (lam)
                (decompile-lam lam globs stack closed))
              lams))]
    [(struct let-one (rhs body type unused?))
     (let ([id (or (extract-id rhs)
                   (gensym (or type (if unused? 'unused 'local))))])
       `(let ([,id ,(decompile-expr rhs globs (cons id stack) closed)])
          ,(decompile-expr body globs (cons id stack) closed)))]
    [(struct let-void (count boxes? body))
     (let ([ids (make-vector count #f)])
       (extract-ids! body ids)
       (let ([vars (for/list ([i (in-range count)]
                              [id (in-vector ids)])
                     (or id (gensym (if boxes? 'localvb 'localv))))])
         `(let ,(map (lambda (i) `[,i ,(if boxes? `(#%box ?) '?)])
                     vars)
            ,(decompile-expr body globs (append vars stack) closed))))]
    [(struct let-rec (procs body))
     `(begin
        (#%set!-rec-values ,(for/list ([p (in-list procs)]
                                       [i (in-naturals)])
                              (list-ref/protect stack i 'let-rec))
                           ,@(map (lambda (proc)
                                    (decompile-expr proc globs stack closed))
                                  procs))
        ,(decompile-expr body globs stack closed))]
    [(struct install-value (count pos boxes? rhs body))
     `(begin
        (,(if boxes? '#%set-boxes! 'set!-values)
         ,(for/list ([i (in-range count)])
            (list-ref/protect stack (+ i pos) 'install-value))
         ,(decompile-expr rhs globs stack closed))
        ,(decompile-expr body globs stack closed))]
    [(struct boxenv (pos body))
     (let ([id (list-ref/protect stack pos 'boxenv)])
       `(begin
          (set! ,id (#%box ,id))
          ,(decompile-expr body globs stack closed)))]
    [(struct branch (test then else))
     `(if ,(decompile-expr test globs stack closed)
          ,(decompile-expr then globs stack closed)
          ,(decompile-expr else globs stack closed))]
    [(struct application (rator rands))
     (let ([stack (append (for/list ([i (in-list rands)]) (gensym 'rand))
                          stack)])
       (annotate-unboxed
        rands
        (annotate-inline
         `(,(decompile-expr rator globs stack closed)
           ,@(map (lambda (rand)
                    (decompile-expr rand globs stack closed))
                  rands)))))]
    [(struct apply-values (proc args-expr))
     `(#%apply-values ,(decompile-expr proc globs stack closed) 
                      ,(decompile-expr args-expr globs stack closed))]
    [(struct with-immed-mark (key-expr val-expr body-expr))
     (let ([id (gensym 'cmval)])
       `(#%call-with-immediate-continuation-mark
         ,(decompile-expr key-expr globs stack closed)
         (lambda (,id) ,(decompile-expr body-expr globs (cons id stack) closed))
         ,(decompile-expr val-expr globs stack closed)))]
    [(struct seq (exprs))
     `(begin ,@(for/list ([expr (in-list exprs)])
                 (decompile-expr expr globs stack closed)))]
    [(struct beg0 (exprs))
     `(begin0
       ,@(for/list ([expr (in-list exprs)])
           (decompile-expr expr globs stack closed))
       ;; Make sure a single expression doesn't look like tail position:
       ,@(if (null? (cdr exprs)) (list #f) null))]
    [(struct with-cont-mark (key val body))
     `(with-continuation-mark
          ,(decompile-expr key globs stack closed)
          ,(decompile-expr val globs stack closed)
          ,(decompile-expr body globs stack closed))]
    [(struct closure (lam gen-id))
     (if (hash-ref closed gen-id #f)
         gen-id
         (begin
           (hash-set! closed gen-id #t)
           `(#%closed ,gen-id ,(decompile-expr lam globs stack closed))))]
    [(? void?) (list 'void)]
    [else `(quote ,expr)]))

(define (decompile-lam expr globs stack closed)
  (match expr
    [(struct closure (lam gen-id)) (decompile-lam lam globs stack closed)]
    [(struct lam (name flags num-params arg-types rest? closure-map closure-types tl-map max-let-depth body))
     (let ([vars (for/list ([i (in-range num-params)]
                            [type (in-list arg-types)])
                   (gensym (format "~a~a-" 
                                   (case type 
                                     [(ref) "argbox"] 
                                     [(val) "arg"]
                                     [else (format "arg~a" type)])
                                   i)))]
           [rest-vars (if rest? (list (gensym 'rest)) null)]
           [captures (map (lambda (v)
                            (list-ref/protect stack v 'lam))
                          (vector->list closure-map))])
       `((,@vars . ,(if rest?
                        (car rest-vars)
                        null))
         ,@(if (and name (not (null? name)))
               `(',name)
               null)
         ,@(if (null? flags) null `('(flags: ,@flags)))
         ,@(if (null? captures)
               null
               `('(captures: ,@(map (lambda (c t)
                                      (if t
                                          `(,t ,c)
                                          c))
                                    captures
                                    closure-types)
                             ,@(if (not tl-map)
                                   '()
                                   (list
                                    (for/list ([pos (in-list (sort (set->list tl-map) <))])
                                      (list-ref/protect (glob-desc-vars globs)
                                                        pos
                                                        'lam)))))))
         ,(decompile-expr body globs
                          (append captures
                                  (append vars rest-vars))
                          closed)))]))

(define (annotate-inline a)
  a)

(define (annotate-unboxed args a)
  a)

;; ----------------------------------------

(define (decompile-deserialize mpis inspector bulk-binding-registry
                               num-mutables mutable-vec
                               num-shares share-vec
                               mutable-fill-vec
                               result-vec)
  ;; Names for shared values:
  (define shared (for/vector ([i (in-range (+ num-mutables num-shares))])
                   (string->symbol (format "~a:~a"
                                           (if (i . < . num-mutables)
                                               'mutable
                                               'shared)
                                           i))))
  (define (infer-name! d i)
    (when (pair? d)
      (define new-name
        (case (car d)
          [(deserialize-scope) 'scope]
          [(srcloc) 'srcloc]
          [else #f]))
      (when new-name
        (vector-set! shared i (string->symbol (format "~a:~a" new-name i))))))

  (define mutables (make-vector num-mutables #f))
  ;; Make mutable shells
  (for/fold ([pos 0]) ([i (in-range num-mutables)])
    (define-values (d next-pos)
      (decode-shell mutable-vec pos mpis inspector bulk-binding-registry shared))
    (vector-set! mutables i d)
    (infer-name! d i)
    next-pos)
  
  ;; Construct shared values
  (define shareds (make-vector num-shares #f))
  (for/fold ([pos 0]) ([i (in-range num-shares)])
    (define-values (d next-pos)
      (decode share-vec pos mpis inspector bulk-binding-registry shared))
    (vector-set! shareds i d)
    (infer-name! d (+ i num-mutables))
    next-pos)
  
  ;; Fill in mutable shells
  (define-values (fill-pos rev-fills)
    (for/fold ([pos 0] [rev-fills null]) ([i (in-range num-mutables)]
                                          [v (in-vector shared)])
      (define-values (fill next-pos)
        (decode-fill! v mutable-fill-vec pos mpis inspector bulk-binding-registry shared))
      (values next-pos (if fill
                           (cons fill rev-fills)
                           rev-fills))))
  
  ;; Construct the final result
  (define-values (result done-pos)
    (decode result-vec 0 mpis inspector bulk-binding-registry shared))

  `(let (,(for/list ([i (in-range num-mutables)])
            `(,(vector-ref shared i) ,(vector-ref mutables i))))
    (let* (,(for/list ([i (in-range num-shares)])
              `(,(vector-ref shared (+ i num-mutables)) ,(vector-ref shareds i))))
      ,@(reverse rev-fills)
      ,result)))

;; Decode the construction of a mutable variable
(define (decode-shell vec pos mpis inspector bulk-binding-registry shared)
  (case (vector-ref vec pos)
    [(#:box) (values (list 'box #f) (add1 pos))]
    [(#:vector) (values `(make-vector ,(vector-ref vec (add1 pos))) (+ pos 2))]
    [(#:hash) (values (list 'make-hasheq) (add1 pos))]
    [(#:hasheq) (values (list 'make-hasheq) (add1 pos))]
    [(#:hasheqv) (values (list 'make-hasheqv) (add1 pos))]
    [else (decode vec pos mpis inspector bulk-binding-registry shared)]))

;; The decoder that is used for most purposes
(define (decode vec pos mpis inspector bulk-binding-registry shared)
  (define-syntax decodes
    (syntax-rules ()
      [(_ (id ...) rhs) (decodes #:pos (add1 pos) (id ...) rhs)]
      [(_ #:pos pos () rhs) (values rhs pos)]
      [(_ #:pos pos ([#:ref id0] id ...) rhs)
       (let-values ([(id0 next-pos) (let ([i (vector-ref vec pos)])
                                      (if (exact-integer? i)
                                          (values (vector-ref shared i) (add1 pos))
                                          (decode vec pos mpis inspector bulk-binding-registry shared)))])
         (decodes #:pos next-pos (id ...) rhs))]
      [(_ #:pos pos (id0 id ...) rhs)
       (let-values ([(id0 next-pos) (decode vec pos mpis inspector bulk-binding-registry shared)])
         (decodes #:pos next-pos (id ...) rhs))]))
  (define-syntax-rule (decode* (deser id ...))
    (decodes (id ...) `(deser ,id ...)))
  (case (vector-ref vec pos)
    [(#:ref)
     (values (vector-ref shared (vector-ref vec (add1 pos)))
             (+ pos 2))]
    [(#:inspector) (values inspector (add1 pos))]
    [(#:bulk-binding-registry) (values bulk-binding-registry (add1 pos))]
    [(#:syntax #:datum->syntax)
     (decodes
      (content [#:ref context] [#:ref srcloc])
      `(deserialize-syntax
        ,content
        ,context
        ,srcloc
        #f
        #f
        ,inspector))]
    [(#:syntax+props)
     (decodes
      (content [#:ref context] [#:ref srcloc] props tamper)
      `(deserialize-syntax
        ,content
        ,context
        ,srcloc
        ,props
        ,tamper
        ,inspector))]
    [(#:srcloc)
     (decode* (srcloc source line column position span))]
    [(#:quote)
     (values (vector-ref vec (add1 pos)) (+ pos 2))]
    [(#:mpi)
     (values `(vector-ref ,mpis ,(vector-ref vec (add1 pos)))
             (+ pos 2))]
    [(#:box)
     (decode* (box-immutable v))]
    [(#:cons)
     (decode* (cons a d))]
    [(#:list #:vector #:set #:seteq #:seteqv)
     (define len (vector-ref vec (add1 pos)))
     (define r (make-vector len))
     (define next-pos
       (for/fold ([pos (+ pos 2)]) ([i (in-range len)])
         (define-values (v next-pos) (decodes #:pos pos (v) v))
         (vector-set! r i v)
         next-pos))
     (values `(,(case (vector-ref vec pos)
                  [(#:list) 'list]
                  [(#:vector) 'vector]
                  [(#:set) 'set]
                  [(#:seteq) 'seteq]
                  [(#:seteqv) 'seteqv])
               ,@(vector->list r))
             next-pos)]
    [(#:hash #:hasheq #:hasheqv)
     (define len (vector-ref vec (add1 pos)))
     (define-values (l next-pos)
       (for/fold ([l null] [pos (+ pos 2)]) ([i (in-range len)])
         (decodes #:pos pos (k v) (list* v k l))))
     (values `(,(case (vector-ref vec pos)
                  [(#:hash) 'hash]
                  [(#:hasheq) 'hasheq]
                  [(#:hasheqv) 'hasheqv])
               ,@(reverse l))
             next-pos)]
    [(#:prefab)
     (define-values (key next-pos) (decodes #:pos (add1 pos) (k) k))
     (define len (vector-ref vec next-pos))
     (define-values (r done-pos)
       (for/fold ([r null] [pos (add1 next-pos)]) ([i (in-range len)])
         (decodes #:pos pos (v) (cons v r))))
     (values `(make-prefab-struct ',key ,@(reverse r))
             done-pos)]
    [(#:scope)
     (decode* (deserialize-scope))]
    [(#:scope+kind)
     (decode* (deserialize-scope kind))]
    [(#:multi-scope)
     (decode* (deserialize-multi-scope name scopes))]
    [(#:shifted-multi-scope)
     (decode* (deserialize-shifted-multi-scope phase multi-scope))]
    [(#:table-with-bulk-bindings)
     (decode* (deserialize-table-with-bulk-bindings syms bulk-bindings))]
    [(#:bulk-binding-at)
     (decode* (deserialize-bulk-binding-at scopes bulk))]
    [(#:representative-scope)
     (decode* (deserialize-representative-scope kind phase))]
    [(#:module-binding)
     (decode* (deserialize-full-module-binding
               module sym phase
               nominal-module
               nominal-phase
               nominal-sym
               nominal-require-phase
               free=id
               extra-inspector
               extra-nominal-bindings))]
    [(#:simple-module-binding)
     (decode* (deserialize-simple-module-binding module sym phase nominal-module))]
    [(#:local-binding)
     (decode* (deserialize-full-local-binding key free=id))]
    [(#:bulk-binding)
     (decode* (deserialize-bulk-binding prefix excepts mpi provide-phase-level phase-shift bulk-binding-registry))]
    [(#:provided)
     (decode* (deserialize-provided binding protected? syntax?))]
    [else
     (values `(quote ,(vector-ref vec pos)) (add1 pos))]))

;; Decode the filling of mutable values, which has its own encoding
;; variant
(define (decode-fill! v vec pos mpis inspector bulk-binding-registry shared)
  (case (vector-ref vec pos)
    [(#f) (values #f (add1 pos))]
    [(#:set-box!)
     (define-values (c next-pos)
       (decode vec (add1 pos) mpis inspector bulk-binding-registry shared))
     (values `(set-box! ,v ,c)
             next-pos)]
    [(#:set-vector!)
     (define len (vector-ref vec (add1 pos)))
     (define-values (l next-pos)
       (for/fold ([l null] [pos (+ pos 2)]) ([i (in-range len)])
         (define-values (c next-pos)
           (decode vec pos mpis inspector bulk-binding-registry shared))
         (values (cons `(vector-set! ,v ,i ,c) l)
                 next-pos)))
     (values `(begin ,@(reverse l)) next-pos)]
    [(#:set-hash!)
     (define len (vector-ref vec (add1 pos)))
     (define-values (l next-pos)
       (for/fold ([l null] [pos (+ pos 2)]) ([i (in-range len)])
         (define-values (key next-pos)
           (decode vec pos mpis inspector bulk-binding-registry shared))
         (define-values (val done-pos)
           (decode vec next-pos mpis inspector bulk-binding-registry shared))
         (values (cons `(hash-set! ,v ,key ,val) l)
                 done-pos)))
     (values `(begin ,@(reverse l)) next-pos)]
    [(#:scope-fill!)
     (define-values (c next-pos)
       (decode vec (add1 pos) mpis inspector bulk-binding-registry shared))
     (values `(deserialize-scope-fill! ,v ,c)
             next-pos)]
    [(#:representative-scope-fill!)
     (define-values (a next-pos)
       (decode vec (add1 pos) mpis inspector bulk-binding-registry shared))
     (define-values (d done-pos)
       (decode vec next-pos mpis inspector bulk-binding-registry shared))
     (values `(deserialize-representative-scope-fill! ,v ,a ,d)
             done-pos)]
    [else
     (error 'deserialize "bad fill encoding: ~v" (vector-ref vec pos))]))
  
;; ----------------------------------------

(struct faslable-correlated-linklet (expr name)
  #:prefab)

(struct faslable-correlated (e source position line column span props)
  #:prefab)

(define (strip-correlated v)
  (let strip ([v v])
    (cond
      [(pair? v)
       (cons (strip (car v))
             (strip (cdr v)))]
      [(faslable-correlated? v)
       (strip (faslable-correlated-e v))]
      [else v])))

;; ----------------------------------------

#;
(begin
  (require scheme/pretty)
  (define (try e)
    (pretty-print
     (decompile
      (zo-parse (let-values ([(in out) (make-pipe)])
                  (write (parameterize ([current-namespace (make-base-namespace)])
                           (compile e))
                         out)
                  (close-output-port out)
                  in)))))
  (pretty-print
   (decompile
    (zo-parse (open-input-file "/home/mflatt/proj/plt/collects/tests/mzscheme/benchmarks/common/sboyer_ss.zo"))))
  #;
  (try '(lambda (q . more)
          (letrec ([f (lambda (x) f)])
            (lambda (g) f)))))
