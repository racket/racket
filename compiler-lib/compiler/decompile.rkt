#lang racket/base
(require compiler/zo-parse
         syntax/modcollapse
         racket/port
         racket/match
         racket/list
         racket/set
         racket/path)

(provide decompile)

;; ----------------------------------------

(define primitive-table
  ;; Figure out number-to-id mapping for kernel functions in `primitive'
  (let ([bindings
         (let ([ns (make-base-empty-namespace)])
           (parameterize ([current-namespace ns])
             (namespace-require ''#%kernel)
             (namespace-require ''#%unsafe)
             (namespace-require ''#%flfxnum)
             (namespace-require ''#%extfl)
             (namespace-require ''#%futures)
             (namespace-require ''#%foreign)
             (for/list ([l (namespace-mapped-symbols)])
               (cons l (with-handlers ([exn:fail? (lambda (x) #f)])
                         (compile l))))))]
        [table (make-hash)])
    (for ([b (in-list bindings)])
      (let ([v (and (cdr b)
                    (zo-parse 
                     (open-input-bytes
                      (with-output-to-bytes
                          (λ () (write (cdr b)))))))])
        (let ([n (match v
                   [(struct compilation-top (_ _ prefix (struct primval (n)))) n]
                   [else #f])])
          (hash-set! table n (car b)))))
    table))

(define (list-ref/protect l pos who)
  (list-ref l pos)
  #;
  (if (pos . < . (length l))
      (list-ref l pos)
      `(OUT-OF-BOUNDS ,who ,pos ,(length l) ,l)))

;; ----------------------------------------

(define-struct glob-desc (vars num-tls num-stxs num-lifts))

;; Main entry:
(define (decompile top)
  (let ([stx-ht (make-hasheq)])
    (match top
      [(struct compilation-top (max-let-depth binding-namess prefix form))
       (let-values ([(globs defns) (decompile-prefix prefix stx-ht)])
         (expose-module-path-indexes
          `(begin
            ,@defns
            ,(decompile-form form globs '(#%globals) (make-hasheq) stx-ht))))]
      [else (error 'decompile "unrecognized: ~e" top)])))

(define (expose-module-path-indexes e)
  ;; This is a nearly general replace-in-graph function. (It seems like a lot
  ;; of work to expose module path index content and sharing, though.)
  (define ht (make-hasheq))
  (define mconses null)
  (define (x-mcons a b)
    (define m (mcons a b))
    (set! mconses (cons (cons m (cons a b)) mconses))
    m)
  (define main
    (let loop ([e e])
      (cond
       [(hash-ref ht e #f)]
       [(module-path-index? e)
        (define ph (make-placeholder #f))
        (hash-set! ht e ph)
        (define-values (name base) (module-path-index-split e))
        (placeholder-set! ph (x-mcons '#%modidx
                                      (x-mcons (loop name)
                                               (x-mcons (loop base)
                                                        null))))
        ph]
       [(pair? e)
        (define ph (make-placeholder #f))
        (hash-set! ht e ph)
        (placeholder-set! ph (cons (loop (car e))
                                   (loop (cdr e))))
        ph]
       [(mpair? e)
        (define m (mcons #f #f))
        (hash-set! ht e m)
        (set! mconses (cons (cons m (cons (loop (mcar e))
                                          (loop (mcdr e))))
                            mconses))
        m]
       [(box? e)
        (define ph (make-placeholder #f))
        (hash-set! ht e ph)
        (placeholder-set! ph (box (loop (unbox e))))
        ph]
       [(vector? e)
        (define ph (make-placeholder #f))
        (hash-set! ht e ph)
        (placeholder-set! ph
                          (for/vector #:length (vector-length e) ([i (in-vector e)])
                                      (loop i)))
        ph]
       [(hash? e)
        (define ph (make-placeholder #f))
        (hash-set! ht e ph)
        (placeholder-set! ph
                          ((cond
                            [(hash-eq? ht)
                             make-hasheq-placeholder]
                            [(hash-eqv? ht)
                             make-hasheqv-placeholder]
                            [else make-hash-placeholder])
                           (for/list ([(k v) (in-hash e)])
                             (cons (loop k) (loop v)))))
        ph]
       [(prefab-struct-key e)
        => (lambda (k)
             (define ph (make-placeholder #f))
             (hash-set! ht e ph)
             (placeholder-set! ph
                               (apply make-prefab-struct
                                      k
                                      (map loop
                                           (cdr (vector->list (struct->vector e))))))
             ph)]
       [else
        e])))
  (define l (make-reader-graph (cons main mconses)))
  (for ([i (in-list (cdr l))])
    (set-mcar! (car i) (cadr i))
    (set-mcdr! (car i) (cddr i)))
  (car l))

(define (decompile-prefix a-prefix stx-ht)
  (match a-prefix
    [(struct prefix (num-lifts toplevels stxs src-insp-desc))
     (let ([lift-ids (for/list ([i (in-range num-lifts)])
                       (gensym 'lift))]
           [stx-ids (map (lambda (i) (gensym 'stx)) 
                         stxs)])
       (values (glob-desc 
                (append 
                 (map (lambda (tl)
                        (match tl
                          [#f '#%linkage]
                          [(? symbol?) (string->symbol (format "_~a" tl))]
                          [(struct global-bucket (name)) 
                           (string->symbol (format "_~a" name))]
                          [(struct module-variable (modidx sym pos phase constantness))
                           (if (and (module-path-index? modidx)
                                    (let-values ([(n b) (module-path-index-split modidx)])
                                      (and (not n) (not b))))
                               (string->symbol (format "_~a" sym))
                               (string->symbol (format "_~s~a@~s~a" 
                                                       sym 
                                                       (match constantness
                                                         ['constant ":c"]
                                                         ['fixed ":f"]
                                                         [(function-shape a pm?) 
                                                          (if pm? ":P" ":p")]
                                                         [(struct-type-shape c) ":t"]
                                                         [(constructor-shape a) ":mk"]
                                                         [(predicate-shape) ":?"]
                                                         [(accessor-shape c) ":ref"]
                                                         [(mutator-shape c) ":set!"]
                                                         [else ""])
                                                       (mpi->string modidx) 
                                                       (if (zero? phase)
                                                           ""
                                                           (format "/~a" phase)))))]
                          [else (error 'decompile-prefix "bad toplevel: ~e" tl)]))
                      toplevels)
                 stx-ids
                 (if (null? stx-ids) null '(#%stx-array))
                 lift-ids)
                (length toplevels)
                (length stxs)
                num-lifts)
               (list*
                `(quote inspector ,src-insp-desc)
                ;; `(quote tls ,toplevels)
                (map (lambda (stx id)
                       `(define ,id ,(if stx
                                         `(#%decode-syntax 
                                           ,(decompile-stx (stx-content stx) stx-ht))
                                         #f)))
                     stxs stx-ids))))]
    [else (error 'decompile-prefix "huh?: ~e" a-prefix)]))

(define (decompile-stx stx stx-ht)
  (or (hash-ref stx-ht stx #f)
      (let ([p (mcons #f #f)])
        (hash-set! stx-ht stx p)
        (match stx
          [(stx-obj datum wrap srcloc props tamper-status)
           (set-mcar! p (case tamper-status
                          [(clean) 'wrap]
                          [(tainted) 'wrap-tainted]
                          [(armed) 'wrap-armed]))
           (set-mcdr! p (mcons
                         (cond
                          [(pair? datum) 
                           (cons (decompile-stx (car datum) stx-ht)
                                 (let loop ([l (cdr datum)])
                                   (cond
                                    [(null? l) null]
                                    [(pair? l)
                                     (cons (decompile-stx (car l) stx-ht)
                                           (loop (cdr l)))]
                                    [else
                                     (decompile-stx l stx-ht)])))]
                          [(vector? datum)
                           (for/vector ([e (in-vector datum)])
                             (decompile-stx e stx-ht))]
                          [(box? datum)
                           (box (decompile-stx (unbox datum) stx-ht))]
                          [else datum])
                         (let* ([l (mcons wrap null)]
                                [l (if (hash-count props)
                                       (mcons props l)
                                       l)]
                                [l (if srcloc
                                       (mcons srcloc l)
                                       l)])
                           l)))
           p]))))

(define (mpi->string modidx)
  (cond
   [(symbol? modidx) modidx]
   [else 
    (collapse-module-path-index modidx)]))

(define (decompile-module mod-form orig-stack stx-ht mod-name)
  (match mod-form
    [(struct mod (name srcname self-modidx
                       prefix provides requires body syntax-bodies unexported 
                       max-let-depth dummy lang-info 
                       internal-context binding-names
                       flags pre-submodules post-submodules))
     (let-values ([(globs defns) (decompile-prefix prefix stx-ht)]
                  [(stack) (append '(#%modvars) orig-stack)]
                  [(closed) (make-hasheq)])
       `(,mod-name ,(if (symbol? name) name (last name)) ....
           (quote self ,self-modidx)
           (quote internal-context 
                  ,(if (stx? internal-context)
                       `(#%decode-syntax 
                         ,(decompile-stx (stx-content internal-context) stx-ht))
                       internal-context))
           (quote bindings ,(for/hash ([(phase ht) (in-hash binding-names)])
                              (values phase
                                      (for/hash ([(sym id) (in-hash ht)])
                                        (values sym
                                                (if (eq? id #t)
                                                    #t
                                                    `(#%decode-syntax 
                                                      ,(decompile-stx (stx-content id) stx-ht))))))))
           (quote language-info ,lang-info)
           ,@(if (null? flags) '() (list `(quote ,flags)))
           ,@(let ([l (apply
                       append
                       (for/list ([req (in-list requires)]
                                  #:when (pair? (cdr req)))
                         (define l (for/list ([mpi (in-list (cdr req))])
                                     (define p (mpi->string mpi))
                                     (if (path? p)
                                         (let ([d (current-load-relative-directory)])
                                           (path->string (if d
                                                             (find-relative-path (simplify-path d #t)
                                                                                 (simplify-path p #f) 
                                                                                 #:more-than-root? #t)
                                                             p)))
                                         p)))
                         (if (eq? 0 (car req))
                             l
                             `((,@(case (car req)
                                    [(#f) `(for-label)]
                                    [(1) `(for-syntax)]
                                    [else `(for-meta ,(car req))])
                                ,@l)))))])
               (if (null? l)
                   null
                   `((require ,@l))))
           (provide ,@(apply
                       append
                       (for/list ([p (in-list provides)])
                         (define phase (car p))
                         (define l
                           (for/list ([pv (in-list (append (cadr p) (caddr p)))])
                             (match pv
                               [(struct provided (name src src-name nom-src src-phase protected?))
                                (define n (if (eq? name src-name)
                                              name
                                              `(rename-out [,src-name ,name])))
                                (if protected?
                                    `(protect-out ,n)
                                    n)])))
                         (if (or (null? l) (eq? phase 0))
                             l
                             `((,@(case phase
                                    [(#f) `(for-label)]
                                    [(1) `(for-syntax)]
                                    [else `(for-meta ,phase)])
                                ,@l))))))
          ,@defns
          ,@(for/list ([submod (in-list pre-submodules)])
              (decompile-module submod orig-stack stx-ht 'module))
          ,@(for/list ([b (in-list syntax-bodies)])
              (let loop ([n (sub1 (car b))])
                (if (zero? n)
                    (cons 'begin
                          (for/list ([form (in-list (cdr b))])
                            (decompile-form form globs stack closed stx-ht)))
                    (list 'begin-for-syntax (loop (sub1 n))))))
          ,@(map (lambda (form)
                   (decompile-form form globs stack closed stx-ht))
                 body)
          ,@(for/list ([submod (in-list post-submodules)])
              (decompile-module submod orig-stack stx-ht 'module*))))]
    [else (error 'decompile-module "huh?: ~e" mod-form)]))

(define (decompile-form form globs stack closed stx-ht)
  (match form
    [(? mod?)
     (decompile-module form stack stx-ht 'module)]
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
    [(struct def-syntaxes (ids rhs prefix max-let-depth dummy))
     `(define-syntaxes ,ids
        ,(let-values ([(globs defns) (decompile-prefix prefix stx-ht)])
           `(let ()
              ,@defns
              ,(decompile-form rhs globs '(#%globals) closed stx-ht))))]
    [(struct seq-for-syntax (exprs prefix max-let-depth dummy))
     `(begin-for-syntax
       ,(let-values ([(globs defns) (decompile-prefix prefix stx-ht)])
          `(let ()
             ,@defns
             ,@(for/list ([rhs (in-list exprs)])
                 (decompile-form rhs globs '(#%globals) closed stx-ht)))))]
    [(struct seq (forms))
     `(begin ,@(map (lambda (form)
                      (decompile-form form globs stack closed stx-ht))
                    forms))]
    [(struct splice (forms))
     `(begin ,@(map (lambda (form)
                      (decompile-form form globs stack closed stx-ht))
                    forms))]
    [(struct req (reqs dummy))
     `(#%require . (#%decode-syntax ,reqs))]
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
    [(struct varref (tl dummy))
     `(#%variable-reference ,(if (eq? tl #t)
                                 '<constant-local>
                                 (decompile-tl tl globs stack closed #t)))]
    [(struct topsyntax (depth pos midpt))
     (list-ref/protect (glob-desc-vars globs) (+ midpt pos) 'topsyntax)]
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
                                      (define tl-pos
                                        (cond
                                         [(or (pos . < . (glob-desc-num-tls globs))
                                              (zero? (glob-desc-num-stxs globs)))
                                          pos]
                                         [(= pos (glob-desc-num-tls globs))
                                          'stx]
                                         [else
                                          (+ pos (glob-desc-num-stxs globs))]))
                                      (if (eq? tl-pos 'stx)
                                          '#%syntax
                                          (list-ref/protect (glob-desc-vars globs)
                                                            tl-pos
                                                            'lam))))))))
         ,(decompile-expr body globs
                          (append captures
                                  (append vars rest-vars))
                          closed)))]))

(define (annotate-inline a)
  a)

(define (annotate-unboxed args a)
  a)

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
