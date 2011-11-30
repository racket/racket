#lang scheme/base
(require compiler/zo-parse
         syntax/modcollapse
         scheme/port
         scheme/match
         racket/set)

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
             (namespace-require ''#%futures)
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
                   [(struct compilation-top (_ prefix (struct primval (n)))) n]
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
      [(struct compilation-top (max-let-depth prefix form))
       (let-values ([(globs defns) (decompile-prefix prefix stx-ht)])
         `(begin
            ,@defns
            ,(decompile-form form globs '(#%globals) (make-hasheq) stx-ht)))]
      [else (error 'decompile "unrecognized: ~e" top)])))

(define (decompile-prefix a-prefix stx-ht)
  (match a-prefix
    [(struct prefix (num-lifts toplevels stxs))
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
                          [(struct module-variable (modidx sym pos phase))
                           (if (and (module-path-index? modidx)
                                    (let-values ([(n b) (module-path-index-split modidx)])
                                      (and (not n) (not b))))
                               (string->symbol (format "_~a" sym))
                               (string->symbol (format "_~s@~s~a" sym (mpi->string modidx) 
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
               (map (lambda (stx id)
                      `(define ,id ,(if stx
                                        `(#%decode-syntax 
                                          ,(decompile-stx (stx-encoded stx) stx-ht))
                                        #f)))
                    stxs stx-ids)))]
    [else (error 'decompile-prefix "huh?: ~e" a-prefix)]))

(define (decompile-stx stx stx-ht)
  (or (hash-ref stx-ht stx #f)
      (let ([p (mcons #f #f)])
        (hash-set! stx-ht stx p)
        (match stx
          [(wrapped datum wraps tamper-status)
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
                         (let loop ([wraps wraps])
                           (cond
                            [(null? wraps) null]
                            [else
                             (or (hash-ref stx-ht wraps #f)
                                 (let ([p (mcons #f #f)])
                                   (hash-set! stx-ht wraps p)
                                   (set-mcar! p (decompile-wrap (car wraps) stx-ht))
                                   (set-mcdr! p (loop (cdr wraps)))
                                   p))]))))
           p]))))

(define (decompile-wrap w stx-ht)
  (or (hash-ref stx-ht w #f)
      (let ([v (match w
                 [(lexical-rename has-free-id-renames?
                                  ignored
                                  alist)
                  `(,(if has-free-id-renames? 'lexical/free-id=? 'lexical) . ,alist)]
                 [(phase-shift amt src dest)
                  `(phase-shift ,amt ,src ,dest)]
                 [(wrap-mark val)
                  val]
                 [(prune sym)
                  `(prune ,sym)]
                 [(module-rename phase kind set-id unmarshals renames mark-renames plus-kern?)
                  `(module-rename ,phase ,kind ,set-id ,unmarshals ,renames ,mark-renames ,plus-kern?)]
                 [(top-level-rename flag)
                  `(top-level-rename ,flag)]
                 [else w])])
        (hash-set! stx-ht w v)
        v)))

(define (mpi->string modidx)
  (cond
   [(symbol? modidx) modidx]
   [else (collapse-module-path-index modidx (current-directory))]))

(define (decompile-module mod-form stack stx-ht)
  (match mod-form
    [(struct mod (name srcname self-modidx prefix provides requires body syntax-bodies unexported 
                       max-let-depth dummy lang-info internal-context))
     (let-values ([(globs defns) (decompile-prefix prefix stx-ht)]
                  [(stack) (append '(#%modvars) stack)]
                  [(closed) (make-hasheq)])
       `(module ,name ....
          ,@defns
          ,@(for/list ([b (in-list syntax-bodies)])
              (let loop ([n (sub1 (car b))])
                (if (zero? n)
                    (cons 'begin
                          (for/list ([form (in-list (cdr b))])
                            (decompile-form form globs stack closed stx-ht)))
                    (list 'begin-for-syntax (loop (sub1 n))))))
          ,@(map (lambda (form)
                   (decompile-form form globs stack closed stx-ht))
                 body)))]
    [else (error 'decompile-module "huh?: ~e" mod-form)]))

(define (decompile-form form globs stack closed stx-ht)
  (match form
    [(? mod?)
     (decompile-module form stack stx-ht)]
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
     (hash-ref primitive-table id)]
    [(struct assign (id rhs undef-ok?))
     `(set! ,(decompile-expr id globs stack closed)
            ,(decompile-expr rhs globs stack closed))]
    [(struct localref (unbox? offset clear? other-clears? flonum?))
     (let ([id (list-ref/protect stack offset 'localref)])
       (let ([e (if unbox?
                    `(#%unbox ,id)
                    id)])
         (if clear?
             `(#%sfs-clear ,e)
             (if flonum?
                 `(#%from-flonum ,e)
                 e))))]
    [(? lam?)
     `(lambda . ,(decompile-lam expr globs stack closed))]
    [(struct case-lam (name lams))
     `(case-lambda
       ,@(map (lambda (lam)
                (decompile-lam lam globs stack closed))
              lams))]
    [(struct let-one (rhs body flonum? unused?))
     (let ([id (or (extract-id rhs)
                   (gensym (if unused? 'unused 'local)))])
       `(let ([,id ,(let ([v (decompile-expr rhs globs (cons id stack) closed)])
                      (if flonum?
                          (list '#%as-flonum v)
                          v))])
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
    [(struct seq (exprs))
     `(begin ,@(for/list ([expr (in-list exprs)])
                 (decompile-expr expr globs stack closed)))]
    [(struct beg0 (exprs))
     `(begin0 ,@(for/list ([expr (in-list exprs)])
                  (decompile-expr expr globs stack closed)))]
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
                                   (case type [(ref) "argbox"] [(flonum) "argfl"] [else "arg"])
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
                                      (if (eq? t 'flonum)
                                          `(flonum ,c)
                                          c))
                                    captures
                                    closure-types)
                             ,@(if (not tl-map)
                                   '()
                                   (list
                                    (for/list ([pos (in-set tl-map)])
                                      (list-ref/protect (glob-desc-vars globs)
                                                        (if (or (pos . < . (glob-desc-num-tls globs))
                                                                (zero? (glob-desc-num-stxs globs)))
                                                            pos
                                                            (+ pos (glob-desc-num-stxs globs) 1))
                                                        'lam)))))))
         ,(decompile-expr body globs
                          (append captures
                                  (append vars rest-vars))
                          closed)))]))

(define (annotate-inline a)
  (if (and (symbol? (car a))
           (case (length a)
             [(2) (memq (car a) '(not null? pair? mpair? symbol?
                                      syntax? char? boolean?
                                      number? real? exact-integer?
                                      fixnum? inexact-real?
                                      procedure? vector? box? string? bytes? eof-object?
                                      zero? negative? exact-nonnegative-integer?  
                                      exact-positive-integer?
                                      car cdr caar cadr cdar cddr
                                      mcar mcdr unbox vector-length syntax-e
                                      add1 sub1 - abs bitwise-not
                                      list list* vector vector-immutable box))]
             [(3) (memq (car a) '(eq? = <= < >= >
                                      bitwise-bit-set? char=?
                                      + - * / quotient remainder min max bitwise-and bitwise-ior bitwise-xor
                                      arithmetic-shift vector-ref string-ref bytes-ref
                                      set-mcar! set-mcdr! cons mcons set-box!
                                      list list* vector vector-immutable))]
             [(4) (memq (car a) '(vector-set! string-set! bytes-set!
                                              list list* vector vector-immutable
                                              + - * / min max bitwise-and bitwise-ior bitwise-xor))]
             [else (memq (car a) '(list list* vector vector-immutable
                                        + - * / min max bitwise-and bitwise-ior bitwise-xor))]))
      (cons '#%in a)
      a))

(define (annotate-unboxed args a)
  (define (unboxable? e s)
    (cond
     [(localref? e) #t]
     [(toplevel? e) #t]
     [(eq? '#%flonum (car s)) #t]
     [(not (expr? e)) #t]
     [else #f]))
  (if (and (symbol? (car a))
           (case (length a)
             [(2) (memq (car a) '(flabs flsqrt ->fl
                                        unsafe-flabs
                                        unsafe-flsqrt
                                        unsafe-fx->fl
                                        flsin flcos fltan
                                        flasin flacos flatan
                                        flexp fllog
                                        flfloor flceiling flround fltruncate
                                        flmin flmax
                                        unsafe-flmin unsafe-flmax))]
             [(3) (memq (car a) '(fl+ fl- fl* fl/
                                      fl< fl> fl<= fl>= fl=
                                      flvector-ref
                                      unsafe-fl+ unsafe-fl- unsafe-fl* unsafe-fl/
                                      unsafe-fl< unsafe-fl>
                                      unsafe-fl=
                                      unsafe-fl<= unsafe-fl>=
                                      unsafe-flvector-ref
                                      unsafe-f64vector-ref))]
             
             [(4) (memq (car a) '(flvector-set!
                                  unsafe-flvector-set!
                                  unsafe-f64vector-set!))]
             [else #f])
           (andmap unboxable? args (cdr a)))
      (cons '#%flonum a)
      a))

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
