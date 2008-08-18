#lang scheme
(require compiler/zo-parse
         syntax/modcollapse)

(provide decompile)

;; ----------------------------------------

(define primitive-table
  ;; Figure out number-to-id mapping for kernel functions in `primitive'
  (let ([bindings
         (let ([ns (make-base-empty-namespace)])
           (parameterize ([current-namespace ns])
             (namespace-require ''#%kernel)
             (for/list ([l (namespace-mapped-symbols)])
               (cons l (with-handlers ([exn:fail? (lambda (x) #f)])
                         (compile l))))))]
        [table (make-hash)])
    (for ([b (in-list bindings)])
      (let ([v (and (cdr b)
                    (zo-parse (let-values ([(in out) (make-pipe)])
                                (write (cdr b) out)
                                (close-output-port out)
                                in)))])
        (let ([n (match v
                   [(struct compilation-top (_ prefix (struct primitive (n)))) n]
                   [else #f])])
          (hash-set! table n (car b)))))
    table))

(define (list-ref/protect l pos)
  (list-ref l pos)
  #;
  (if (pos . < . (length l))
      (list-ref l pos)
      'OUT-OF-BOUNDS))

;; ----------------------------------------

;; Main entry:
(define (decompile top)
  (match top
    [(struct compilation-top (_ prefix (and (? mod?) mod)))
     (decompile-module mod)]
    [(struct compilation-top (_ prefix form))
     (let-values ([(globs defns) (decompile-prefix prefix)])
       `(begin
          ,@defns
          ,(decompile-form form globs '(#%prefix))))]
    [else (error 'decompile "unrecognized: ~e" top)]))

(define (decompile-prefix a-prefix)
  (match a-prefix
    [(struct prefix (num-lifts toplevels stxs))
     (let ([lift-ids (for/list ([i (in-range num-lifts)])
                    (gensym 'lift))]
           [stx-ids (map (lambda (i) (gensym 'stx)) 
                         stxs)])
       (values (append 
                (map (lambda (tl)
                       (match tl
                         [(struct global-bucket (name)) name]
                         [(struct module-variable (modidx sym pos phase))
                          (if (and (module-path-index? modidx)
                                   (let-values ([(n b) (module-path-index-split modidx)])
                                     (and (not n) (not b))))
                              sym
                              (string->symbol (format "~s@~s~a" sym (mpi->string modidx) 
                                                      (if (zero? phase)
                                                          ""
                                                          (format "/~a" phase)))))]
                         [else (error 'decompile-prefix "bad toplevel: ~e" tl)]))
                     toplevels)
                stx-ids
                (if (null? stx-ids) null '(#%stx-array))
                lift-ids)
               (map (lambda (stx id)
                      `(define ,id (decode-stx ,(stx-encoded stx))))
                    stxs stx-ids)))]
    [else (error 'decompile-prefix "huh?: ~e" a-prefix)]))

(define (mpi->string modidx)
  (cond
   [(symbol? modidx) modidx]
   [else (collapse-module-path-index modidx (current-directory))]))

(define (decompile-module mod-form)
  (match mod-form
    [(struct mod (name self-modidx prefix provides requires body syntax-body))
     (let-values ([(globs defns) (decompile-prefix prefix)])
       `(module ,name ....
          ,@defns
          ,@(map (lambda (form)
                   (decompile-form form globs '(#%prefix)))
                 syntax-body)
          ,@(map (lambda (form)
                   (decompile-form form globs '(#%prefix)))
                 body)))]
    [else (error 'decompile-module "huh?: ~e" mod-form)]))

(define (decompile-form form globs stack)
  (match form
    [(struct def-values (ids rhs))
     `(define-values ,(map (lambda (tl)
                             (match tl
                               [(struct toplevel (depth pos flags))
                                (list-ref/protect globs pos)]))
                           ids)
        ,(decompile-expr rhs globs stack))]
    [(struct def-syntaxes (ids rhs prefix max-let-depth))
     `(define-syntaxes ,ids
        ,(let-values ([(globs defns) (decompile-prefix prefix)])
           `(let ()
              ,@defns
              ,(decompile-expr rhs globs '(#%prefix)))))]
    [(struct def-for-syntax (ids rhs prefix max-let-depth))
     `(define-values-for-syntax ,ids
        ,(let-values ([(globs defns) (decompile-prefix prefix)])
           `(let ()
             ,@defns
             ,(decompile-expr rhs globs '(#%prefix)))))]
    [(struct sequence (forms))
     `(begin ,@(map (lambda (form)
                      (decompile-form form globs stack))
                    forms))]
    [(struct splice (forms))
     `(begin ,@(map (lambda (form)
                      (decompile-form form globs stack))
                    forms))]
    [else
     (decompile-expr form globs stack)]))

(define (extract-name name)
  (if (symbol? name)
      (gensym name)
      (if (vector? name)
          (gensym (vector-ref name 0))
          #f)))

(define (extract-id expr)
  (match expr
    [(struct lam (name flags num-params rest? closure-map max-let-depth body))
     (extract-name name)]
    [(struct case-lam (name lams))
     (extract-name name)]
    [(struct closure (lam gen-id))
     (extract-id lam)]
    [(struct indirect (v))
     (extract-id v)]
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
             
(define (decompile-expr expr globs stack)
  (match expr
    [(struct toplevel (depth pos flags))
     (list-ref/protect globs pos)]
    [(struct topsyntax (depth pos midpt))
     (list-ref/protect globs (+ midpt pos))]
    [(struct primitive (id))
     (hash-ref primitive-table id)]
    [(struct assign (id rhs undef-ok?))
     `(set! ,(decompile-expr id globs stack)
            ,(decompile-expr rhs globs stack))]
    [(struct localref (unbox? offset flags))
     (let ([id (list-ref/protect stack offset)])
       (if unbox?
           `(#%unbox ,id)
           id))]
    [(struct lam (name flags num-params rest? closure-map max-let-depth body))
     (let ([vars (for/list ([i (in-range num-params)]) (gensym 'arg))]
           [rest-vars (if rest? (list (gensym 'rest)) null)])
       `(lambda (,@vars . ,(if rest?
                               (car rest-vars)
                               null))
          ,@(if name
                `(',name)
                null)
          ,(decompile-expr body globs (append
                                       (map (lambda (v)
                                              (list-ref/protect stack v))
                                            (vector->list closure-map))
                                       (append vars rest-vars)))))]
    [(struct let-one (rhs body))
     (let ([id (or (extract-id rhs)
                   (gensym 'local))])
       `(let ([,id ,(decompile-expr rhs globs (cons id stack))])
          ,(decompile-expr body globs (cons id stack))))]
    [(struct let-void (count boxes? body))
     (let ([ids (make-vector count #f)])
       (extract-ids! body ids)
       (let ([vars (for/list ([i (in-range count)]
                              [id (in-vector ids)])
                     (or id (gensym 'localv)))])
         `(let ,(map (lambda (i) `[,i ,(if boxes? `(#%box ?) '?)])
                     vars)
            ,(decompile-expr body globs (append vars stack)))))]
    [(struct let-rec (procs body))
     `(begin
        (set!-rec-values ,(for/list ([p (in-list procs)]
                                     [i (in-naturals)])
                            (list-ref/protect stack i))
                         ,@(map (lambda (proc)
                                  (decompile-expr proc globs stack))
                                procs))
        ,(decompile-expr body globs stack))]
    [(struct install-value (count pos boxes? rhs body))
     `(begin
        (,(if boxes? '#%set-boxes! 'set!-values)
         ,(for/list ([i (in-range count)])
            (list-ref/protect stack (+ i pos)))
         ,(decompile-expr rhs globs stack))
        ,(decompile-expr body globs stack))]
    [(struct boxenv (pos body))
     (let ([id (list-ref/protect stack pos)])
       `(begin
          (set! ,id (#%box ,id))
          ,(decompile-expr body globs stack)))]
    [(struct branch (test then else))
     `(if ,(decompile-expr test globs stack)
          ,(decompile-expr then globs stack)
          ,(decompile-expr else globs stack))]
    [(struct application (rator rands))
     (let ([stack (append (for/list ([i (in-list rands)]) (gensym 'rand))
                          stack)])
       `(,(decompile-expr rator globs stack)
         ,@(map (lambda (rand)
                  (decompile-expr rand globs stack))
                rands)))]
    [(struct apply-values (proc args-expr))
     `(apply-values ,(decompile-expr proc globs stack) 
                    ,(decompile-expr args-expr globs stack))]
    [(struct sequence (exprs))
     `(begin ,@(for/list ([expr (in-list exprs)])
                 (decompile-expr expr globs stack)))]
    [(struct beg0 (exprs))
     `(begin0 ,@(for/list ([expr (in-list exprs)])
                  (decompile-expr expr globs stack)))]
    [(struct closure (lam gen-id))
     `(CLOSED ,gen-id ,(decompile-expr lam globs stack))]
    [(struct indirect (val))
     (if (closure? val)
         (closure-gen-id val)
         '???)]
    [else `(quote ,expr)]))

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
