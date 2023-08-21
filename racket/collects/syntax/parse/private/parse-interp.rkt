#lang racket/base
(require (for-syntax racket/base
                     racket/list
                     racket/pretty
                     "minimatch.rkt"
                     "rep-attrs.rkt"
                     "rep-data.rkt"
                     "rep-patterns.rkt"
                     (submod "rep-patterns.rkt" simple)
                     "rep.rkt"
                     "opt-interp.rkt"
                     "const-expr.rkt"
                     "kws.rkt")
         racket/stxparam
         "residual.rkt"
         "residual-interp.rkt"
         (except-in "runtime.rkt" fail))

;; ============================================================
;; Pattern Compiler

;; Patterns are compiled to the functions in "Pattern Runtime Support" and a
;; custom backtracking monad.

;; The environment of attribute bindings is represented by a runtime list
;; ("renv" = "reversed environment", ie latest binding at the head of the
;; list). The `renv` contains only the values; the attribute names currently in
;; scope are tracked by the compiler as `aenv`. To evaluate expressions in
;; the scope of the attribute bindings, the environment is reflected to actual
;; attribute bindings using `wrap-expr{,s}`.

;; An AEnv is (Listof (U IAttr #f)). It describes the structure of the
;; corresponding REnv at runtime. A #f entry corresponds to a bundle of wrapped
;; expressions collected together in a vector, used to implement `~do` scoping.
;; The AEnv and REnv are both ordered with the most recent matched first.

(begin-for-syntax

  (define SIMPLIFY? #t)

  ;; compile-pattern : SinglePattern -> Expr[SMatcher]
  (define (compile-pattern p) (compile-*pattern p compile-s))

  ;; compile-hpattern : HeadPattern -> Expr[HMatcher]
  (define (compile-hpattern hp) (compile-*pattern hp compile-h))

  ;; compile-*pattern : *Pattern (*Pattern -> Expr[*Matcher]) -> Expr[*Matcher]
  (define (compile-*pattern p compile-p)
    (define p* (if SIMPLIFY? (simplify p) p))
    ;; (pretty-print p*)
    (define stx (with-reset-bundle (lambda () (F (compile-p p* null)))))
    ;; (pretty-print (syntax->datum stx))
    stx)

  ;; ----------------------------------------
  ;; simplify : *Pattern -> *Pattern

  (struct pat:trivial pat:simple (original) #:prefab)

  (define (simplify p)
    ;; Recognize common simple patterns (no sub-expressions, no effects, succeed
    ;; at most once, etc) for specialized interpreter, more compact expansion.
    (define (simplify-transform p)
      (define (make simple [triv? #f])
        (cond [triv? (pat:trivial (pattern-attrs p) simple p)]
              [else (pat:simple (pattern-attrs p) simple)]))
      (match p
        [(pat:any) (make '_ #t)]
        [(pat:svar name) (make 'var #t)]
        [(pat:integrated name _ '"identifier" (? false/false-expr?))
         (make (if name 'id '_id) #t)]
        [(pat:integrated name _ '"expression" (? false/false-expr?))
         (make (if name 'expr '_expr) #t)]
        [(pat:seq-end) (make 'seq-end #t)]
        [(pat:datum '()) (make '() #t)]
        [(pat:datum d) (make (sim:datum d) #t)]
        [(pat:pair (? pat:simple? hp) (? pat:simple? tp))
         (make (cons (pat:simple-simple hp) (pat:simple-simple tp)))]
        [(pat:dots (list (ehpat attrs (hpat:single (pat:simple _ s)) repc '#f))
                   (pat:simple _ '()))
         (cond [(eq? repc #f)
                (make (sim:dots s (length attrs) #f)
                      (member s '(var id expr)))]
               [(equal? repc repc:plus)
                (make (sim:dots s (length attrs) #t)
                      (member s '(var id expr)))]
               [else p])]
        [(pat:describe (pat:simple _ s) (? datum-expr? desc) transp? (? false/false-expr?))
         (make (sim:describe s (datum-expr-value desc) transp?))]
        [_ p]))
    ;; ... but unconvert isolated trivial patterns, because translation to
    ;; specialized matcher is probably better.
    (define (detrivialize-transform p)
      (match p
        [(pat:trivial _ _ original) original]
        [_ p]))
    (let* ([p (pattern-transform p simplify-transform)]
           [p (pattern-transform p detrivialize-transform)])
      p))

  ;; ----------------------------------------

  ;; Delayed[X] = X | (-> X), where X does not overlap with Procedure
  (define-syntax-rule (D e ...) (lambda () e ...))
  (define (F d) (if (procedure? d) (d) d))

  (define (pattern-attrs* p) (pattern-attrs p #t))

  ;; ----------------------------------------
  ;; compile-s : SinglePattern AEnv -> Delayed[Expr[SMatcher]]
  (define (compile-s p aenv)
    (match p
      [(pat:any)
       #'s-any]
      [(pat:svar name)
       #'s-var]
      [(pat:var/p name parser argu nested-attrs role opts)
       (with-syntax ([parser parser]
                     [bind-name? (and name #t)]
                     [bind-nested? (pair? nested-attrs)]
                     [-args+role (wrap-exprs aenv (append (argu->exprs argu) (list role)))])
         #'(s-parser parser -args+role (quote bind-name?) (quote bind-nested?)))]
      [(pat:reflect obj argu attr-decls name nested-attrs)
       (with-syntax ([bind-name? (and name #t)]
                     [attr-decls attr-decls]
                     [arity (arguments->arity argu)]
                     [-obj+args (wrap-exprs aenv (cons obj (argu->exprs argu)))])
         #`(s-reflect -obj+args (quote arity) (quote attr-decls) (quote bind-name?)))]
      [(pat:integrated name pred desc role)
       (or (and name (false/false-expr? role)
                (match desc
                  ['"expression" #`s-match-expr]
                  ['"identifier" #`s-match-id]
                  [_ #f]))
           (with-syntax ([-role (wrap-exprs aenv (list role))])
             #`(s-integrated #,(and name #t) #,pred #,desc -role)))]
      [(pat:literal lit in-phase lit-phase)
       (define phases
         (cond [(and (eq? in-phase slpl-expr) (eq? lit-phase slpl-expr)) #'(quote #f)]
               [else (wrap-exprs aenv (list in-phase lit-phase))]))
       (with-syntax ([lit lit] [-phases phases])
         #'(s-literal (quote-syntax lit) -phases))]
      [(pat:datum '())
       #'s-null]
      [(pat:datum datum)
       #`(s-datum (quote #,datum))]
      [(pat:action ap sp)
       (define ap-iattrs (pattern-attrs* ap))
       (define ap-expr (compile-a ap aenv))
       (define sp-expr (compile-s sp (append (reverse ap-iattrs) aenv)))
       (D #`(p-action #,(F ap-expr) #,(F sp-expr)))]
      [(pat:head headp tailp)
       (define headp-iattrs (pattern-attrs* headp))
       (define headp-expr (compile-h headp aenv))
       (define tailp-expr (compile-s tailp (append (reverse headp-iattrs) aenv)))
       (D #`(s-head #,(F headp-expr) #,(F tailp-expr)))]
      [(pat:dots heads tailp)
       (compile-dots heads tailp aenv)]
      [(pat:and ps)
       (for/fold ([acc null] [aenv aenv] #:result (D #`(s-and #,@(map F (reverse acc)))))
                 ([p (in-list ps)])
         (values (cons (compile-s p aenv) acc)
                 (append (reverse (pattern-attrs* p)) aenv)))]
      [(pat:or attrs ps _)
       ;; Don't use attrss from pat:or; doesn't include ~do bundle slot (#f).
       ;; Use with-reset-bundle to reset bundle for each disjunct.
       (for/fold ([acc null] #:result (D #`(p-or #,@(map F (reverse acc)))))
                 ([p (in-list ps)])
         (cons (with-reset-bundle
                 (lambda () (reorder attrs (pattern-attrs* p) (compile-s p aenv) #f)))
               acc))]
      [(pat:not sp)
       (define sp-expr (compile-s sp aenv))
       (D #`(s-not #,(F sp-expr)))]
      [(pat:pair headp tailp)
       (define headp-iattrs (pattern-attrs* headp))
       (define headp-expr (compile-s headp aenv))
       (define headp-first-desc (first-desc-s headp))
       (define tailp-expr (compile-s tailp (append (reverse headp-iattrs) aenv)))
       (D #`(s-pair #,(F headp-expr) #,(F tailp-expr) (quote #,headp-first-desc)))]
      [(pat:vector sp)
       (define sp-expr (compile-s sp aenv))
       (D #`(s-vector #,(F sp-expr)))]
      [(pat:box sp)
       (define sp-expr (compile-s sp aenv))
       (D #`(s-box #,(F sp-expr)))]
      [(pat:pstruct key sp)
       (define sp-expr (compile-s sp aenv))
       (with-syntax ([key key])
         (D #`(s-pstruct (quote key) #,(F sp-expr))))]
      [(pat:describe sp desc transp? role)
       (with-syntax ([-info (wrap-exprs aenv (list desc role))])
         (define sp-expr (compile-s sp aenv))
         (D #`(s-describe -info (quote #,transp?) #,(F sp-expr))))]
      [(pat:delimit sp)
       (define sp-expr (compile-s sp aenv))
       (D #`(p-delimit #,(F sp-expr)))]
      [(pat:commit sp)
       (define sp-expr (compile-s sp aenv))
       (D #`(p-commit #,(F sp-expr)))]
      [(pat:ord sp group index)
       (with-syntax ([group group] [index index])
         (define sp-expr (compile-s sp aenv))
         (D #`(p-ord #,(F sp-expr) (quote #s(ord group index)))))]
      [(pat:post sp)
       (define sp-expr (compile-s sp aenv))
       (D #`(p-post #,(F sp-expr)))]
      [(pat:simple iattrs simple)
       (with-syntax ([simple simple])
         #'(s-simple (quote simple)))]
      [(pat:seq-end)
       #'s-seq-end]))

  ;; A Reordering is (cons Nat (vector Nat/#f ...))
  ;; eg (cons N (vector K_1 ... K_M)) means drop M, add N, each K_i in [0..N-1] unique or #f

  (define (reorder to-iattrs from-iattrs p head-pattern?)
    (cond [(make-reordering to-iattrs from-iattrs)
           => (lambda (reordering)
                (if head-pattern?
                    (D #`(h-reorder (quote #,reordering) #,(F p)))
                    (D #`(s-reorder (quote #,reordering) #,(F p)))))]
          [else p]))

  (define (make-reordering to-iattrs from-iattrs)
    (define (attr=? a1 a2) (and a1 a2 (bound-identifier=? (attr-name a1) (attr-name a2))))
    (cond [(and (null? to-iattrs) (null? from-iattrs))
           #f]
          [(and (pair? to-iattrs) (pair? from-iattrs)
                (attr=? (car to-iattrs) (car from-iattrs)))
           (make-reordering (cdr to-iattrs) (cdr from-iattrs))]
          [else
           (define r-to-iattrs (reverse to-iattrs))
           (define r-from-iattrs (reverse from-iattrs))
           (define add-n (length r-to-iattrs))
           (define reassignment
             (for/vector ([from-a (in-list r-from-iattrs)])
               (for/or ([to-idx (in-naturals)]
                        [to-a (in-list r-to-iattrs)]
                        #:when (attr=? from-a to-a))
                 to-idx)))
           (cons add-n reassignment)]))

  (define (first-desc-s sp)
    (match sp
      [(pat:any) '(any)]
      [(pat:svar _) '(any)]
      [(pat:var/p _ _ _ _ _ (scopts _ _ _ desc)) desc]
      [(pat:integrated name pred desc role) desc]
      [(pat:literal lit in-phase lit-phase) `(literal ,(syntax-e lit))]
      [(pat:datum datum) `(datum ,datum)]
      [(pat:describe sp desc transp? role)
       (and (datum-expr? desc) (datum-expr-value desc))]
      [(pat:delimit sp) (first-desc-s sp)]
      [(pat:commit sp) (first-desc-s sp)]
      [(pat:ord sp group index) (first-desc-s sp)]
      [(pat:post sp) (first-desc-s sp)]
      [_ #f]))

  (define (first-desc-h hp)
    (match hp
      [(hpat:single sp) (first-desc-s sp)]
      [(hpat:var/p _ _ _ _ _ (scopts _ _ _ desc)) desc]
      [(hpat:seq (pat:pair p _)) (first-desc-s p)]
      [(hpat:describe _ desc _ _)
       (and (datum-expr? desc) (datum-expr-value desc))]
      [(hpat:delimit hp) (first-desc-h hp)]
      [(hpat:commit hp) (first-desc-h hp)]
      [(hpat:ord hp _ _) (first-desc-h hp)]
      [(hpat:post hp) (first-desc-h hp)]
      [_ #f]))

  (define (compile-dots heads tailp aenv)
    (define (simple-repc? v) (or (eq? v #f) (equal? v repc:plus)))
    (define tail-null? (match tailp [(pat:datum '()) #t] [_ #f]))
    (or (and tail-null?
             (match heads
               [(list (ehpat _ (hpat:single headp) (? simple-repc? repc) '#f))
                (match headp
                  [(pat:svar name)
                   ;; (x ... . ()) or (x ...+ . ())
                   #`(s-dots1-v-null #,(and name #t) #,(and repc #t))]
                  [(pat:integrated name pred? desc (? false/false-expr?))
                   ;; (x:s ... . ()) where s is an integrable stxclass like id or expr
                   #`(s-dots1-i-null #,(and name #t) #,pred? (quote #,desc) #,(and repc #t))]
                  [_ #f])]
               [_ #f]))
        (match heads
          [(list (ehpat head-iattrs headp (? simple-repc? repc) '#f))
           (with-syntax ([headp-first-desc (first-desc-h headp)]
                         [attrs-n (length head-iattrs)])
             (define headp-expr (with-reset-bundle (lambda () (compile-h headp aenv))))
             (define tailp-expr (compile-s tailp (append (reverse head-iattrs) aenv)))
             (D #`(s-dots1 #,(F headp-expr) #,(F tailp-expr)
                           (quote attrs-n) (quote headp-first-desc) #,(and repc #t))))]
          [_ (compile-dots-general heads tailp aenv)])))

  (define (compile-dots-general heads tailp aenv)
    (define heads-iattrs (append-iattrs (map pattern-attrs* heads)))
    (define head-infos (for/list ([eh (in-list heads)])
                         (compile-eh eh heads-iattrs aenv)))
    (define tailp-expr (compile-s tailp (append (reverse heads-iattrs) aenv)))
    (D (with-syntax ([(h-expr ...) (map F (map car head-infos))]
                     [((attrs-map first-desc min max -name+under+over default) ...)
                      (map cdr head-infos)]
                     [tailp-expr (F tailp-expr)])
         #'(s-dots (list h-expr ...) '(attrs-map ...) '(first-desc ...)
                   '(min ...) '(max ...) (list -name+under+over ...) (list default ...)
                   tailp-expr))))

  (define (compile-eh eh heads-iattrs aenv)
    (match eh
      [(ehpat iattrs hp repc check-null?)
       (list* (with-reset-bundle (lambda () (compile-h hp aenv)))
              (cons (or (rep:once? repc) (rep:optional? repc))
                    (eh-attrs-map iattrs heads-iattrs))
              (and repc (first-desc-h hp))
              (match repc
                ['#f
                 (list 0 +inf.0 #''#f #''#f)]
                [(rep:once name under-msg over-msg)
                 (list 1 1 (wrap-exprs aenv (list name under-msg over-msg)) #''#f)]
                [(rep:optional name over-msg defaults)
                 (list 0 1 (wrap-exprs aenv (list name #f over-msg))
                       (if (pair? defaults) (eh-optional-default iattrs defaults aenv) #''#f))]
                [(rep:bounds min max name under-msg over-msg)
                 (list min max (wrap-exprs aenv (list name under-msg over-msg)) #''#f)]))]))

  ;; AttrsMap = (cons Boolean (vector Nat/#f))
  ;; single? mode, mapping of local renv indexes to combined-heads-renv indexes

  (define (eh-attrs-map iattrs all-iattrs)
    (define r-all-iattrs (reverse all-iattrs))
    (for/vector ([a (in-list (reverse iattrs))])
      (for/or ([aa (in-list r-all-iattrs)] [n (in-naturals)] #:when (equal? a aa)) n)))

  (define (eh-optional-default iattrs defaults aenv)
    (define rexprs
      (for/list ([iattr (in-list (reverse iattrs))])
        (define name (attr-name iattr))
        (or (for/or ([default (in-list defaults)])
              (define d-name (attr-name (action:bind-attr default)))
              (and (bound-identifier=? name d-name)
                   (action:bind-expr default)))
            #''#f)))
    (wrap-exprs aenv (list (allow-intern #`(list #,@rexprs)))))

  ;; ----------------------------------------
  (define (compile-a ap aenv)
    (match ap
      [(action:cut)
       #'a-cut]
      [(action:fail cnd-expr msg-expr)
       (with-syntax ([-cnd (wrap-exprs aenv (list cnd-expr))]
                     [-msg (wrap-exprs aenv (list msg-expr))])
         #'(a-fail -cnd -msg))]
      [(action:bind attr expr)
       (with-syntax ([-expr (wrap-exprs aenv (list expr))])
         #'(a-bind -expr))]
      [(action:and ps)
       (for/fold ([acc null] [aenv aenv] #:result (D #`(a-and #,@(map F (reverse acc)))))
                 ([p (in-list ps)])
         (values (cons (compile-a p aenv) acc)
                 (append (reverse (pattern-attrs* p)) aenv)))]
      [(action:parse sp expr)
       (define sp-expr (compile-s sp aenv))
       (with-syntax ([-expr (wrap-exprs aenv (list expr))])
         (D #`(a-parse #,(F sp-expr) -expr)))]
      [(action:do stmts)
       (define b (current-bundle))
       (define b2 (new-bundle))
       (define proc-expr
         (D (with-syntax ([(stmt ...) stmts]
                          [(expr ...) (map F (reverse (bundle-rexprs b2)))])
              #'(lambda () (no-shadow stmt) ... (vector expr ...)))))
       (define -proc (bundle! aenv (D (wrap-exprs aenv (list (F proc-expr)) #f))))
       (current-bundle b2)
       (D #`(a-do #,(F -proc)))]
      [(action:undo stmts)
       (define proc-expr
         (with-syntax ([(stmt ...) stmts])
           #'(lambda () stmt ... (void))))
       (with-syntax ([-proc (wrap-exprs aenv (list proc-expr))])
         #'(a-undo -proc))]
      [(action:ord ap group index)
       (with-syntax ([group group] [index index])
         (define ap-expr (compile-a ap aenv))
         (D #`(a-ord #,(F ap-expr) (quote #s(ord group index)))))]
      [(action:post ap)
       (define ap-expr (compile-a ap aenv))
       (D #`(a-post #,(F ap-expr)))]))

  ;; ----------------------------------------
  (define (compile-h hp aenv)
    (match hp
      [(hpat:single sp)
       (with-syntax ([sp-first-desc (first-desc-s sp)])
         (define sp-expr (compile-s sp aenv))
         (D #`(h-single #,(F sp-expr) (quote sp-first-desc))))]
      [(hpat:var/p name parser argu nested-attrs role scopts)
       (with-syntax ([parser parser]
                     [bind-name? (and name #t)]
                     [bind-nested? (pair? nested-attrs)]
                     [-args+role (wrap-exprs aenv (append (argu->exprs argu) (list role)))])
         #'(h-parser parser -args+role (quote bind-name?) (quote bind-nested?)))]
      [(hpat:reflect obj argu attr-decls name nested-attrs)
       (with-syntax ([bind-name? (and name #t)]
                     [attr-decls attr-decls]
                     [arity (arguments->arity argu)]
                     [-obj+args (wrap-exprs aenv (cons obj (argu->exprs argu)))])
         #'(h-reflect -obj+args (quote arity) (quote attr-decls) (quote bind-name?)))]
      [(hpat:seq sp)
       (define sp-expr (compile-s sp aenv))
       (D #`(h-seq #,(F sp-expr)))]
      [(hpat:action ap hp)
       (define ap-iattrs (pattern-attrs* ap))
       (define ap-expr (compile-a ap aenv))
       (define hp-expr (compile-h hp (append (reverse ap-iattrs) aenv)))
       (D #`(p-action #,(F ap-expr) #,(F hp-expr)))]
      [(hpat:and hp sp)
       (define hp-iattrs (pattern-attrs* hp))
       (define hp-expr (compile-h hp aenv))
       (define sp-expr (compile-s sp (append (reverse hp-iattrs) aenv)))
       (D #`(h-and #,(F hp-expr) #,(F sp-expr)))]
      [(hpat:or attrs ps _)
       (for/fold ([acc null] #:result (D #`(p-or #,@(map F (reverse acc)))))
                 ([p (in-list ps)])
         (cons (with-reset-bundle
                 (lambda () (reorder attrs (pattern-attrs* p) (compile-h p aenv) #t)))
               acc))]
      [(hpat:describe hp desc transp? role)
       (with-syntax ([-info (wrap-exprs aenv (list desc role))])
         (define hp-expr (compile-h hp aenv))
         (D #`(h-describe -info (quote #,transp?) #,(F hp-expr))))]
      [(hpat:delimit hp)
       (define hp-expr (compile-h hp aenv))
       (D #`(p-delimit #,(F hp-expr)))]
      [(hpat:commit hp)
       (define hp-expr (compile-h hp aenv))
       (D #`(p-commit #,(F hp-expr)))]
      [(hpat:ord hp group index)
       (with-syntax ([group group] [index index])
         (define hp-expr (compile-h hp aenv))
         (D #`(h-ord #,(F hp-expr) (quote #s(ord group index)))))]
      [(hpat:post hp)
       (define hp-expr (compile-h hp aenv))
       (D #`(h-post #,(F hp-expr)))]
      [(hpat:peek hp)
       (define hp-expr (compile-h hp aenv))
       (D #`(h-peek #,(F hp-expr)))]
      [(hpat:peek-not hp)
       (define hp-expr (compile-h hp aenv))
       (D #`(h-peek-not #,(F hp-expr)))]))
  )

;; ============================================================
;; Wrapped Expressions (see also residual-interp.rkt)

(begin-for-syntax
  ;; A Bundle is (bundle (Listof Expr) Nat).
  (struct bundle (rexprs next) #:mutable #:transparent)
  (define (new-bundle) (bundle null 0))

  (define current-bundle (make-parameter #f))

  (define (with-reset-bundle proc)
    (parameterize ((current-bundle (current-bundle))) (proc)))

  ;; bundle! : AEnv Expr[Wrapped[t...] -> Expr[Wrapped[t...]]
  (define (bundle! aenv -expr)
    (cond [(syntax-case -expr (quote) [(quote _) #t] [_ #f])
           -expr]
          [(current-bundle)
           => (lambda (b)
                (set-bundle-rexprs! b (cons -expr (bundle-rexprs b)))
                (let ([next (bundle-next b)])
                  (set-bundle-next! b (add1 next))
                  (define offset (find-bundle-offset aenv))
                  #`(quote #s(bundle-ref #,offset #,next))))]
          [else -expr]))

  ;; find-bundle-offset : AEnv -> Nat/#f
  (define (find-bundle-offset aenv)
    ;; (eprintf "find: ~v\n" (for/list ([a aenv]) (if a (syntax-e (attr-name a)) #f)))
    (for/first ([a (in-list aenv)] [n (in-naturals)] #:when (not a)) n))

  ;; wrap-exprs : AEnv (List Expr[t] ...) -> Expr[Wrapped[t...]]
  ;; Wrap exprs, each must be single-valued, optimize if all constant.
  (define (wrap-exprs aenv exprs [bundle? #t])
    (cond [(andmap datum-expr? exprs)
           #`(quote #,(map datum-expr-value exprs))]
          [else
           (define -expr (wrap-expr* aenv #`(values #,@exprs)))
           (if bundle? (bundle! aenv -expr) -expr)]))

  ;; wrap-expr* : AEnv Expr[t...] -> Expr[Wrapped[t...]]
  ;; Wrap one expr, may return multiple values, no optimization for constant.
  (define (wrap-expr* aenv expr)
    (define n (find-bundle-offset aenv))
    (define naenv (if n (take aenv n) aenv))
    (define tmps (generate-temporaries naenv))
    (with-syntax ([(a ...) naenv] [n n] [(tmp ...) tmps] [expr expr])
      #`(lambda (renv)
          (let-values ([(tmp ...) (list->values 'n renv)])
            (let-attributes ([a tmp] ...) expr)))))

  (define (argu->exprs argu)
    (match argu
      [(arguments pargs kws kwargs)
       (with-syntax ([(kw ...) kws] [(kwarg ...) kwargs] [(parg ...) pargs])
         (list #'(quote (kw ...))
               (allow-intern #'(list kwarg ...))
               (allow-intern #'(list parg ...))))]))
  )

;; ============================================================

(begin-for-syntax

  (define optimize-matrix0 (make-optimizer first-desc-s))

  (define (compile-pattern-matrix rows)
    (define expr (F (compile-matrix rows)))
    (log-syntax-parse-info "compiled matrix:\n~a"
                           (pretty-format (syntax->datum expr) #:mode 'print))
    expr)

  (define (compile-row row)
    (define (compile-s* p aenv)
      (let ([p (if SIMPLIFY? (simplify p) p)])
        (compile-s p aenv)))
    (match row
      [(row1 aenv ps k)
       (define ps-exprs
         (for/fold ([aenv aenv] [acc null] #:result (reverse acc))
                   ([p (in-list ps)])
           (values (append (reverse (pattern-attrs* p)) aenv)
                   (cons (compile-s* p aenv) acc))))
       (D #`(m-row (list #,@(map F ps-exprs)) #,k))]
      [(row/and mat)
       (define mat-expr (compile-matrix mat))
       (D #`(m-and #,(F mat-expr)))]
      [(row/pair first-descs mat)
       (define mat-expr (compile-matrix mat))
       (D #`(m-pair (quote #,first-descs) #,(F mat-expr)))]
      [(row/same sp mat)
       (define sp-expr (compile-s* sp null)) ;; FIXME???
       (define mat-expr (compile-matrix mat))
       (D #`(m-same #,(F sp-expr) #,(F mat-expr)))]))

  (define (compile-matrix rows)
    (define (reset/compile-row row)
      (with-reset-bundle (lambda () (compile-row row))))
    (match rows
      [(list row)
       (reset/compile-row row)]
      [rows
       (define row-exprs (map reset/compile-row rows))
       (D #`(m-or (list #,@(map F row-exprs))))])))

;; ============================================================

(provide (for-syntax codegen-rhs codegen-clauses))

(begin-for-syntax

  (define (codegen-rhs name formals relsattrs the-rhs splicing? [ctx #f])
    (define-values (transparent? description variants defs commit? delimit-cut?)
      (match the-rhs
        [(rhs _ transparent? description variants defs commit? delimit-cut?)
         (values transparent? description variants defs commit? delimit-cut?)]))
    (define vdefss (map variant-definitions variants))
    (define formals* (rewrite-formals formals #'x #'rl))
    (define patterns (map variant-pattern variants))
    (define no-fail?
      (and (not splicing?) ;; FIXME: commit? needed?
           (patterns-cannot-fail? patterns)))
    (when (and no-fail? ctx) (log-syntax-parse-debug "(stxclass) cannot fail: ~e" ctx))
    (define patterns-p
      (cond [(and (not splicing?)
                  (let ([init-rows
                         (for/list ([p (in-list patterns)])
                           (define p-attrs (pattern-attrs* p))
                           (define exp-attrs (reorder-iattrs relsattrs p-attrs))
                           (define k
                             (cond [(make-reordering exp-attrs p-attrs)
                                    => (lambda (reo) #`(reordering (quote #,reo)))]
                                   [else #'values]))
                           (row1 null (list p) k))])
                    (optimize-matrix0 ctx init-rows)))
             => (lambda (rows) #`(s-matrix #,(compile-pattern-matrix rows)))]
            [else
             (define (compile+reorder p)
               (define cp (if splicing? (compile-hpattern p) (compile-pattern p)))
               (define p-attrs (pattern-attrs* p))
               (define exp-attrs (reorder-iattrs relsattrs p-attrs))
               (F (reorder exp-attrs p-attrs cp splicing?)))
             (match patterns
               [(list p) (compile+reorder p)]
               [ps #`(p-or #,@(map compile+reorder ps))])]))
    (define body-p
      (let* ([p patterns-p]
             [p (cond [delimit-cut? #`(p-delimit #,p)]
                      [else p])]
             [p (cond [commit? #`(p-commit #,p)]
                      [else p])]
             [p (with-syntax ([-info (wrap-exprs null (list description #''#f))])
                  (cond [splicing? #`(h-describe -info (quote #,transparent?) #,p rl)]
                        [else #`(s-describe -info (quote #,transparent?) #,p rl)]))])
        p))
    (with-syntax ([formals* formals*]
                  [(def ...) defs]
                  [((vdef ...) ...) vdefss]
                  [description description]
                  [transparent? transparent?]
                  [delimit-cut? delimit-cut?]
                  [no-fail? #`(quote #,no-fail?)]
                  [sk/adapter
                   (if splicing?
                       #'(lambda (fh cp us renv rx rcx rpr)
                           (apply sk/parser fh us rx rcx rpr (reverse renv)))
                       #'(lambda (fh cp us renv)
                           (apply sk/parser fh us (reverse renv))))]
                  [body-p body-p])
      #`(lambda (x cx pr es us fh cp rl sk/parser . formals*)
          (with ([this-syntax x]
                 [this-role rl])
            def ...
            vdef ... ...
            (#%expression
             (syntax-parameterize ((this-context-syntax
                                    (make-this-context-syntax-transformer #'pr)))
               (let ([es (if no-fail? #f es)])
                 ((body-p x cx pr es null) sk/adapter fh cp us))))))))

  (define (codegen-clauses who context x all-defs patterns body-exprs ctx track-literals?)
    (define result-id (datum->syntax #f (string->unreadable-symbol "result")))
    (define result-attr (attr result-id 0 #f))
    (define no-fail? (patterns-cannot-fail? patterns))
    (when (and no-fail? ctx) (log-syntax-parse-debug "cannot fail: ~e" ctx))
    (define clause-ps
      (for/list ([p (in-list patterns)] [expr (in-list body-exprs)])
        (define bind-p (action:bind result-attr #`(lambda () #,expr)))
        (pat:and (list p (pat:action bind-p (pat:any))))))
    (define parser
      (cond [(let ([rows (for/list ([p (in-list clause-ps)])
                           (row1 null (list p) #'values))])
               (optimize-matrix0 ctx rows))
             => (lambda (rows)
                  #`(s-matrix #,(compile-pattern-matrix rows)))]
            [else #`(p-or #,@(map compile-pattern clause-ps))]))
    (with-syntax ([(who context x) (list who context x)]
                  [(def ...) all-defs]
                  [parser parser])
      #`(run-clauses
         (quote who) context x (quote #,no-fail?) (quote #,track-literals?)
         (let-values () def ... parser)))))
