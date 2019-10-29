#lang racket/base
(require "../common/struct-star.rkt"
         "../common/set.rkt"
         "../syntax/syntax.rkt"
         "../syntax/property.rkt"
         "../syntax/scope.rkt"
         "../syntax/taint.rkt"
         "../syntax/match.rkt"
         "../namespace/namespace.rkt"
         "../common/module-path.rkt"
         "../syntax/binding.rkt"
         "env.rkt"
         "free-id-set.rkt"
         "../syntax/track.rkt"
         "../syntax/error.rkt"
         "syntax-id-error.rkt"
         "dup-check.rkt"
         "../namespace/core.rkt"
         "context.rkt"
         "allowed-context.rkt"
         "main.rkt"
         "body.rkt"
         "set-bang-trans.rkt"
         "rename-trans.rkt"
         "reference-record.rkt"
         "prepare.rkt"
         "log.rkt"
         "parsed.rkt"
         "../compile/correlate.rkt")

;; ----------------------------------------

;; Common expansion for `lambda` and `case-lambda`
(define (lambda-clause-expander s disarmed-s formals bodys ctx log-renames-tag)
  (define sc (new-scope 'local))
  (define phase (expand-context-phase ctx))
  ;; Parse and check formal arguments:
  (define ids (parse-and-flatten-formals formals sc disarmed-s))
  (check-no-duplicate-ids ids phase s #:what "argument name")
  ;; Bind each argument and generate a corresponding key for the
  ;; expand-time environment:
  (define counter (root-expand-context-counter ctx))
  (define local-sym (and (expand-context-normalize-locals? ctx) 'arg))
  (define keys (for/list ([id (in-list ids)])
                 (add-local-binding! id phase counter #:in s #:local-sym local-sym)))
  (define body-env (for/fold ([env (expand-context-env ctx)]) ([key (in-list keys)]
                                                               [id (in-list ids)])
                     (env-extend env key (local-variable id))))
  (define sc-formals (add-scope formals sc))
  (define sc-bodys (for/list ([body (in-list bodys)]) (add-scope body sc)))
  (log-expand ctx log-renames-tag sc-formals (datum->syntax #f sc-bodys))
  ;; Expand the function body:
  (define body-ctx (struct*-copy expand-context ctx
                                 [env body-env]
                                 [scopes (cons sc (expand-context-scopes ctx))]
                                 [binding-layer (increment-binding-layer ids ctx sc)]
                                 [frame-id #:parent root-expand-context #f]))
  (define exp-body (expand-body sc-bodys body-ctx #:source (keep-as-needed ctx s #:keep-for-error? #t)))
  ;; Return formals (with new scope) and expanded body:
  (values (if (expand-context-to-parsed? ctx) 
              (unflatten-like-formals keys formals)
              sc-formals)
          exp-body))

(add-core-form!
 'lambda
  (lambda (s ctx)
    (log-expand ctx 'prim-lambda)
    (define disarmed-s (syntax-disarm s))
    (define-match m disarmed-s '(lambda formals body ...+))
    (define rebuild-s (keep-as-needed ctx s #:keep-for-parsed? #t))
    (define-values (formals body)
      (lambda-clause-expander s disarmed-s (m 'formals) (m 'body) ctx 'lambda-renames))
    (if (expand-context-to-parsed? ctx)
        (parsed-lambda rebuild-s formals body)
        (rebuild
         rebuild-s
         `(,(m 'lambda) ,formals ,@body)))))

(add-core-form!
 'λ
 ;; A macro:
 (lambda (s)
   (define-match m s '(lam-id formals _ ...+))
   (define ids (parse-and-flatten-formals (m 'formals) #f s))
   (define ctx (get-current-expand-context #:fail-ok? #t))
   (define phase (if ctx
                     (expand-context-phase ctx)
                     0))
   (check-no-duplicate-ids ids phase s #:what "argument name")
   (datum->syntax
    s
    (cons (datum->syntax (syntax-shift-phase-level core-stx phase)
                         'lambda
                         (m 'lam-id)
                         (m 'lam-id))
          (cdr (syntax-e s)))
    s
    s)))

(add-core-form!
 'case-lambda
 (lambda (s ctx)
   (log-expand ctx 'prim-case-lambda)
   (define disarmed-s (syntax-disarm s))
   (define-match m disarmed-s '(case-lambda [formals body ...+] ...))
   (define-match cm disarmed-s '(case-lambda clause ...))
   (define rebuild-s (keep-as-needed ctx s #:keep-for-parsed? #t))
   (define clauses
     (for/list ([formals (in-list (m 'formals))]
                [body (in-list (m 'body))]
                [clause (in-list (cm 'clause))])
       (log-expand ctx 'next)
       (define rebuild-clause (keep-as-needed ctx clause))
       (define-values (exp-formals exp-body)
         (lambda-clause-expander s disarmed-s formals body ctx 'lambda-renames))
       (if (expand-context-to-parsed? ctx)
           (list exp-formals exp-body)
           (rebuild rebuild-clause `[,exp-formals ,@exp-body]))))
   (if (expand-context-to-parsed? ctx)
       (parsed-case-lambda rebuild-s clauses)
       (rebuild
        rebuild-s
        `(,(m 'case-lambda) ,@clauses)))))

(define (parse-and-flatten-formals all-formals sc s)
  (let loop ([formals all-formals])
    (cond
     [(identifier? formals) (list (add-scope formals sc))]
     [(syntax? formals)
      (define p (syntax-e formals))
      (cond
       [(pair? p) (loop p)]
       [(null? p) null]
       [else (raise-syntax-error #f "not an identifier" s p)])]
     [(pair? formals)
      (unless (identifier? (car formals))
        (raise-syntax-error #f "not an identifier" s (car formals)))
      (cons (if sc
                (add-scope (car formals) sc)
                (car formals))
            (loop (cdr formals)))]
     [(null? formals)
      null]
     [else
      (raise-syntax-error "bad argument sequence" s all-formals)])))

(define (unflatten-like-formals keys formals)
  (let loop ([keys keys] [formals formals])
    (cond
     [(null? formals) null]
     [(pair? formals) (cons (car keys) (loop (cdr keys) (cdr formals)))]
     [(syntax? formals) (loop keys (syntax-e formals))]
     [else (car keys)])))

;; ----------------------------------------

;; Common expansion for `let[rec]-[syntaxes+]values`
(define (make-let-values-form #:log-tag log-tag
                              #:syntaxes? [syntaxes? #f]
                              #:rec? [rec? #f]
                              #:split-by-reference? [split-by-reference? #f]
                              #:renames-log-tag [renames-log-tag 'let-renames])
  (lambda (s ctx)
    (log-expand ctx log-tag)
    (define disarmed-s (syntax-disarm s))
    (define-match stx-m disarmed-s #:when syntaxes?
      '(letrec-syntaxes+values
        ([(id:trans ...) trans-rhs] ...)
           ([(id:val ...) val-rhs] ...)
        body ...+))
    (define-match val-m disarmed-s #:unless syntaxes?
      '(let-values ([(id:val ...) val-rhs] ...)
         body ...+))
    (define sc (new-scope 'local))
    (define body-sc (and rec? (new-scope 'letrec-body)))
    (define phase (expand-context-phase ctx))
    (define frame-id (and syntaxes?
                          (make-reference-record))) ; accumulates info on referenced variables
    ;; Add the new scope to each binding identifier:
    (define trans-idss (for/list ([ids (in-list (if syntaxes? (stx-m 'id:trans) null))])
                         (for/list ([id (in-list ids)])
                           (add-scope id sc))))
    (define val-idss (for/list ([ids (in-list (if syntaxes? (stx-m 'id:val) (val-m 'id:val)))])
                       (for/list ([id (in-list ids)])
                         (add-scope id sc))))
    (define val-rhss (if rec?
                         (for/list ([rhs (in-list (if syntaxes? (stx-m 'val-rhs) (val-m 'val-rhs)))])
                           (add-scope rhs sc))
                         (if syntaxes? (stx-m 'val-rhs) (val-m 'val-rhs))))
    (define val-clauses ; for syntax tracking
      (cond
        [syntaxes?
         (define-match m disarmed-s '(_ _ (clause ...) . _))
         (m 'clause)]
        [else
         (define-match m disarmed-s '(_ (clause ...) . _))
         (m 'clause)]))
    (check-no-duplicate-ids (list trans-idss val-idss) phase s)
    ;; Bind each left-hand identifier and generate a corresponding key
    ;; fo the expand-time environment:
    (define counter (root-expand-context-counter ctx))
    (define local-sym (and (expand-context-normalize-locals? ctx) 'loc))
    (define trans-keyss (for/list ([ids (in-list trans-idss)])
                          (for/list ([id (in-list ids)])
                            (add-local-binding! id phase counter
                                                #:frame-id frame-id #:in s
                                                 #:local-sym local-sym))))
    (define val-keyss (for/list ([ids (in-list val-idss)])
                        (for/list ([id (in-list ids)])
                          (add-local-binding! id phase counter
                                              #:frame-id frame-id #:in s
                                              #:local-sym local-sym))))
    ;; Add new scope to body:
    (define bodys (for/list ([body (in-list (if syntaxes? (stx-m 'body) (val-m 'body)))])
                    (define new-body (add-scope body sc))
                    (if rec?
                        (add-scope new-body body-sc)
                        new-body)))
    (log-expand... ctx (lambda (obs)
                         (log-let-renames obs renames-log-tag val-idss val-rhss bodys
                                          trans-idss (and syntaxes? (stx-m 'trans-rhs)) sc)))
    ;; Evaluate compile-time expressions (if any):
    (when syntaxes?
      (log-expand ctx 'prepare-env)
      (prepare-next-phase-namespace ctx))
    (define trans-valss (for/list ([rhs (in-list (if syntaxes? (stx-m 'trans-rhs) '()))]
                                   [ids (in-list trans-idss)])
                          (log-expand* ctx ['next] ['enter-bind])
                          (define trans-val (eval-for-syntaxes-binding 'letrec-syntaxes+values
                                                                       (add-scope rhs sc) ids ctx))
                          (log-expand ctx 'exit-bind)
                          trans-val))
    ;; Fill expansion-time environment:
    (define rec-val-env
      (for/fold ([env (expand-context-env ctx)]) ([keys (in-list val-keyss)]
                                                  [ids (in-list val-idss)]
                                                  #:when #t
                                                  [key (in-list keys)]
                                                  [id (in-list ids)])
        (env-extend env key (local-variable id))))
    (define rec-env (for/fold ([env rec-val-env]) ([keys (in-list trans-keyss)]
                                                   [vals (in-list trans-valss)]
                                                   [ids (in-list trans-idss)])
                      (for/fold ([env env]) ([key (in-list keys)]
                                             [val (in-list vals)]
                                             [id (in-list ids)])
                        (maybe-install-free=id-in-context! val id phase ctx)
                        (env-extend env key val))))
    ;; Expand right-hand sides and body
    (define expr-ctx (as-expression-context ctx))
    (define orig-rrs (expand-context-reference-records expr-ctx))
    (define rec-ctx (struct*-copy expand-context expr-ctx
                                  [env rec-env]
                                  [scopes (let ([scopes (cons sc (expand-context-scopes ctx))])
                                            (if rec?
                                                (cons body-sc scopes)
                                                scopes))]
                                  [reference-records (if split-by-reference?
                                                         (cons frame-id orig-rrs)
                                                         orig-rrs)]
                                  [binding-layer (increment-binding-layer
                                                  (cons trans-idss val-idss)
                                                  ctx
                                                  sc)]))
    (define letrec-values-id
      (and (not (expand-context-to-parsed? ctx))
           (if syntaxes?
               (core-id 'letrec-values phase)
               (val-m 'let-values))))
    
    (define rebuild-s (keep-as-needed ctx s #:keep-for-error? #t))
    (define val-name-idss (if (expand-context-to-parsed? ctx)
                              (for/list ([val-ids (in-list val-idss)])
                                (for/list ([val-id (in-list val-ids)])
                                  (datum->syntax #f (syntax-e val-id) val-id val-id)))
                              val-idss))

    (when syntaxes?
      (log-expand... ctx (lambda (obs) (log-letrec-values obs val-idss val-rhss bodys))))

    (define (get-body)
      (log-expand* ctx #:unless (and syntaxes? (null? val-idss)) ['next-group])
      (define body-ctx (struct*-copy expand-context rec-ctx
                                     [reference-records orig-rrs]))
      (expand-body bodys (as-tail-context body-ctx #:wrt ctx) #:source rebuild-s))

    (define result-s
      (cond
        [(not split-by-reference?)
         (define clauses
           (for/list ([ids (in-list val-name-idss)]
                      [keys (in-list val-keyss)]
                      [rhs (in-list val-rhss)]
                      [clause (in-list val-clauses)])
             (log-expand ctx 'next)
             (define exp-rhs (expand rhs (if rec?                               
                                             (as-named-context rec-ctx ids)
                                             (as-named-context expr-ctx ids))))
             (if (expand-context-to-parsed? ctx)
                 (list keys exp-rhs)
                 (datum->syntax #f `[,ids ,exp-rhs] clause clause))))
         (define exp-body (get-body))
         (when frame-id
           (reference-record-clear! frame-id))
         (if (expand-context-to-parsed? ctx)
             (if rec?
                 (parsed-letrec-values rebuild-s val-name-idss clauses exp-body)
                 (parsed-let-values rebuild-s val-name-idss clauses exp-body))
             (rebuild
              rebuild-s
              `(,letrec-values-id ,clauses ,@exp-body)))]
        [else
         (expand-and-split-bindings-by-reference
          val-idss val-keyss val-rhss val-clauses
          #:split? #t
          #:frame-id frame-id #:ctx rec-ctx
          #:source rebuild-s #:had-stxes? syntaxes?
          #:get-body get-body #:track? #t)]))
    
    (if (expand-context-to-parsed? ctx)
        result-s
        (attach-disappeared-transformer-bindings result-s trans-idss))))

(define (log-let-renames obs renames-log-tag val-idss val-rhss bodys
                         trans-idss trans-rhss sc)
  (define vals+body (cons (for/list ([val-ids (in-list val-idss)]
                                     [val-rhs (in-list val-rhss)])
                            (datum->syntax #f `[,val-ids ,val-rhs]))
                          (datum->syntax #f bodys)))
  (...log-expand obs [renames-log-tag (if (not trans-rhss)
                                          vals+body
                                          (cons
                                           (for/list ([trans-ids (in-list trans-idss)]
                                                      [trans-rhs (in-list trans-rhss)])
                                             (datum->syntax #f `[,trans-ids ,(add-scope trans-rhs sc)]))
                                           vals+body))]))

(define (log-letrec-values obs val-idss val-rhss bodys)
  (...log-expand obs ['next-group])
  (unless (null? val-idss)
    (...log-expand obs ['prim-letrec-values])
    (log-let-renames obs 'let-renames val-idss val-rhss bodys
                     #f #f #f)))

(add-core-form!
 'let-values
 (make-let-values-form #:log-tag 'prim-let-values))

(add-core-form!
 'letrec-values
 (make-let-values-form #:rec? #t #:log-tag 'prim-letrec-values))

(add-core-form!
 'letrec-syntaxes+values
 (make-let-values-form #:syntaxes? #t #:rec? #t #:split-by-reference? #t
                       #:log-tag 'prim-letrec-syntaxes+values
                       #:renames-log-tag 'letrec-syntaxes-renames))

;; ----------------------------------------

(add-core-form!
 '#%stratified-body
 (lambda (s ctx)
   (log-expand ctx 'prim-#%stratified)
   (define disarmed-s (syntax-disarm s))
   (define-match m disarmed-s '(#%stratified-body body ...+))
   (define rebuild-s (keep-as-needed ctx s #:keep-for-error? #t))
   (define exp-body (expand-body (m 'body) ctx #:stratified? #t #:source rebuild-s))
   (if (expand-context-to-parsed? ctx)
       (parsed-begin rebuild-s exp-body)
       (rebuild
        rebuild-s
        (if (null? (cdr exp-body))
            (car exp-body)
            `(,(core-id 'begin (expand-context-phase ctx))
              ,@exp-body))))))

;; ----------------------------------------

(add-core-form!
 '#%datum
 (lambda (s ctx)
   (log-expand ctx 'prim-#%datum)
   (define disarmed-s (syntax-disarm s))
   (define-match m disarmed-s '(#%datum . datum))
   (define datum (m 'datum))
   (when (and (syntax? datum)
              (keyword? (syntax-e datum)))
     (raise-syntax-error '#%datum "keyword misused as an expression" #f datum))
   (define phase (expand-context-phase ctx))
   (if (and (expand-context-to-parsed? ctx)
            (free-id-set-empty? (expand-context-stops ctx)))
       (parsed-quote (keep-properties-only~ s) (syntax->datum datum))
       (rebuild
        s
        (list (core-id 'quote phase)
              datum)))))

;; '#%kernel `#%app` treats an empty combination as a literal null
(add-core-form!
 '#%app
 (lambda (s ctx)
   (log-expand ctx 'prim-#%app)
   (define disarmed-s (syntax-disarm s))
   (define-match m disarmed-s '(#%app e ...))
   (define es (m 'e))
   (cond
    [(null? es)
     (define phase (expand-context-phase ctx))
     (if (expand-context-to-parsed? ctx)
         (parsed-quote (keep-properties-only~ s) null)
         (rebuild
          s
          (list (core-id 'quote phase)
                null)))]
    [else
     (define keep-for-parsed? keep-source-locations?)
     (define rebuild-s (keep-as-needed ctx s #:keep-for-parsed? keep-for-parsed?))
     (define prefixless (cdr (syntax-e disarmed-s)))
     (define rebuild-prefixless (and (syntax? prefixless)
                                     (keep-as-needed ctx prefixless #:keep-for-parsed? keep-for-parsed?)))
     (define expr-ctx (as-expression-context ctx))
     (log-expand* expr-ctx ['enter-list (datum->syntax #f es s)] ['next])
     (define rest-es (cdr es))
     (define exp-rator (expand (car es) expr-ctx))
     (define exp-es (for/list ([e (in-list rest-es)])
                      (log-expand expr-ctx 'next)
                      (expand e expr-ctx)))
     (cond
       [(expand-context-to-parsed? ctx)
        (parsed-app (or rebuild-prefixless rebuild-s) exp-rator exp-es)]
       [else
        (define es (let ([exp-es (cons exp-rator exp-es)])
                     (if rebuild-prefixless
                         (rebuild rebuild-prefixless exp-es)
                         exp-es)))
        (log-expand expr-ctx 'exit-list (datum->syntax #f es rebuild-s))
        (rebuild rebuild-s (cons (m '#%app) es))])])))


(add-core-form!
 'quote
 (lambda (s ctx)
   (log-expand ctx 'prim-quote)
   (define-match m (syntax-disarm s) '(quote datum))
   (if (expand-context-to-parsed? ctx)
       (parsed-quote (keep-properties-only~ s) (syntax->datum (m 'datum)))
       s)))

(add-core-form!
 'quote-syntax
 (lambda (s ctx)
   (log-expand ctx 'prim-quote-syntax)
   (define disarmed-s (syntax-disarm s))
   (define-match m-local disarmed-s #:try '(quote-syntax datum #:local))
   (define-match m disarmed-s #:unless (m-local) '(quote-syntax datum))
   (cond
    [(m-local)
     ;; #:local means don't prune, and it counts as a reference to
     ;; all variables for letrec splitting
     (reference-records-all-used! (expand-context-reference-records ctx))
     (define-match m-kw disarmed-s '(_ _ kw))
     (if (expand-context-to-parsed? ctx)
         (parsed-quote-syntax (keep-properties-only~ s) (m-local 'datum))
         (rebuild
          s
          `(,(m-local 'quote-syntax) ,(m-local 'datum) ,(m-kw 'kw))))]
    [else
     ;; otherwise, prune scopes up to transformer boundary:
     (define use-site-scopes (root-expand-context-use-site-scopes ctx))
     (define datum-s (remove-scopes (remove-scopes (m 'datum) (expand-context-scopes ctx))
                                    (if use-site-scopes (unbox use-site-scopes) '())))
     (if (and (expand-context-to-parsed? ctx)
              (free-id-set-empty? (expand-context-stops ctx)))
         (parsed-quote-syntax (keep-properties-only~ s) datum-s)
         (rebuild
          s
          `(,(m 'quote-syntax)
            ,datum-s)))])))

(add-core-form!
 'if
 (lambda (s ctx)
   (log-expand ctx 'prim-if)
   (define disarmed-s (syntax-disarm s))
   (define-match bad-m disarmed-s #:try '(_ _ _))
   (when (bad-m) (raise-syntax-error #f "missing an \"else\" expression" s))
   (define-match m disarmed-s '(if tst thn els))
   (define expr-ctx (as-expression-context ctx))
   (define tail-ctx (as-tail-context expr-ctx #:wrt ctx))
   (define rebuild-s (keep-as-needed ctx s))
   (define exp-tst (expand (m 'tst) expr-ctx))
   (log-expand ctx 'next)
   (define exp-thn (expand (m 'thn) tail-ctx))
   (log-expand ctx 'next)
   (define exp-els (expand (m 'els) tail-ctx))
   (if (expand-context-to-parsed? ctx)
       (parsed-if rebuild-s exp-tst exp-thn exp-els)
       (rebuild
        rebuild-s
        (list (m 'if) exp-tst exp-thn exp-els)))))

(add-core-form!
 'with-continuation-mark
 (lambda (s ctx)
   (log-expand ctx 'prim-with-continuation-mark)
   (define disarmed-s (syntax-disarm s))
   (define-match m disarmed-s '(with-continuation-mark key val body))
   (define expr-ctx (as-expression-context ctx))
   (define rebuild-s (keep-as-needed ctx s))
   (define exp-key (expand (m 'key) expr-ctx))
   (log-expand ctx 'next)
   (define exp-val (expand (m 'val) expr-ctx))
   (log-expand ctx 'next)
   (define exp-body (expand (m 'body) (as-tail-context expr-ctx #:wrt ctx)))
   (if (expand-context-to-parsed? ctx)
       (parsed-with-continuation-mark rebuild-s exp-key exp-val exp-body)
       (rebuild
        rebuild-s
        (list (m 'with-continuation-mark) exp-key exp-val exp-body)))))

(define (make-begin log-tag parsed-begin
                    #:list-start-index list-start-index
                    #:last-is-tail? last-is-tail?)
 (lambda (s ctx)
   (log-expand ctx log-tag)
   (define disarmed-s (syntax-disarm s))
   (define-match m disarmed-s '(begin e ...+))
   (define expr-ctx (if last-is-tail?
                        (as-begin-expression-context ctx)
                        (as-expression-context ctx)))
   (define rebuild-s (keep-as-needed ctx s))
   (define exp-es
     (let loop ([es (m 'e)] [index list-start-index])
       (when (zero? index)
         (log-expand... ctx
                        (lambda (obs)
                          (unless (zero? list-start-index)
                            (...log-expand obs ['next]))
                          (...log-expand obs ['enter-list (datum->syntax #f es rebuild-s)]))))
       (cond
        [(null? es) null]
        [else
         (define rest-es (cdr es))
         (log-expand ctx 'next)
         (cons (expand (car es) (if (and last-is-tail? (null? rest-es))
                                    (as-tail-context expr-ctx #:wrt ctx)
                                    expr-ctx))
               (loop rest-es (sub1 index)))])))
   (log-expand ctx 'exit-list (datum->syntax #f (list-tail exp-es list-start-index) rebuild-s))
   (if (expand-context-to-parsed? ctx)
       (parsed-begin rebuild-s exp-es)
       (rebuild
        rebuild-s
        (cons (m 'begin) exp-es)))))

(add-core-form!
 'begin
 (let ([nonempty-begin (make-begin 'prim-begin parsed-begin #:list-start-index 0 #:last-is-tail? #t)])
   (lambda (s ctx)
     ;; Empty `begin` allowed in 'top-level and 'module contexts,
     ;; which might get here via `local-expand`:
     (define context (expand-context-context ctx))
     (cond
      [(or (eq? context 'top-level) (eq? context 'module))
       (define disarmed-s (syntax-disarm s))
       (define-match m disarmed-s #:try '(begin))
       (if (m)
           s
           (nonempty-begin s ctx))]
      [else
       (nonempty-begin s ctx)]))))

(add-core-form!
 'begin0
 (make-begin 'prim-begin0 parsed-begin0 #:list-start-index 1 #:last-is-tail? #f))

(define (register-eventual-variable!? id ctx)
  (cond
   [(and (expand-context-need-eventually-defined ctx)
         ((expand-context-phase ctx) . >= . 1))
    ;; In top level or `begin-for-syntax`, encountered a reference to a
    ;; variable that might be defined later; record it for later checking
    (hash-update! (expand-context-need-eventually-defined ctx)
                  (expand-context-phase ctx)
                  (lambda (l) (cons id l))
                  null)
    #t]
   [else #f]))

(add-core-form!
 '#%top
 (lambda (s ctx [implicit-omitted? #f])
   (log-expand ctx 'prim-#%top)
   (define disarmed-s (syntax-disarm s))
   (define id (cond
               [implicit-omitted?
                ;; As a special favor to `local-expand`, the expander
                ;; has avoided making `#%top` explicit
                s]
               [else
                (define-match m disarmed-s '(#%top . id))
                (m 'id)]))
   (define b (resolve+shift id (expand-context-phase ctx)
                            #:ambiguous-value 'ambiguous))
   (cond
    [(eq? b 'ambiguous)
     (raise-ambiguous-error id ctx)]
    [(and b
          (module-binding? b)
          (eq? (module-binding-module b) (root-expand-context-self-mpi ctx)))
     ;; Allow `#%top` in a module or top-level where it refers to the same
     ;; thing that the identifier by itself would refer to; in that case
     ;; `#%top` can be stripped within a module
     (if (expand-context-to-parsed? ctx)
         (parsed-id id b #f)
         (cond
          [(top-level-module-path-index? (module-binding-module b)) s]
          [else id]))]
    [(register-eventual-variable!? id ctx)
     ;; Must be in a module, and we'll check the binding later, so strip `#%top`:
     (if (expand-context-to-parsed? ctx)
         (parsed-id id b #f)
         id)]
    [else
     (cond
      [(not (expand-context-allow-unbound? ctx))
       ;; In a module, unbound or out of context:
       (raise-unbound-syntax-error #f "unbound identifier" id #f null
                                   (syntax-debug-info-string id ctx))]
      [else
       ;; At the top level:
       (define tl-id (add-scope id (root-expand-context-top-level-bind-scope ctx)))
       (define tl-b (resolve tl-id (expand-context-phase ctx)))
       (cond
        [tl-b
         ;; Expand to a reference to a top-level variable, instead of
         ;; a local or required variable; don't include the temporary
         ;; binding scope in an expansion, though, in the same way that
         ;; `define-values` expands without it
         (if (expand-context-to-parsed? ctx)
             (parsed-top-id tl-id tl-b #f)
             (cond
              [implicit-omitted? id]
              [else
               (define-match m disarmed-s '(#%top . id))
               (rebuild s (cons (m '#%top) id))]))]
        [else (if (expand-context-to-parsed? ctx)
                  (parsed-top-id id b #f)
                  s)])])])))

(add-core-form!
 'set!
 (lambda (s ctx)
   (log-expand ctx 'prim-set!)
   (define disarmed-s (syntax-disarm s))
   (define-match m disarmed-s '(set! id rhs))
   (define orig-id (m 'id))
   (let rename-loop ([id orig-id] [from-rename? #f])
     (define binding (resolve+shift id (expand-context-phase ctx)
                                    #:ambiguous-value 'ambiguous
                                    #:immediate? #t))
     (when (eq? binding 'ambiguous)
       (raise-ambiguous-error id ctx))
     (define-values (t primitive? insp protected?) (if binding
                                                       (lookup binding ctx s)
                                                       (values #f #f #f #f)))
     (log-expand ctx 'resolve id)
     (cond
      [(or (variable? t)
           (and (not binding)
                (or (register-eventual-variable!? id ctx)
                    (expand-context-allow-unbound? ctx))))
       (when (and (module-binding? binding)
                  (not (inside-module-context? (module-binding-module binding)
                                               (root-expand-context-self-mpi ctx))))
         (raise-syntax-error #f "cannot mutate module-required identifier" s id))
       (log-expand ctx 'next)
       (register-variable-referenced-if-local! binding)
       (define rebuild-s (keep-as-needed ctx s))
       (define exp-rhs (expand (m 'rhs) (as-expression-context ctx)))
       (if (expand-context-to-parsed? ctx)
           (parsed-set! rebuild-s (parsed-id id binding #f) exp-rhs)
           (rebuild
            rebuild-s
            (list (m 'set!)
                  (substitute-variable id t #:no-stops? (free-id-set-empty-or-just-module*? (expand-context-stops ctx)))
                  exp-rhs)))]
      [(not binding)
       (raise-unbound-syntax-error #f "unbound identifier" s id null
                                   (syntax-debug-info-string id ctx))]
      [(set!-transformer? t)
       (cond
        [(not-in-this-expand-context? t ctx)
         (expand (avoid-current-expand-context (substitute-set!-rename s disarmed-s (m 'set!) (m 'rhs) id from-rename? ctx) t ctx)
                 ctx)]
        [else
         (define-values (exp-s re-ctx)
           (apply-transformer t insp s orig-id ctx binding #:origin-id orig-id))
         (cond
          [(expand-context-just-once? ctx) exp-s]
          [else (expand exp-s re-ctx)])])]
      [(rename-transformer? t)
       (cond
        [(not-in-this-expand-context? t ctx)
         (expand (avoid-current-expand-context (substitute-set!-rename s disarmed-s (m 'set!) (m 'rhs) id from-rename? ctx t) t ctx)
                 ctx)]
        [else (rename-loop (syntax-track-origin (rename-transformer-target-in-context t ctx) id id) #t)])]
      [else
       (raise-syntax-error #f "cannot mutate syntax identifier" s id)]))))

(define (substitute-set!-rename s disarmed-s set!-id id rhs-s from-rename? ctx [t #f])
  (cond
   [(or t from-rename?)
    (define new-id (if t
                       (rename-transformer-target-in-context t ctx)
                       id))
    (syntax-rearm (datum->syntax disarmed-s (list set!-id new-id rhs-s) disarmed-s disarmed-s)
                  s)]
   [else s]))

(add-core-form!
 '#%variable-reference
 (lambda (s ctx)
   (log-expand ctx 'prim-#%variable-reference)
   (define disarmed-s (syntax-disarm s))
   (define-match id-m disarmed-s #:try '(#%variable-reference id))
   (define-match top-m disarmed-s #:unless (id-m) #:try '(#%variable-reference (#%top . id)))
   (define-match empty-m disarmed-s #:unless (or (id-m) (top-m)) '(#%variable-reference))
   (cond
    [(or (id-m) (top-m))
     (define var-id (if (id-m) (id-m 'id) (top-m 'id)))
     (define binding (resolve+shift var-id (expand-context-phase ctx)
                                    #:ambiguous-value 'ambiguous))
     (when (eq? binding 'ambiguous)
       (raise-ambiguous-error var-id ctx))
     (unless (or binding
                 (expand-context-allow-unbound? ctx))
       (raise-unbound-syntax-error #f "unbound identifier" s var-id null
                                   (syntax-debug-info-string var-id ctx)))
     (define-values (t primitive? insp-of-t protected?)
       (if binding
           (lookup binding ctx var-id
                   #:in s
                   #:out-of-context-as-variable? (expand-context-in-local-expand? ctx))
           (values #f #f #f #f)))
     (when (and t (not (variable? t)))
       (raise-syntax-error #f "identifier does not refer to a variable" var-id s))
     (if (expand-context-to-parsed? ctx)
         (parsed-#%variable-reference (keep-properties-only~ s)
                                      (cond
                                        [(top-m) (parsed-top-id var-id binding #f)]
                                        [primitive? (parsed-primitive-id var-id binding #f)]
                                        [else (parsed-id var-id binding #f)]))
         s)]
    [else
     (if (expand-context-to-parsed? ctx)
         (parsed-#%variable-reference (keep-properties-only~ s) #f)
         s)])))

(add-core-form!
 '#%expression
 (lambda (s ctx)
   (log-expand ctx 'prim-#%expression)
   (define disarmed-s (syntax-disarm s))
   (define-match m disarmed-s '(#%expression e))
   (define rebuild-s (keep-as-needed ctx s #:for-track? #t))
   (define exp-e (expand (m 'e) (as-tail-context (as-expression-context ctx)
                                                 #:wrt ctx)))
   (if (expand-context-to-parsed? ctx)
       exp-e
       (cond
         [(or (and (expand-context-in-local-expand? ctx)
                   (expand-context-keep-#%expression? ctx))
              (eq? 'top-level (expand-context-context ctx)))
          (rebuild
           rebuild-s
           `(,(m '#%expression) ,exp-e))]
         [else
          (define result-s (syntax-track-origin exp-e rebuild-s))
          (log-expand ctx 'tag result-s)
          result-s]))))

;; ----------------------------------------

;; Historically in '#%kernel, should be moved out
(add-core-form!
 'unquote
 (lambda (s ctx)
   (raise-syntax-error #f "not in quasiquote" s)))
(add-core-form!
 'unquote-splicing
 (lambda (s ctx)
   (raise-syntax-error #f "not in quasiquote" s)))
