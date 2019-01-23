#lang racket/base
(require "match.rkt"
         "wrap.rkt")

;; Convert `lambda`s to make them fully closed, which is compatible
;; with JIT compilation of the `lambda` or separate ahead-of-time
;; compilation (as opposed to compiling a whole linklet).

;; If `convert-size-threshold` is #f, then every `lambda` is
;; converted. If it's a number, then only `lambda`s smaller than the
;; threshold are converted, and and no `lambda` within a converted
;; `lambda` is converted. So, supplying a numerical threshold is
;; useful for drawing a boundary between compiled and non-compiled
;; code, as opposed to a true JIT setup.

;; If `need-lift?` is #t, then a converted function never contains
;; a direct reference to a converted function. Instead, the converted
;; function takes an argument to access other converted functions.
;; That way, the converted functions are completely independent.

;; An environment maps a variables that needs to be passed into the
;; closed code:
;;
;;   * id -> '#:direct --- ready by the time it's needed and immutable
;;
;;   * id -> expression --- rewrite access to expression
;;
;;   * id -> `(self ,m) --- a reference to the enclosing function; can
;;                          use directly in rator position, otherwise
;;                          use m

(provide jitify-schemified-linklet)

(struct convert-mode (sizes called? lift? no-more-conversions?))

(define lifts-id (gensym 'jits))

(define (jitify-schemified-linklet v
                                   need-extract?
                                   need-lift?
                                   convert-size-threshold ; #f or a number; see above
                                   extractable-annotation)

  ;; Constucts a closed `lambda` form as wrapped with
  ;; `extractable-annotaton` and generates an application of
  ;; `extract[-closed]-id` to the wrapped form.
  (define (make-jit-on-call free-vars argss v name env convert-mode body-lifts lifts)
    (define ids (for/list ([id (in-hash-keys free-vars)])
                  id))
    (define (extract-id m id)
      (match m
        [`(variable-ref ,var) var]
        [`(unbox ,var) var]
        [`(unbox/check-undefined ,var ,_) var]
        [`(self ,m ,orig-id) orig-id]
        [`(self ,m) (extract-id m id)]
        [`,_ id]))
    (define captures (hash-keys
                      ;; `extract-id` for different `id`s can produce the
                      ;; same `id`, so hash and then convert to a list
                      (for/hash ([id (in-list ids)])
                        (values (extract-id (hash-ref env id) id) #t))))
    (define jitted-proc
      (or (match (and name
                      (hash-ref free-vars (unwrap name) #f)
                      (hash-ref env (unwrap name) #f))
            [`(self ,m ,orig-name)
             (cond
               [(eq? orig-name name)
                (define self-id (extract-id m name))
                `(let ([,self-id ,orig-name])
                   (letrec ([,name ,v])
                     ,name))]
               [else #f])]
            [`,_ #f])
          (match (and name
                      (hash-ref env (unwrap name) #f))
            [`(self . ,_)
             ;; Might have a direct self-call, so use `letrec`:
             `(letrec ([,name ,v])
                ,name)]
            [`,_ #f])
          (cond
            [name
             ;; No direct self-reference, but encourage the compiler
             ;; to name the procedure:
             `(let ([,name ,v])
                ,name)]
            [else v])))
    (define arity-mask (argss->arity-mask argss))
    (cond
      [(and (null? captures)
            (no-lifts? body-lifts))
       (define e (extractable-annotation jitted-proc arity-mask name))
       (define-values (get-e new-lifts)
         (cond
           [(convert-mode-need-lift? convert-mode) (add-lift e lifts)]
           [else (values `',e lifts)]))
       (values (if need-extract?
                   `(jitified-extract-closed ,get-e)
                   get-e)
               new-lifts)]
      [else
       (define e (extractable-annotation `(lambda ,(if (no-lifts? body-lifts)
                                                       captures
                                                       (cons lifts-id captures))
                                            ,jitted-proc)
                                        arity-mask
                                        name))
       (define-values (all-captures new-lifts)
         (cond
           [(no-lifts? body-lifts)
            (values captures lifts)]
           [(not (convert-mode-need-lift? convert-mode))
            (values (cons `',(lifts->datum body-lifts) captures) lifts)]
           [else
            (define-values (get-sub-lift new-lifts) (add-lift (lifts->datum body-lifts) lifts))
            (values (cons get-sub-lift captures) new-lifts)]))
       (define-values (get-e newer-lifts)
         (cond
           [(convert-mode-need-lift? convert-mode) (add-lift e new-lifts)]
           [else (values `',e new-lifts)]))
       (values (if need-extract?
                   `((jitified-extract ,get-e) . ,all-captures)
                   `(,get-e . ,all-captures))
               newer-lifts)]))

  ;; ----------------------------------------

  (define (top)
    ;; Match outer shape of a linklet produced by `schemify-linklet`
    ;; and lift in the linklet body:
    (let loop ([v v] [env #hasheq()])
      (match v
        [`(lambda ,args . ,body)
         (define new-body (jitify-schemified-body body (plain-add-args env args)))
         (reannotate v `(lambda ,args . ,new-body))]
        [`(let* ,bindings ,body)
         (define new-body (loop body (add-bindings env bindings)))
         (reannotate v `(let* ,bindings ,new-body))])))

  (define (jitify-schemified-body body env)
    (define top-env
      (for/fold ([env env]) ([v (in-list body)])
        (let loop ([v v] [env env])
          (match v
            [`(variable-set! ,var-id ,id . ,_)
             (hash-set env (unwrap id) `(variable-ref ,(unwrap var-id)))]
            [`(call-with-module-prompt ,_ ',ids ,_ ,var-ids ...)
             (for/fold ([env env]) ([id (in-list ids)]
                                    [var-id (in-list var-ids)])
               (hash-set env (unwrap id) `(variable-ref ,(unwrap var-id))))]
            [`(define ,id ,rhs) (plain-add-args env id #f)]
            [`(define-values ,ids ,rhs) (plain-add-args env ids #f)]
            [`(begin . ,vs)
             (for/fold ([env env]) ([v (in-wrap-list vs)])
               (loop v env))]
            [`,_ env]))))
    (let loop ([body body])
      (for/list ([v (in-list body)])
        (match v
          [`(variable-set! ,var-id ,id ',constance)
           (when constance
             ;; From now on, a direct reference is ok
             (set! top-env (hash-set top-env (unwrap id) '#:direct)))
           v]
          [`(define ,id ,rhs)
           ;; If there's a direct reference to `id` in `rhs`, then
           ;; `id` must not be mutable
           (define self-env (add-self top-env #hasheq() id))
           (reannotate v `(define ,id ,(jitify-top-expr rhs self-env id)))]
          [`(define-values ,ids ,rhs)
           (reannotate v `(define-values ,ids ,(jitify-top-expr rhs top-env #f)))]
          [`(begin . ,vs)
           (reannotate v `(begin . ,(loop vs)))]
          [`,_ (jitify-top-expr v top-env #f)]))))

  (define (jitify-top-expr v env name)
    ;; The `mutables` table doesn't track shadowing on the assumption
    ;; that local variable names are sufficiently distinguished to prevent
    ;; one mutable variable from polluting another in a different scope
    (define mutables (find-mutable #hasheq() v #hasheq()))
    (define convert-mode (init-convert-mode v))
    (define-values (new-v free lifts) (jitify-expr v env mutables #hasheq() no-lifts convert-mode name #f))
    (if (no-lifts? lifts)
        new-v
        `(let ([,lifts-id ',(lifts->datum lifts)])
           ,new-v)))

  ;; The `name` argument is a name to be given to the expresison `v`
  ;;  if it's a function. It also corresponds to a name that can be
  ;;  called directly, as long as it's mapped in `env` to a '(self ...)
  ;;  value.
  ;; The `in-name` argument is the current self `name` that is in effect
  ;;  for the current expression. It might be mapped to '(self ...)
  ;;  and need to be unmapped for a more nested function.
  (define (jitify-expr v env mutables free lifts convert-mode name in-name)
    (match v
      [`(lambda ,args . ,body)
       (define convert? (convert-mode-convert-lambda? convert-mode v))
       (define body-convert-mode (convert-mode-lambda-body-mode convert-mode convert?))
       (define self-env (if convert?
                            (activate-self (deactivate-self env in-name) name)
                            env))
       (define body-env (add-args self-env args mutables body-convert-mode))
       (define body-in-name (if convert? (or name '#:anonymous) in-name))
       (define body-lifts (if convert? no-lifts lifts))
       (define-values (new-body lam-body-free new-body-lifts)
         (jitify-body body body-env mutables #hasheq() body-lifts body-convert-mode #f body-in-name))
       (define lam-free (remove-args lam-body-free args))
       (define new-v (reannotate v `(lambda ,args . ,(mutable-box-bindings args mutables body-convert-mode
                                                                           new-body))))
       (define-values (converted-v new-lifts)
         (if (not convert?)
             (values new-v new-body-lifts)
             (make-jit-on-call lam-free (list args) new-v name self-env convert-mode new-body-lifts lifts)))
       (values converted-v
               (union-free free lam-free)
               new-lifts)]
      [`(case-lambda [,argss . ,bodys] ...)
       (define convert? (convert-mode-convert-lambda? convert-mode v))
       (define body-convert-mode (convert-mode-lambda-body-mode convert-mode convert?))
       (define self-env (if convert?
                            (activate-self (deactivate-self env in-name) name)
                            env))
       (define body-in-name (if convert? (or name '#:anonymous) in-name))
       (define body-lifts (if convert? no-lifts lifts))
       (define-values (rev-new-bodys lam-free new-body-lifts)
         (for/fold ([rev-new-bodys '()] [lam-free #hasheq()] [body-lifts body-lifts]) ([args (in-list argss)]
                                                                                       [body (in-list bodys)])
           (define body-env (add-args self-env args mutables body-convert-mode))
           (define-values (new-body lam-body-free new-body-lifts)
             (jitify-body body body-env mutables #hasheq() body-lifts body-convert-mode #f body-in-name))
           (values (cons new-body rev-new-bodys)
                   (union-free (remove-args lam-body-free args)
                               lam-free)
                   new-body-lifts)))
       (define new-v (reannotate v
                                 `(case-lambda
                                    ,@(for/list ([args (in-list argss)]
                                                 [body (in-list (reverse rev-new-bodys))])
                                        `[,args . ,(mutable-box-bindings args mutables body-convert-mode
                                                                         body)]))))
       (define-values (converted-v new-lifts)
         (if (not convert?)
             (values new-v new-body-lifts)
             (make-jit-on-call lam-free argss new-v name self-env convert-mode new-body-lifts lifts)))
       (values converted-v
               (union-free free lam-free)
               new-lifts)]
      [`(let . ,_) (jitify-let v env mutables free lifts convert-mode name in-name)]
      [`(letrec . ,_) (jitify-let v env mutables free lifts convert-mode name in-name)]
      [`(letrec* . ,_) (jitify-let v env mutables free lifts convert-mode name in-name)]
      [`(begin . ,vs)
       (define-values (new-body new-free new-lifts)
         (jitify-body vs env mutables free lifts convert-mode name in-name))
       (values (reannotate v `(begin . ,new-body))
               new-free
               new-lifts)]
      [`(begin0 ,v0 . ,vs)
       (define-values (new-v0 v0-free v0-lifts)
         (jitify-expr v0 env mutables free lifts (convert-mode-non-tail convert-mode) name in-name))
       (define-values (new-body new-free new-lifts)
         (jitify-body vs env mutables v0-free v0-lifts (convert-mode-non-tail convert-mode) #f in-name))
       (values (reannotate v `(begin0 ,new-v0 . ,new-body))
               new-free
               new-lifts)]
      [`($value ,e)
       (define-values (new-e new-free new-lifts)
         (jitify-expr e env mutables free lifts convert-mode name in-name))
       (values (reannotate v `($value ,new-e))
               new-free
               new-lifts)]
      [`(if ,tst ,thn ,els)
       (define sub-convert-mode (convert-mode-non-tail convert-mode))
       (define-values (new-tst new-free/tst new-lifts/tst)
         (jitify-expr tst env mutables free lifts sub-convert-mode #f in-name))
       (define-values (new-thn new-free/thn new-lifts/thn)
         (jitify-expr thn env mutables new-free/tst new-lifts/tst convert-mode name in-name))
       (define-values (new-els new-free/els new-lifts/els)
         (jitify-expr els env mutables new-free/thn new-lifts/thn convert-mode name in-name))
       (values (reannotate v `(if ,new-tst ,new-thn ,new-els))
               new-free/els
               new-lifts/els)]
      [`(with-continuation-mark ,key ,val ,body)
       (define sub-convert-mode (convert-mode-non-tail convert-mode))
       (define-values (new-key new-free/key new-lifts/key)
         (jitify-expr key env mutables free lifts sub-convert-mode #f in-name))
       (define-values (new-val new-free/val new-lifts/val)
         (jitify-expr val env mutables new-free/key new-lifts/key sub-convert-mode #f in-name))
       (define-values (new-body new-free/body new-lifts/body)
         (jitify-expr body env mutables new-free/val new-lifts/val convert-mode name in-name))
       (values (reannotate v `(with-continuation-mark ,new-key ,new-val ,new-body))
               new-free/body
               new-lifts/body)]
      [`(quote ,_) (values v free lifts)]
      [`(set! ,var ,rhs)
       (define-values (new-rhs new-free new-lifts)
         (jitify-expr rhs env mutables free lifts (convert-mode-non-tail convert-mode) var in-name))
       (define id (unwrap var))
       (define dest (hash-ref env id #f))
       (cond
         [(and (not in-name)
               (match dest
                 [`(variable-ref ,_) #t]
                 [`,_ #f]))
          ;; Not under lambda: don't rewrite references to definitions
          (values `(set! ,var ,new-rhs)
                  new-free
                  new-lifts)]
         [else
          (define newer-free (if dest
                                 (hash-set new-free id dest)
                                 new-free))
          (define new-v
            (match (hash-ref env id '#:direct)
              [`#:direct (reannotate v `(set! ,var ,new-rhs))]
              [`(self ,m . ,_) (error 'set! "[internal error] self-referenceable ~s" id)]
              [`(variable-ref ,var-id) (reannotate v `(variable-set! ,var-id ,new-rhs '#f))]
              [`(unbox ,box-id) (reannotate v `(set-box! ,box-id ,new-rhs))]
              [`(unbox/check-undefined ,box-id ,_) (reannotate v `(set-box!/check-undefined ,box-id ,new-rhs ',var))]))
          (values new-v newer-free new-lifts)])]
      [`(call-with-values ,proc1 ,proc2)
       (define proc-convert-mode (convert-mode-called convert-mode))
       (define-values (new-proc1 new-free1 new-lifts1)
         (jitify-expr proc1 env mutables free lifts proc-convert-mode #f in-name))
       (define-values (new-proc2 new-free2 new-lifts2)
         (jitify-expr proc2 env mutables new-free1 new-lifts1 proc-convert-mode #f in-name))
       (define call-with-values-id (if (and (lambda? new-proc1) (lambda? new-proc2))
                                       'call-with-values
                                       '#%call-with-values))
       (values (reannotate v `(,call-with-values-id ,new-proc1 ,new-proc2))
               new-free2
               new-lifts2)]
      [`(call-with-module-prompt ,proc ,var-info ...)
       (define proc-convert-mode (convert-mode-called convert-mode))
       (define-values (new-proc new-free new-lifts)
         (jitify-expr proc env mutables free lifts proc-convert-mode #f in-name))
       (values (reannotate v `(call-with-module-prompt ,new-proc . ,var-info))
               new-free
               new-lifts)]
      [`(#%app ,_ ...)
       (define-values (new-vs new-free new-lifts)
         (jitify-body (wrap-cdr v) env mutables free lifts (convert-mode-non-tail convert-mode) #f in-name))
       (values (reannotate v `(#%app . ,new-vs))
               new-free
               new-lifts)]
      [`(,rator ,_ ...)
       (define u (unwrap rator))
       (match (and (symbol? u) (hash-ref env u #f))
         [`(self ,_ ,orig-id)
          ;; Keep self call as direct
          (define-values (new-vs new-free new-lifts)
            (jitify-body (wrap-cdr v) env mutables free lifts (convert-mode-non-tail convert-mode) #f in-name))
          (values (reannotate v `(,rator . ,new-vs))
                  new-free
                  new-lifts)]
         [`,x
          (define-values (new-vs new-free new-lifts)
            (jitify-body v env mutables free lifts (convert-mode-non-tail convert-mode) #f in-name))
          (values (reannotate v new-vs)
                  new-free
                  new-lifts)])]
      [`,var
       (define id (unwrap var))
       (define dest (hash-ref env id #f))
       (cond
         [(and (not in-name)
               (match dest
                 [`(variable-ref ,_) #t]
                 [`,_ #f]))
          ;; Not under lambda: don't rewrite references to definitions
          (values var free lifts)]
         [else
          (define new-var
            (match dest
              [`#f var]
              [`#:direct var]
              [`(self ,u . ,_) (reannotate v u)]
              [`,u (reannotate v u)]))
          (define new-free
            (if dest
                (hash-set free id dest)
                free))
          (values new-var
                  new-free
                  lifts)])]))

  (define (lambda? v)
    (match v
      [`(lambda . ,_) #t]
      [`(case-lambda . ,_) #t]
      [`,_ #f]))

  (define (jitify-body vs env mutables free lifts convert-mode name in-name)
    (let loop ([vs vs] [free free] [lifts lifts])
      (cond
        [(wrap-null? vs) (values null free lifts)]
        [(wrap-null? (wrap-cdr vs))
         (define-values (new-v new-free new-lifts)
           (jitify-expr (wrap-car vs) env mutables free lifts convert-mode name in-name))
         (values (list new-v) new-free new-lifts)]
        [else
         (define-values (new-v new-free new-lifts)
           (jitify-expr (wrap-car vs) env mutables free lifts (convert-mode-non-tail convert-mode) #f in-name))
         (define-values (new-rest newer-free newer-lifts)
           (loop (wrap-cdr vs) new-free new-lifts))
         (values (cons new-v new-rest)
                 newer-free
                 newer-lifts)])))

  (define (jitify-let v env mutables free lifts convert-mode name in-name)
    (match v
      [`(,let-form ([,ids ,rhss] ...) . ,body)
       (define rec?
         (and (case (unwrap let-form)
                [(letrec letrec*) #t]
                [else #f])
              ;; Use simpler `let` code if we're not responsible for boxing:
              (convert-mode-box-mutables? convert-mode)))
       (define rhs-convert-mode (convert-mode-non-tail convert-mode))
       (define rhs-env (if rec?
                           (add-args/unbox env ids mutables
                                           (lambda (var) #t)
                                           (not (for/and ([rhs (in-list rhss)])
                                                  (lambda? rhs)))
                                           convert-mode)
                           env))
       (define-values (rev-new-rhss rhs-free rhs-lifts)
         (for/fold ([rev-new-rhss '()] [free #hasheq()] [lifts lifts]) ([id (in-list ids)]
                                                                        [rhs (in-list rhss)])
           (define self-env
             (if rec?
                 (add-self rhs-env mutables id)
                 rhs-env))
           (define-values (new-rhs rhs-free rhs-lifts)
             (jitify-expr rhs self-env mutables free lifts rhs-convert-mode id in-name))
           (values (cons new-rhs rev-new-rhss) rhs-free rhs-lifts)))
       (define local-env
         (add-args/unbox env ids mutables
                         (lambda (var) (and rec? (hash-ref rhs-free var #f)))
                         #f
                         convert-mode))
       (define-values (new-body new-free new-lifts)
         (jitify-body body local-env mutables (union-free free rhs-free) rhs-lifts convert-mode name in-name))
       (define new-v
         (cond
           [(not rec?)
            ;; Wrap boxes around rhs results as needed:
            `(,let-form ,(for/list ([id (in-list ids)]
                                    [new-rhs (in-list (reverse rev-new-rhss))])
                           `[,id ,(if (and (convert-mode-box-mutables? convert-mode)
                                           (hash-ref mutables (unwrap id) #f))
                                      `(box ,new-rhs)
                                      new-rhs)])
                        . ,new-body)]
           [else
            ;; Allocate boxes first, then fill in
            `(let ,(for*/list ([id (in-list ids)]
                               #:when (hash-ref rhs-free (unwrap id) #f))
                     `[,id (box unsafe-undefined)])
               ;; Using nested `let`s to force left-to-right
               ,(for/fold ([body (body->expr new-body)]) ([id (in-list (reverse ids))]
                                                          [new-rhs (in-list rev-new-rhss)])
                  `(let (,(cond
                            [(hash-ref rhs-free (unwrap id) #f)
                             `[,(gensym 'ignored) (set-box! ,id ,new-rhs)]]
                            [(hash-ref mutables (unwrap id) #f)
                             `[,id (box ,new-rhs)]]
                            [else `[,id ,new-rhs]]))
                     ,body)))]))
       (values (reannotate v new-v)
               (remove-args new-free ids)
               new-lifts)]))

  (define (mutable-box-bindings args mutables convert-mode body)
    (cond
      [(convert-mode-box-mutables? convert-mode)
       (define bindings
         (let loop ([args args])
           (cond
             [(wrap-null? args) null]
             [(wrap-pair? args)
              (define id (wrap-car args))
              (define var (unwrap id))
              (define rest (loop (wrap-cdr args)))
              (if (hash-ref mutables var #f)
                  (cons `[,id (box ,id)] rest)
                  rest)]
             [else (loop (list args))])))
       (if (null? bindings)
           body
           `((let ,bindings . ,body)))]
      [else body]))

  ;; ----------------------------------------

  ;; When mutables and convert mode are not relevant:
  (define (plain-add-args env args [replace? #t])
    (define (add-one id)
      (define u-id (unwrap id))
      (if (or replace?
              (not (hash-ref env u-id #f)))
          (hash-set env u-id '#:direct)
          env))
    (match args
      [`(,id . ,args)
       (plain-add-args (add-one id) args replace?)]
      [`() env]
      [`,id (add-one id)]))

  ;; Add a binding to an environment, record whether it needs
  ;; to be unboxed on reference:
  (define (add-args env args mutables convert-mode)
    (define (add-one id)
      (define u (unwrap id))
      (define val (if (and (convert-mode-box-mutables? convert-mode)
                           (hash-ref mutables u #f))
                      `(unbox ,id)
                      '#:direct))
      (hash-set env u val))
    (match args
      [`(,id . ,args)
       (add-args (add-one id) args mutables convert-mode)]
      [`() env]
      [`,id (add-one id)]))

  ;; Further generalization of `add-args` to add undefined-checking
  ;; variant of unbox:
  (define (add-args/unbox env args mutables var-rec? maybe-undefined? convert-mode)
    (define (add-one id)
      (define var (unwrap id))
      (cond
        [maybe-undefined? (hash-set env var `(unbox/check-undefined ,id ',id))]
        [(not (or (var-rec? var) (and (convert-mode-box-mutables? convert-mode)
                                      (hash-ref mutables var #f))))
         (hash-set env var '#:direct)]
        [else (hash-set env var `(unbox ,id))]))
    (match args
      [`(,id . ,args)
       (add-args/unbox (add-one id) args mutables var-rec? maybe-undefined? convert-mode)]
      [`() env]
      [`,id (add-one id)]))

  (define (remove-args env args)
    (match args
      [`(,id . ,args)
       (remove-args (hash-remove env (unwrap id)) args)]
      [`() env]
      [`,id (hash-remove env (unwrap id))]))

  (define (add-bindings env bindings)
    (match bindings
      [`([,ids ,_] ...)
       (for/fold ([env env]) ([id (in-list ids)])
         (plain-add-args env id))]))

  (define (add-self env mutables name)
    (define u (unwrap name))
    (cond
      [(hash-ref mutables u #f)
       env]
      [else
       (hash-set env u `(self ,(hash-ref env u '#:direct)))]))

  ;; Adjust an environment to indicate that `name` in an application
  ;; position is a self-call, which helps preserves the visiblilty of
  ;; loops to a later compiler
  (define (activate-self env name)
    (cond
      [name
       (define (genself) (gensym 'self))
       (define u (unwrap name))
       (define new-m
         (match (hash-ref env u #f)
           [`(self #:direct)
            `(self ,(genself) ,name)]
           [`(self (variable-ref ,orig-id))
            `(self (variable-ref ,orig-id) ,orig-id)]
           [`(self (unbox ,orig-id))
            `(self (unbox ,(genself)) ,orig-id)]
           [`(self (unbox/check-undefined ,orig-id ,sym))
            `(self (unbox/check-undefined ,(genself) ,sym) ,orig-id)]
           [`,_ #f]))
       (if new-m
           (hash-set env u new-m)
           env)]
      [else env]))

  ;; Adjust an environment to indicate that applying `name` is no
  ;; longer a self call
  (define (deactivate-self env name)
    (cond
      [name
       (define u (unwrap name))
       (match (hash-ref env u #f)
         [`(self ,m ,_) (hash-set env u m)]
         [`,_ env])]
      [else env]))

  ;; ----------------------------------------

  (define (argss->arity-mask argss)
    (for/fold ([mask 0]) ([args (in-list argss)])
      (bitwise-ior mask
                   (let loop ([args args] [count 0])
                     (cond
                       [(wrap-null? args) (arithmetic-shift 1 count)]
                       [(wrap-pair? args) (loop (wrap-cdr args) (add1 count))]
                       [else (bitwise-xor -1 (sub1 (arithmetic-shift 1 count)))])))))

  (define (de-dot args)
    (cond
      [(wrap-pair? args) (cons (wrap-car args)
                               (de-dot (wrap-cdr args)))]
      [else (list args)]))

  (define (union-free a b)
    (cond
      [((hash-count b) . < . (hash-count a)) (union-free b a)]
      [else
       (for/fold ([b b]) ([(k v) (in-hash a)])
         (hash-set b k v))]))

  (define (body->expr body)
    (cond
      [(and (wrap-pair? body) (wrap-null? (wrap-cdr body)))
       (wrap-car body)]
      [else `(begin . ,body)]))

  ;; ----------------------------------------

  (define (find-mutable env v accum)
    (match v
      [`(lambda ,args . ,body)
       (body-find-mutable (plain-add-args env args) body accum)]
      [`(case-lambda [,argss . ,bodys] ...)
       (for/fold ([accum accum]) ([args (in-list argss)]
                                  [body (in-list bodys)])
         (body-find-mutable (plain-add-args env args) body accum))]
      [`(let . ,_) (find-mutable-in-let env v accum)]
      [`(letrec . ,_) (find-mutable-in-let env v accum)]
      [`(letrec* . ,_) (find-mutable-in-let env v accum)]
      [`(begin . ,vs) (body-find-mutable env vs accum)]
      [`(begin0 . ,vs) (body-find-mutable env vs accum)]
      [`(if ,tst ,thn ,els)
       (find-mutable env tst
                     (find-mutable env thn
                                   (find-mutable env els accum)))]
      [`(with-continuation-mark ,key ,val ,body)
       (find-mutable env key
                     (find-mutable env val
                                   (find-mutable env body accum)))]
      [`(quote ,_) accum]
      [`(set! ,var ,rhs)
       (define id (unwrap var))
       (find-mutable env rhs (if (hash-ref env id #f)
                                 (hash-set accum id #t)
                                 accum))]
      [`(,_ ...) (body-find-mutable env v accum)]
      [`,_ accum]))

  (define (body-find-mutable env body accum)
    (for/fold ([accum accum]) ([v (in-wrap-list body)])
      (find-mutable env v accum)))

  (define (find-mutable-in-let env v accum)
    (match v
      [`(,let-form ([,ids ,rhss] ...) . ,body)
       (define local-env
         (for/fold ([env env]) ([id (in-list ids)])
           (plain-add-args env id)))
       (define rhs-env
         (case (unwrap let-form)
           [(letrec letrec* letrec*-values) local-env]
           [else env]))
       (body-find-mutable local-env
                          body
                          (for/fold ([accum accum]) ([id (in-list ids)]
                                                     [rhs (in-list rhss)])
                            (find-mutable rhs-env rhs accum)))]))

  ;; ----------------------------------------
  ;; Convert mode
  ;;
  ;; If there's no size threshold for conversion, then convert mode is
  ;; a pair of 'called or 'not-called (where the former means "definitely
  ;; called, so don't bother wrapper) and 'lift or 'no-lift.
  ;;
  ;; If there's a size threshold, then a convert mode is a
  ;; `convert-mode` instance.

  (define (init-convert-mode v)
    (cond
      [convert-size-threshold
       (convert-mode (record-sizes v) #f #f #f)]
      [else '(not-called . no-lift)]))

  (define (convert-mode-convert-lambda? cm v)
    (cond
      [(pair? cm) (eq? (car cm) 'not-called)]
      [(convert-mode-called? cm) #f]
      [(convert-mode-no-more-conversions? cm) #f]
      [((hash-ref (convert-mode-sizes cm) v) . >= . convert-size-threshold) #f]
      [else #t]))

  (define (convert-mode-lambda-body-mode cm convert?)
    (cond
      [(convert-mode? cm)
       (if convert?
           (convert-mode 'not-needed #f need-lift? #t)
           (convert-mode-non-tail cm))]
      [else (if (or (not need-lift?)
                    (and (eq? 'no-lift (cdr cm))
                         (not convert?)))
                '(not-called . no-lift)
                '(not-called . lift))]))

  (define (convert-mode-non-tail cm)
    (cond
      [(convert-mode? cm)
       (struct-copy convert-mode cm
                    [called? #f])]
      [else (if (eq? 'no-lift (cdr cm))
                '(not-called . no-lift)
                '(not-called . lift))]))

  (define (convert-mode-called cm)
    (cond
      [(convert-mode? cm)
       (struct-copy convert-mode cm
                    [called? #t])]
      [else (if (eq? 'no-lift (cdr cm))
                '(called . no-lift)
                '(called . lift))]))

  (define (convert-mode-box-mutables? cm)
    (cond
      [(convert-mode? cm)
       (not (convert-mode-no-more-conversions? cm))]
      [else #t]))

  (define (convert-mode-need-lift? cm)
    (cond
      [(convert-mode? cm) (convert-mode-lift? cm)]
      [else (eq? 'lift (cdr cm))]))

  ;; ----------------------------------------

  ;; Keep lifts in a list, in reverse order of eventual
  ;; registration in a vector, and prefix the list with its length
  
  (define no-lifts '(0))

  (define (no-lifts? v)
    (zero? (car v)))

  (define (lifts->datum v)
    (list->vector (reverse (cdr v))))

  (define (add-lift e lifts)
    (values `(unsafe-vector-ref ,lifts-id ,(car lifts))
            (cons (add1 (car lifts))
                  (cons e (cdr lifts)))))

  ;; ----------------------------------------

  (define (record-sizes v)
    (let ([sizes (make-hasheq)])
      (record-sizes! v sizes)
      sizes))

  (define (record-size! v sizes size)
    (hash-set! sizes v size)
    size)

  (define (record-sizes! v sizes)
    (match v
      [`(lambda ,args . ,body)
       (record-size! v sizes (body-record-sizes! body sizes))]
      [`(case-lambda [,_ . ,bodys] ...)
       (define new-size
         (for/sum ([body (in-list bodys)])
           (body-record-sizes! body sizes)))
       (record-size! v sizes new-size)]
      [`(let . ,_) (record-sizes-in-let! v sizes)]
      [`(letrec . ,_) (record-sizes-in-let! v sizes)]
      [`(letrec* . ,_) (record-sizes-in-let! v sizes)]
      [`(begin . ,vs) (add1 (body-record-sizes! vs sizes))]
      [`(begin0 . ,vs) (add1 (body-record-sizes! vs sizes))]
      [`(if ,tst ,thn ,els)
       (+ 1
          (record-sizes! tst sizes)
          (record-sizes! thn sizes)
          (record-sizes! els sizes))]
      [`(with-continuation-mark ,key ,val ,body)
       (+ 1
          (record-sizes! key sizes)
          (record-sizes! val sizes)
          (record-sizes! body sizes))]
      [`(quote ,_) 1]
      [`(set! ,_ ,rhs)
       (add1 (record-sizes! rhs sizes))]
      [`(,_ ...) (body-record-sizes! v sizes)]
      [`,_ 1]))

  (define (body-record-sizes! body sizes)
    (for/sum ([v (in-wrap-list body)])
      (record-sizes! v sizes)))

  (define (record-sizes-in-let! v sizes)
    (match v
      [`(,let-form ([,_ ,rhss] ...) . ,body)
       (+ 1
          (for/sum ([rhs (in-list rhss)])
            (record-sizes! rhs sizes))
          (body-record-sizes! body sizes))]))

  ;; ----------------------------------------
  
  (top))

;; ============================================================

(module+ main
  (require racket/pretty)
  (struct wrapped (proc arity-mask name) #:prefab)
  (pretty-print
   (jitify-schemified-linklet (values ; datum->correlated
                               '(lambda (iv xv do-immediate)
                                  (define top (letrec ([odd (lambda (x) (even x))]
                                                       [even (lambda (x) (odd x))]
                                                       [selfx (lambda (x) (selfx x))]
                                                       [selfy (lambda (x) (vector (selfy x) selfy))])
                                                (odd 5)))
                                  (define top-selfx (lambda (x) (top-selfx x)))
                                  (variable-set! top-selfx-var top-selfx 'const)
                                  (define top-selfy (lambda (x) (vector (top-selfy x) top-selfy)))
                                  (variable-set! top-selfy-var top-selfy 'const)
                                  (call-with-values (lambda (x) (x (lambda (w) (w))))
                                    (lambda (z w) 10))
                                  (call-with-values (lambda (x) (x (lambda (w) (w))))
                                    (letrec ([selfz (lambda (z) (selfz (selfz z)))])
                                      (lambda (z w) (selfz w))))
                                  (call-with-values (lambda (x) (x (lambda (w) (w))))
                                    void)
                                  (define y (letrec ([f (lambda (x) (f (cons x x)))]
                                                     [g (lambda (q) (set! f g) (f q))])
                                              (list (lambda (f) (list x)))))
                                  (define x (lambda (j) j))
                                  (define x1 (lambda () (lambda () (other iv))))
                                  (define x2 (lambda () (letrec ([other (lambda () (other iv))])
                                                          other)))
                                  (define whatever (begin (variable-set! xv x 'const) (void)))
                                  (define end (letrec ([w (lambda (x) (let ([proc (lambda (x) x)])
                                                                        (proc q)))]
                                                       [q q])
                                                (lambda (j) (set! q j))))
                                  (define topz (letrec ([helper (lambda (x)
                                                                  (helper (topz x)))])
                                                 (lambda (y) (helper y))))
                                  (variable-set! topz-var topz 'const)
                                  (do-immediate topz)
                                  (define sets-arg (lambda (x)
                                                     (values (lambda () (set! x (add1 x)))
                                                             (lambda () x))))
                                  (define nested-3 (lambda (x)
                                                     (lambda ()
                                                       (lambda () x))))
                                  (letrec ([outer
                                            (lambda (x)
                                              (letrec ([inner
                                                        (lambda (y)
                                                          (outer y))])
                                                (inner x)))])
                                    (outer 5))
                                  (lambda () (let ([x 5]) (set! x 6) x))
                                  (case-lambda
                                    [(q) (x q)]
                                    [() (lambda () (x1))])))
                              #t
                              #t ; need-lift?
                              #f ; size threshold
                              wrapped)))
