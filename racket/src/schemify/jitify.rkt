#lang racket/base
(require "match.rkt"
         "wrap.rkt")

;; Convert `lambda`s to make them fully closed, which is compatible
;; with JIT compilation of the `lambda` or separate ahead-of-time
;; compilation (as opposed to compiling a whole linklet).

;; If `convert-size-threashold` is #f, then every `lambda` is
;; converted. If it's a number, then only `lambda`s smaller than the
;; threshold are converted, and and no `lambda` within a converted
;; `lambda` is converted. So, supplying a numerical threshold is
;; useful for drawing a boundary between compiled and non-compiled
;; code, as opposed to a true JIT setup.

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

(define (jitify-schemified-linklet v
                                   need-extract?
                                   convert-size-threshold ; #f or a number; see above
                                   extractable-annotation
                                   reannotate)

  ;; Constucts a closed `lambda` form as wrapped with
  ;; `extractable-annotaton` and generates an application of
  ;; `extract[-closed]-id` to the wrapped form.
  (define (make-jit-on-call free-vars argss v name env)
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
      [(null? captures)
       (let ([e (extractable-annotation jitted-proc arity-mask name)])
         (if need-extract?
             `(jitified-extract-closed ',e)
             `',e))]
      [else
       (let ([e (extractable-annotation `(lambda ,captures
                                           ,jitted-proc)
                                        arity-mask
                                        name)])
         (if need-extract?
             `((jitified-extract ',e) . ,captures)
             `(',e . ,captures)))]))

  ;; ----------------------------------------

  (define (top)
    ;; Match outer shape of a linklet produced by `schemify-linklet`
    ;; and lift in the linklet body:
    (let loop ([v v] [env #hasheq()])
      (match v
        [`(lambda ,args . ,body)
         (define new-body (jitify-schemified-body body (plain-add-args env args)))
         (if (for/and ([old (in-list body)]
                       [new (in-list new-body)])
               (eq? old new))
             v
             (reannotate v `(lambda ,args . ,new-body)))]
        [`(let* ,bindings ,body)
         (define new-body (loop body (add-bindings env bindings)))
         (if (eq? body new-body)
             v
             (reannotate v `(let* ,bindings ,new-body)))])))

  (define (jitify-schemified-body body env)
    (define top-env
      (for/fold ([env env]) ([v (in-list body)])
        (let loop ([v v] [env env])
          (match v
            [`(variable-set! ,var-id ,id . ,_)
             (hash-set env (unwrap id) `(variable-ref ,(unwrap var-id)))]
            [`(define ,_ (begin (variable-set! ,var-id ,id . ,_) (void)))
             (hash-set env (unwrap id) `(variable-ref ,(unwrap var-id)))]
            [`(define ,id ,rhs) (plain-add-args env id)]
            [`(define-values ,ids ,rhs) (plain-add-args env ids)]
            [`(begin . ,vs)
             (for/fold ([env env]) ([v (in-wrap-list vs)])
               (loop v env))]
            [`,_ env]))))
    (let loop ([body body])
      (for/list ([v (in-list body)])
        (match v
          [`(variable-set! ,var-id ,id . ,_) v]
          [`(define ,_ (begin (variable-set! ,var-id ,id . ,_) (void))) v]
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
    (define-values (new-v free) (jitify-expr v env mutables #hasheq() convert-mode name #f))
    new-v)

  ;; The `name` argument is a name to be given to the expresison `v`
  ;;  if it's a function. It also corresponds to a name that can be
  ;;  called directly, as long as it's mapped in `env` to a '(self ...)
  ;;  value.
  ;; The `in-name` argument is the current self `name` that is in effect
  ;;  for the current expression. It might be mapped to '(self ...)
  ;;  and need to be unmapped for a more nested function.
  (define (jitify-expr v env mutables free convert-mode name in-name)
    (match v
      [`(lambda ,args . ,body)
       (define convert? (convert-mode-convert-lambda? convert-mode v))
       (define body-convert-mode (convert-mode-lambda-body-mode convert-mode convert?))
       (define self-env (if convert?
                            (activate-self (deactivate-self env in-name) name)
                            env))
       (define body-env (add-args self-env args mutables body-convert-mode))
       (define body-in-name (if convert? (or name '#:anonymous) in-name))
       (define-values (new-body lam-body-free)
         (jitify-body body body-env mutables #hasheq() body-convert-mode #f body-in-name))
       (define lam-free (remove-args lam-body-free args))
       (define new-v (reannotate v `(lambda ,args . ,(mutable-box-bindings args mutables body-convert-mode
                                                                           new-body))))
       (values (if (not convert?)
                   new-v
                   (make-jit-on-call lam-free (list args) new-v name self-env))
               (union-free free lam-free))]
      [`(case-lambda [,argss . ,bodys] ...)
       (define convert? (convert-mode-convert-lambda? convert-mode v))
       (define body-convert-mode (convert-mode-lambda-body-mode convert-mode convert?))
       (define self-env (if convert?
                            (activate-self (deactivate-self env in-name) name)
                            env))
       (define body-in-name (if convert? (or name '#:anonymous) in-name))
       (define-values (rev-new-bodys lam-free)
         (for/fold ([rev-new-bodys '()] [lam-free #hasheq()]) ([args (in-list argss)]
                                                               [body (in-list bodys)])
           (define body-env (add-args self-env args mutables body-convert-mode))
           (define-values (new-body lam-body-free)
             (jitify-body body body-env mutables #hasheq() body-convert-mode #f body-in-name))
           (values (cons new-body rev-new-bodys)
                   (union-free (remove-args lam-body-free args)
                               lam-free))))
       (define new-v (reannotate v
                                 `(case-lambda
                                    ,@(for/list ([args (in-list argss)]
                                                 [body (in-list (reverse rev-new-bodys))])
                                        `[,args . ,(mutable-box-bindings args mutables body-convert-mode
                                                                         body)]))))
       (values (if (not convert?)
                   new-v
                   (make-jit-on-call lam-free argss new-v name self-env))
               (union-free free lam-free))]
      [`(let . ,_) (jitify-let v env mutables free convert-mode name in-name)]
      [`(letrec . ,_) (jitify-let v env mutables free convert-mode name in-name)]
      [`(letrec* . ,_) (jitify-let v env mutables free convert-mode name in-name)]
      [`(begin . ,vs)
       (define-values (new-body new-free) (jitify-body vs env mutables free convert-mode name in-name))
       (values (reannotate v `(begin . ,new-body))
               new-free)]
      [`(begin0 ,v0 . ,vs)
       (define-values (new-v0 v0-free)
         (jitify-expr v0 env mutables free (convert-mode-non-tail convert-mode) name in-name))
       (define-values (new-body new-free)
         (jitify-body vs env mutables v0-free (convert-mode-non-tail convert-mode) #f in-name))
       (values (reannotate v `(begin0 ,new-v0 . ,new-body))
               new-free)]
      [`(pariah ,e)
       (define-values (new-e new-free) (jitify-expr e env mutables free convert-mode name in-name))
       (values (reannotate v `(pariah ,new-e))
               new-free)]
      [`(if ,tst ,thn ,els)
       (define sub-convert-mode (convert-mode-non-tail convert-mode))
       (define-values (new-tst new-free/tst) (jitify-expr tst env mutables free sub-convert-mode #f in-name))
       (define-values (new-thn new-free/thn) (jitify-expr thn env mutables new-free/tst convert-mode name in-name))
       (define-values (new-els new-free/els) (jitify-expr els env mutables new-free/thn convert-mode name in-name))
       (values (reannotate v `(if ,new-tst ,new-thn ,new-els))
               new-free/els)]
      [`(with-continuation-mark ,key ,val ,body)
       (define sub-convert-mode (convert-mode-non-tail convert-mode))
       (define-values (new-key new-free/key) (jitify-expr key env mutables free sub-convert-mode #f in-name))
       (define-values (new-val new-free/val) (jitify-expr val env mutables new-free/key sub-convert-mode #f in-name))
       (define-values (new-body new-free/body) (jitify-expr body env mutables new-free/val convert-mode name in-name))
       (values (reannotate v `(with-continuation-mark ,new-key ,new-val ,new-body))
               new-free/body)]
      [`(quote ,_) (values v free)]
      [`(set! ,var ,rhs)
       (define-values (new-rhs new-free) (jitify-expr rhs env mutables free (convert-mode-non-tail convert-mode) var in-name))
       (define id (unwrap var))
       (define dest (hash-ref env id #f))
       (cond
         [(and (not in-name)
               (match dest
                 [`(variable-ref ,_) #t]
                 [`,_ #f]))
          ;; Not under lambda: don't rewrite references to definitions
          (values `(set! ,var ,new-rhs)
                  new-free)]
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
          (values new-v newer-free)])]
      [`(call-with-values ,proc1 ,proc2)
       (define proc-convert-mode (convert-mode-called convert-mode))
       (define-values (new-proc1 new-free1) (jitify-expr proc1 env mutables free proc-convert-mode #f in-name))
       (define-values (new-proc2 new-free2) (jitify-expr proc2 env mutables new-free1 proc-convert-mode #f in-name))
       (define call-with-values-id (if (and (lambda? new-proc1) (lambda? new-proc2))
                                       'call-with-values
                                       '#%call-with-values))
       (values (reannotate v `(,call-with-values-id ,new-proc1 ,new-proc2))
               new-free2)]
      [`(#%app ,_ ...)
       (define-values (new-vs new-free)
         (jitify-body (wrap-cdr v) env mutables free (convert-mode-non-tail convert-mode) #f in-name))
       (values (reannotate v `(#%app . ,new-vs))
               new-free)]
      [`(,rator ,_ ...)
       (define u (unwrap rator))
       (match (and (symbol? u) (hash-ref env u #f))
         [`(self ,_ ,orig-id)
          ;; Keep self call as direct
          (define-values (new-vs new-free)
            (jitify-body (wrap-cdr v) env mutables free (convert-mode-non-tail convert-mode) #f in-name))
          (values (reannotate v `(,rator . ,new-vs))
                  new-free)]
         [`,x
          (define-values (new-vs new-free)
            (jitify-body v env mutables free (convert-mode-non-tail convert-mode) #f in-name))
          (values (reannotate v new-vs)
                  new-free)])]
      [`,var
       (define id (unwrap var))
       (define dest (hash-ref env id #f))
       (cond
         [(and (not in-name)
               (match dest
                 [`(variable-ref ,_) #t]
                 [`,_ #f]))
          ;; Not under lambda: don't rewrite references to definitions
          (values var free)]
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
                  new-free)])]))

  (define (lambda? v)
    (match v
      [`(lambda . ,_) #t]
      [`(case-lambda . ,_) #t]
      [`,_ #f]))

  (define (jitify-body vs env mutables free convert-mode name in-name)
    (let loop ([vs vs] [free free])
      (cond
        [(wrap-null? vs) (values null free)]
        [(wrap-null? (wrap-cdr vs))
         (define-values (new-v new-free)
           (jitify-expr (wrap-car vs) env mutables free convert-mode name in-name))
         (values (list new-v) new-free)]
        [else
         (define-values (new-v new-free)
           (jitify-expr (wrap-car vs) env mutables free (convert-mode-non-tail convert-mode) #f in-name))
         (define-values (new-rest newer-free)
           (loop (wrap-cdr vs) new-free))
         (values (cons new-v new-rest)
                 newer-free)])))

  (define (jitify-let v env mutables free convert-mode name in-name)
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
       (define-values (rev-new-rhss rhs-free)
         (for/fold ([rev-new-rhss '()] [free #hasheq()]) ([id (in-list ids)]
                                                          [rhs (in-list rhss)])
           (define self-env
             (if rec?
                 (add-self rhs-env mutables id)
                 rhs-env))
           (define-values (new-rhs rhs-free)
             (jitify-expr rhs self-env mutables free rhs-convert-mode id in-name))
           (values (cons new-rhs rev-new-rhss) rhs-free)))
       (define local-env
         (add-args/unbox env ids mutables
                         (lambda (var) (and rec? (hash-ref rhs-free var #f)))
                         #f
                         convert-mode))
       (define-values (new-body new-free)
         (jitify-body body local-env mutables (union-free free rhs-free) convert-mode name in-name))
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
               (remove-args new-free ids))]))

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
  (define (plain-add-args env args)
    (define (add-one id)
      (hash-set env (unwrap id) '#:direct))
    (match args
      [`(,id . ,args)
       (plain-add-args (add-one id) args)]
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
  ;; simply 'called or 'not-called.
  ;;
  ;; If there's a size threshold, then a convert mode is a
  ;; `convert-mode` instance.

  (struct convert-mode (sizes called? no-more-conversions?))
  
  (define (init-convert-mode v)
    (cond
      [convert-size-threshold
       (convert-mode (record-sizes v) #f #f)]
      [else 'not-called]))

  (define (convert-mode-convert-lambda? cm v)
    (cond
      [(eq? cm 'called) #f]
      [(eq? cm 'not-called) #t]
      [(convert-mode-called? cm) #f]
      [(convert-mode-no-more-conversions? cm) #f]
      [((hash-ref (convert-mode-sizes cm) v) . >= . convert-size-threshold) #f]
      [else #t]))

  (define (convert-mode-lambda-body-mode cm convert?)
    (cond
      [(convert-mode? cm)
       (if convert?
           (convert-mode 'not-needed #f #t)
           (convert-mode-non-tail cm))]
      [else 'not-called]))

  (define (convert-mode-non-tail cm)
    (cond
      [(convert-mode? cm)
       (struct-copy convert-mode cm
                    [called? #f])]
      [else 'not-called]))

  (define (convert-mode-called cm)
    (cond
      [(convert-mode? cm)
       (struct-copy convert-mode cm
                    [called? #t])]
      [else 'called]))

  (define (convert-mode-box-mutables? cm)
    (cond
      [(convert-mode? cm)
       (not (convert-mode-no-more-conversions? cm))]
      [else #t]))

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
                                  (letrec ([outer
                                            (lambda (x)
                                              (letrec ([inner
                                                        (lambda (y)
                                                          (outer y))])
                                                (inner x)))])
                                    (outer 5))
                                  (lambda () (let ([x 5]) (set! x 6) x))))
                              #t
                              #f ; size threshold
                              vector
                              (lambda (v u) u)
                              values)))
