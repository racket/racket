;; Replace `define` to perform simple function lifting, which avoids
;; having to allocate closures for local loops (i.e., a more
;; Racket-like allocation model). Since it only has to work for
;; Rumble's implementation, the lifter doesn't have to be general or
;; scalable. The lifter transforms unexpanded source expressions, so
;; it needs to recognize all of the forms that are used inside
;; `define` forms.

;; Only functions bound with named `let`, normal `let` with `lambda`,
;; and `let*` with `lamdba` are lifted, and the lifter assumes that a
;; named `let`'s identifier is used only in application position.
;; Local `define` is not allowed.

;; To bind a `let`-bound function that is not used only in an
;; application position, wrap it with `escapes-ok`.

;; If a function F includes a call to a function G, function G has a
;; free variable X, and function F has an argument X, then the lifter
;; doesn't work (and it reports an error). Help the lifter in that
;; case by picking a different name for one of the Xs.

;; If a "loop" is a non-tail loop or if has many free variables, then
;; lifting may be counterproductive (by making a bad trade for less
;; allocation but slower GCs). Use `define/no-lift` in that case.

;; Select `define/lift` as the default mode:
(define-syntax (define stx)
  (syntax-case stx ()
    [(_ . r) #'(define/lift . r)]))

(define-syntax (define/lift stx)
  (letrec ([lift-local-functions
            ;; Convert `e` to return
            ;;  (list new-list (list lifted-defn ...))
            ;; The `env` argument is a list of symbols (not identifiers),
            ;; and the `binds` argument is a list of syntax bindings
            ;;   #`(bind-form ([id rhs] ...))
            ;; to be copied over to any lifted form. Also, the `rhs`
            ;; of a `bind-form` can contain free-variable and
            ;; called-variable information for a previously lifted
            ;; function, so that its free variables can be added
            ;; as needed to a newly lifted function that calls the
            ;; lifted one.
            ;; Earlier entries in `binds` shadow later ones, and
            ;; entires in `env` shadow `binds` entries.
            (lambda (e env binds mutated)
              (syntax-case e (quote begin lambda case-lambda
                                    let letrec let* let-values
                                    fluid-let-syntax let-syntax
                                    cond define set!)
                [(define . _)
                 (syntax-error e "don't use nested `define`:")]
                [(quote _)
                 (list e '())]
                [(begin e)
                 (lift-local-functions #'e env binds mutated)]
                [(seq e ...)
                 (and (symbol? (syntax->datum #'seq))
                      (or (free-identifier=? #'seq #'begin)
                          (free-identifier=? #'seq #'begin0)
                          (free-identifier=? #'seq #'if)))
                 (with-syntax ([((new-e lifts) ...)
                                (map (lambda (e)
                                       (lift-local-functions e env binds mutated))
                                     #'(e ...))])
                   (list #'(seq new-e ...)
                         (append-all #'(lifts ...))))]
                [(lambda args e ...)
                 (with-syntax ([(body lifts)
                                (lift-local-functions #'(begin e ...)
                                                      (add-args env #'args)
                                                      binds
                                                      mutated)])
                   #`((lambda args body)
                      lifts))]
                [(case-lambda [args e ...] ...)
                 (with-syntax ([((body lifts) ...)
                                (map (lambda (args body)
                                       (lift-local-functions body
                                                             (add-args env args)
                                                             binds
                                                             mutated))
                                     #'(args ...)
                                     #'((begin e ...) ...))])
                   (list #'(case-lambda [args body] ...)
                         (append-all #'(lifts ...))))]
                [(let loop ([arg val] ...) e ...)
                 (symbol? (syntax->datum #'loop))
                 (generate-lifted env binds mutated
                                  #'loop          ; name
                                  #'(arg ...)     ; argument names
                                  #'(begin e ...) ; body
                                  #t              ; recursive
                                  (lambda (defn-to-lift new-loop-name free-vars wrap-bind-of-lifted)
                                    (with-syntax ([(free-var ...) free-vars]
                                                  [new-loop-name new-loop-name]
                                                  [defn-to-lift defn-to-lift])
                                      #`((new-loop-name val ... free-var ...)
                                         (defn-to-lift)))))]
                [(let* () e ...)
                 (lift-local-functions #`(begin e ...) env binds mutated)]
                [(let* ([id rhs] . more-binds) e ...)
                 (lift-local-functions #`(let ([id rhs]) (let* more-binds e ...)) env binds mutated)]
                [(let . _)
                 (lift-local-functions-in-let/lift-immediate e env binds mutated)]
                [(letrec . _)
                 (lift-local-functions-in-let e env binds mutated #t)]
                [(let-values ([(id ...) rhs] ...) e ...)
                 (with-syntax ([((new-rhs lifts) ...)
                                (map (lambda (rhs)
                                       (lift-local-functions rhs env binds mutated))
                                     #'(rhs ...))])
                   (with-syntax ([(new-body body-lifts)
                                  (lift-local-functions #'(begin e ...)
                                                        (add-args env (#%apply append #'((id ...) ...)))
                                                        binds
                                                        mutated)])
                     (list #'(let-values ([(id ...) new-rhs] ...) new-body)
                           (append #'body-lifts (append-all #'(lifts ...))))))]
                [(fluid-let-syntax ([id rhs] ...) e ...)
                 (with-syntax ([(new-body body-lifts)
                                (lift-local-functions #'(begin e ...)
                                                      (remove-args env #'(id ...))
                                                      (cons #'(fluid-let-syntax ([id rhs] ...))
                                                            binds)
                                                      mutated)])
                   #`((fluid-let-syntax ([id rhs] ...) new-body)
                      body-lifts))]
                [(let-syntax ([id rhs] ...) e ...)
                 (with-syntax ([(new-body body-lifts)
                                (lift-local-functions #'(begin e ...)
                                                      (remove-args env #'(id ...))
                                                      (cons #'(let-syntax ([id rhs] ...))
                                                            binds)
                                                      mutated)])
                   #`((let-syntax ([id rhs] ...) new-body)
                      body-lifts))]
                [(cond [e ...] ...)
                 (with-syntax ([(((new-e lifts) ...) ...)
                                (map (lambda (es)
                                       (map (lambda (e)
                                              (lift-local-functions e env binds mutated))
                                            es))
                                     #'((e ...) ...))])
                   (list #'(cond [new-e ...] ...)
                         (append-all (append-all #'((lifts ...) ...)))))]
                [(set! id rhs)
                 (track-mutated! mutated #'id 'mutated)
                 (with-syntax ([(new-rhs lifts) (lift-local-functions #'rhs env binds mutated)])
                   #'((set! id new-rhs)
                      lifts))]
                [(rator rand ...)
                 (with-syntax ([((new-e lifts) ...)
                                (map (lambda (e)
                                       (lift-local-functions e env binds mutated))
                                     #'(rator rand ...))])
                   (list #'(new-e ...)
                         (append-all #'(lifts ...))))]
                [_ (list e '())]))]

           [lift-local-functions-in-let
            (lambda (e env binds mutated rec?)
              (syntax-case e ()
                [(form ([id rhs] ...) e ...)
                 (let ([body-env (add-args env #'(id ...))])
                   (with-syntax ([((new-rhs lifts) ...)
                                  (map (lambda (rhs)
                                         (lift-local-functions rhs (if rec? body-env env) binds mutated))
                                       #'(rhs ...))])
                     (with-syntax ([(new-body body-lifts)
                                    (lift-local-functions #'(begin e ...) body-env binds mutated)])
                       (list #'(form ([id new-rhs] ...) new-body)
                             (append #'body-lifts (append-all #'(lifts ...)))))))]))]

           [lift-local-functions-in-let/lift-immediate
            ;; Split `lambda` bindings for other bindings, then lift the `lambda`s
            (lambda (e env binds mutated)
              (syntax-case e ()
                [(form ([id rhs] ...) . body)
                 (let ([body-env (add-args env #'(id ...))])
                   (let-values ([(proc-binds other-binds)
                                 (split-proc-binds #'([id rhs] ...))])
                     (cond
                      [(null? proc-binds)
                       (lift-local-functions-in-let e env binds mutated #f)]
                      [else
                       (let loop ([proc-binds proc-binds]
                                  [e (with-syntax ([other-binds other-binds])
                                       #'(form other-binds . body))]
                                  [lifts '()])
                         (cond
                          [(null? proc-binds)
                           (with-syntax ([(new-e e-lifts) (lift-local-functions e env binds mutated)])
                             (list #'new-e
                                   (append lifts #'e-lifts)))]
                          [else
                           (with-syntax ([[id (_ rhs-args rhs-e ...)] (car proc-binds)])
                             (generate-lifted
                              env binds mutated
                              #'id                ; name
                              #'rhs-args          ; argument names
                              #'(begin rhs-e ...) ; body
                              #f                  ; not recursive
                              (lambda (defn-to-lift new-id free-vars wrap-bind-of-lifted)
                                (loop (cdr proc-binds)
                                      (wrap-bind-of-lifted e)
                                      (cons defn-to-lift lifts)))))]))])))]))]

           [split-proc-binds
            ;; Helper to split `lambda` from non-`lambda`
            (lambda (form-binds)
              (let loop ([binds form-binds] [proc-binds '()] [other-binds '()])
                (cond
                 [(null? binds)
                  (values (reverse proc-binds)
                          (reverse other-binds))]
                 [else
                  (syntax-case (car binds) (lambda)
                    [[_ (lambda (arg ...) . _)]
                     (loop (cdr binds)
                           (cons (car binds) proc-binds)
                           other-binds)]
                    [_
                     (loop (cdr binds)
                           proc-binds
                           (cons (car binds) other-binds))])])))]

           [generate-lifted
            ;; Takes pieces for a function to lift an generates the lifted version
            (lambda (env binds mutated name args body rec? k)
              (let* ([ids (if rec? (cons name args) args)]
                     [binds (filter-shadowed-binds binds (add-args env ids))]
                     [body-env (remove-args env ids)]
                     [direct-free-vars (extract-free-vars body body-env)]
                     [direct-called-vars (extract-free-vars body (binds-to-env binds))])
                (for-each (lambda (free-var) (track-mutated! mutated free-var 'must-not)) direct-free-vars)
                (let-values ([(free-vars called-vars) (extract-bind-vars binds body-env direct-free-vars direct-called-vars)])
                  (let ([free-vars (unique-ids free-vars)]
                        [called-vars (unique-ids called-vars)])
                    (with-syntax ([(free-var ...) free-vars]
                                  [(called-var ...) called-vars]
                                  [new-name (datum->syntax
                                             name
                                             (chez:gensym (chez:symbol->string (syntax->datum name))))]
                                  [body (let loop ([body body]
                                                   [binds binds])
                                          (cond
                                           [(null? binds) body]
                                           [else (with-syntax ([(form form-binds) (car binds)]
                                                               [body body])
                                                   (loop #'(form form-binds body)
                                                         (cdr binds)))]))]
                                  [name name]
                                  [(arg ...) args])
                      (let ([wrap-bind-of-lifted
                             (lambda (body)
                               (with-syntax ([body body])
                                 #'(let-syntax ([name (begin ; this pattern is recognized by `extract-bind-free-vars`
                                                        '(FREE-VARS free-var ...)
                                                        '(CALLED-VARS called-var ...)
                                                        (lambda (stx)
                                                          (syntax-case stx ()
                                                            [(_ call-arg (... ...))
                                                             #'(new-name call-arg (... ...) free-var ...)]
                                                            [_ (syntax-error stx "lifted procedure escapes:")])))])
                                     body)))])
                        (with-syntax ([wrapped-body (if rec?
                                                        (wrap-bind-of-lifted #'body)
                                                        #'body)])
                          (k #`(define/lift new-name
                                 (lambda (arg ... free-var ...)
                                   wrapped-body))
                             #'new-name
                             free-vars
                             wrap-bind-of-lifted))))))))]

           [extract-free-vars
            ;; For an expression that is going to be lifted, find all the free
            ;; variables so they can be added to call sites of the enclosing
            ;; lifted function. Only variables in `env` are candidate free
            ;; variables.
            (lambda (e env)
              (syntax-case e (quote begin lambda case-lambda
                                    let* let letrec let-values
                                    fluid-let-syntax let-syntax
                                    set!)
                [id
                 (symbol? (syntax->datum #'id))
                 (if (chez:memq (syntax->datum #'id) env)
                     (list #'id)
                     '())]
                [(set! id rhs)
                 (if (chez:memq (syntax->datum #'id) env)
                     (syntax-error #'id "cannot mutate variable added to lifted procedure:")
                     (extract-free-vars #'rhs env))]
                [(quote _) '()]
                [(seq e ...)
                 (and (symbol? (syntax->datum #'seq))
                      (or (free-identifier=? #'seq #'begin)
                          (free-identifier=? #'seq #'begin0)
                          (free-identifier=? #'seq #'if)
                          (free-identifier=? #'seq #'cond)))
                 (#%apply append (map (lambda (e)
                                        (extract-free-vars e env))
                                      #'(e ...)))]
                [(lambda args e ...)
                 (extract-free-vars #'(begin e ...)
                                    (remove-args env #'args))]
                [(case-lambda [args e ...] ...)
                 (#%apply
                  append
                  (map (lambda (args body)
                         (extract-free-vars body (remove-args env args)))
                       #'(args ...)
                       #'((begin e ...) ...)))]
                [(let loop ([arg val] ...) e ...)
                 (symbol? (syntax->datum #'loop))
                 (append
                  (extract-free-vars #'(begin val ...) env)
                  (extract-free-vars #'(begin e ...)
                                     (remove-args env #'(loop arg ...))))]
                [(let* () e ...)
                 (extract-free-vars #`(begin e ...) env)]
                [(let* ([id rhs] . binds) e ...)
                 (extract-free-vars #`(let ([id rhs]) (let* binds e ...)) env)]
                [(let ([id rhs] ...) e ...)
                 (append
                  (extract-free-vars #'(begin rhs ...) env)
                  (extract-free-vars #'(begin e ...) (remove-args env #'(id ...))))]
                [(let-values ([(id ...) rhs] ...) e ...)
                 (append
                  (extract-free-vars #'(begin rhs ...) env)
                  (extract-free-vars #'(begin e ...) (remove-args env (#%apply append #'((id ...) ...)))))]
                [(letrec ([id rhs] ...) e ...)
                 (extract-free-vars #'(begin rhs ... e ...) (remove-args env #'(id ...)))]
                [(fluid-let-syntax ([id rhs] ...) e ...)
                 (extract-free-vars #'(begin e ...) (remove-args env #'(id ...)))]
                [(let-syntax ([id rhs] ...) e ...)
                 (extract-free-vars #'(begin e ...) (remove-args env #'(id ...)))]
                [(rator rand ...)
                 (extract-free-vars #'(begin rator rand ...) env)]
                [_ '()]))]

           [filter-shadowed-binds
            ;; Simplify `binds` to drop bindings that are shadowned by
            ;; `env` or by earlier bindings
            (lambda (binds env)
              (let loop ([binds binds]
                         [env env])
                (cond
                 [(null? binds) '()]
                 [else (with-syntax ([(form ([id rhs] ...)) (car binds)])
                         (with-syntax ([([id rhs] ...)
                                        ;; Filter any `ids` that are shadowed
                                        (let loop ([ids #'(id ...)] [rhss #'(rhs ...)])
                                          (cond
                                           [(null? ids) '()]
                                           [(chez:memq (syntax->datum (car ids)) env)
                                            (loop (cdr ids) (cdr rhss))]
                                           [else (cons (list (car ids) (car rhss))
                                                       (loop (cdr ids) (cdr rhss)))]))])
                           (cons #'(form ([id rhs] ...))
                                 (loop (cdr binds)
                                       (add-args env #'(id ...))))))])))]

           [binds-to-env
            ;; Extract the identifiers of `binds` into an environment
            (lambda (binds)
              (let loop ([binds binds] [env '()])
                (cond
                 [(null? binds) env]
                 [else
                  (loop (cdr binds)
                        (syntax-case (car binds) ()
                          [(form ([id rhs] ...))
                           (add-args env #'(id ...))]))])))]

           [extract-bind-vars
            ;; Add new variables to `free-vars` and `called-vars` based on
            ;; entries in `all-binds` that will be called (because they're
            ;; referenced in `called-vars`). A fixpoint calculation is needed,
            ;; since calling a lifted function may add new free variables and
            ;; new called variables.
            (lambda (all-binds env free-vars called-vars)
              (let loop ([binds all-binds] [added? #f] [free-vars free-vars] [called-vars called-vars] [did-ids '()])
                (cond
                 [(null? binds) (if added?
                                    ;; Loop to fixpoint
                                    (loop all-binds #f free-vars called-vars did-ids)
                                    ;; Found fixpoint
                                    (values free-vars called-vars))]
                 [else (syntax-case (car binds) (FREE-VARS CALLED-VARS begin quote)
                         [(form ([id (begin
                                       '(FREE-VARS free-var ...)
                                       '(CALLED-VARS called-var ...)
                                       _)]))
                          (and (id-member? #'id called-vars)
                               (not (chez:memq (syntax->datum #'id) did-ids)))
                          (loop (cdr binds)
                                #t
                                (append (#%map (lambda (free-var)
                                                 (if (chez:memq (syntax->datum free-var) env)
                                                     free-var
                                                     (syntax-error free-var "wrong variable at call site; lifter needs your help by renaming:")))
                                               #'(free-var ...))
                                        free-vars)
                                (append #'(called-var ...)
                                        called-vars)
                                (cons (syntax->datum #'id) did-ids))]
                         [_
                          ;; Not a lifted-function binding
                          (loop (cdr binds) added? free-vars called-vars did-ids)])])))]

           [add-args
            ;; Add identifiers (accomdating rest args) to an environment
            (lambda (env args)
              (let add-args ([env env] [args (syntax->datum args)])
                (cond
                 [(null? args) env]
                 [(pair? args) (add-args (cons (car args) env)
                                         (cdr args))]
                 [else (cons args env)])))]

           [remove-args
            ;; Remove identifiers (accomdating rest args) from an environment
            (lambda (env args)
              (let remove-args ([env env] [args (syntax->datum args)])
                (cond
                 [(null? args) env]
                 [(pair? args) (remove-args (#%remq (car args) env)
                                            (cdr args))]
                 [else (#%remq args env)])))]

           [track-mutated!
            (lambda (mutated id state)
              (let ([old-state (hashtable-ref mutated (syntax->datum id) #f)])
                (when (and old-state
                           (not (eq? old-state state)))
                  (syntax-error id "lift seems to need to close over mutated variable:"))
                (hashtable-set! mutated (syntax->datum id) state)))]

           [unique-ids
            (lambda (l)
              (let loop ([l l])
                (cond
                 [(null? l) '()]
                 [(id-member? (car l) (cdr l))
                  (loop (cdr l))]
                 [else (cons (car l) (loop (cdr l)))])))]

           [id-member?
            (lambda (id l)
              (let loop ([l l])
                (cond
                 [(null? l) #f]
                 [else (or (free-identifier=? id (car l))
                           (loop (cdr l)))])))]

           [append-all
            (lambda (l)
              (#%apply append l))])

    ;; Traverse the right-hand side of a definition to extract lifts
    (syntax-case stx ()
      [(_ (id . args) e ...)
       #'(define/lift id (lambda args e ...))]
      [(_ id rhs)
       (with-syntax ([(new-rhs (lift ...)) (lift-local-functions
                                            #'rhs
                                            '()
                                            '()
                                            (make-eq-hashtable))])
         #'(define/no-lift id
             (let ()
               lift ...
               new-rhs)))])))

(define-syntax (escapes-ok stx)
  (syntax-case stx ()
    [(_ e) #'e]))
