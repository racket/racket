;;; Copyright (c) 2000-2015 Dipanwita Sarkar, Andrew W. Keep, R. Kent Dybvig, Oscar Waddell
;;; See the accompanying file Copyright for details

(library (tests compiler)
  (export
    ;; languages
    LP L0 L1 L2 L3 L4 L5 L6 L7 L8 L9 L10 L11 L12 L13 L14 L15 L16 L17 L18

    ;; parsers
    parse-LP parse-L0 parse-L1 parse-L2 parse-L3 parse-L4 parse-L5 parse-L6
    parse-L7 parse-L8 parse-L9 parse-L10 parse-L11 parse-L13 parse-L14
    parse-L15 parse-L16 parse-L17 parse-L18

    ;; unparsers
    unparse-LP unparse-L0 unparse-L1 unparse-L2 unparse-L3 unparse-L4
    unparse-L5 unparse-L6 unparse-L7 unparse-L8 unparse-L9 unparse-L10
    unparse-L11 unparse-L12 unparse-L13 unparse-L14 unparse-L15 unparse-L16
    unparse-L17 unparse-L18

    ;; passes
    verify-scheme remove-implicit-begin remove-unquoted-constant
    remove-one-armed-if uncover-settable remove-impure-letrec remove-set!
    sanitize-binding remove-anonymous-lambda uncover-free convert-closure
    lift-letrec explicit-closure normalize-context remove-complex-opera*
    remove-anonymous-call introduce-dummy-rp remove-nonunary-let
    return-of-set! explicit-labels
    
    ;; preprocessor
    rename-vars/verify-legal)
  (import (rnrs) (nanopass) (tests helpers) (tests synforms) (nanopass nano-syntax-dispatch))

  (define-language LP
    (terminals
      (variable (x))
      (datum (d))
      (user-primitive (pr)))
    (Expr (e body)
      d
      x
      pr
      (set! x e)
      (if e1 e2)
      (if e1 e2 e3)
      (begin e1 ... e2)
      (lambda (x ...) body1 ... body2)
      (let ((x e) ...) body1 ... body2)
      (letrec ((x e) ...) body1 ... body2)
      (e0 e1 ...)))

  (define-parser parse-LP LP)

  (define-language L0 (extends LP)
    (Expr (e body)
      (- d
         x
         pr
         (e0 e1 ...))
      (+ (datum d)
         (var x)
         (primapp pr e ...)
         (app e0 e1 ...))))

  (define-parser parse-L0 L0)

  (define-who rename-vars/verify-legal
    (lambda (expr)
      (define keywords '(quote set! if begin let letrec lambda))
      (define extend-env*
        (lambda (x* env)
          (let f ([x* x*] [rx* '()] [env env])
            (if (null? x*)
                (values (reverse rx*) env)
                (let ([x (car x*)])
                  (let ([rx (gen-symbol x)])
                    (f (cdr x*) (cons rx rx*) (cons (cons x rx) env))))))))
      (let f ([expr expr] [env '()])
        (define f* (lambda (e* env) (map (lambda (e) (f e env)) e*)))
        (with-output-language (L0 Expr)
          (syncase expr
            [,const (guard (constant? const)) `(datum ,const)]
            [(quote ,lit) (guard (not (assq 'quote env))) `(datum ,lit)]
            [,var
             (guard (symbol? var))
             (cond
               [(assq var env) => (lambda (a) `(var ,(cdr a)))]
               [(memq var keywords) (error who "invalid reference to keyword" var)]
               [else (error who "reference to unbound var" var)])]
            [(set! ,var ,rhs)
             (guard (not (assq 'set! env)) (symbol? var))
             (cond
               [(assq var env) => (lambda (a) `(set! ,(cdr a) ,(f rhs env)))]
               [(memq var keywords) (error who "set! of keyword" expr)]
               [else (error who "set! of unbound var" expr)])]
            [(if ,e0 ,e1)
             (guard (not (assq 'if env)))
             `(if ,(f e0 env) ,(f e1 env))]
            [(if ,e0 ,e1 ,e2)
             (guard (not (assq 'if env)))
             `(if ,(f e0 env) ,(f e1 env) ,(f e2 env))]
            [(begin ,e* ... ,e)
             (guard (not (assq 'begin env)))
             `(begin ,(f* e* env) ... ,(f e env))]
            [(let ([,x* ,rhs*] ...) ,e* ... ,e)
             (guard (for-all symbol? x*) (set? x*))
             (let-values ([(x* new-env) (extend-env* x* env)])
               `(let ([,x* ,(f* rhs* env)] ...)
                  ,(f* e* new-env) ... ,(f e new-env)))]
            [(letrec ([,x* ,rhs*] ...) ,e* ... ,e)
             (guard (for-all symbol? x*) (set? x*))
             (let-values ([(x* env) (extend-env* x* env)])
               `(letrec ([,x* ,(f* rhs* env)] ...)
                  ,(f* e* env) ... ,(f e env)))]
            [(lambda (,x* ...) ,e* ... ,e)
             (guard (not (assq 'lambda env)) (for-all symbol? x*) (set? x*))
             (let-values ([(x* env) (extend-env* x* env)])
               `(lambda (,x* ...) ,(f* e* env) ... ,(f e env)))]
            [(,prim ,rand* ...)
             (guard (not (assq prim env)) (user-primitive? prim)
               (= (cadr (assq prim list-of-user-primitives)) (length rand*)))
             `(primapp ,prim ,(f* rand* env) ...)]
            [(,rator ,rand* ...) `(app ,(f rator env) ,(f* rand* env) ...)]
            [else (error who "invalid expression" expr)])))))

  (define-pass verify-scheme : LP (ir) -> L0 ()
    (definitions
      (define invalid-var?
        (lambda (x env)
          (cond
            [(memq x env) #f]
            [(keyword? x) "keyword"]
            [(user-primitive? x) "user-primitive"]
            [else "unbound variable"])))

      (define valid-bindings?
        (lambda (ls)
          (for-all variable? ls)))

      (define duplicate-names?
        (lambda (var*)
          (let f ([ls var*] [dups '()])
            (cond
              [(null? ls) (if (null? dups) #f dups)]
              [(and (memq (car ls) (cdr ls)) (not (memq (car ls) dups)))
               (f (cdr ls) (cons (car ls) dups))]
              [else (f (cdr ls) dups)]))))
     (define format-list
        (lambda (ls)
          (case (length ls)
            [(0) ""]
            [(1) (format "~s" (car ls))]
            [(2) (format "~s and ~s" (car ls) (cadr ls))]
            [else (let f ([a (car ls)] [ls (cdr ls)])
                    (if (null? ls)
                        (format "and ~s" a)
                        (format "~s, ~a" a (f (car ls) (cdr ls)))))]))))
    (Expr : Expr (ir [env '()]) -> Expr ()
      [,d `(datum ,d)]
      [,x (let ([invalid? (invalid-var? x env)])
            (if invalid?
                (error 'verify-scheme (format "reference to ~a ~s" invalid? x))
                `(var ,x)))]
      [(set! ,x ,e)
       (let ([invalid? (invalid-var? x env)])
         (if invalid?
             (error 'verify-scheme (format "assignment to ~a ~s" invalid? x))
             (let ([e (Expr e env)])
               `(set! ,x ,e))))]
      [(lambda (,x ...) ,body1 ... ,body2)
       (cond
         [(not (valid-bindings? x))
          (error 'verify-scheme
                 (format "invalid binding list ~a in lambda form" x))]
         [(duplicate-names? x)
          =>
          (lambda (x)
            (error 'verify-scheme
                   (format "duplicate bindings ~a in lambda form"
                           (format-list x))))]
         [else
           (let ([env (append env x)])
             (let ([body1 (map (lambda (x) (Expr x env)) body1)]
                   [body2 (Expr body2 env)])
               `(lambda (,x ...) ,body1 ... ,body2)))])]
      [(let ((,x ,e) ...) ,body1 ... ,body2) ;; track variables
       (cond
         [(not (valid-bindings? x))
          (error 'verify-scheme
                 (format "invalid binding list ~a in let form" x))]
         [(duplicate-names? x)
          =>
          (lambda (x)
            (error 'verify-scheme
                   (format "duplicate bindings ~a in let form"
                           (format-list x))))]
         [else
           (let ([e (map (lambda (x) (Expr x env)) e)])
             (let ([env (append env x)])
               (let ([body1 (map (lambda (x) (Expr x env)) body1)]
                     [body2 (Expr body2 env)])
                 `(let ((,x ,e) ...) ,body1 ... ,body2))))])]
      [(letrec ((,x ,e) ...) ,body1 ... ,body2) ;; track variables
       (cond
         [(not (valid-bindings? x))
          (error 'verify-scheme
                 (format "invalid binding list ~a in letrec form" x))]
         [(duplicate-names? x)
          =>
          (lambda (x)
            (error 'verify-scheme
                   (format "duplicate bindings ~a in letrec form"
                           (format-list x))))]
         [else
           (let ([env (append env x)])
             (let ([e (map (lambda (x) (Expr x env)) e)])
               (let ([body1 (map (lambda (x) (Expr x env)) body1)]
                     [body2 (Expr body2 env)])
                 `(letrec ((,x ,e) ...) ,body1 ... ,body2))))])]
      [(,e0 ,e1 ...)
       (let ([e1 (map (lambda (x) (Expr x env)) e1)])
         (if (and (symbol? e0) (user-primitive? e0))
             `(primapp ,e0 ,e1 ...)
             `(app ,(Expr e0 env) ,e1 ...)))]))

  (define-language L1 (extends L0)
    (Expr (e body)
      (- (lambda (x ...) body1 ... body2)
         (let ((x e) ...) body1 ... body2)
         (letrec ((x e) ...) body1 ... body2))
      (+ (lambda (x ...) body)
         (let ((x e) ...) body)
         (letrec ((x e) ...) body))))

  (define-parser parse-L1 L1)

  (define-pass remove-implicit-begin : L0 (ir) -> L1 ()
    (process-expr-expr : Expr (ir) -> Expr ()
      [(lambda (,x ...) ,[body1] ... ,[body2])
       `(lambda (,x ...) (begin ,body1 ... ,body2))]
      [(let ((,x ,[e]) ...) ,[body1] ... ,[body2])
       `(let ((,x ,e) ...) (begin ,body1 ... ,body2))]
      [(letrec ((,x ,[e]) ...) ,[body1] ... ,[body2])
       `(letrec ((,x ,e) ...) (begin ,body1 ... ,body2))]))

  (define-language L2 (extends L1)
    (Expr (e body)
      (- (datum d))
      (+ (quoted-const d))))

  (define-parser parse-L2 L2)

  (define-pass remove-unquoted-constant : L1 (ir) -> L2 ()
    (process-expr-expr : Expr (ir) -> Expr ()
      [(datum ,d) `(quoted-const ,d)]))

  (define-language L3 (extends L2) (Expr (e body) (- (if e1 e2))))

  (define-parser parse-L3 L3)

  (define-pass remove-one-armed-if : L2 (ir) -> L3 ()
    (process-expr-expr : Expr (ir) -> Expr ()
      [(if ,[e1] ,[e2]) `(if ,e1 ,e2 (primapp void))]))

  (define-language L4 (extends L3)
    (Expr (e body)
      (- (lambda (x ...) body)
         (let ((x e) ...) body)
         (letrec ((x e) ...) body))
      (+ (lambda (x ...) sbody)
         (let ((x e) ...) sbody)
         (letrec ((x e) ...) sbody)))
    (SetBody (sbody) (+ (settable (x ...) body) => body)))

  (define-parser parse-L4 L4)

  (define-pass uncover-settable : L3 (ir) -> L4 ()
    (definitions
      (define Expr*
        (lambda (e* asgn-var*)
          (if (null? e*)
              (values '() asgn-var*)
              (let-values ([(e asgn-var*) (Expr (car e*) asgn-var*)])
                (let-values ([(e* asgn-var*) (Expr* (cdr e*) asgn-var*)])
                  (values (cons e e*) asgn-var*)))))))
    (Expr : Expr (ir asgn-var*) -> Expr (asgn-var*)
      [(set! ,x ,[e asgn-var*]) (values `(set! ,x ,e) (set-cons x asgn-var*))]
      [(lambda (,x* ...) ,[body asgn-var*])
       (let ([set-x* (intersection asgn-var* x*)])
         (values `(lambda (,x* ...) (settable (,set-x* ...) ,body))
           (difference asgn-var* set-x*)))]
      [(let ([,x* ,e*]...) ,[body asgn-var*])
       (let ([set-x* (intersection asgn-var* x*)])
         (let-values ([(e* asgn-var*) (Expr* e* (difference asgn-var* set-x*))])
           (values `(let ([,x* ,e*] ...) (settable (,set-x* ...) ,body)) asgn-var*)))]
      [(letrec ([,x* ,e*]...) ,[body asgn-var*])
       (let-values ([(e* asgn-var*) (Expr* e* asgn-var*)])
         (let ([set-x* (intersection asgn-var* x*)])
           (values `(letrec ((,x* ,e*) ...) (settable (,set-x* ...) ,body))
             (difference asgn-var* set-x*))))]
     ; TODO: this code used to be supported by the automatic combiners, we've
     ; abandoned this in favor of threading, but we've not added threading yet
      [(app ,[e asgn-var*] ,e* ...)
       (let-values ([(e* asgn-var*) (Expr* e* asgn-var*)])
         (values `(app ,e ,e* ...) asgn-var*))]
      [(primapp ,pr ,e* ...)
       (let-values ([(e* asgn-var*) (Expr* e* asgn-var*)])
         (values `(primapp ,pr ,e* ...) asgn-var*))]
      [(if ,[e0 asgn-var*] ,e1 ,e2)
       (let-values ([(e1 asgn-var*) (Expr e1 asgn-var*)])
         (let-values ([(e2 asgn-var*) (Expr e2 asgn-var*)])
           (values `(if ,e0 ,e1 ,e2) asgn-var*)))]
      [(begin ,e* ... ,[e asgn-var*])
       (let-values ([(e* asgn-var*) (Expr* e* asgn-var*)])
         (values `(begin ,e* ... ,e) asgn-var*))])
    (let-values ([(e asgn-var*) (Expr ir '())]) e))

  (define-language L5 (extends L4)
    (Expr (e body)
      (+ lexpr
         (letrec ((x lexpr) ...) body))
      (- (lambda (x ...) sbody)
         (letrec ((x e) ...) sbody)))
    (LambdaExpr (lexpr) (+ (lambda (x ...) sbody))))

  (define-parser parse-L5 L5)

  (define-pass remove-impure-letrec : L4 (ir) -> L5 ()
    (process-expr-expr : Expr (ir) -> Expr ()
      [(lambda (,x ...) ,[sbody])
       (in-context LambdaExpr `(lambda (,x ...) ,sbody))]
      [(letrec ((,x1 (lambda (,x2 ...) ,[sbody1])) ...) (settable () ,[body2]))
       (let ([lambdabody (map
                           (lambda (x sbody)
                             (in-context LambdaExpr `(lambda (,x ...) ,sbody)))
                           x2 sbody1)])
         `(letrec ((,x1 ,lambdabody) ...) ,body2))]
      [(letrec ((,x1 ,[e]) ...) (settable (,x2 ...) ,[body]))
       (let ()
         (define void-maker
           (lambda (ids)
             (letrec ((helper (lambda (ls)
                                (if (null? (cdr ls))
                                    (list (in-context Expr `(primapp void)))
                                    (cons (in-context Expr `(primapp void))
                                          (helper (cdr ls)))))))
               (helper (iota (length ids))))))
         (let* ([new-ids (map gen-symbol x1)]
                [voids (void-maker x1)]
                [bodies (map (lambda (lhs id)
                               `(set! ,lhs (var ,id))) x1 new-ids)]
                [rbodies (reverse bodies)]
                [new-body (cdr rbodies)]
                [rest-bodies (car rbodies)])
           `(let ([,x1 ,voids] ...)
              (settable (,x1 ...)
                (begin
                  (primapp void)
                  (let ([,new-ids ,e] ...)
                    ;;**** this need not be from the output nonterminal ****
                    (settable ()
                      (begin ,new-body ...  ,rest-bodies)))
                  ,body)))))])
    (process-setbody-setbody : SetBody (ir) -> SetBody ()
      [(settable (,x ...) ,[body]) `(settable (,x ...) ,body)])
    (process-expr-lexpr : Expr (ir) -> LambdaExpr ()
      [(lambda (,x ...) ,[sbody]) `(lambda (,x ...) ,sbody)])
    (process-setbody-expr : SetBody (ir) -> Expr ()
      [(settable (,x ...) ,[body]) `,body]))

  (define-language L6 (extends L5)
    (Expr (e body)
      (- (let ((x e) ...) sbody)
         (set! x e))
      (+ (let ((x e) ...) body)))
    (LambdaExpr (lexpr)
      (- (lambda (x ...) sbody))
      (+ (lambda (x ...) body)))
    (SetBody (sbody) (- (settable (x ...) body))))

  (define-parser parse-L6 L6)

  (define-pass remove-set! : L5 (ir) -> L6 ()
    (Expr : Expr (ir [set* '()]) -> Expr ()
      [(var ,x) (if (memq x set*) `(primapp car (var ,x)) `(var ,x))]
      [(set! ,x ,[e set* -> e]) `(primapp set-car! (var ,x) ,e)]
      [(let ((,x ,[e set* -> e]) ...) ,sbody)
       (let ([body (SetBody sbody x e set*)])
         `,body)])
    (LambdaExpr : LambdaExpr (ir set*) -> LambdaExpr ()
      [(lambda (,x ...) ,[sbody x '() set* -> body]) `,body])
    (SetBody : SetBody (ir x* e* set*) -> Expr ()
      [(settable () ,[body set* -> body])
       (if (null? e*)
           `(lambda (,x* ...) ,body)
           `(let ([,x* ,e*] ...) ,body))]
      [(settable (,x ...) ,[body (append x set*) -> body])
       (let ()
         (define settable-bindings
           (lambda (var* set*)
             (if (null? var*) (values '() '() '())
               (let ([var (car var*)])
                 (let-values ([(var* lhs* rhs*)
                               (settable-bindings (cdr var*) set*)])
                   (if (memq var set*)
                       (let ([tmp (gen-symbol var)])
                         (values (cons tmp var*)
                                 (cons var lhs*)
                                 (cons (in-context
                                         Expr
                                         `(primapp cons (var ,tmp)
                                                   (primapp void))) rhs*)))
                       ;; **** (primapp void) is still a problem here ****
                       (values (cons var var*) lhs* rhs*)))))))
         (let-values ([(x* lhs* rhs*) (settable-bindings x* x)])
           ;; **** cannot have (let (,(apply append bindings*)) ---) or
           ;;      some such, due to nano-syntax-dispatch
           ;;      the problem is not that we don't allow ,(arbitrary
           ;;      function call) in the metaparser
           (if (null? e*)
               `(lambda (,x* ...) (let ([,lhs* ,rhs*] ...) ,body))
               `(let ([,x* ,e*] ...) (let ([,lhs* ,rhs*] ...) ,body)))))]))

  (define-pass sanitize-binding : L6 (ir) -> L6 ()
    (Expr : Expr (ir [rhs? #f]) -> Expr (#f)
      [(var ,x) (values `(var ,x) #f)]
      [(if ,[e1 #f -> e1 ig1] ,[e2 #f -> e2 ig2] ,[e3 #f -> e3 ig3])
       (values `(if ,e1 ,e2 ,e3) #f)]
      [(begin ,[e1 #f -> e1 ig1] ... ,[e2 #f -> e2 ig2])
       (values `(begin ,e1 ... ,e2) #f)]
      [(primapp ,pr ,[e #f -> e ig] ...) (values `(primapp ,pr ,e ...) #f)]
      [(app ,[e0 #f -> e0 ig0] ,[e1 #f -> e1 ig1] ...)
       (values `(app ,e0 ,e1 ...) #f)]
      [(quoted-const ,d) (values `(quoted-const ,d) #f)]
      [(let ([,x ,[e #t -> e lambda?]] ...) ,[body #f -> body ig])
       (let-values ([(let-x* let-e* letrec-x* letrec-e*)
                     (let f ([x x] [e e] [lambda? lambda?])
                       (if (null? x)
                           (values '() '() '() '())
                           (let-values ([(let-x let-e letrec-x letrec-e)
                                         (f (cdr x) (cdr e) (cdr lambda?))])
                             (let ([lhs (car x)]
                                   [rhs (car e)]
                                   [rhs-lambda? (car lambda?)])
                               (if rhs-lambda?
                                   (values let-x let-e (cons lhs letrec-x)
                                           (cons rhs letrec-e))
                                   (values (cons lhs let-x) (cons rhs let-e)
                                           letrec-x letrec-e))))))])
         (if (null? letrec-x*)
             (values `(let ([,let-x* ,let-e*] ...) ,body) #f)
             (if (null? let-x*)
                 (values `(letrec ([,letrec-x* ,letrec-e*] ...) ,body) #f)
                 (values `(letrec ([,letrec-x* ,letrec-e*] ...)
                            (let ([,let-x* ,let-e*] ...) ,body)) #f))))]
      [(letrec ([,x1 (lambda (,x2 ...) ,[body1 #f -> body1 ig1])] ...)
         ,[body2 #f -> body2 ig2])
       (values `(letrec ([,x1 (lambda (,x2 ...) ,body1)] ...) ,body2) #f)])
    (LambdaExpr : LambdaExpr (ir [rhs? #f]) -> LambdaExpr (dummy)
      [(lambda (,x ...) ,[body #f -> body ig])
       (values `(lambda (,x ...) ,body) #t)]))

  (define-language L7 (extends L6) (Expr (e body) (- lexpr)))

  (define-parser parse-L7 L7)

  (define-pass remove-anonymous-lambda : L6 (ir) -> L7 ()
    (Expr : Expr (ir) -> Expr ()
      [(lambda (,x ...) ,[body])
       (let ([anon (gen-symbol 'anon)])
         `(letrec ([,anon (lambda (,x ...) ,body)]) (var ,anon)))]))

#;
  (define-pass remove-anonymous-lambda : L6 (ir) -> L7 ()
    (Expr : Expr (ir) -> Expr ()
      [(lambda (,x ...) ,[body])
       (let ([anon (gen-symbol 'anon)])
         `(letrec ([,anon (lambda (,x ...) ,body)]) (var ,anon)))]
      [(var ,x) `(var ,x)]
      [(quoted-const ,d) `(quoted-const ,d)]
      [(if ,[e1] ,[e2] ,[e3]) `(if ,e1 ,e2 ,e3)]
      [(begin ,[e1] ... ,[e2]) `(begin ,e1 ... ,e2)]
      [(let ([,x ,[e]] ...) ,[body]) `(let ([,x ,e] ...) ,body)]
      [(letrec ([,x ,[lexpr]] ...) ,[body])
       `(letrec ([,x ,lexpr] ...) ,body)]
      [(primapp ,pr ,[e] ...) `(primapp ,pr ,e ...)]
      [(app ,[e0] ,[e1] ...) `(app ,e0 ,e1 ...)])
    (LambdaExpr : LambdaExpr (ir) -> LambdaExpr ()
      [(lambda (,x ...) ,[body]) `(lambda (,x ...) ,body)]))

  (define-language L8 (extends L7)
    (entry Expr)
    (LambdaExpr (lexpr) (- (lambda (x ...) body)))
    (FreeExp (free-body) (+ (free (x ...) body) => body))
    (LambdaExpr (lexpr) (+ (lambda (x ...) free-body))))

  (define-parser parse-L8 L8)

  (define-pass uncover-free : L7 (ir) -> L8 ()
    (definitions
      (define LambdaExpr*
        (lambda (lexpr* free*)
          (if (null? lexpr*)
              (values '() free*)
              (let-values ([(lexpr free*) (LambdaExpr (car lexpr*) free*)])
                (let-values ([(lexpr* free*) (LambdaExpr* (cdr lexpr*) free*)])
                  (values (cons lexpr lexpr*) free*))))))
      (define Expr*
        (lambda (e* free*)
          (if (null? e*)
              (values '() free*)
              (let-values ([(e free*) (Expr (car e*) free*)])
                (let-values ([(e* free*) (Expr* (cdr e*) free*)])
                  (values (cons e e*) free*)))))))
    (Expr : Expr (ir free*) -> Expr (free*)
      [(letrec ([,x* ,lexpr*] ...) ,[body free*])
       (let-values ([(e* free*) (LambdaExpr* lexpr* free*)])
         (values `(letrec ([,x* ,e*] ...) ,body) (difference free* x*)))]
      [(let ([,x* ,e*] ...) ,[body free*])
       (let-values ([(e* free*) (Expr* e* (difference free* x*))])
         (values `(let ([,x* ,e*] ...) ,body) free*))]
      [(var ,x) (values `(var ,x) (cons x free*))]
     ; TODO: get threaded variables working so we don't need to do this by hand
      [(app ,[e free*] ,e* ...)
       (let-values ([(e* free*) (Expr* e* free*)])
         (values `(app ,e ,e* ...) free*))]
      [(primapp ,pr ,e* ...)
       (let-values ([(e* free*) (Expr* e* free*)])
         (values `(primapp ,pr ,e* ...) free*))]
      [(if ,[e1 free*] ,e2 ,e3)
       (let-values ([(e2 free*) (Expr e2 free*)])
         (let-values ([(e3 free*) (Expr e3 free*)])
           (values `(if ,e1 ,e2 ,e3) free*)))]
      [(begin ,e* ... ,[e free*])
       (let-values ([(e* free*) (Expr* e* free*)])
         (values `(begin ,e* ... ,e) free*))])
    (LambdaExpr : LambdaExpr (ir free*) -> LambdaExpr (free*)
      [(lambda (,x* ...) ,[body free*])
       (let ([free* (difference free* x*)])
         (values `(lambda (,x* ...) (free (,free* ...) ,body)) free*))])
    (let-values ([(e free*) (Expr ir '())]) e))

  (define-language L9
    (terminals
      (variable (x))
      (datum (d))
      (user-primitive (pr)))
    (Expr (e body)
      (var x)
      (quoted-const d)
      (if e1 e2 e3)
      (begin e1 ... e2)
      (let ((x e) ...) body)
      (letrec ((x lexpr) ...) c-letrec)
      (primapp pr e ...)
      (app e0 e1 ...)
      (anonymous-call e0 e1 ...))
    (LambdaExpr (lexpr)
      (lambda (x ...) bf-body))
    (BindFree (bf-body)
      (bind-free (x1 x2 ...) body))
    (Closure (c-exp)
      (closure x1 x2 ...))
    (ClosureLetrec (c-letrec)
      (closure-letrec ((x c-exp) ...) body)))

  (define-parser parse-L9 L9)

  (define-pass convert-closure : L8 (ir) -> L9 ()
    (Expr : Expr (ir [direct '()]) -> Expr ()
      [(app (var ,x) ,[e1 direct -> e1] ...)
       (guard (assq x direct))
       `(app (var ,(cdr (assq x direct))) (var ,x) ,e1 ...)]
      [(app ,[e0 direct -> e0] ,[e1 direct -> e1] ...)
       `(anonymous-call ,e0 ,e1 ...)]
      [(letrec ([,x1 (lambda (,x2 ...) (free (,x3 ...) ,body1))] ...) ,body2)
       (let ([code-name* (map gen-label x1)]
             [cp* (map (lambda (x) (gen-symbol 'cp)) x1)])
         (let* ([direct (append (map cons x1 code-name*) direct)]
                [body1 (map (lambda (exp)(Expr exp direct)) body1)]
                [bind-free* (map (lambda (cp formal* free* lbody)
                                   (in-context LambdaExpr
                                              `(lambda (,cp ,formal* ...)
                                                 (bind-free (,cp ,free* ...)
                                                            ,lbody))))
                                 cp* x2 x3 body1)]
                [closure* (map (lambda (code-name free*)
                                 (in-context Closure
                                            `(closure ,code-name ,free* ...)))
                               code-name* x3)])
           `(letrec ([,code-name* ,bind-free*] ...)
              (closure-letrec ([,x1 ,closure*] ...)
                ,(Expr body2 direct)))))]))

  (define-language L10 (extends L9)
    (entry LetrecExpr)
    (LetrecExpr (lrexpr) (+ (letrec ((x lexpr) ...) e)))
    (Expr (e body)
      (- (letrec ((x lexpr) ...) c-letrec))
      (+ (closure-letrec ((x c-exp) ...) body)))
    (ClosureLetrec (c-letrec) (- (closure-letrec ((x c-exp) ...) body))))

  (define-parser parse-L10 L10)

  (define-pass lift-letrec : L9 (ir) -> L10 ()
    (definitions
      (define Expr*
        (lambda (e* binding*)
          (if (null? e*)
              (values '() binding*)
              (let-values ([(e binding*) (Expr (car e*) binding*)])
                (let-values ([(e* binding*) (Expr* (cdr e*) binding*)])
                  (values (cons e e*) binding*))))))
      (define LambdaExpr*
        (lambda (lexpr* binding*)
          (if (null? lexpr*)
              (values '() binding*)
              (let-values ([(lexpr binding*) (LambdaExpr (car lexpr*) binding*)])
                (let-values ([(lexpr* binding*) (LambdaExpr* (cdr lexpr*) binding*)])
                  (values (cons lexpr lexpr*) binding*)))))))
    (Expr : Expr (ir binding*) -> Expr (binding*)
     ; TODO: we'd like to do this using variable threading!
      [(var ,x) (values `(var ,x) binding*)]
      [(quoted-const ,d) (values `(quoted-const ,d) binding*)]
      [(if ,e1 ,e2 ,[e3 binding*])
       (let-values ([(e1 binding*) (Expr e1 binding*)])
         (let-values ([(e2 binding*) (Expr e2 binding*)])
           (values `(if ,e1 ,e2 ,e3) binding*)))]
      [(begin ,e1 ... ,[e2 binding*])
       (let-values ([(e1 binding*) (Expr* e1 binding*)])
         (values `(begin ,e1 ... ,e2) binding*))]
      [(let ([,x*  ,e*] ...) ,[body binding*])
       (let-values ([(e* binding*) (Expr* e* binding*)])
         (values `(let ([,x* ,e*] ...) ,body) binding*))]
      [(primapp ,pr ,e* ...)
       (let-values ([(e* binding*) (Expr* e* binding*)])
         (values `(primapp ,pr ,e* ...) binding*))]
      [(app ,[e binding*] ,e* ...)
       (let-values ([(e* binding*) (Expr* e* binding*)])
         (values `(app ,e ,e* ...) binding*))]
      [(anonymous-call ,[e binding*] ,e* ...)
       (let-values ([(e* binding*) (Expr* e* binding*)])
         (values `(anonymous-call ,e ,e* ...) binding*))]
      [(letrec ((,x* ,lexpr*) ...) ,[e binding*])
       (let-values ([(lexpr* binding*) (LambdaExpr* lexpr* binding*)])
         (values e (append (map cons x* lexpr*) binding*)))])
    (LambdaExpr : LambdaExpr (ir binding*) -> LambdaExpr (binding*)
      [(lambda (,x* ...) ,[bf-body binding*])
       (values `(lambda (,x* ...) ,bf-body) binding*)])
    (BindFree : BindFree (ir binding*) -> BindFree (binding*)
      [(bind-free (,x ,x* ...) ,[body binding*])
       (values `(bind-free (,x ,x* ...) ,body) binding*)])
    (ClosureLetrec : ClosureLetrec (ir binding*) -> Expr (binding*)
      [(closure-letrec ([,x* ,[c-exp*]] ...) ,[body binding*])
       (values `(closure-letrec ([,x* ,c-exp*] ...) ,body) binding*)])
    (let-values ([(e binding*) (Expr ir '())])
      (let ([x* (map car binding*)] [e* (map cdr binding*)])
        `(letrec ([,x* ,e*] ...) ,e))))

  (define-language L11 (extends L10)
    (entry LetrecExpr)
    (terminals
      (+ (system-primitive (spr))))
    (Expr (e body)
      (- (closure-letrec ((x c-exp) ...) body))
      (+ (sys-primapp spr e ...)))
    (BindFree (bf-body) (- (bind-free (x1 x2 ...) body)))
    (Closure (c-exp) (- (closure x1 x2 ...)))
    (LambdaExpr (lexpr)
      (- (lambda (x ...) bf-body))
      (+ (lambda (x ...) body))))

  (define-parser parse-L11 L11)

  (define-pass explicit-closure : L10 (ir) -> L11 ()
    (LetrecExpr : LetrecExpr (ir) -> LetrecExpr ()
      [(letrec ((,x ,[lexpr]) ...) ,e)
       (let ([e (Expr e '() '())]) `(letrec ((,x ,lexpr) ...) ,e))])

    (Expr : Expr (ir [cp '()] [env '()]) -> Expr ()
      [(var ,x)
       (let ([i (list-index x env)])
         (if (>= i 0)
             `(sys-primapp closure-ref (var ,cp) (quoted-const ,i))
             `(var ,x)))]
      [(closure-letrec ((,x ,[c-exp -> e free**]) ...)
         ,[body cp env -> body])
       (let* ([e* (append (apply append
                                 (map
                                   (lambda (lhs free*)
                                     (map
                                       (lambda (i free)
                                         `(sys-primapp
                                            closure-set!
                                            (var ,lhs)
                                            (quoted-const ,i)
                                            ,(let ([ind (list-index free env)])
                                               (if (>= ind 0)
                                                   `(sys-primapp
                                                      closure-ref
                                                      (var ,cp)
                                                      (quoted-const ,ind))
                                                   `(var ,free)))))
                                       (iota (length free*)) free*))
                                   x free**))
                          (list body))])
         (let* ([re* (reverse e*)] [e1 (cdr re*)] [e2 (car re*)])
           `(let ([,x ,e] ...) (begin ,e1 ... ,e2))))])
    (BindFree : BindFree (ir) -> Expr ()
      [(bind-free (,x1 ,x2 ...) ,[body x1 x2 -> body]) `,body])
    (Closure : Closure (ir) -> Expr (dummy)
      [(closure ,x1 ,x2 ...)
       (values `(sys-primapp make-closure (var ,x1)
                             (quoted-const ,(length x2))) x2)])
    (LambdaExpr : LambdaExpr (ir) -> LambdaExpr ()
      [(lambda (,x ...) ,[bf-body -> body]) `(lambda (,x ...) ,body)]))

  (define-language L12
    (terminals
      (variable (x))
      (datum (d))
      (value-primitive (vp))
      (predicate-primitive (pp))
      (effect-primitive (ep))
      (system-primitive (spr)))
    (LetrecExpr (lrexpr)
      (letrec ((x lexpr) ...) v))
    (LambdaExpr (lexpr)
      (lambda (x ...) v))
    (Value (v)
      (var x)
      (quoted-const d)
      (if p1 v2 v3)
      (begin f0 ... v1)
      (let ((x v1) ...) v2)
      (primapp vp v ...)
      (sys-primapp spr v ...)
      (anonymous-call v0 v1 ...)
      (app v0 v1 ...))
    (Predicate (p)
      (true)
      (false)
      (if p1 p2 p3)
      (begin f0 ... p1)
      (let ((x v) ...) p)
      (primapp pp v ...)
      (sys-primapp spr v ...)
      (anonymous-call v0 v1 ...)
      (app v0 v1 ...))
    (Effect (f)
      (nop)
      (if p1 f2 f3)
      (begin f0 ... f1)
      (let ((x v) ...) f)
      (primapp ep v ...)
      (sys-primapp spr v ...)
      (anonymous-call v0 v1 ...)
      (app v0 v1 ...)))

  (define-parser parse-L12 L12)

  (define-pass normalize-context : L11 (ir) -> L12 ()
    (LetrecExpr : LetrecExpr (ir) -> LetrecExpr ()
      [(letrec ((,x ,[lexpr]) ...) ,[v]) `(letrec ((,x ,lexpr) ...) ,v)])
    (LambdaExpr : LambdaExpr (ir) -> LambdaExpr ()
      [(lambda (,x ...) ,[v]) `(lambda (,x ...) ,v)])
    (Value : Expr (ir) -> Value ()
      [(var ,x) `(var ,x)]
      [(quoted-const ,d) `(quoted-const ,d)]
      [(if ,[p0] ,[v1] ,[v2]) `(if ,p0 ,v1 ,v2)]
      [(begin ,[f0] ... ,[v1]) `(begin ,f0 ... ,v1)]
      [(let ((,x ,[v1]) ...) ,[v2]) `(let ((,x ,v1) ...) ,v2)]
      [(primapp ,pr ,[p])
       (guard (equal? pr 'not))
       `(if ,p (quoted-const #f) (quoted-const #t))]
      [(primapp ,pr ,[v0] ...)
       (guard (predicate-primitive? pr))
       `(if (primapp ,pr ,v0 ...) (quoted-const #t) (quoted-const #f))]
      [(primapp ,pr ,[v0] ...)
       (guard (value-primitive? pr))
       `(primapp ,pr ,v0 ...)]
      [(primapp ,pr ,[v0] ...)
       (guard (effect-primitive? pr))
       `(begin (primapp ,pr ,v0 ...) (primapp void))]
      [(sys-primapp ,spr ,[v0] ...)
       (guard (predicate-primitive? spr))
       `(if (sys-primapp ,spr ,v0 ...) (quoted-const #t) (quoted-const #f))]
      [(sys-primapp ,spr ,[v0] ...)
       (guard (value-primitive? spr))
       `(sys-primapp ,spr ,v0 ...)]
      [(sys-primapp ,spr ,[v0] ...)
       (guard (effect-primitive? spr))
       `(begin (primapp ,spr ,v0 ...) (primapp void))]
      [(anonymous-call ,[v0] ,[v1] ...) `(anonymous-call ,v0 ,v1 ...)]
      [(app ,[v0] ,[v1] ...) `(app ,v0 ,v1 ...)])
    (Predicate : Expr (ir) -> Predicate ()
      [(var ,x)
       `(if (primapp eq? (var ,x) (quoted-const #f)) (false) (true))]
      [(quoted-const ,d) (if d `(true) `(false))]
      [(if ,[p0] ,[p1] ,[p2]) `(if ,p0 ,p1 ,p2)]
      [(begin ,[f0] ... ,[p1]) `(begin ,f0 ... ,p1)]
      [(let ((,x ,[v]) ...) ,[p]) `(let ((,x ,v) ...) ,p)]
      [(primapp ,pr ,[p]) (guard (equal? pr 'not)) `(if ,p (false) (true))]
      [(primapp ,pr ,[v0] ...)
       (guard (predicate-primitive? pr))
       `(primapp ,pr ,v0 ...)]
      [(primapp ,pr ,[v0] ...)
       (guard (value-primitive? pr))
       `(if (primapp eq? (primapp ,pr ,v0 ...) (quoted-const #f))
            (false) (true))]
      [(primapp ,pr ,[v0] ...)
       (guard (effect-primitive? pr))
       `(begin (primapp ,pr ,v0 ...)(true))]
      [(sys-primapp ,spr ,[v0] ...)
       (guard (predicate-primitive? spr))
       `(sys-primapp ,spr ,v0 ...)]
      [(sys-primapp ,spr ,[v0] ...)
       (guard (value-primitive? spr))
       `(if (primapp eq? (sys-primapp ,spr ,v0 ...) (quoted-const #f))
            (false) (true))]
      [(sys-primapp ,spr ,[v0] ...)
       (guard (effect-primitive? spr))
       `(begin (sys-primapp ,spr ,v0 ...)(true))]
      [(anonymous-call ,[v0] ,[v1] ...)
       `(if (primapp eq? (anonymous-call ,v0 ,v1 ...) (quoted-const #f))
            (false) (true))]
      [(app ,[v0] ,[v1] ...)
       `(if (primapp eq? (app ,v0 ,v1 ...) (quoted-const #f))
            (false) (true))])
    (Effect : Expr (ir) -> Effect ()
      [(var ,x) `(nop)]
      [(quoted-const ,d) `(nop)]
      [(if ,[p0] ,[f1] ,[f2]) `(if ,p0 ,f1 ,f2)]
      [(begin ,[f0] ... ,[f1]) `(begin ,f0 ... ,f1)]
      [(let ((,x ,[v]) ...) ,[f]) `(let ((,x ,v) ...) ,f)]
      [(primapp ,pr ,[f]) (guard (equal? pr 'not)) f]
      [(primapp ,pr ,[f0] ...)
       (guard (or (predicate-primitive? pr) (value-primitive? pr)))
       (if (null? f0) `(nop) `(begin ,f0 ... (nop)))]
      [(primapp ,pr ,[v0] ...)
       (guard (effect-primitive? pr))
       `(primapp ,pr ,v0 ...)]
      [(sys-primapp ,spr ,[f0] ...)
       (guard (or (predicate-primitive? spr) (value-primitive? spr)))
       (if (null? f0) `(nop) `(begin ,f0 ... (nop)))]
      [(sys-primapp ,spr ,[v0] ...)
       (guard (effect-primitive? spr))
       `(sys-primapp ,spr ,v0 ...)]
      [(anonymous-call ,[v0] ,[v1] ...) `(anonymous-call ,v0 ,v1 ...)]
      [(app ,[v0] ,[v1] ...) `(app ,v0 ,v1 ...)]))

  (define-language L13
    (terminals
      (variable (x))
      (datum (d))
      (value-primitive (vp))
      (predicate-primitive (pp))
      (effect-primitive (ep))
      (system-primitive (spr)))
    (LetrecExpr (lrexpr)
      (letrec ((x lexpr) ...) v))
    (LambdaExpr (lexpr)
      (lambda (x ...) v))
    (Triv (t)
      (var x)
      (quoted-const d))
    (Value (v)
      t
      (if p1 v2 v3)
      (begin f0 ... v1)
      (let ((x v1) ...) v2)
      (primapp vp t ...)
      (sys-primapp spr t ...)
      (anonymous-call t0 t1 ...)
      (app t0 t1 ...))
    (Predicate (p)
      (true)
      (false)
      (if p1 p2 p3)
      (begin f0 ... p1)
      (let ((x v) ...) p)
      (primapp pp t ...)
      (sys-primapp spr t ...)
      (anonymous-call t0 t1 ...)
      (app t0 t1 ...))
    (Effect (f)
      (nop)
      (if p1 f2 f3)
      (begin f0 ... f1)
      (let ((x v) ...) f)
      (primapp ep t ...)
      (sys-primapp spr t ...)
      (anonymous-call t0 t1 ...)
      (app t0 t1 ...)))

  (define-parser parse-L13 L13)

  (define-pass remove-complex-opera* : L12 (ir) -> L13 ()
    (definitions
      (define remove-nulls
        (lambda (ls)
          (if (null? ls)
              '()
              (if (null? (car ls))
                  (remove-nulls (cdr ls))
                  (cons (car ls) (remove-nulls (cdr ls))))))))
    (LetrecExpr : LetrecExpr (ir) -> LetrecExpr ()
      [(letrec ((,x ,[lexpr]) ...) ,[v]) `(letrec ((,x ,lexpr) ...) ,v)])
    (LambdaExpr : LambdaExpr (ir) -> LambdaExpr ()
      [(lambda (,x ...) ,[v]) `(lambda (,x ...) ,v)])
    (Opera : Value (ir) -> Triv (dummy)
      [(var ,x) (values `(var ,x)  '())]
      [(quoted-const ,d) (values `(quoted-const ,d) '())]
      ;  [,[v]  (let ([tmp (gen-symbol 'tmp)])
      ; 	     (values `(var ,tmp)
      ; 	       (list tmp (in-context Value `,v))))])
      [(if ,[p1] ,[v2] ,[v3])
       (let ([tmp (gen-symbol 'tmp)])
         (values `(var ,tmp)
                 (list tmp (in-context Value `(if ,p1 ,v2 ,v3)))))]
      [(begin ,[f0] ... ,[v1])
       (let ([tmp (gen-symbol 'tmp)])
         (values `(var ,tmp)
                 (list tmp (in-context Value `(begin ,f0 ... ,v1)))))]
      [(let ((,x ,[v1]) ...) ,[v2])
       (let ([tmp (gen-symbol 'tmp)])
         (values `(var ,tmp)
                 (list tmp (in-context Value `(let ((,x ,v1) ...) ,v2)))))]
      [(primapp ,vp ,[t* binding*] ...)
        (let ([binding* (remove-nulls binding*)])
          (let ([tmp (gen-symbol 'tmp)])
            (if (null? binding*)
                (values `(var ,tmp)
                  (list tmp (in-context Value `(primapp ,vp ,t* ...))))
                (let ([x (map car binding*)] [v (map cadr binding*)])
                  (values `(var ,tmp)
                    (list tmp
                      (in-context
                        Value `(let ((,x ,v) ...)
                                 (primapp ,vp ,t* ...)))))))))]
      [(sys-primapp ,spr ,[t* binding*]...)
       (let ([binding* (remove-nulls binding*)])
         (let ([tmp (gen-symbol 'tmp)])
           (if (null? binding*)
               (values `(var ,tmp)
                       (list tmp (in-context
                                   Value `(sys-primapp ,spr ,t* ...))))
               (let ([x (map car binding*)][v (map cadr binding*)])
                 (values
                   `(var ,tmp)
                   (list
                     tmp (in-context
                           Value `(let ((,x ,v) ...)
                                    (sys-primapp ,spr ,t* ...)))))))))]
      [(anonymous-call ,[v0 binding] ,[v1 binding*] ...)
         (let ([binding* (remove-nulls (cons binding binding*))]
               [tmp (gen-symbol 'tmp)])
           (if (null? binding*)
               (values `(var ,tmp)
                       (list tmp (in-context Value
                                             `(anonymous-call ,v0 ,v1 ...))))
               (let ([x (map car binding*)] [v (map cadr binding*)])
                 (values `(var ,tmp)
                         (list tmp (in-context
                                     Value
                                     `(let ((,x ,v) ...)
                                        (anonymous-call ,v0 ,v1 ...))))))))]
      [(app ,[v0] ,[t* binding*] ...)
       (let ([binding* (remove-nulls binding*)])
         (let ([tmp (gen-symbol 'tmp)])
           (if (null? binding*)
               (values `(var ,tmp)
                       (list tmp (in-context Value `(app ,v0 ,t* ...))))
               (let ([x (map car binding*)] [v (map cadr binding*)])
                 (values `(var ,tmp)
                         (list tmp
                               (in-context Value
                                           `(let ((,x ,v) ...)
                                              (app ,v0 ,t* ...)))))))))])
    (Value  : Value (ir) -> Value ()
      [(var ,x) (in-context Triv `(var ,x))]
      [(quoted-const ,d) (in-context Triv `(quoted-const ,d))]
      [(if ,[p1] ,[v2] ,[v3]) `(if ,p1 ,v2 ,v3)]
      [(begin ,[f0] ... ,[v1]) `(begin ,f0 ... ,v1)]
      [(let ((,x ,[v1]) ...) ,[v2]) `(let ((,x ,v1) ...) ,v2)]
      [(primapp ,vp ,[t* binding*] ...)
       (let ([binding* (remove-nulls binding*)])
         (if (null? binding*)
             `(primapp ,vp ,t* ...)
             (let ([x (map car binding*)] [v (map cadr binding*)])
               `(let ((,x ,v) ...)
                  (primapp ,vp ,t* ...)))))]
      [(sys-primapp ,spr ,[t* binding*] ...)
       (let ([binding* (remove-nulls binding*)])
         (if (null? binding*)
             `(sys-primapp ,spr ,t* ...)
             (let ([x (map car binding*)][v (map cadr binding*)])
               `(let ((,x ,v) ...)
                  (sys-primapp ,spr ,t* ...)))))]
      [(anonymous-call ,[t0 binding] ,[t1 binding*] ...)
       (let ([binding* (remove-nulls (cons binding binding*))])
         (if (null? binding*)
             `(anonymous-call ,t0 ,t1 ...)
             (let ([x (map car binding*)] [v (map cadr binding*)])
               `(let ((,x ,v) ...)
                  (anonymous-call ,t0 ,t1 ...)))))]
      [(app ,[v0] ,[t* binding*] ...)
       (let ([binding* (remove-nulls binding*)])
         (if (null? binding*)
             `(app ,v0 ,t* ...)
             (let ([x (map car binding*)] [v (map cadr binding*)])
               `(let ((,x ,v) ...) (app ,v0 ,t* ...)))))])
    (Predicate : Predicate (ir) -> Predicate ()
      [(let ((,x ,[v]) ...) ,[p]) `(let ((,x ,v) ...) ,p)]
      [(primapp ,pp ,[t* binding*] ...)
       (let ([binding* (remove-nulls binding*)])
         (if (null? binding*)
             `(primapp ,pp ,t* ...)
             (let ([x (map car binding*)] [v (map cadr binding*)])
               `(let ((,x ,v) ...)
                  (primapp ,pp ,t* ...)))))]
      [(sys-primapp ,spr ,[t* binding*] ...)
       (let ([binding* (remove-nulls binding*)])
         (if (null? binding*)
             `(sys-primapp ,spr ,t* ...)
             (let ([x (map car binding*)] [v (map cadr binding*)])
               `(let ((,x ,v) ...)
                  (sys-primapp ,spr ,t* ...)))))]
      [(anonymous-call ,[t0 binding] ,[t1 binding*]...)
       (let ([binding* (remove-nulls (cons binding binding*))])
         (if (null? binding*)
             `(anonymous-call ,t0 ,t1 ...)
             (let ([x (map car binding*)] [v (map cadr binding*)])
               `(let ((,x ,v) ...)
                  (anonymous-call ,t0 ,t1 ...)))))]
      [(app ,[v0] ,[t* binding*] ...)
       (let ([binding* (remove-nulls binding*)])
         (if (null? binding*)
             `(app ,v0 ,t* ...)
             (let ([x (map car binding*)] [v (map cadr binding*)])
               `(let ((,x ,v) ...) (app ,v0 ,t* ...)))))])
    (Effect : Effect (ir) -> Effect ()
      [(let ((,x ,[v]) ...) ,[f]) `(let ((,x ,v) ...) ,f)]
      [(primapp ,ep ,[t* binding*] ...)
       (let ([binding* (remove-nulls binding*)])
         (if (null? binding*)
             `(primapp ,ep ,t* ...)
             (let ([x (map car binding*)] [v (map cadr binding*)])
               `(let ((,x ,v) ...)
                  (primapp ,ep ,t* ...)))))]
      [(sys-primapp ,spr ,[t* binding*] ...)
       (let ([binding* (remove-nulls binding*)])
         (if (null? binding*)
             `(sys-primapp ,spr ,t* ...)
             (let ([x (map car binding*)] [v (map cadr binding*)])
               `(let ((,x ,v) ...)
                  (sys-primapp ,spr ,t* ...)))))]
      [(anonymous-call ,[t0 binding] ,[t1 binding*] ...)
       (let ([binding* (remove-nulls (cons binding binding*))])
         (if (null? binding*)
             `(anonymous-call ,t0 ,t1 ...)
             (let ([x (map car binding*)] [v (map cadr binding*)])
               `(let ((,x ,v) ...) (anonymous-call ,t0 ,t1 ...)))))]
      [(app ,[v0] ,[t* binding*] ...)
       (let ([binding* (remove-nulls binding*)])
         (if (null? binding*)
             `(app ,v0 ,t* ...)
             (let ([x (map car binding*)] [v (map cadr binding*)])
               `(let ((,x ,v) ...) (app ,v0 ,t* ...)))))]))

  (define-language L14 (extends L13)
    (entry LetrecExpr)
    (Value (v) (- (anonymous-call t0 t1 ...)))
    (Predicate (p) (- (anonymous-call t0 t1 ...)))
    (Effect (f) (- (anonymous-call t0 t1 ...))))

  (define-pass remove-anonymous-call : L13 (ir) -> L14 ()
    (Value : Value (ir) -> Value ()
      [(anonymous-call ,[t0] ,[t1] ...)
       (let ([tmp (gen-symbol 'tmp)])
         `(let ([,tmp (sys-primapp procedure-code ,t0)])
            (app (var ,tmp) ,t0 ,t1 ...)))])
    (Predicate : Predicate (ir) -> Predicate ()
      [(anonymous-call ,[t0] ,[t1] ...)
       (let ([tmp (gen-symbol 'tmp)])
         `(let ([,tmp (sys-primapp procedure-code ,t0)])
            (app (var ,tmp) ,t0 ,t1 ...)))])
    (Effect : Effect (ir) -> Effect ()
      [(anonymous-call ,[t0] ,[t1] ...)
       (let ([tmp (gen-symbol 'tmp)])
         `(let ([,tmp (sys-primapp procedure-code ,t0)])
            (app (var ,tmp) ,t0 ,t1 ...)))]))

  (define-parser parse-L14 L14)

  (define-language L15
    (terminals
      (variable (x))
      (datum (d))
      (value-primitive (vp))
      (predicate-primitive (pp))
      (effect-primitive (ep))
      (system-primitive (spr)))
    (LetrecExpr (lrexpr)
      (letrec ((x1 lexpr) ...) rnexpr))
    (RunExpr (rnexpr)
      (run (x) tl))
    (LambdaExpr (lexpr)
      (lambda (x ...) tl))
    (Triv (t)
      (var x)
      (quoted-const d))
    (Application (a)
      (app t0 t1 ...))
    (Tail (tl)
      (return t1 t2)
      (if p1 tl2 tl3)
      (begin f0 ... tl1)
      (let ((x ntl1) ...) tl2)
      (app t0 t1 ...))
    (Nontail (ntl)
      t
      (if p1 ntl2 ntl3)
      (begin f0 ... ntl1)
      (let ((x ntl1) ...) ntl2)
      (primapp vp t ...)
      (sys-primapp spr t ...)
      (return-point x a))
    (Predicate (p)
      (true)
      (false)
      (if p1 p2 p3)
      (begin f0 ... p1)
      (let ((x ntl) ...) p)
      (primapp pp t ...)
      (sys-primapp spr t ...))
    (Effect (f)
      (nop)
      (if p1 f2 f3)
      (begin f0 ... f1)
      (let ((x ntl) ...) f)
      (primapp ep t ...)
      (sys-primapp spr t ...)
      (return-point x a)))

  (define-parser parse-L15 L15)

  ; (define process-tail
  ;       (lambda (expr rp)
  ;         (match expr
  ;           [(quote ,datum) `(return ,rp (quote ,datum))]
  ;           [,var (guard (symbol? var)) `(return ,rp ,var)]
  ;           [(if ,test ,[conseq] ,[altern])
  ;            `(if ,(process-nontail test) ,conseq ,altern)]
  ;           [(begin ,expr* ...)
  ;            `(begin
  ;               ,@((foldl '())
  ;                  (lambda (expr)
  ;                    (lambda (expr*)
  ;                      (if (null? expr*)
  ;                          (cons (process-tail expr rp) expr*)
  ;                          (cons (process-nontail expr) expr*))))
  ;                  expr*))]
  ;           [(let ([,lhs* ,rhs*] ...) ,[body])
  ;            (let ([rhs* (map process-nontail rhs*)])
  ;              `(let ([,lhs* ,rhs*] ...)
  ;                 ,body))]
  ;           [(,prim ,rand* ...)
  ;            (guard (primitive? prim))
  ;            (let ([rand* (map process-nontail rand*)])
  ;              (let ([tmp (gen-symbol 'tmp)])
  ;                `(let ([,tmp (,prim ,rand* ...)])
  ;                   (return ,rp ,tmp))))]
  ;           [(,rator ,rand* ...)
  ;            (let ([rator (process-nontail rator)]
  ;                  [rand* (map process-nontail rand*)])
  ;              `(,rator ,rp ,rand* ...))]
  ;           [,expr (error 'insert-dummy-rp "Invalid expression: ~s" expr)])))
  ;     (define process-nontail
  ;       (lambda (expr)
  ;         (match expr
  ;           [(quote ,datum) `(quote ,datum)]
  ;           [,var (guard (symbol? var)) `,var]
  ;           [(if ,[test] ,[conseq] ,[altern])
  ;            `(if ,test ,conseq ,altern)]
  ;           [(begin ,[expr*] ...) `(begin ,expr* ...)]
  ;           [(let ([,lhs* ,[rhs*]] ...) ,[body])
  ;            `(let ([,lhs* ,rhs*] ...) ,body)]
  ;           [(,prim ,[rand*] ...)
  ;            (guard (primitive? prim))
  ;            `(,prim ,rand* ...)]
  ;           [(,[rator] ,[rand*] ...)
  ;            (let ([label (gen-label (gen-symbol 'lab))])
  ;              `(return-point ,label
  ;                 (,rator ,label ,rand* ...)))]
  ;           [,expr (error 'insert-dummy-rp "Invalid expression: ~s" expr)])))
  ;     (define process-lambda
  ;       (lambda (expr)
  ;         (match expr
  ;           [(lambda (,formal* ...)  ,body)
  ;            (let ([rp (gen-symbol 'rp)])
  ;              `(lambda (,rp ,formal* ...)
  ;                 ,(process-tail body rp)))])))
  ;     (define process-letrec
  ;       (lambda (expr)
  ;         (match expr
  ;           [(letrec ([,lhs* ,rhs*] ...) ,body)
  ;            (let ([rhs* (map process-lambda rhs*)])
  ;              (let ([rp (gen-symbol 'rp)])
  ;                `(letrec ([,lhs* ,rhs*] ...)
  ;                   (run (,rp)
  ;                     ,(process-tail body rp)))))])))

  (define-pass introduce-dummy-rp : L14 (ir) -> L15 ()
    (LetrecExpr : LetrecExpr (ir) -> LetrecExpr ()
      [(letrec ((,x ,[lexpr]) ...) ,[rnexpr])
       `(letrec ((,x ,lexpr) ...) ,rnexpr)])
    (LambdaExpr : LambdaExpr (ir) -> LambdaExpr ()
      [(lambda (,x ...) ,v)
       (let ([rp (gen-symbol 'rp)])
         (let ([tl (ValueTail v rp)])
           `(lambda (,rp ,x ...) ,tl)))])
    (ValueRun : Value (ir) -> RunExpr ()
      [(var ,x) (let ([rp (gen-symbol 'rp)])
                  `(run (,rp) (return (var ,rp) (var ,x))))]
      [(quoted-const ,d)
       (let ([rp (gen-symbol 'rp)])
         `(run (,rp) (return (var ,rp) (quoted-const ,d))))]
      [(if ,[p1] ,v2 ,v3)
       (let ([rp (gen-symbol 'rp)])
         (let ([tl2 (ValueTail v2 rp)]
               [tl3 (ValueTail v3 rp)])
           `(run (,rp) (if ,p1 ,tl2 ,tl3))))]
      [(begin ,[f0] ... ,v1)
       (let ([rp (gen-symbol 'rp)])
         (let ([tl1 (ValueTail v1 rp)])
           `(run (,rp) (begin ,f0 ... ,tl1))))]
      [(let ((,x ,[ntl1]) ...) ,v2)
       (let ([rp (gen-symbol 'rp)])
         (let ([tl2 (ValueTail v2 rp)])
           `(run (,rp) (let ((,x ,ntl1) ...) ,tl2))))]
      [(primapp ,vp ,[t] ...)
       (let ([rp (gen-symbol 'rp)])
         (let ([tmp (gen-symbol 'tmp)])
           `(run (,rp) (let ([,tmp (primapp ,vp ,t ...)])
                         (return (var ,rp) (var ,tmp))))))]
      [(sys-primapp ,spr ,[t] ...)
       (let ([rp (gen-symbol 'rp)])
         (let ([tmp (gen-symbol 'tmp)])
           `(run (,rp) (let ([,tmp (primapp ,spr ,t ...)])
                         (return (var ,rp) (var ,tmp))))))]
      [(app ,[t0] ,[t1] ...)
       (let ([rp (gen-symbol 'rp)])
         `(run (,rp)(app ,t0 (var ,rp) ,t1 ...)))])
    (ValueTail : Value (ir rp) -> Tail ()
      [(var ,x) `(return (var ,rp) (var ,x))]
      [(quoted-const ,d) `(return (var ,rp) (quoted-const ,d))]
      [(if ,[p1] ,[ValueTail : v2 rp -> tl2] ,[ValueTail : v3 rp -> tl3])
       `(if ,p1 ,tl2 ,tl3)]
      [(begin ,[f0] ... ,[ValueTail : v1 rp -> tl1]) `(begin ,f0 ... ,tl1)]
      [(let ((,x ,[ntl1]) ...) ,[ValueTail : v2 rp -> tl2])
       `(let ((,x ,ntl1) ...) ,tl2)]
      [(primapp ,vp ,[t] ...)
       (let ([tmp (gen-symbol 'tmp)])
         `(let ([,tmp (primapp ,vp ,t ...)])
            (return (var ,rp) (var ,tmp))))]
      [(sys-primapp ,spr ,[t] ...)
       (let ([tmp (gen-symbol 'tmp)])
         `(let ([,tmp (primapp ,spr ,t ...)])
            (return (var ,rp) (var ,tmp))))]
      [(app ,[t0] ,[t1] ...) `(app ,t0 (var ,rp) ,t1 ...)])
    (ValueNTail : Value (ir) -> Nontail ()
      [(if ,[p1] ,[ntl2] ,[ntl3]) `(if ,p1 ,ntl2 ,ntl3)]
      [(begin ,[f0] ... ,[ntl1]) `(begin ,f0 ... ,ntl1)]
      [(let ((,x ,[ntl1]) ...) ,[ntl2]) `(let ((,x ,ntl1) ...) ,ntl2)]
      [(app ,[t0] ,[t1] ...)
       (let ([label (gen-label (gen-symbol 'lab))])
         `(return-point ,label (app ,t0 (var ,label) ,t1 ...)))])
    (Predicate : Predicate (ir) -> Predicate ()
      [(let ((,x ,[ntl1]) ...) ,[p]) `(let ((,x ,ntl1) ...) ,p)])
    (Effect : Effect (ir) -> Effect ()
      [(let ((,x ,[ntl1]) ...) ,[f]) `(let ((,x ,ntl1) ...) ,f)]
      [(app ,[t0] ,[t1] ...)
       (let ([label (gen-label (gen-symbol 'lab))])
         `(return-point ,label (app ,t0 (var ,label) ,t1 ...)))]))

  (define-language L16 (extends L15)
    (entry LetrecExpr)
    (Tail (tl)
      (- (let ((x ntl1) ...) tl2))
      (+ (let ((x ntl1)) tl2)))
    (Nontail (ntl)
      (- (let ((x ntl1) ...) ntl2))
      (+ (let ((x ntl1)) ntl2)))
    (Predicate (p)
      (- (let ((x ntl) ...) p))
      (+ (let ((x ntl)) p)))
    (Effect (f)
      (- (let ((x ntl) ...) f))
      (+ (let ((x ntl)) f))))

  (define-parser parse-L16 L16)

  (define-pass remove-nonunary-let : L15 (ir) -> L16 ()
    (Tail : Tail (ir) -> Tail ()
      [(let ((,x ,[ntl]) ...) ,[tl])
       (let loop ([lhs* x] [rhs* ntl])
         (if (null? lhs*)
             tl
             (let ([x (car lhs*)]
                   [ntl (car rhs*)]
                   [tl (loop (cdr lhs*) (cdr rhs*))])
               `(let ((,x ,ntl)) ,tl))))])
    (Nontail : Nontail (ir) -> Nontail ()
      [(let ((,x ,[ntl1]) ...) ,[ntl2])
       (let loop ([lhs* x] [rhs* ntl1])
         (if (null? lhs*)
             ntl2
             (let ([x (car lhs*)]
                   [ntl1 (car rhs*)]
                   [ntl2 (loop (cdr lhs*) (cdr rhs*))])
               `(let ((,x ,ntl1)) ,ntl2))))])
    (Predicate : Predicate (ir) -> Predicate ()
      [(let ((,x ,[ntl]) ...) ,[p])
       (let loop ([lhs* x] [rhs* ntl])
         (if (null? lhs*)
             p
             (let ([x (car lhs*)]
                   [ntl (car rhs*)]
                   [p (loop (cdr lhs*) (cdr rhs*))])
               `(let ((,x ,ntl)) ,p))))])
    (Effect : Effect (ir) -> Effect ()
      [(let ((,x ,[ntl]) ...) ,[f])
       (let loop ([lhs* x] [rhs* ntl])
         (if (null? lhs*)
             f
             (let ([x (car lhs*)]
                   [ntl (car rhs*)]
                   [f (loop (cdr lhs*) (cdr rhs*))])
               `(let ((,x ,ntl)) ,f))))]))

  (define-language L17 (extends L16)
    (entry LetrecExpr)
    (RunExpr (rnexpr)
      (- (run (x) tl))
      (+ (run (x) dec)))
    (LambdaExpr (lexpr)
      (- (lambda (x ...) tl))
      (+ (lambda (x ...) dec)))
    (DeclareExpr (dec) (+ (declare (x ...) tl)))
    (Tail (tl) (- (let ((x ntl1)) tl2)))
    (Nontail (ntl)
      (- t
         (if p1 ntl2 ntl3)
         (begin f0 ... ntl1)
         (let ((x ntl1)) ntl2)
         (primapp vp t ...)
         (sys-primapp spr t ...)
         (return-point x a)))
    (RhsExpr (rhs)
      (+ t
         (if p1 rhs2 rhs3)
         (begin f0 ... rhs1)
         (primapp vp t ...)
         (sys-primapp spr t ...)
         (return-point x a)))
    (Predicate (p) (- (let ((x ntl)) p)))
    (Effect (f)
      (- (let ((x ntl)) f))
      (+ (set! x rhs))))

  (define-parser parse-L17 L17)

  (define-pass return-of-set! : L16 (ir) -> L17 ()
    (definitions
      (define Effect*
        (lambda (f* var*)
          (if (null? f*)
              (values '() var*)
              (let-values ([(f var*) (Effect (car f*) var*)])
                (let-values ([(f* var*) (Effect* (cdr f*) var*)])
                  (values (cons f f*) var*)))))))
    (RunExpr : RunExpr (ir) -> RunExpr ()
      [(run (,x) ,[tl '() -> tl var*]) `(run (,x) (declare (,var* ...) ,tl))])
    (LambdaExpr : LambdaExpr (ir) -> LambdaExpr ()
      [(lambda (,x* ...) ,[tl '() -> tl var*]) `(lambda (,x* ...) (declare (,var* ...) ,tl))])
    (Tail : Tail (ir var*) -> Tail (var*)
      [(let ([,x ,ntl]) ,[tl var*])
       (let-values ([(rhs var*) (Nontail ntl var*)])
         (values `(begin (set! ,x ,rhs) ,tl) (cons x var*)))]
      [(if ,[p1 var*] ,tl2 ,tl3)
       (let-values ([(tl2 var*) (Tail tl2 var*)])
         (let-values ([(tl3 var*) (Tail tl3 var*)])
           (values `(if ,p1 ,tl2 ,tl3) var*)))]
      [(begin ,f* ... ,[tl var*])
       (let-values ([(f* var*) (Effect* f* var*)])
         (values `(begin ,f* ... ,tl) var*))])
    (Nontail : Nontail (ir var*) -> RhsExpr (var*)
      [(let ((,x ,ntl1)) ,[rhs2 var*])
       (let-values ([(rhs1 var*) (Nontail ntl1 var*)])
         (values `(begin (set! ,x ,rhs1) ,rhs2) (cons x var*)))]
      [(if ,[p1 var*] ,ntl2 ,ntl3)
       (let-values ([(rhs2 var*) (Nontail ntl2 var*)])
         (let-values ([(rhs3 var*) (Nontail ntl3 var*)])
           (values `(if ,p1 ,rhs2 ,rhs3) var*)))]
      [(begin ,f* ... ,[rhs var*])
       (let-values ([(f* var*) (Effect* f* var*)])
         (values `(begin ,f* ... ,rhs) var*))]
     ; TODO: something we could do better here? Triv->Rhs is effectively just this code
      [(quoted-const ,d) (values `(quoted-const ,d) var*)]
      [(var ,x) (values `(var ,x) var*)])
    (Effect : Effect (ir var*) -> Effect (var*)
      [(let ([,x ,ntl]) ,[f var*])
       (let-values ([(rhs var*) (Nontail ntl var*)])
         (values `(begin (set! ,x ,rhs) ,f) var*))]
      [(if ,[p1 var*] ,f2 ,f3)
       (let-values ([(f2 var*) (Effect f2 var*)])
         (let-values ([(f3 var*) (Effect f3 var*)])
           (values `(if ,p1 ,f2 ,f3) var*)))]
      [(begin ,f* ... ,[f var*])
       (let-values ([(f* var*) (Effect* f* var*)])
         (values `(begin ,f* ... ,f) var*))])
    (Predicate : Predicate (ir var*) -> Predicate (var*)
      [(let ([,x ,ntl]) ,[p var*])
       (let-values ([(rhs var*) (Nontail ntl var*)])
         (values `(begin (set! ,x ,rhs) ,p) (cons x var*)))]
      [(if ,[p1 var*] ,p2 ,p3)
       (let-values ([(p2 var*) (Predicate p2 var*)])
         (let-values ([(p3 var*) (Predicate p3 var*)])
           (values `(if ,p1 ,p2 ,p3) var*)))]
      [(begin ,f* ... ,[p var*])
       (let-values ([(f* var*) (Effect* f* var*)])
         (values `(begin ,f* ... ,p) var*))]))

  (define-language L18 (extends L17)
    (entry LetrecExpr)
    (Triv (t) (+ (label x))))

  (define-parser parse-L18 L18)

  (define-pass explicit-labels : L17 (ir) -> L18 ()
    (LetrecExpr : LetrecExpr (ir [labs '()]) -> LetrecExpr ()
      [(letrec ((,x ,[lexpr x -> lexpr]) ...) ,[rnexpr x -> rnexpr])
       `(letrec ((,x ,lexpr) ...) ,rnexpr)])
    (LambdaExpr : LambdaExpr (ir labs) -> LambdaExpr ())
    (Triv : Triv (ir labs) -> Triv ()
      [(var ,x) (if (memq x labs) `(label ,x) `(var ,x))])
    (Application : Application (ir labs) -> Application ())
    (DeclareExpr : DeclareExpr (ir labs) -> DeclareExpr ())
    (RunExpr : RunExpr (ir labs) -> RunExpr ())
    (Tail : Tail (ir labs) -> Tail ())
    (RhsExpr : RhsExpr (ir labs) -> RhsExpr ()
      [(return-point ,x ,a) (let ([a (Application a (cons x labs))])
                              `(return-point ,x ,a))])
    (Predicate : Predicate (ir labs) -> Predicate ())
    (Effect : Effect (ir labs) -> Effect ()
      [(return-point ,x ,a) (let ([a (Application a (cons x labs))])
                              `(return-point ,x ,a))])))
