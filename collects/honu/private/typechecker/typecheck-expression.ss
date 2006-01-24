(module typecheck-expression mzscheme
  
  (require (all-except (lib "list.ss" "srfi" "1") any)
           (lib "contract.ss")
           (lib "plt-match.ss")
           (lib "struct.ss")
           "../../ast.ss"
           "../../readerr.ss"
           "../../tenv.ss"
           "../tools/general.ss"
           "typecheck-parameters.ss"
           "type-utils.ss")
  
  (provide/contract [typecheck-expression ((((syntax/c symbol?) . -> . (union ast:type? false/c))
                                            (union ast:type? false/c)
                                            ast:expr?)
                                           . ->* .
                                           (ast:expr? 
                                            ast:type?))])
  
  ;; lenv  : ((syntax/c symbol?) . -> . (union ast:type false/c))
  ;;   lexical environment (includes top-level bindings and
  ;;   binding for #'this if inside class or mixin)
  ;; ctype : (union ast:type? false/c)
  ;;   type of context for expression
  ;; expr  : ast:expr?
  ;;   expression to typecheck
  
  (define (typecheck-expression lenv ctype expr)
    (match expr
      [(struct ast:expr:self (stx))
       (cond
         [(lenv #'this) => (lambda (t)
                             (if (<:_P t ctype)
                                 (values expr t)
                                 (raise-honu-type-error stx ctype t)))]
         [else (raise-read-error-with-stx
                "Found use of 'this' outside of class or mixin"
                stx)])]
      [(struct ast:expr:tuple/select (stx slot arg))
       ;; changed to have a special "select" type that does the following:
       ;; (type-select n t) <:_P (type-tuple args) if
       ;;   a) (>= (length args) n)
       ;;   b) (type-equal? (list-ref args (- n 1)) t)
       ;;   c) (list-ref args m) <:_P (type-top) for all m =/= n (vacuously true)
       (let-values ([(e1 t1) (typecheck-expression lenv (make-ast:type:partial/tuple stx slot ctype) arg)])
         (if (not (ast:type:tuple? t1))
             (raise-read-error-with-stx "Tried to use select with non-tuple expression" stx))
         (let ([etype (list-ref (ast:type:tuple-elems t1) (- slot 1))])
           (values (copy-struct ast:expr:tuple/select expr
                     [ast:expr:tuple/select-arg e1])
                   etype)))]
      [(struct ast:expr:var (stx name))
       (cond
         [(lenv name) => (lambda (t)
                           (if (<:_P t ctype)
                               (values expr t)
                               (raise-honu-type-error stx ctype t)))]
         [else (raise-read-error-with-stx
                (format "Unbound variable ~a" (syntax-e name))
                stx)])]
      [(struct ast:expr:assign (stx lhs rhs))
       ;; the context type for the lhs is a vacuous one.
       (let*-values ([(e1 t1) (typecheck-expression lenv (make-top-type (ast-syntax lhs)) lhs)]
                     [(e2 t2) (typecheck-expression lenv t1 rhs)])
         (let ([void-type (make-void-type stx)])
           (if (<:_P void-type ctype)
               (values (copy-struct ast:expr:assign expr
                         [ast:expr:assign-lhs e1]
                         [ast:expr:assign-rhs e2])
                       void-type)
               (raise-honu-type-error stx ctype void-type))))]
      [(struct ast:expr:apply (stx func arg))
       (let*-values ([(e1 t1) (typecheck-expression lenv (make-func-type (ast-syntax func)
                                                                                     (make-bottom-type (ast-syntax func))
                                                                                     ctype) func)]
                     [(e2 t2) (typecheck-expression lenv (ast:type:function-input t1) arg)])
         (let ([ret-type (ast:type:function-output t1)])
           (if (<:_P ret-type ctype)
               (values (copy-struct ast:expr:apply expr
                         [ast:expr:apply-func e1]
                         [ast:expr:apply-arg  e2])
                       ret-type)
               (raise-honu-type-error stx ctype ret-type))))]
      [(struct ast:expr:literal (stx type value))
       (if (<:_P type ctype)
           (values expr type)
           (raise-honu-type-error stx ctype type))]
      [(struct ast:expr:unary/op (stx op op-stx _ arg))
       (case op
         [(not)
          (let-values ([(e1 t1) (typecheck-expression lenv (make-bool-type (ast-syntax arg)) arg)])
            (let ([ret-type (make-bool-type stx)])
              (if (<:_P ret-type ctype)
                  (values (copy-struct ast:expr:unary/op expr
                            [ast:expr:unary/op-rator-type t1]
                            [ast:expr:unary/op-arg     e1])
                          ret-type)
                  (raise-honu-type-error stx ctype ret-type))))]
         [(minus)
          (let-values ([(e1 t1) (typecheck-expression lenv (make-top-type (ast-syntax arg)) arg)])
            (if (not (ast:type:primitive? t1))
                (raise-read-error-with-stx
                 "Invalid type for argument to unary minus"
                 (ast-syntax arg))
                (let ([ret-type (case (ast:type:primitive-name t1)
                                  [(int)   (make-int-type   (ast-syntax arg))]
                                  [(float) (make-float-type (ast-syntax arg))]
                                  [else    (raise-read-error-with-stx
                                            (format "Argument to unary minus must be int or float type, got ~a"
                                                    (printable-type t1))
                                            (ast-syntax arg))])])
                  (if (<:_P ret-type ctype)
                      (values (copy-struct ast:expr:unary/op expr
                                [ast:expr:unary/op-rator-type t1]
                                [ast:expr:unary/op-arg     e1])
                              ret-type)
                      (raise-honu-type-error stx ctype ret-type)))))]
         [else
          (raise-read-error-with-stx
           "Unknown operator"
           op-stx)])]
      [(struct ast:expr:binary/op (stx op op-stx _ larg rarg))
       (case op
         ;; binary boolean operators
         [(or and)
          (let-values ([(e1 t1) (typecheck-expression lenv (make-bool-type (ast-syntax larg)) larg)]
                       [(e2 t2) (typecheck-expression lenv (make-bool-type (ast-syntax rarg)) rarg)])
            (let ([ret-type (make-bool-type stx)])
              (if (<:_P ret-type ctype)
                  (values (copy-struct ast:expr:binary/op expr
                            [ast:expr:binary/op-rator-type (make-bool-type (ast-syntax larg))]
                            [ast:expr:binary/op-left    e1]
                            [ast:expr:binary/op-right    e2])
                          ret-type)
                  (raise-honu-type-error stx ctype ret-type))))]
         [(clseq)
          (let-values ([(e1 t1) (typecheck-expression lenv (make-any-type (ast-syntax larg)) larg)]
                       [(e2 t2) (typecheck-expression lenv (make-any-type (ast-syntax rarg)) rarg)])
            (let ([ret-type (make-bool-type stx)])
              (if (<:_P ret-type ctype)
                  (values (copy-struct ast:expr:binary/op expr
                            [ast:expr:binary/op-rator-type (make-any-type (ast-syntax larg))]
                            [ast:expr:binary/op-left    e1]
                            [ast:expr:binary/op-right    e2])
                          ret-type)
                  (raise-honu-type-error stx ctype ret-type))))]
         [(equal neq)
          (let-values ([(e1 t1) (typecheck-expression lenv (make-top-type (ast-syntax larg)) larg)]
                       [(e2 t2) (typecheck-expression lenv (make-top-type (ast-syntax rarg)) rarg)])
            (let ([ret-type (make-bool-type stx)]
                  [arg-type (cond
                              [(and (<:_P t1 (make-any-type (ast-syntax larg)))
                                    (<:_P t2 (make-any-type (ast-syntax rarg))))
                               (make-any-type (ast-syntax larg))]
                              [(check-prim-types-for-binop stx t1 t2) => (lambda (t) t)])])
              (if (<:_P ret-type ctype)
                  (values (copy-struct ast:expr:binary/op expr
                            [ast:expr:binary/op-rator-type arg-type]
                            [ast:expr:binary/op-left    e1]
                            [ast:expr:binary/op-right    e2])
                          ret-type)
                  (raise-honu-type-error stx ctype ret-type))))]
         [(le lt ge gt)
          (let-values ([(e1 t1) (typecheck-expression lenv (make-top-type (ast-syntax larg)) larg)]
                       [(e2 t2) (typecheck-expression lenv (make-top-type (ast-syntax rarg)) rarg)])
            (let ([ret-type (make-bool-type stx)]
                  [arg-type (check-prim-types-for-binop stx t1 t2)])
              (if (<:_P ret-type ctype)
                  (values (copy-struct ast:expr:binary/op expr
                            [ast:expr:binary/op-rator-type arg-type]
                            [ast:expr:binary/op-left    e1]
                            [ast:expr:binary/op-right    e2])
                          ret-type)
                  (raise-honu-type-error stx ctype ret-type))))]
         [(plus)
          (let-values ([(e1 t1) (typecheck-expression lenv (make-top-type (ast-syntax larg)) larg)]
                       [(e2 t2) (typecheck-expression lenv (make-top-type (ast-syntax rarg)) rarg)])
            (let ([arg-type (check-prim-types-for-binop stx t1 t2)])
              (case (ast:type:primitive-name arg-type)
                [(int float string)
                 (if (<:_P arg-type ctype)
                     (values (copy-struct ast:expr:binary/op expr
                               [ast:expr:binary/op-rator-type arg-type]
                               [ast:expr:binary/op-left    e1]
                               [ast:expr:binary/op-right    e2])
                             arg-type)
                     (raise-honu-type-error stx ctype arg-type))]
                [else (raise-read-error-with-stx
                       "The plus operator requires both arguments to be of type int, type float, or type string"
                       stx)])))]
         [(minus times div)
          (let-values ([(e1 t1) (typecheck-expression lenv (make-top-type (ast-syntax larg)) larg)]
                       [(e2 t2) (typecheck-expression lenv (make-top-type (ast-syntax rarg)) rarg)])
            (let ([arg-type (check-prim-types-for-binop stx t1 t2)])
              (case (ast:type:primitive-name arg-type)
                [(int float)
                 (if (<:_P arg-type ctype)
                     (values (copy-struct ast:expr:binary/op expr
                               [ast:expr:binary/op-rator-type arg-type]
                               [ast:expr:binary/op-left    e1]
                               [ast:expr:binary/op-right    e2])
                             arg-type)
                     (raise-honu-type-error stx ctype arg-type))]
                [else (raise-read-error-with-stx
                       "Arithmetic operator requires both arguments to be of type int or type float"
                       stx)])))]
         [(mod)
          (let-values ([(e1 t1) (typecheck-expression lenv (make-int-type (ast-syntax larg)) larg)]
                       [(e2 t2) (typecheck-expression lenv (make-int-type (ast-syntax rarg)) rarg)])
            (let ([ret-type (make-int-type stx)])
              (if (<:_P ret-type ctype)
                  (values (copy-struct ast:expr:binary/op expr
                            [ast:expr:binary/op-rator-type (make-int-type (ast-syntax larg))]
                            [ast:expr:binary/op-left    e1]
                            [ast:expr:binary/op-right    e2])
                          ret-type)
                  (raise-honu-type-error stx ctype ret-type))))]
         [else 
          (raise-read-error-with-stx
           "Unknown operator"
           op-stx)])]
      [(struct ast:expr:function (stx ret-type args body))
       (if (not (type-valid? ret-type))
           (raise-read-error-with-stx
            "Return type of anonymous function is invalid"
            (ast-syntax ret-type)))
       (let ([conflicting-name (get-first-non-unique-name (map ast:formal-name args))])
         (if conflicting-name
             (raise-read-error-with-stx
              (format "Variable name ~a used more than once in function arguments"
                      (printable-key conflicting-name))
              conflicting-name)))
       (for-each (lambda (t)
                   (if (not (type-valid? t))
                       (raise-read-error-with-stx
                        "Type of argument of anonymous function is invalid"
                        (ast-syntax t))))
                 (map ast:formal-type args))
       ;; since we have explicit return type annotations now, we use them for the body's ctype and rtype.
       (let ([body-lenv  (fold (lambda (f e)
                                 (extend-fenv (ast:formal-name f)
                                              (ast:formal-type f)
                                              e))
                               lenv args)])
         (let-values ([(body _) (parameterize ([current-return-type ret-type])
                                  (typecheck-expression body-lenv ret-type body))])
           ;; we also have the lambda's return type be what was explicitly annotated instead of what we got back
           (let ([lam-type (make-func-type stx (make-tuple-type stx (map ast:formal-type args)) ret-type)])
             (if (<:_P lam-type ctype)
                 (values (copy-struct ast:expr:function expr
                           [ast:expr:function-body body])
                         lam-type)
                 (raise-honu-type-error stx ctype lam-type)))))]
      [(struct ast:expr:if (stx test then else))
       (if else
           (let-values ([(e1 t1) (typecheck-expression lenv (make-bool-type (ast-syntax test)) test)]
                        [(e2 t2) (typecheck-expression lenv ctype then)]
                        [(e3 t3) (typecheck-expression lenv ctype else)])
             ;; this should work, but I get the following:
             ;; -- context expected 1 value, received 2 values: #<struct:ast:expr:if> #<struct:ast:type:primitive>
             ;; with the arrow going from values -> cond, so I'm going to rewrite as a nested-if for now.
             (cond
               ;; if we had a context type and got to this point, then both t2 and t3 must be related to it (either equal
               ;; or subtypes), so use it as the type of the entire if expression.
               ;;
               ;; No, we can't do this.  Think about the fact where we use an if expression on the left-hand side of a
               ;; member access -- we need to get the most specific type back as the result, not the (vacuous) context
               ;; type.  If the branches are of unrelated types, then having a context type doesn't help us all the time.
               ;; [ctype
               ;;   (values (copy-struct ast:expr:if expr
               ;;             [ast:expr:if-test e1]
               ;;             [ast:expr:if-then e2]
               ;;             [ast:expr:if-else e3])
               ;;           ctype)]
               ;; if there was no ctype, then we require either t2 <: t3 or t3 <: t2, and we'll pick the supertype.
               [(<:_P t2 t3)
                (values (copy-struct ast:expr:if expr
                          [ast:expr:if-test e1]
                          [ast:expr:if-then e2]
                          [ast:expr:if-else e3])
                        t3)]
               [(<:_P t3 t2)
                (values (copy-struct ast:expr:if expr
                          [ast:expr:if-test e1]
                          [ast:expr:if-then e2]
                          [ast:expr:if-else e3])
                        t2)]
               ;; if we got to here, then there's no good type to use as the type of the entire if expression, so
               ;; raise an error.
               [else (raise-read-error-with-stx
                      "Branches of if statement are of unrelated types"
                      stx)]))
           ;; if else is #f, there was no else branch, so the then branch must be of void type.
           (let-values ([(e1 t1) (typecheck-expression lenv (make-bool-type (ast-syntax test)) test)]
                        [(e2 t2) (typecheck-expression lenv (make-void-type (ast-syntax then)) then)])
             (let ([ret-type (make-void-type stx)])
               (if (<:_P ret-type ctype)
                   (values (copy-struct ast:expr:if expr
                             [ast:expr:if-test e1]
                             [ast:expr:if-then e2])
                           ret-type)
                   (raise-read-error-with-stx
                    "Found if expression without else branch in non-void context"
                    stx)))))]
      [(struct ast:expr:cast (stx obj type))
       (if (not (type-valid? type))
           (raise-read-error-with-stx
            "Type argument of cast is not a valid type"
            (ast-syntax type)))
       (let-values ([(e1 t1) (typecheck-expression lenv (make-any-type (ast-syntax obj)) obj)])
         (if (<:_P type ctype)
             (values (copy-struct ast:expr:cast expr
                       [ast:expr:cast-object e1])
                     type)
             (raise-honu-type-error stx ctype type)))]
      [(struct ast:expr:isa (stx obj type))
       (if (not (type-valid? type))
           (raise-read-error-with-stx
            "Type argument of isa is not a valid type"
            (ast-syntax type)))
       (let-values ([(e1 t1) (typecheck-expression lenv (make-any-type (ast-syntax obj)) obj)])
         (let ([ret-type (make-bool-type stx)])
           (if (<:_P ret-type ctype)
               (values (copy-struct ast:expr:isa expr
                         [ast:expr:isa-object e1])
                       ret-type)
               (raise-honu-type-error stx ctype ret-type))))]
      [(struct ast:expr:member (stx 'my _ name _))
       (cond
         [(lenv name)
          =>
          (lambda (t)
            (if (ast:type:method? t)
                (let ([fun-type (make-func-type (ast-syntax t) (ast:type:method-input t) (ast:type:method-output t))])
                  (if (<:_P fun-type ctype)
                      (values (copy-struct ast:expr:member expr
                                           [ast:expr:member-method? #t])
                              fun-type)
                      (raise-honu-type-error stx ctype fun-type)))
                (if (<:_P t ctype)
                    (values expr t)
                    (raise-honu-type-error stx ctype t))))]
         [else (raise-read-error-with-stx
                (format "Static member ~a not found" (syntax-e name))
                stx)])]
      [(struct ast:expr:member (stx obj _ name _))
       (let-values ([(e1 t1) (typecheck-expression lenv (make-any-type (ast-syntax obj)) obj)])
         ;; if obj was something like error or return, which do not give us a valid type for
         ;; getting the appropriate member...
         (if (ast:type:bot? t1)
             (raise-read-error-with-stx
              "Attempt to access member of an expression which does not return"
              stx))
         ;; if obj was null...
         (if (ast:type:object:null? t1)
             (raise-read-error-with-stx
              "Null has no fields or methods"
              stx))
         (let ([t (get-member-type t1 name)])
           (cond
             [(ast:type:method? t)
              (let ([fun-type (make-func-type (ast-syntax t) (ast:type:method-input t) (ast:type:method-output t))])
                (if (<:_P fun-type ctype)
                    (values (copy-struct ast:expr:member expr
                              [ast:expr:member-object     e1]
                              [ast:expr:member-object-type    t1]
                              [ast:expr:member-method? #t])
                            fun-type)
                    (raise-honu-type-error stx ctype fun-type)))]
             [t
              (if (<:_P t ctype)
                  (values (copy-struct ast:expr:member expr
                            [ast:expr:member-object  e1]
                            [ast:expr:member-object-type t1])
                          t)
                  (raise-honu-type-error stx ctype t))]
             [else (raise-read-error-with-stx
                    (format "Member ~a not found in type ~a" (syntax-e name) (printable-type t1))
                    stx)])))]
      [(struct ast:expr:new (stx class type args))
       (let ([class-entry (get-class-entry class)]
             [new-type    (if type type ctype)])
         ;; the following can only be triggered if the type annontation isn't a type
         (if (and type (not (type-valid? type)))
             (raise-read-error-with-stx
              (format "Type annotation ~a on new statement is not a valid type" (printable-type new-type))
              (ast-syntax new-type)))
         ;; the following two checks can only be triggered if there is no type annotation
         (if (ast:type:top? new-type)
             (raise-read-error-with-stx
              "type of instantiation must be explicitly annotated"
              stx))
         (if (not (<:_P new-type (make-any-type stx)))
             (raise-read-error-with-stx
              (format "new statement appears in context of non-interface type ~a"
                      (printable-type new-type))
              stx))
         ;; the class must implement a subtype of the type we're instantiating it at
         (if (not (ormap (lambda (t)
                           (<:_P t new-type))
                         (tenv:class-impls class-entry)))
             (raise-read-error-with-stx
              (format "class ~a does not implement a subtype of type ~a"
                      (printable-key class)
                      (printable-type new-type))
              stx))
         (let ([args (check-inits stx (lambda (e t)
                                             (typecheck-expression lenv t e))
                                  (tenv:class-inits class-entry) args)])
           (if (<:_P new-type ctype)
               (values (copy-struct ast:expr:new expr
                         [ast:expr:new-type new-type]
                         [ast:expr:new-args args])
                       new-type)
               (raise-honu-type-error stx ctype new-type))))]
      [(struct ast:expr:while (stx cond body))
       (let-values ([(e1 t1) (typecheck-expression lenv (make-bool-type (ast-syntax cond)) cond)]
                    [(e2 t2) (typecheck-expression lenv (make-void-type (ast-syntax body)) body)])
         (let ([ret-type (make-void-type stx)])
           (if (<:_P ret-type ctype)
               (values (copy-struct ast:expr:while expr
                         [ast:expr:while-test e1]
                         [ast:expr:while-body e2])
                       ret-type)
               (raise-honu-type-error stx ctype ret-type))))]
      [(struct ast:expr:cond (stx clauses else))
       (if else
           (let-values ([(clauses types) (map-two-values (lambda (c)
                                                           (typecheck-cond-clause lenv ctype c))
                                                         clauses)]
                        [(else    etype) (typecheck-expression lenv ctype else)])
             (cond
               ;; if ctype exists, just use it
               ;;
               ;; can't do this, see if for reasoning
               ;;  [ctype
               ;;   (values (copy-struct ast:expr:cond expr
               ;;             [ast:expr:cond-clauses clauses])
               ;;           ctype)]
               ;; otherwise find the most super type of all the branches
               [(pick-super-type-from-list (cons etype types))
                =>
                (lambda (t)
                  (values (copy-struct ast:expr:cond expr
                            [ast:expr:cond-clauses clauses]
                            [ast:expr:cond-else    else])
                          t))]
               ;; otherwise we're hosed for determining a type -- throw an error
               [else
                (raise-read-error-with-stx
                 "At least two branches of the cond statement have unrelated types"
                 stx)]))
           ;; if else is #f, there was no else branch, so the cond clauses must be of void type.
           (let-values ([(clauses types) (map-two-values (lambda (c)
                                                           (typecheck-cond-clause lenv
                                                                                  (make-void-type (ast-syntax c)) c))
                                                         clauses)])
             (let ([ret-type (make-void-type stx)])
               (if (<:_P ret-type ctype)
                   (values (copy-struct ast:expr:cond expr
                             [ast:expr:cond-clauses clauses])
                           ret-type)
                   (raise-read-error-with-stx
                    "Cond expression without an else branch found in non-void context"
                    stx)))))]
      [(struct ast:expr:return (stx body))
       ;; returns don't return to their context, but to the context of the method or function call in which
       ;; they were invoked.  Because of this a) rtype must not be #f (else we're not in a method or function
       ;; body) and b) the type of a return statement is the bottom type (same as error).
       (if (current-return-type)
           ;; we use rtype as the context type here, since that's the type that needs to be returned.
           (let-values ([(e1 _) (typecheck-expression lenv (current-return-type) body)])
             ;; we don't need to check (bottom-type) <:_P ctype, because that's vacuously true.
             (values (copy-struct ast:expr:return expr
                       [ast:expr:return-result e1])
                     (make-bottom-type stx)))
           (raise-read-error-with-stx
            "Return statement found outside body of method or function"
            stx))]
      [(struct ast:expr:tuple (stx vals))
       (cond
         [(ast:type:tuple? ctype)
          ;; we have a tuple context type, so use its contents, but make
          ;; sure it's the right length.
          (if (not (= (length vals) (length (ast:type:tuple-elems ctype))))
              (raise-read-error-with-stx
               (format "Expected tuple of length ~a, got tuple of length ~a"
                       (length vals)
                       (length (ast:type:tuple-elems ctype)))
               stx))
          (let-values ([(vals types) (map-two-values (lambda (e t)
                                                       (typecheck-expression lenv t e))
                                                     vals (ast:type:tuple-elems ctype))])
            (values (copy-struct ast:expr:tuple expr
                      [ast:expr:tuple-elems vals])
                    (make-tuple-type stx types)))]
         ;; we must be in hte context of a select expression, so
         [(ast:type:partial/tuple? ctype)
          (if (not (<= (ast:type:partial/tuple-position ctype) (length vals)))
              (raise-read-error-with-stx
               (format "Expected tuple of length at least ~a, got tuple of length ~a"
                       (ast:type:partial/tuple-position ctype)
                       (length vals))
               stx))
          (let-values ([(vals types) (map-two-values (lambda (e t)
                                                       (typecheck-expression lenv t e))
                                                     vals (gen-top-except-for (length vals)
                                                                              (ast:type:partial/tuple-position ctype)
                                                                              (ast:type:partial/tuple-elem ctype)))])
            (values (copy-struct ast:expr:tuple expr
                      [ast:expr:tuple-elems vals])
                    (make-tuple-type stx types)))]
         ;; if we have the top type here, then we either a) don't care about the type or
         ;; b) are going to check it after we return, so just do the simple thing -- since
         ;; we have no knowledge about what's wanted, we just check each component with ctype.
         [(ast:type:top? ctype)
          (let-values ([(vals types) (map-two-values (lambda (e)
                                                       (typecheck-expression lenv
                                                                             (make-top-type (ast-syntax e)) e))
                                                     vals)])
            (values (copy-struct ast:expr:tuple expr
                      [ast:expr:tuple-elems vals])
                    (make-tuple-type stx types)))]
         [else (raise-read-error-with-stx
                "Expected non-tuple expression (or tuple of length 1) here"
                stx)])]
      [(struct ast:expr:let (_ bindings body))
       (let*-values ([(bindings lenv) (map-and-fold (lambda (bind lenv)
                                                      (typecheck-binding lenv bind))
                                                    lenv
                                                    bindings)]
                     [(e1 t1) (typecheck-expression lenv ctype body)])
         (values (copy-struct ast:expr:let expr
                   [ast:expr:let-bindings bindings]
                   [ast:expr:let-body     e1])
                 t1))]
      [(struct ast:expr:sequence (_ effects value))
       (let-values ([(effects _)
                     (map-two-values
                      (lambda (e)
                        (typecheck-expression lenv (make-void-type (ast-syntax e)) e))
                      effects)]
                     [(e1 t1) (typecheck-expression lenv ctype value)])
         (values (copy-struct ast:expr:sequence expr
                   [ast:expr:sequence-statements effects]
                   [ast:expr:sequence-result   e1])
                 t1))]))
  
  ;; bindings have no ctype because they're always in the void type context
  ;; they return the elaborated binding and a new environment extended with the
  ;; type of the bound variable.
  (define (typecheck-binding lenv binding)
    (match binding
      [(struct ast:defn:binding (stx names types value))
       ;; make sure to remove all the #f for don't care arguments.
       (let ([conflicting-name (get-first-non-unique-name (filter (lambda (n) n) names))])
         (if conflicting-name
             (raise-read-error-with-stx
              (format "Variable name ~a used more than once in binding form"
                      (printable-key conflicting-name))
              conflicting-name)))
       (for-each (lambda (n t)
                   (if (and (not (and (not n)
                                      (ast:type:top? t)))
                            (not (type-valid? t)))
                       (raise-read-error-with-stx
                        "Type of locally bound variable is undefined"
                        (ast-syntax t))))
                 names types)
       (let-values ([(e1 t1) (typecheck-expression lenv (make-tuple-type (ast-syntax value) types) value)])
         (values (copy-struct ast:defn:binding binding
                   [ast:defn:binding-init e1])
                 (fold (lambda (name type lenv)
                         (if name
                             (extend-fenv name type lenv)
                             lenv))
                       lenv names types)))]))
  
  (define (typecheck-cond-clause lenv ctype clause)
    (match clause
      [(struct ast:cond/clause (stx pred rhs))
       (let-values ([(e1 t1) (typecheck-expression lenv (make-bool-type (ast-syntax pred)) pred)]
                    [(e2 t2) (typecheck-expression lenv ctype rhs)])
         (values (copy-struct ast:cond/clause clause
                   [ast:cond/clause-test e1]
                   [ast:cond/clause-result  e2])
                 t2))]))
            
  (define (check-prim-types-for-binop stx t1 t2)
    (cond
      [(and (ast:type:bot? t1)
            (ast:type:primitive?   t2))
       t2]
      [(and (ast:type:primitive?   t1)
            (ast:type:bot? t2))
       t1]
      [(and (ast:type:primitive? t1)
            (ast:type:primitive? t2)
            (type-equal? t1 t2))
       t1]
      [else
       (raise-read-error-with-stx
        (format "Expected primitive types for binary operator, got ~a and ~a"
                (printable-type t1)
                (printable-type t2))
        stx)]))

  (define (check-inits stx type-fun inits new-args)
    (let-values ([(new-args remaining-inits)
                  (map-and-fold (lambda (arg inits)
                                  (let*-values ([(init remaining-inits)
                                                 (find-init inits (ast:named/arg-name arg))]
                                                [(e1 t1)
                                                 (type-fun (ast:named/arg-actual arg) (tenv:init-type init))])
                                    (values (copy-struct ast:named/arg arg
                                              [ast:named/arg-actual e1])
                                            remaining-inits)))
                                inits new-args)])
      (if (andmap tenv:init-optional? remaining-inits)
          new-args
          (raise-read-error-with-stx
           (format "No value assigned for init arg ~a" 
                   (printable-key (tenv:init-name (car remaining-inits))))
           stx))))
  
  ;; find-inits takes the name of an init arg to find in a list of inits and
  ;; returns both the init (if found) and the list minus that init
  (define (find-init inits name)
    (let loop ([inits  inits]
               [passed '()])
      (cond
        [(null? inits)
         (raise-read-error-with-stx
          (format "class does not have an init arg with name ~a"
                  (printable-key name))
          name)]
        [(tenv-key=? name (tenv:init-name (car inits)))
         (values (car inits)
                 (append (reverse passed) (cdr inits)))]
        [else
         (loop (cdr inits)
               (cons (car inits) passed))])))
        
  
  ;; assumes a non-empty list
  (define (pick-super-type-from-list ts)
    (define (pick-super-type-with-acc ts t)
      (cond
        ;; t is a super-type of all the other branches
        [(andmap (lambda (t2)
                   (<:_P t2 t))
                 ts)
         t]
        ;; if there's a type t2 in ts that is not equal to t
        ;; but t <:_P t2, then recur with t2 instead.
        [(find (lambda (t2)
                 (and (not (type-equal? t t2))
                      (<:_P t t2)))
               ts)
         =>
         (lambda (t)
           (pick-super-type-with-acc ts t))]
        ;; there are no types in ts that are a super-type of t,
        ;; but t is not equal to or a super-type of all the types
        ;; in ts, we know that not all the types in the list are
        ;; related and thus we fail.
        [else #f]))
    (pick-super-type-with-acc ts (car ts)))
  
  (define (gen-top-except-for n k t)
    (cond
      [(= n 0) (list)]
      [(= k 1) (cons t (gen-top-except-for (- n 1) (- k 1) t))]
      [else    (cons (make-top-type #f) (gen-top-except-for (- n 1) (- k 1) t))]))

  )
