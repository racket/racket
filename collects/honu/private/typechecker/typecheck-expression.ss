(module typecheck-expression mzscheme
  
  (require (all-except (lib "list.ss" "srfi" "1") any)
           (lib "contract.ss")
           (lib "plt-match.ss")
           (lib "struct.ss")
           "../../ast.ss"
           "../../readerr.ss"
           "../../tenv.ss"
           "../../utils.ss"
           "typecheck-parameters.ss"
           "type-utils.ss")
  
  (provide/contract [typecheck-expression ((((syntax/c symbol?) . -> . (union honu:type? false/c))
                                            (union honu:type? false/c)
                                            honu:expr?)
                                           . ->* .
                                           (honu:expr? 
                                            honu:type?))])
  
  ;; lenv  : ((syntax/c symbol?) . -> . (union honu:type false/c))
  ;;   lexical environment (includes top-level bindings and
  ;;   binding for #'this if inside class or mixin)
  ;; ctype : (union honu:type? false/c)
  ;;   type of context for expression
  ;; expr  : honu:expr?
  ;;   expression to typecheck
  
  (define (typecheck-expression lenv ctype expr)
    (match expr
      [(struct honu:this (stx))
       (cond
         [(lenv #'this) => (lambda (t)
                             (if (<:_P t ctype)
                                 (values expr t)
                                 (raise-honu-type-error stx ctype t)))]
         [else (raise-read-error-with-stx
                "Found use of 'this' outside of class or mixin"
                stx)])]
      [(struct honu:select (stx slot arg))
       ;; changed to have a special "select" type that does the following:
       ;; (type-select n t) <:_P (type-tuple args) if
       ;;   a) (>= (length args) n)
       ;;   b) (type-equal? (list-ref args (- n 1)) t)
       ;;   c) (list-ref args m) <:_P (type-top) for all m =/= n (vacuously true)
       (let-values ([(e1 t1) (typecheck-expression lenv (make-honu:type-select stx slot ctype) arg)])
         (if (not (honu:type-tuple? t1))
             (raise-read-error-with-stx "Tried to use select with non-tuple expression" stx))
         (let ([etype (list-ref (honu:type-tuple-args t1) (- slot 1))])
           (values (copy-struct honu:select expr
                     [honu:select-arg e1])
                   etype)))]
      [(struct honu:var (stx name))
       (cond
         [(lenv name) => (lambda (t)
                           (if (<:_P t ctype)
                               (values expr t)
                               (raise-honu-type-error stx ctype t)))]
         [else (raise-read-error-with-stx
                (format "Unbound variable ~a" (syntax-e name))
                stx)])]
      [(struct honu:assn (stx lhs rhs))
       ;; the context type for the lhs is a vacuous one.
       (let*-values ([(e1 t1) (typecheck-expression lenv (make-top-type (honu:ast-stx lhs)) lhs)]
                     [(e2 t2) (typecheck-expression lenv t1 rhs)])
         (let ([void-type (make-void-type stx)])
           (if (<:_P void-type ctype)
               (values (copy-struct honu:assn expr
                         [honu:assn-lhs e1]
                         [honu:assn-rhs e2])
                       void-type)
               (raise-honu-type-error stx ctype void-type))))]
      [(struct honu:call (stx func arg))
       (let*-values ([(e1 t1) (typecheck-expression lenv (make-func-type (honu:ast-stx func)
                                                                                     (make-bottom-type (honu:ast-stx func))
                                                                                     ctype) func)]
                     [(e2 t2) (typecheck-expression lenv (honu:type-func-arg t1) arg)])
         (let ([ret-type (honu:type-func-ret t1)])
           (if (<:_P ret-type ctype)
               (values (copy-struct honu:call expr
                         [honu:call-func e1]
                         [honu:call-arg  e2])
                       ret-type)
               (raise-honu-type-error stx ctype ret-type))))]
      [(struct honu:lit (stx type value))
       (if (<:_P type ctype)
           (values expr type)
           (raise-honu-type-error stx ctype type))]
      [(struct honu:un-op (stx op op-stx _ arg))
       (case op
         [(not)
          (let-values ([(e1 t1) (typecheck-expression lenv (make-bool-type (honu:ast-stx arg)) arg)])
            (let ([ret-type (make-bool-type stx)])
              (if (<:_P ret-type ctype)
                  (values (copy-struct honu:un-op expr
                            [honu:un-op-op-type t1]
                            [honu:un-op-arg     e1])
                          ret-type)
                  (raise-honu-type-error stx ctype ret-type))))]
         [(minus)
          (let-values ([(e1 t1) (typecheck-expression lenv (make-top-type (honu:ast-stx arg)) arg)])
            (if (not (honu:type-prim? t1))
                (raise-read-error-with-stx
                 "Invalid type for argument to unary minus"
                 (honu:ast-stx arg))
                (let ([ret-type (case (honu:type-prim-name t1)
                                  [(int)   (make-int-type   (honu:ast-stx arg))]
                                  [(float) (make-float-type (honu:ast-stx arg))]
                                  [else    (raise-read-error-with-stx
                                            (format "Argument to unary minus must be int or float type, got ~a"
                                                    (printable-type t1))
                                            (honu:ast-stx arg))])])
                  (if (<:_P ret-type ctype)
                      (values (copy-struct honu:un-op expr
                                [honu:un-op-op-type t1]
                                [honu:un-op-arg     e1])
                              ret-type)
                      (raise-honu-type-error stx ctype ret-type)))))]
         [else
          (raise-read-error-with-stx
           "Unknown operator"
           op-stx)])]
      [(struct honu:bin-op (stx op op-stx _ larg rarg))
       (case op
         ;; binary boolean operators
         [(or and)
          (let-values ([(e1 t1) (typecheck-expression lenv (make-bool-type (honu:ast-stx larg)) larg)]
                       [(e2 t2) (typecheck-expression lenv (make-bool-type (honu:ast-stx rarg)) rarg)])
            (let ([ret-type (make-bool-type stx)])
              (if (<:_P ret-type ctype)
                  (values (copy-struct honu:bin-op expr
                            [honu:bin-op-op-type (make-bool-type (honu:ast-stx larg))]
                            [honu:bin-op-larg    e1]
                            [honu:bin-op-rarg    e2])
                          ret-type)
                  (raise-honu-type-error stx ctype ret-type))))]
         [(clseq)
          (let-values ([(e1 t1) (typecheck-expression lenv (make-any-type (honu:ast-stx larg)) larg)]
                       [(e2 t2) (typecheck-expression lenv (make-any-type (honu:ast-stx rarg)) rarg)])
            (let ([ret-type (make-bool-type stx)])
              (if (<:_P ret-type ctype)
                  (values (copy-struct honu:bin-op expr
                            [honu:bin-op-op-type (make-any-type (honu:ast-stx larg))]
                            [honu:bin-op-larg    e1]
                            [honu:bin-op-rarg    e2])
                          ret-type)
                  (raise-honu-type-error stx ctype ret-type))))]
         [(equal neq)
          (let-values ([(e1 t1) (typecheck-expression lenv (make-top-type (honu:ast-stx larg)) larg)]
                       [(e2 t2) (typecheck-expression lenv (make-top-type (honu:ast-stx rarg)) rarg)])
            (let ([ret-type (make-bool-type stx)]
                  [arg-type (cond
                              [(and (<:_P t1 (make-any-type (honu:ast-stx larg)))
                                    (<:_P t2 (make-any-type (honu:ast-stx rarg))))
                               (make-any-type (honu:ast-stx larg))]
                              [(check-prim-types-for-binop stx t1 t2) => (lambda (t) t)])])
              (if (<:_P ret-type ctype)
                  (values (copy-struct honu:bin-op expr
                            [honu:bin-op-op-type arg-type]
                            [honu:bin-op-larg    e1]
                            [honu:bin-op-rarg    e2])
                          ret-type)
                  (raise-honu-type-error stx ctype ret-type))))]
         [(le lt ge gt)
          (let-values ([(e1 t1) (typecheck-expression lenv (make-top-type (honu:ast-stx larg)) larg)]
                       [(e2 t2) (typecheck-expression lenv (make-top-type (honu:ast-stx rarg)) rarg)])
            (let ([ret-type (make-bool-type stx)]
                  [arg-type (check-prim-types-for-binop stx t1 t2)])
              (if (<:_P ret-type ctype)
                  (values (copy-struct honu:bin-op expr
                            [honu:bin-op-op-type arg-type]
                            [honu:bin-op-larg    e1]
                            [honu:bin-op-rarg    e2])
                          ret-type)
                  (raise-honu-type-error stx ctype ret-type))))]
         [(plus)
          (let-values ([(e1 t1) (typecheck-expression lenv (make-top-type (honu:ast-stx larg)) larg)]
                       [(e2 t2) (typecheck-expression lenv (make-top-type (honu:ast-stx rarg)) rarg)])
            (let ([arg-type (check-prim-types-for-binop stx t1 t2)])
              (case (honu:type-prim-name arg-type)
                [(int float string)
                 (if (<:_P arg-type ctype)
                     (values (copy-struct honu:bin-op expr
                               [honu:bin-op-op-type arg-type]
                               [honu:bin-op-larg    e1]
                               [honu:bin-op-rarg    e2])
                             arg-type)
                     (raise-honu-type-error stx ctype arg-type))]
                [else (raise-read-error-with-stx
                       "The plus operator requires both arguments to be of type int, type float, or type string"
                       stx)])))]
         [(minus times div)
          (let-values ([(e1 t1) (typecheck-expression lenv (make-top-type (honu:ast-stx larg)) larg)]
                       [(e2 t2) (typecheck-expression lenv (make-top-type (honu:ast-stx rarg)) rarg)])
            (let ([arg-type (check-prim-types-for-binop stx t1 t2)])
              (case (honu:type-prim-name arg-type)
                [(int float)
                 (if (<:_P arg-type ctype)
                     (values (copy-struct honu:bin-op expr
                               [honu:bin-op-op-type arg-type]
                               [honu:bin-op-larg    e1]
                               [honu:bin-op-rarg    e2])
                             arg-type)
                     (raise-honu-type-error stx ctype arg-type))]
                [else (raise-read-error-with-stx
                       "Arithmetic operator requires both arguments to be of type int or type float"
                       stx)])))]
         [(mod)
          (let-values ([(e1 t1) (typecheck-expression lenv (make-int-type (honu:ast-stx larg)) larg)]
                       [(e2 t2) (typecheck-expression lenv (make-int-type (honu:ast-stx rarg)) rarg)])
            (let ([ret-type (make-int-type stx)])
              (if (<:_P ret-type ctype)
                  (values (copy-struct honu:bin-op expr
                            [honu:bin-op-op-type (make-int-type (honu:ast-stx larg))]
                            [honu:bin-op-larg    e1]
                            [honu:bin-op-rarg    e2])
                          ret-type)
                  (raise-honu-type-error stx ctype ret-type))))]
         [else 
          (raise-read-error-with-stx
           "Unknown operator"
           op-stx)])]
      [(struct honu:lambda (stx ret-type args body))
       (if (not (type-valid? ret-type))
           (raise-read-error-with-stx
            "Return type of anonymous function is invalid"
            (honu:ast-stx ret-type)))
       (let ([conflicting-name (get-first-non-unique-name (map honu:formal-name args))])
         (if conflicting-name
             (raise-read-error-with-stx
              (format "Variable name ~a used more than once in function arguments"
                      (printable-key conflicting-name))
              conflicting-name)))
       (for-each (lambda (t)
                   (if (not (type-valid? t))
                       (raise-read-error-with-stx
                        "Type of argument of anonymous function is invalid"
                        (honu:ast-stx t))))
                 (map honu:formal-type args))
       ;; since we have explicit return type annotations now, we use them for the body's ctype and rtype.
       (let ([body-lenv  (fold (lambda (f e)
                                 (extend-fenv (honu:formal-name f)
                                              (honu:formal-type f)
                                              e))
                               lenv args)])
         (let-values ([(body _) (parameterize ([current-return-type ret-type])
                                  (typecheck-expression body-lenv ret-type body))])
           ;; we also have the lambda's return type be what was explicitly annotated instead of what we got back
           (let ([lam-type (make-func-type stx (make-tuple-type stx (map honu:formal-type args)) ret-type)])
             (if (<:_P lam-type ctype)
                 (values (copy-struct honu:lambda expr
                           [honu:lambda-body body])
                         lam-type)
                 (raise-honu-type-error stx ctype lam-type)))))]
      [(struct honu:if (stx test then else))
       (if else
           (let-values ([(e1 t1) (typecheck-expression lenv (make-bool-type (honu:ast-stx test)) test)]
                        [(e2 t2) (typecheck-expression lenv ctype then)]
                        [(e3 t3) (typecheck-expression lenv ctype else)])
             ;; this should work, but I get the following:
             ;; -- context expected 1 value, received 2 values: #<struct:honu:if> #<struct:honu:type-prim>
             ;; with the arrow going from values -> cond, so I'm going to rewrite as a nested-if for now.
             (cond
               ;; if we had a context type and got to this point, then both t2 and t3 must be related to it (either equal
               ;; or subtypes), so use it as the type of the entire if expression.
               ;;
               ;; No, we can't do this.  Think about the fact where we use an if expression on the left-hand side of a
               ;; member access -- we need to get the most specific type back as the result, not the (vacuous) context
               ;; type.  If the branches are of unrelated types, then having a context type doesn't help us all the time.
               ;; [ctype
               ;;   (values (copy-struct honu:if expr
               ;;             [honu:if-cond e1]
               ;;             [honu:if-then e2]
               ;;             [honu:if-else e3])
               ;;           ctype)]
               ;; if there was no ctype, then we require either t2 <: t3 or t3 <: t2, and we'll pick the supertype.
               [(<:_P t2 t3)
                (values (copy-struct honu:if expr
                          [honu:if-cond e1]
                          [honu:if-then e2]
                          [honu:if-else e3])
                        t3)]
               [(<:_P t3 t2)
                (values (copy-struct honu:if expr
                          [honu:if-cond e1]
                          [honu:if-then e2]
                          [honu:if-else e3])
                        t2)]
               ;; if we got to here, then there's no good type to use as the type of the entire if expression, so
               ;; raise an error.
               [else (raise-read-error-with-stx
                      "Branches of if statement are of unrelated types"
                      stx)]))
           ;; if else is #f, there was no else branch, so the then branch must be of void type.
           (let-values ([(e1 t1) (typecheck-expression lenv (make-bool-type (honu:ast-stx test)) test)]
                        [(e2 t2) (typecheck-expression lenv (make-void-type (honu:ast-stx then)) then)])
             (let ([ret-type (make-void-type stx)])
               (if (<:_P ret-type ctype)
                   (values (copy-struct honu:if expr
                             [honu:if-cond e1]
                             [honu:if-then e2])
                           ret-type)
                   (raise-read-error-with-stx
                    "Found if expression without else branch in non-void context"
                    stx)))))]
      [(struct honu:cast (stx obj type))
       (if (not (type-valid? type))
           (raise-read-error-with-stx
            "Type argument of cast is not a valid type"
            (honu:ast-stx type)))
       (let-values ([(e1 t1) (typecheck-expression lenv (make-any-type (honu:ast-stx obj)) obj)])
         (if (<:_P type ctype)
             (values (copy-struct honu:cast expr
                       [honu:cast-obj e1])
                     type)
             (raise-honu-type-error stx ctype type)))]
      [(struct honu:isa (stx obj type))
       (if (not (type-valid? type))
           (raise-read-error-with-stx
            "Type argument of isa is not a valid type"
            (honu:ast-stx type)))
       (let-values ([(e1 t1) (typecheck-expression lenv (make-any-type (honu:ast-stx obj)) obj)])
         (let ([ret-type (make-bool-type stx)])
           (if (<:_P ret-type ctype)
               (values (copy-struct honu:isa expr
                         [honu:isa-obj e1])
                       ret-type)
               (raise-honu-type-error stx ctype ret-type))))]
      [(struct honu:member (stx 'my _ name _))
       (cond
         [((current-class-environment) name)
          =>
          (lambda (t)
            (if (honu:type-disp? t)
                (let ([fun-type (make-func-type (honu:ast-stx t) (honu:type-disp-arg t) (honu:type-disp-ret t))])
                  (if (<:_P fun-type ctype)
                      (values (copy-struct honu:member expr
                                           [honu:member-method? #t])
                              fun-type)
                      (raise-honu-type-error stx ctype fun-type)))
                (if (<:_P t ctype)
                    (values expr t)
                    (raise-honu-type-error stx ctype t))))]
         [else (raise-read-error-with-stx
                (format "Static member ~a not found" (syntax-e name))
                stx)])]
      [(struct honu:member (stx obj _ name _))
       (let-values ([(e1 t1) (typecheck-expression lenv (make-any-type (honu:ast-stx obj)) obj)])
         ;; if obj was something like error or return, which do not give us a valid type for
         ;; getting the appropriate member...
         (if (honu:type-bot? t1)
             (raise-read-error-with-stx
              "Attempt to access member of an expression which does not return"
              stx))
         ;; if obj was null...
         (if (honu:type-iface-bot? t1)
             (raise-read-error-with-stx
              "Null has no fields or methods"
              stx))
         (let ([t (get-member-type t1 name)])
           (cond
             [(honu:type-disp? t)
              (let ([fun-type (make-func-type (honu:ast-stx t) (honu:type-disp-arg t) (honu:type-disp-ret t))])
                (if (<:_P fun-type ctype)
                    (values (copy-struct honu:member expr
                              [honu:member-obj     e1]
                              [honu:member-elab    t1]
                              [honu:member-method? #t])
                            fun-type)
                    (raise-honu-type-error stx ctype fun-type)))]
             [t
              (if (<:_P t ctype)
                  (values (copy-struct honu:member expr
                            [honu:member-obj  e1]
                            [honu:member-elab t1])
                          t)
                  (raise-honu-type-error stx ctype t))]
             [else (raise-read-error-with-stx
                    (format "Member ~a not found in type ~a" (syntax-e name) (printable-type t1))
                    stx)])))]
      [(struct honu:new (stx class type args))
       (let ([class-entry (get-class-entry class)]
             [new-type    (if type type ctype)])
         ;; the following can only be triggered if the type annontation isn't a type
         (if (and type (not (type-valid? type)))
             (raise-read-error-with-stx
              (format "Type annotation ~a on new statement is not a valid type" (printable-type new-type))
              (honu:ast-stx new-type)))
         ;; the following two checks can only be triggered if there is no type annotation
         (if (honu:type-top? new-type)
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
               (values (copy-struct honu:new expr
                         [honu:new-type new-type]
                         [honu:new-args args])
                       new-type)
               (raise-honu-type-error stx ctype new-type))))]
      [(struct honu:while (stx cond body))
       (let-values ([(e1 t1) (typecheck-expression lenv (make-bool-type (honu:ast-stx cond)) cond)]
                    [(e2 t2) (typecheck-expression lenv (make-void-type (honu:ast-stx body)) body)])
         (let ([ret-type (make-void-type stx)])
           (if (<:_P ret-type ctype)
               (values (copy-struct honu:while expr
                         [honu:while-cond e1]
                         [honu:while-body e2])
                       ret-type)
               (raise-honu-type-error stx ctype ret-type))))]
      [(struct honu:cond (stx clauses else))
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
               ;;   (values (copy-struct honu:cond expr
               ;;             [honu:cond-clauses clauses])
               ;;           ctype)]
               ;; otherwise find the most super type of all the branches
               [(pick-super-type-from-list (cons etype types))
                =>
                (lambda (t)
                  (values (copy-struct honu:cond expr
                            [honu:cond-clauses clauses]
                            [honu:cond-else    else])
                          t))]
               ;; otherwise we're hosed for determining a type -- throw an error
               [else
                (raise-read-error-with-stx
                 "At least two branches of the cond statement have unrelated types"
                 stx)]))
           ;; if else is #f, there was no else branch, so the cond clauses must be of void type.
           (let-values ([(clauses types) (map-two-values (lambda (c)
                                                           (typecheck-cond-clause lenv
                                                                                  (make-void-type (honu:ast-stx c)) c))
                                                         clauses)])
             (let ([ret-type (make-void-type stx)])
               (if (<:_P ret-type ctype)
                   (values (copy-struct honu:cond expr
                             [honu:cond-clauses clauses])
                           ret-type)
                   (raise-read-error-with-stx
                    "Cond expression without an else branch found in non-void context"
                    stx)))))]
      [(struct honu:return (stx body))
       ;; returns don't return to their context, but to the context of the method or function call in which
       ;; they were invoked.  Because of this a) rtype must not be #f (else we're not in a method or function
       ;; body) and b) the type of a return statement is the bottom type (same as error).
       (if (current-return-type)
           ;; we use rtype as the context type here, since that's the type that needs to be returned.
           (let-values ([(e1 _) (typecheck-expression lenv (current-return-type) body)])
             ;; we don't need to check (bottom-type) <:_P ctype, because that's vacuously true.
             (values (copy-struct honu:return expr
                       [honu:return-body e1])
                     (make-bottom-type stx)))
           (raise-read-error-with-stx
            "Return statement found outside body of method or function"
            stx))]
      [(struct honu:tuple (stx vals))
       (cond
         [(honu:type-tuple? ctype)
          ;; we have a tuple context type, so use its contents, but make
          ;; sure it's the right length.
          (if (not (= (length vals) (length (honu:type-tuple-args ctype))))
              (raise-read-error-with-stx
               (format "Expected tuple of length ~a, got tuple of length ~a"
                       (length vals)
                       (length (honu:type-tuple-args ctype)))
               stx))
          (let-values ([(vals types) (map-two-values (lambda (e t)
                                                       (typecheck-expression lenv t e))
                                                     vals (honu:type-tuple-args ctype))])
            (values (copy-struct honu:tuple expr
                      [honu:tuple-vals vals])
                    (make-tuple-type stx types)))]
         ;; we must be in hte context of a select expression, so
         [(honu:type-select? ctype)
          (if (not (<= (honu:type-select-slot ctype) (length vals)))
              (raise-read-error-with-stx
               (format "Expected tuple of length at least ~a, got tuple of length ~a"
                       (honu:type-select-slot ctype)
                       (length vals))
               stx))
          (let-values ([(vals types) (map-two-values (lambda (e t)
                                                       (typecheck-expression lenv t e))
                                                     vals (gen-top-except-for (length vals)
                                                                              (honu:type-select-slot ctype)
                                                                              (honu:type-select-type ctype)))])
            (values (copy-struct honu:tuple expr
                      [honu:tuple-vals vals])
                    (make-tuple-type stx types)))]
         ;; if we have the top type here, then we either a) don't care about the type or
         ;; b) are going to check it after we return, so just do the simple thing -- since
         ;; we have no knowledge about what's wanted, we just check each component with ctype.
         [(honu:type-top? ctype)
          (let-values ([(vals types) (map-two-values (lambda (e)
                                                       (typecheck-expression lenv
                                                                             (make-top-type (honu:ast-stx e)) e))
                                                     vals)])
            (values (copy-struct honu:tuple expr
                      [honu:tuple-vals vals])
                    (make-tuple-type stx types)))]
         [else (raise-read-error-with-stx
                "Expected non-tuple expression (or tuple of length 1) here"
                stx)])]
      [(struct honu:let (_ bindings body))
       (let*-values ([(bindings lenv) (map-and-fold (lambda (bind lenv)
                                                      (typecheck-binding lenv bind))
                                                    lenv
                                                    bindings)]
                     [(e1 t1) (typecheck-expression lenv ctype body)])
         (values (copy-struct honu:let expr
                   [honu:let-bindings bindings]
                   [honu:let-body     e1])
                 t1))]
      [(struct honu:seq (_ effects value))
       (let-values ([(effects _)
                     (map-two-values
                      (lambda (e)
                        (typecheck-expression lenv (make-void-type (honu:ast-stx e)) e))
                      effects)]
                     [(e1 t1) (typecheck-expression lenv ctype value)])
         (values (copy-struct honu:seq expr
                   [honu:seq-effects effects]
                   [honu:seq-value   e1])
                 t1))]))
  
  ;; bindings have no ctype because they're always in the void type context
  ;; they return the elaborated binding and a new environment extended with the
  ;; type of the bound variable.
  (define (typecheck-binding lenv binding)
    (match binding
      [(struct honu:binding (stx names types value))
       ;; make sure to remove all the #f for don't care arguments.
       (let ([conflicting-name (get-first-non-unique-name (filter (lambda (n) n) names))])
         (if conflicting-name
             (raise-read-error-with-stx
              (format "Variable name ~a used more than once in binding form"
                      (printable-key conflicting-name))
              conflicting-name)))
       (for-each (lambda (n t)
                   (if (and (not (and (not n)
                                      (honu:type-top? t)))
                            (not (type-valid? t)))
                       (raise-read-error-with-stx
                        "Type of locally bound variable is undefined"
                        (honu:ast-stx t))))
                 names types)
       (let-values ([(e1 t1) (typecheck-expression lenv (make-tuple-type (honu:ast-stx value) types) value)])
         (values (copy-struct honu:binding binding
                   [honu:binding-value e1])
                 (fold (lambda (name type lenv)
                         (if name
                             (extend-fenv name type lenv)
                             lenv))
                       lenv names types)))]))
  
  (define (typecheck-cond-clause lenv ctype clause)
    (match clause
      [(struct honu:cond-clause (stx pred rhs))
       (let-values ([(e1 t1) (typecheck-expression lenv (make-bool-type (honu:ast-stx pred)) pred)]
                    [(e2 t2) (typecheck-expression lenv ctype rhs)])
         (values (copy-struct honu:cond-clause clause
                   [honu:cond-clause-pred e1]
                   [honu:cond-clause-rhs  e2])
                 t2))]))
            
  (define (check-prim-types-for-binop stx t1 t2)
    (cond
      [(and (honu:type-bot? t1)
            (honu:type-prim?   t2))
       t2]
      [(and (honu:type-prim?   t1)
            (honu:type-bot? t2))
       t1]
      [(and (honu:type-prim? t1)
            (honu:type-prim? t2)
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
                                                 (find-init inits (honu:name-arg-name arg))]
                                                [(e1 t1)
                                                 (type-fun (honu:name-arg-value arg) (tenv:init-type init))])
                                    (values (copy-struct honu:name-arg arg
                                              [honu:name-arg-value e1])
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
