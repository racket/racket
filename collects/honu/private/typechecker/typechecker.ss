(module typechecker mzscheme
  
  (require (lib "contract.ss")
           (lib "plt-match.ss")
           (lib "struct.ss")
           (all-except (lib "list.ss" "srfi" "1") any)
           "../../ast.ss"
           "../../readerr.ss"
           "../../tenv.ss"
           "../../utils.ss"
           "typecheck-class-utils.ss"
           "typecheck-expression.ss"
           "type-utils.ss")
  
  (provide/contract [typecheck (tenv?
                                tenv?
                                (listof honu:defn?)
                                . -> .
                                (listof honu:defn?))]
                    [typecheck-defn (tenv?
                                     tenv?
                                     honu:defn?
                                     . -> .
                                     honu:defn?)])
  ;; since lenv is a hashtable and thus will be mutated, we don't need to return it from
  ;; typecheck or typecheck-defn.
  (define (typecheck tenv lenv defns)
    (map (lambda (d)
           (typecheck-defn tenv lenv d))
         defns))

  (define (typecheck-defn tenv lenv defn)
    (match defn
      [(struct honu:function (stx name type args body))
       (if (not (type-valid? tenv type))
           (raise-read-error-with-stx
            "Return type of function is undefined"
            (honu:ast-stx type)))
       (for-each (lambda (t)
                   (if (not (type-valid? tenv t))
                       (raise-read-error-with-stx
                        "Type of function argument is undefined"
                        (honu:ast-stx type))))
                 (map honu:formal-type args))
       (let ([func-type (make-func-type stx (make-tuple-type stx (map honu:formal-type args)) type)])
         (extend-tenv name (make-tenv:value stx func-type) lenv)
         (let-values ([(e1 t1) (typecheck-expression tenv (lambda (name) #f)
                                                     (fold (lambda (a e)
                                                             (extend-fenv (honu:formal-name a)
                                                                          (honu:formal-type a)
                                                                          e))
                                                           (wrap-as-function lenv)
                                                           args)
                                                     type type body)])
           (copy-struct honu:function defn
             [honu:function-body e1])))]
      [(struct honu:bind-top (stx names types value))
       (for-each (lambda (n t)
                   (if (and (not (and (not n)
                                      (honu:type-top? t)))
                            (not (type-valid? tenv t)))
                       (raise-read-error-with-stx
                        "Type of top-level bound variable is undefined"
                        (honu:ast-stx t))))
                 names types)
       (let-values ([(e1 t1) (typecheck-expression tenv (lambda (name) #f) (wrap-as-function lenv) 
                                                   (make-tuple-type stx types) #f value)])
         (for-each (lambda (n t)
                     (if n (extend-tenv n (make-tenv:value stx t) lenv)))
                   names types)
         (copy-struct honu:bind-top defn
           [honu:bind-top-value e1]))]
      [(struct honu:iface (stx name supers members))
       (for-each (lambda (t)
                   (if (not (type-valid? tenv t))
                       (raise-read-error-with-stx
                        "No definition for supertype"
                        (honu:ast-stx t))))
                 supers)
       (for-each (lambda (m)
                   (typecheck-member-decl tenv m))
                 members)
       defn]
      [(struct honu:class (stx name type final? impls inits members exports))
       (if (not (type-valid? tenv type))
           (raise-read-error-with-stx
            "Self-type of class is undefined"
            (honu:ast-stx type)))
       (for-each (lambda (t)
                   (if (not (type-valid? tenv t))
                       (raise-read-error-with-stx
                        "Implemented type is undefined"
                        (honu:ast-stx type))))
                 impls)
       (for-each (lambda (t)
                   (if (not (type-valid? tenv t))
                       (raise-read-error-with-stx
                        "Type of init slot is undefined"
                        (honu:ast-stx type))))
                 (map honu:formal-type inits))
       (let ([cenv (fold (lambda (a e)
                           (extend-fenv (honu:formal-name a)
                                        (honu:formal-type a)
                                        e))
                         (lambda (n) #f)
                         inits)])
         (let-values ([(members cenv) (typecheck-members tenv cenv (wrap-as-function lenv) type members)])
           (typecheck-exports tenv cenv type impls exports)
           (copy-struct honu:class defn
             [honu:class-members members])))]
      [(struct honu:mixin (stx name type arg-type final? impls inits withs
                           supernew members-before members-after exports))
       (if (not (type-valid? tenv arg-type))
           (raise-read-error-with-stx
            "Argument type of mixin is undefined"
            (honu:ast-stx arg-type)))
       (if (not (type-valid? tenv type))
           (raise-read-error-with-stx
            "Result type of mixin is undefined"
            (honu:ast-stx type)))
       (for-each (lambda (t)
                   (if (not (type-valid? tenv t))
                       (raise-read-error-with-stx
                        "Implemented type is undefined"
                        (honu:ast-stx type))))
                 impls)
       (for-each (lambda (t)
                   (if (not (type-valid? tenv t))
                       (raise-read-error-with-stx
                        "Type of init slot is undefined"
                        (honu:ast-stx type))))
                 (map honu:formal-type inits))       
       (for-each (lambda (t)
                   (if (not (type-valid? tenv t))
                       (raise-read-error-with-stx
                        "Type of expected init slot is undefined"
                        (honu:ast-stx type))))
                 (map honu:formal-type withs))
       (let ([cenv (fold (lambda (a e)
                           (extend-fenv (honu:formal-name a)
                                        (honu:formal-type a)
                                        e))
                         (lambda (n) #f)
                         inits)])
         (let*-values ([(members-before cenv) (typecheck-members  tenv cenv (wrap-as-function lenv) type  members-before)]
                       [(supernew)            (typecheck-supernew tenv cenv (wrap-as-function lenv) withs supernew)]
                       [(cenv)                (extend-cenv-with-type-members tenv cenv arg-type)]
                       [(members-after  cenv) (typecheck-members  tenv cenv (wrap-as-function lenv) type  members-after)])
           (typecheck-exports tenv cenv type impls exports)
           (copy-struct honu:mixin defn
             [honu:mixin-members-before members-before]
             [honu:mixin-super-new      supernew]
             [honu:mixin-members-after  members-after])))]
      ;; we basically do all the checks when we create the tenv entry for the subclass (plus typechecking the base
      ;; class and mixin), so no need to check again.
      [(struct honu:subclass (_ _ _ _))
       defn]
      [else (raise-read-error-with-stx
             "Haven't typechecked that type of definition yet."
             (honu:ast-stx defn))]))

  (define (typecheck-member-decl tenv member)
    (match member
      [(struct honu:field-decl (stx name type))
       (if (not (type-valid? tenv type))
           (raise-read-error-with-stx
            "Type of field is undefined"
            stx))]
      [(struct honu:method-decl (stx name type args))
       (if (not (type-valid? tenv type))
           (raise-read-error-with-stx
            "Return type of method is undefined"
            (honu:ast-stx type)))
       (for-each (lambda (t)
                   (if (not (type-valid? tenv t))
                       (raise-read-error-with-stx
                        "Type of method argument is undefined"
                        (honu:ast-stx type))))
                 args)]))

  )
