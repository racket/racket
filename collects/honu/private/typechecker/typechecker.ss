(module typechecker mzscheme
  
  (require (lib "contract.ss")
           (lib "plt-match.ss")
           (lib "struct.ss")
           (prefix srfi1: (lib "list.ss" "srfi" "1"))
           "../../ast.ss"
           "../../readerr.ss"
           "../../tenv.ss"
           "../../utils.ss"
           "typecheck-class-utils.ss"
           "typecheck-expression.ss"
           "typecheck-parameters.ss"
           "type-utils.ss")
  
  (provide/contract [typecheck ((listof honu:defn?)
                                . -> .
                                (listof honu:defn?))]
                    [typecheck-defn (honu:defn?
                                     . -> .
                                     honu:defn?)])
  (define (typecheck defns)
    (let loop ([defns   defns]
               [results '()])
      (cond
        [(null? defns) (reverse results)]
        ;; we allow functions to be mutually recursive in Algol-like fashion
        ;; (i.e. if they are no intervening non-function definitions)
        [(honu:function? (car defns))
         (let-values ([(funcs remaining) (srfi1:span honu:function? defns)])
           (loop remaining (append (typecheck-functions funcs) results)))]
        [else (loop (cdr defns) (cons (typecheck-defn (car defns)) results))])))
  
  (define (typecheck-functions funcs)
    (define (check-function-type func)
      (match func
        [(struct honu:function (stx name type args body))
         (if (not (type-valid? type))
             (raise-read-error-with-stx
              "Return type of function is undefined"
              (honu:ast-stx type)))
         (let ([conflicting-name (get-first-non-unique-name (map honu:formal-name args))])
           (if conflicting-name
               (raise-read-error-with-stx
                (format "Argument name ~a used more than once"
                        (printable-key conflicting-name))
                conflicting-name)))
         (for-each (lambda (t)
                     (if (not (type-valid? t))
                         (raise-read-error-with-stx
                          "Type of function argument is undefined"
                          (honu:ast-stx type))))
                   (map honu:formal-type args))
         (make-func-type stx (make-tuple-type stx (map honu:formal-type args)) type)]))
    ;; first we add the functions to the lexical environment so that when we typecheck
    ;; the bodies, they'll be in scope.
    (for-each (lambda (f)
                (extend-lenv (honu:function-name f)
                             (make-tenv:value (honu:ast-stx f) (check-function-type f))))
              funcs)
    (let loop ([funcs     funcs]
               [new-funcs '()])
      (if (null? funcs)
          ;; don't reverse it, because we want to keep these in the same order in typecheck,
          ;; which will eventually reverse everything
          new-funcs
          (match (car funcs)
            [(struct honu:function (stx name type args body))
             (let-values
                 ([(e1 t1)
                   (parameterize ([current-return-type type])
                     (typecheck-expression (srfi1:fold (lambda (a e)
                                                         (extend-fenv (honu:formal-name a)
                                                                      (honu:formal-type a)
                                                                      e))
                                                       (wrap-lenv)
                                                       args)
                                           type body))])
               (loop (cdr funcs)
                     (cons (copy-struct honu:function (car funcs)
                             [honu:function-body e1])
                           new-funcs)))]))))
  
  (define (typecheck-defn defn)
    (match defn
      [(struct honu:bind-top (stx names types value))
       (for-each (lambda (n t)
                   (if (and (not (and (not n)
                                      (honu:type-top? t)))
                            (not (type-valid? t)))
                       (raise-read-error-with-stx
                        "Type of top-level bound variable is undefined"
                        (honu:ast-stx t))))
                 names types)
       (let-values ([(e1 t1) (typecheck-expression (wrap-lenv) (make-tuple-type stx types) value)])
         (for-each (lambda (n t)
                     (if n (extend-lenv n (make-tenv:value stx t))))
                   names types)
         (copy-struct honu:bind-top defn
           [honu:bind-top-value e1]))]
      [(struct honu:iface (stx name supers members))
       (for-each (lambda (t)
                   (if (not (type-valid? t))
                       (raise-read-error-with-stx
                        "No definition for supertype"
                        (honu:ast-stx t))))
                 supers)
       (let ([conflicting-name (get-first-non-unique-name (map (lambda (d)
                                                                 (cond
                                                                   [(honu:field-decl? d)
                                                                    (honu:field-decl-name d)]
                                                                   [(honu:method-decl? d)
                                                                    (honu:method-decl-name d)]))
                                                               members))])
         (if conflicting-name
             (raise-read-error-with-stx
              (format "Field/method name ~a used more than once"
                      (printable-key conflicting-name))
              conflicting-name)))
       (for-each (lambda (m)
                   (typecheck-member-decl m))
                 members)
       defn]
      [(struct honu:class (stx name type final? impls inits members exports))
       (if (not (type-valid? type))
           (raise-read-error-with-stx
            "Self-type of class is undefined"
            (honu:ast-stx type)))
       (for-each (lambda (t)
                   (if (not (type-valid? t))
                       (raise-read-error-with-stx
                        "Implemented type is undefined"
                        (honu:ast-stx type))))
                 impls)
       (let ([conflicting-name (get-first-non-unique-name (append (map honu:formal-name inits)
                                                                  (map (lambda (d)
                                                                         (cond
                                                                           [(honu:init-field? d)
                                                                            (honu:init-field-name d)]
                                                                           [(honu:field? d)
                                                                            (honu:field-name d)]
                                                                           [(honu:method? d)
                                                                            (honu:method-name d)]))
                                                                       members)))])
         (if conflicting-name
             (raise-read-error-with-stx
              (format "Init/field/method name ~a used more than once"
                      (printable-key conflicting-name))
              conflicting-name)))
       (for-each (lambda (t)
                   (if (not (type-valid? t))
                       (raise-read-error-with-stx
                        "Type of init slot is undefined"
                        (honu:ast-stx type))))
                 (map honu:formal-type inits))
       (let ([cenv (srfi1:fold (lambda (a e)
                           (extend-fenv (honu:formal-name a)
                                        (honu:formal-type a)
                                        e))
                         (lambda (n) #f)
                         inits)])
         (let*-values ([(lenv)         (extend-fenv #'this type (wrap-lenv))]
                       [(members cenv) (typecheck-members cenv lenv type members)])
           (typecheck-exports cenv type impls exports)
           (copy-struct honu:class defn
             [honu:class-members members])))]
      [(struct honu:mixin (stx name type arg-type final? impls inits withs
                           supernew members-before members-after exports))
       (if (not (type-valid? arg-type))
           (raise-read-error-with-stx
            "Argument type of mixin is undefined"
            (honu:ast-stx arg-type)))
       (if (not (type-valid? type))
           (raise-read-error-with-stx
            "Result type of mixin is undefined"
            (honu:ast-stx type)))
       (for-each (lambda (t)
                   (if (not (type-valid? t))
                       (raise-read-error-with-stx
                        "Implemented type is undefined"
                        (honu:ast-stx type))))
                 impls)
       (let* ([arg-tentry (get-type-entry arg-type)]
              [conflicting-name (get-first-non-unique-name (append (map tenv:member-name
                                                                        (append (tenv:type-members arg-tentry)
                                                                                (tenv:type-inherited arg-tentry)))
                                                                   (map honu:formal-name inits)
                                                                   (map (lambda (d)
                                                                          (cond
                                                                            [(honu:init-field? d)
                                                                             (honu:init-field-name d)]
                                                                            [(honu:field? d)
                                                                             (honu:field-name d)]
                                                                            [(honu:method? d)
                                                                             (honu:method-name d)]))
                                                                        (append members-before
                                                                                members-after))))])
         (if conflicting-name
             (raise-read-error-with-stx
              (format "Init/field/method name ~a used more than once in mixin or conflicts with members of argument type"
                      (printable-key conflicting-name))
              (honu:ast-stx defn))))
       (for-each (lambda (t)
                   (if (not (type-valid? t))
                       (raise-read-error-with-stx
                        "Type of init slot is undefined"
                        (honu:ast-stx type))))
                 (map honu:formal-type inits))
       (let ([conflicting-name (get-first-non-unique-name (map honu:formal-name withs))])
         (if conflicting-name
             (raise-read-error-with-stx
              (format "Init name ~a used more than once in expected init slots"
                      (printable-key conflicting-name))
              conflicting-name)))
       (for-each (lambda (t)
                   (if (not (type-valid? t))
                       (raise-read-error-with-stx
                        "Type of expected init slot is undefined"
                        (honu:ast-stx type))))
                 (map honu:formal-type withs))
       (let ([cenv (srfi1:fold (lambda (a e)
                           (extend-fenv (honu:formal-name a)
                                        (honu:formal-type a)
                                        e))
                         (lambda (n) #f)
                         inits)])
         (let*-values ([(lenv)                (extend-fenv #'this type (wrap-lenv))]
                       [(members-before cenv) (typecheck-members cenv lenv type  members-before)]
                       [(supernew)            (typecheck-supernew cenv lenv withs supernew)]
                       [(cenv)                (extend-cenv-with-type-members cenv arg-type)]
                       [(members-after  cenv) (typecheck-members cenv lenv type  members-after)])
           (typecheck-exports cenv type impls exports)
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

  (define (typecheck-member-decl member)
    (match member
      [(struct honu:field-decl (stx name type))
       (if (not (type-valid? type))
           (raise-read-error-with-stx
            "Type of field is undefined"
            stx))]
      [(struct honu:method-decl (stx name type args))
       (if (not (type-valid? type))
           (raise-read-error-with-stx
            "Return type of method is undefined"
            (honu:ast-stx type)))
       (for-each (lambda (t)
                   (if (not (type-valid? t))
                       (raise-read-error-with-stx
                        "Type of method argument is undefined"
                        (honu:ast-stx type))))
                 args)]))

  )
