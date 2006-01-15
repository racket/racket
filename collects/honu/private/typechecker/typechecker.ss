(module typechecker mzscheme
  
  (require (lib "contract.ss")
           (lib "plt-match.ss")
           (lib "struct.ss")
           (prefix srfi1: (lib "list.ss" "srfi" "1"))
           "../../ast.ss"
           "../../readerr.ss"
           "../../tenv.ss"
           "../../utils.ss"
           "typecheck-utils.ss"
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
         (check-valid-type! "function return type" type)
         (let ([conflicting-name (get-first-non-unique-name (map honu:formal-name args))])
           (if conflicting-name
               (raise-read-error-with-stx
                (format "Argument name ~a used more than once"
                        (printable-key conflicting-name))
                conflicting-name)))
         (for-each (curry check-valid-type! "function argument type")
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

  ;; typecheck-bind-top : BindTop -> BindTop
  ;; Typechecks a top-level binding and produces the annotated version.
  (define (typecheck-bind-top bind-top)
    (match bind-top
      [(struct honu:bind-top (stx names types value))
       (for-each (lambda (n t)
                   (if (not (and (not n)
                                 (honu:type-top? t)))
                       (check-valid-type! "top-level bound variable type" t)))
                 names types)
       (let-values ([(e1 t1) (typecheck-expression (wrap-lenv) (make-tuple-type stx types) value)])
         (for-each (lambda (n t)
                     (if n (extend-lenv n (make-tenv:value stx t))))
                   names types)
         (copy-struct honu:bind-top bind-top
           [honu:bind-top-value e1]))]))

  ;; typecheck-iface : IFace -> IFace
  ;; Typechecks an interface definition and produces the annotated version.
  (define (typecheck-iface iface)
    (match iface
      [(struct honu:iface (stx name supers members))
       (for-each (curry check-valid-type! "interface supertype")
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
       iface]))

  ;; typecheck-class : Class -> Class
  ;; Typechecks a class definition and produces the annotated version.
  (define (typecheck-class class)
    (match class
      [(struct honu:class (stx name type final? impls inits members exports))
       (check-valid-type! "class self-type" type)
       (for-each (curry check-valid-type! "implemented type of class")
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
       (for-each (curry check-valid-type! "init slot type")
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
           (copy-struct honu:class class
             [honu:class-members members])))]))

  ;; typecheck-mixin : Mixin -> Mixin
  ;; Typechecks a mixin definition and produces the annotated version.
  (define (typecheck-mixin mixin)
    (match mixin
      [(struct honu:mixin (stx name type arg-type final? impls inits withs
                           supernew members-before members-after exports))
       (check-valid-type! "mixin argument type" arg-type)
       (check-valid-type! "mixin result type" type)
       (for-each (curry check-valid-type! "mixin implemented type")
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
              (honu:ast-stx mixin))))
       (for-each (curry check-valid-type! "init slot type")
                 (map honu:formal-type inits))
       (let ([conflicting-name (get-first-non-unique-name (map honu:formal-name withs))])
         (if conflicting-name
             (raise-read-error-with-stx
              (format "Init name ~a used more than once in expected init slots"
                      (printable-key conflicting-name))
              conflicting-name)))
       (for-each (curry check-valid-type! "type of expected init slot")
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
           (copy-struct honu:mixin mixin
             [honu:mixin-members-before members-before]
             [honu:mixin-super-new      supernew]
             [honu:mixin-members-after  members-after])))]))

  ;; typecheck-subclass : Subclass -> Subclass
  ;; Typechecks a subclass definition and produces the annotated version.
  (define (typecheck-subclass subclass)
    (match subclass
      ;; we basically do all the checks when we create the tenv entry for the subclass (plus typechecking the base
      ;; class and mixin), so no need to check again.
      [(struct honu:subclass (_ _ _ _))
       subclass]))
  
  ;; typecheck-defn : Defn -> Defn
  ;; Typechecks a top-level definition and produces the annotated version.
  (define (typecheck-defn defn)
    (cond
     [(honu:bind-top? defn) (typecheck-bind-top defn)]
     [(honu:iface? defn) (typecheck-iface defn)]
     [(honu:class? defn) (typecheck-class defn)]
     [(honu:mixin? defn) (typecheck-mixin defn)]
     [(honu:subclass? defn) (typecheck-subclass defn)]
     [else (raise-read-error-with-stx
            "Haven't implemented typechecking for that type of definition yet."
            (honu:ast-stx defn))]))

  (define (typecheck-member-decl member)
    (match member
      [(struct honu:field-decl (stx name type))
       (check-valid-type! "field type" type)]
      [(struct honu:method-decl (stx name type args))
       (check-valid-type! "method return type" type)
       (for-each (curry check-valid-type! "method argument type")
                 args)]))

  )
