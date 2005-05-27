(module honu-typecheck-class-utils mzscheme
  
  (require (lib "struct.ss")
           (lib "plt-match.ss")
           (lib "list.ss" "srfi" "1")
           (prefix list: (lib "list.ss")))
  
  (require "../../ast.ss")
  (require "../../utils.ss")
  (require "../../tenv.ss")
  (require "honu-type-utils.ss")
  (require "honu-typecheck-exp.ss")
  
  (require "../../read-error-with-stx.ss")

  (provide honu-typecheck-slotdefns)
  (define (honu-typecheck-slotdefns tenv env cenv init-cenv defns)
    (honu-typecheck-slotdefns-helper tenv env cenv init-cenv defns (list)))
  
  (define (honu-typecheck-slotdefns-helper tenv env cenv init-cenv defns new-defns)
    (cond
      [(null? defns) (values (reverse new-defns) env cenv init-cenv)]
      [(honu-init-field? (car defns))
       (let-values (((new-defn new-env new-cenv new-init-cenv)
                     (honu-typecheck-init-field tenv env cenv init-cenv (car defns))))
         (honu-typecheck-slotdefns-helper tenv new-env new-cenv 
                                          new-init-cenv (cdr defns) (cons new-defn new-defns)))]
      [(honu-field? (car defns))
       (let-values (((new-defn new-env new-cenv new-init-cenv)
                     (honu-typecheck-field tenv env cenv init-cenv (car defns))))
         (honu-typecheck-slotdefns-helper tenv new-env new-cenv 
                                          new-init-cenv (cdr defns) (cons new-defn new-defns)))]
      [(honu-method? (car defns))
       (let loop ((mdefns (list (car defns)))
                  (rest-defns (cdr defns)))
         (if (or (null? rest-defns)
                 (not (honu-method? (car rest-defns))))
             (let-values (((new-mdefns new-env new-cenv new-init-cenv)
                           (honu-typecheck-methods tenv env cenv init-cenv mdefns)))
               (honu-typecheck-slotdefns-helper tenv new-env new-cenv new-init-cenv rest-defns
                                                (append (reverse new-mdefns) new-defns)))
             (loop (cons (car rest-defns) mdefns) (cdr rest-defns))))]
      [else (raise-read-error-with-stx
             "Unexpected type of slotdefn."
             (honu-ast-src-stx (car defns)))]))
  
  (define (honu-typecheck-init-field tenv env cenv init-cenv defn)
    (match-let ([(struct honu-init-field (stx name type value)) defn])
      (if (honu-type-in-tenv? tenv type)
          (if value
              (let-values ([(e1 t1) ((honu-typecheck-exp tenv env init-cenv)
                                     value type)])
                (values (copy-struct honu-init-field defn
                          (honu-init-field-value e1))
                        env
                        (extend-env cenv name type)
                        (extend-env init-cenv name type)))
              (values defn env
                      (extend-env cenv name type)
                      (extend-env init-cenv name type)))
          (raise-read-error-with-stx
           "Type of init field not found in program."
           (honu-ast-src-stx type)))))
  
  (define (honu-typecheck-field tenv env cenv init-cenv defn)
    (match-let ([(struct honu-field (stx name type value)) defn])
      (if (honu-type-in-tenv? tenv (honu-field-type defn))
          (let-values ([(e1 t1) ((honu-typecheck-exp tenv env init-cenv)
                                 value type)])
            (values (copy-struct honu-field defn
                      (honu-field-value e1))
                    env
                    (extend-env cenv name type)
                    (extend-env init-cenv name type)))
          (raise-read-error-with-stx
           "Type of field not found in program."
           (honu-ast-src-stx type)))))

  (define (honu-typecheck-methods tenv env cenv init-cenv mdefns)
    (let* ((new-cenv (fold (lambda (d i)
                             (extend-env i (honu-method-name d)
                                         (make-honu-dispatch-type (honu-ast-src-stx d)
                                                                (list (env #'this))
                                                                (honu-method-arg-types d)
                                                                (honu-method-type d))))
                           cenv mdefns))
           (new-init-cenv (fold (lambda (d i)
                                  (extend-env i (honu-method-name d)
                                              (make-honu-dispatch-type (honu-ast-src-stx d)
                                                                     (list (env #'this))
                                                                     (honu-method-arg-types d)
                                                                     (honu-method-type d))))
                                init-cenv mdefns))
           (new-mdefns (map (lambda (d)
                              (honu-typecheck-method tenv env new-cenv d))
                            mdefns)))
      (values new-mdefns env new-cenv new-init-cenv)))

  (define (honu-typecheck-method tenv env cenv defn)
    (match-let ([(struct honu-method (stx name type arg-names arg-types body)) defn])
      (if (or (honu-top-type? type) ;; we allow void only in method return types
              (honu-type-in-tenv? tenv type))
          (let ([new-env (fold (lambda (n t env)
                                 (extend-env env n t))
                               env arg-names arg-types)])
            (check-arg-types tenv arg-types) ;; will raise exception if one fails
            (let-values (((e1 t1) ((honu-typecheck-exp tenv new-env cenv) body (if (honu-top-type? type) #f type))))
              (if (<:_P tenv t1 type)
                  (copy-struct honu-method defn
                               (honu-method-body e1))
                  (raise-read-error-with-stx
                   "Body of method's type does not match declared return type"
                   (honu-ast-src-stx body)))))
          (raise-read-error-with-stx
           "Return type of method does not exist in program."
           (honu-ast-src-stx type)))))
  
  (define (check-arg-types tenv types)
    (cond
      [(null? types) #t]
      [(not (honu-type-in-tenv? tenv (car types)))
       (raise-read-error-with-stx
        "Argument type of method does not exist in program."
        (honu-ast-src-stx (car types)))]
      [else (check-arg-types tenv (cdr types))]))

  (provide check-init-slots)
  (define (check-init-slots tenv names types)
    (cond
      [(null? types) #t]
      [(not (honu-type-in-tenv? tenv (car types)))
       (raise-read-error-with-stx
        (format "Type for init slot ~a does not exist in program."
                (printable-key (car names)))
        (honu-ast-src-stx (car types)))]
      [else (check-init-slots tenv (cdr names) (cdr types))]))

  (provide check-impl-types)
  (define (check-impl-types tenv types)
    (cond
      [(null? types) #t]
      [(not (honu-iface-type-in-tenv? tenv (car types)))
       (raise-read-error-with-stx
        "Type in implements list does not exist in program."
        (honu-ast-src-stx (car types)))]
      [else (check-impl-types tenv (cdr types))]))
  
  (provide honu-typecheck-export)
  (define (honu-typecheck-export tenv cenv expdec)
    (define (check-export-name old new)
      (let ((old-type (cenv old)))
        (if old-type
            (let ((new-type
                   (cond
                     [(honu-prim-type? old-type)
                      (get-field-type tenv (honu-export-type expdec) new)]
                     [(honu-iface-type? old-type)
                      (get-field-type tenv (honu-export-type expdec) new)]
                     [(honu-func-type? old-type)
                      (get-field-type tenv (honu-export-type expdec) new)]
                     [(honu-dispatch-type? old-type)
                      (get-method-type tenv (honu-export-type expdec) new)]
                     [else (raise-read-error-with-stx
                            "Unexpected class of type in check-export-name."
                            (honu-ast-src-stx old-type))])))
              (cond
                [(not new-type)
                 (raise-read-error-with-stx
                  "Public name to be exported to not found in class/mixin type."
                  new)]
                [(and (honu-dispatch-type? old-type)
                      (not (<:_P tenv old-type new-type)))
                 (raise-read-error-with-stx
                  "Method to be exported is not a subtype of the public type."
                  old)]
                [(and (not (honu-dispatch-type? old-type))
                      (not (honu-type-equal? old-type new-type)))
                 (raise-read-error-with-stx
                  "Field to be exported is not an exact type match for the public type."
                  old)]
                [else (void)])) ; The current one checks, we won't have to alter anything.
            (raise-read-error-with-stx
             "Local name to be exported not found in class/mixin."
             old))))
    ;; yes, in the check to make sure they're all exported, I convert to symbols and
    ;; then run the sorting thing, then just use equal?.  I really should make a
    ;; version that tells you _WHICH_ wasn't exported.
    (let ([sorted-type-fields-and-methods
           (sort-names (map (lambda (p) (printable-key (car p)))
                            (get-fields-and-methods tenv (honu-export-type expdec))))]
          [sorted-new-names (sort-names (map printable-key 
                                             (honu-export-new-names expdec)))])
      (if (not (equal? sorted-type-fields-and-methods sorted-new-names))
                 (raise-read-error-with-stx
                  "Not all fields and methods in export type exported."
                  (honu-ast-src-stx expdec))))
    (for-each check-export-name
              (honu-export-old-names expdec)
              (honu-export-new-names expdec)))

  ;; symbol list -> symbol list (sorted)
  (define (sort-names list)
    (list:quicksort list
                    (lambda (a b)
                      (string<? (symbol->string a)
                                (symbol->string b)))))
          
  (provide check-impls-and-exports)
  (define (check-impls-and-exports tenv cenv sub-type impl-types exports)
    (for-each (lambda (i)
                (if (ormap (lambda (t)
                             (honu-type-equal? t i))
                           (map honu-export-type exports))
                    (void)
                    (raise-read-error-with-stx
                     "No export statement for implemented type."
                     (honu-ast-src-stx i))))
              impl-types)
    (if (ormap (lambda (t)
                 (honu-type-equal? t sub-type))
               (map honu-export-type exports))
        (void)
        (raise-read-error-with-stx
         "No export statement for type of this."
         (honu-ast-src-stx sub-type)))
    (for-each (lambda (e)
                (if (or (ormap (lambda (t)
                                 (honu-type-equal? t (honu-export-type e)))
                               impl-types)
                        (honu-type-equal? sub-type (honu-export-type e)))
                    (honu-typecheck-export tenv cenv e)
                    (raise-read-error-with-stx
                     "Export statement for type that is not implemented or type of this."
                     (honu-ast-src-stx e))))
              exports))
  )
