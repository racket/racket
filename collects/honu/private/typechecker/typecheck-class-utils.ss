(module typecheck-class-utils mzscheme
  
  (require (lib "list.ss" "srfi" "1")
           (lib "plt-match.ss")
           (lib "struct.ss")
           "../../ast.ss"
           "../../readerr.ss"
           "../../tenv.ss"
           "../../utils.ss"
           "typecheck-expression.ss"
           "typecheck-parameters.ss"
           "type-utils.ss")
  
  (provide extend-cenv-with-type-members typecheck-members typecheck-supernew typecheck-exports)
  (define (typecheck-exports cenv selftype init-impls exports)
    (let loop ([impls   init-impls]
               [exports exports])
      (cond
        [(and (null? exports)
              (null? impls))
         (if (not (s:member selftype init-impls (lambda (t1 t2) (type-equal? t1 t2))))
             (raise-read-error-with-stx
              (format "No export statement for self type ~a"
                      (printable-type selftype))
              (honu:ast-stx selftype))
             ;; the selftype was already covered by one of the implemented types, so
             ;; we can just stop.
             (void))]
        [(null? exports)
         (raise-read-error-with-stx
          (format "No export statement for implemented type ~a"
                  (printable-type (car impls)))
          (honu:ast-stx (car impls)))]
        [(null? impls)
         (if (s:member selftype init-impls (lambda (t1 t2) (type-equal? t1 t2)))
             (raise-read-error-with-stx
              (format "Extra export statement for unimplemented type ~a"
                      (printable-type (honu:export-type (car exports))))
              (honu:ast-stx (car exports)))
             (let-values ([(matched non-matches) (partition-first (lambda (e)
                                                                    (type-equal? (honu:export-type e) selftype))
                                                                  exports)])
               (if (not matched)
                   (raise-read-error-with-stx
                    (format "No export statement for self type ~a"
                            (printable-type selftype))
                    (honu:ast-stx selftype))
                   (let ([type-entry (get-type-entry selftype)])
                     (typecheck-export cenv type-entry matched)
                     (if (not (null? non-matches))
                         (raise-read-error-with-stx
                          (format "Extra export statement for unimplemented type ~a"
                                  (printable-type (honu:export-type (car exports))))
                          (honu:ast-stx (car exports)))
                         (void))))))]
        [else
         (let-values ([(matched non-matches) (partition-first (lambda (t)
                                                                (type-equal? (honu:export-type (car exports)) t))
                                                              impls)])
           (if (not matched)
               (raise-read-error-with-stx
                (format "Extra export statement for unimplemented type ~a"
                        (honu:export-type (car exports)))
                (honu:ast-stx (car exports)))
               (let* ([type-entry (get-type-entry matched)]
                      [export (car exports)])
                 (typecheck-export cenv type-entry export)
                 (loop non-matches (cdr exports)))))])))
  
  (define (typecheck-export cenv type-entry export)
    ;; make sure to use both defined members and inherited members here
    (let loop ([type-members (append (tenv:type-members type-entry) (tenv:type-inherited type-entry))]
               [export-binds (honu:export-binds export)])
      (cond
        [(and (null? type-members)
              (null? export-binds))
         (void)]
        [(null? type-members)
         (raise-read-error-with-stx
          (format "No member named ~a in type ~a"
                  (printable-key  (honu:exp-bind-new (car export-binds)))
                  (printable-type (honu:export-type export)))
          (honu:exp-bind-new (car export-binds)))]
        [(null? export-binds)
         (raise-read-error-with-stx
          (format "Member ~a of type ~a was not exported"
                  (printable-key  (tenv:member-name (car type-members)))
                  (printable-type (honu:export-type export)))
          (honu:ast-stx export))]
        [else
         (let-values ([(matched non-matches) (partition-first (lambda (m)
                                                                (tenv-key=? (tenv:member-name m)
                                                                            (honu:exp-bind-new (car export-binds))))
                                                              type-members)]
                      [(cenv-entry)          (cenv (honu:exp-bind-old (car export-binds)))])
           (cond
             [(not cenv-entry)
              (raise-read-error-with-stx
               (format "No static member named ~a"
                       (printable-key (honu:exp-bind-old (car export-binds))))
               (honu:exp-bind-old (car export-binds)))]
             [(not matched)
              (raise-read-error-with-stx
               (format "No member named ~a in type ~a"
                       (printable-key  (honu:exp-bind-new (car export-binds)))
                       (printable-type (honu:export-type export)))
               (honu:exp-bind-new (car export-binds)))]
             ;; if it's a method, then allow exporting a subtype
             [(honu:type-disp? (tenv:member-type matched))
              (if (<:_P cenv-entry (tenv:member-type matched))
                  (loop non-matches (cdr export-binds))
                  (raise-read-error-with-stx
                   (format "Exported static member ~a has type ~a which is not a subtype of ~a's type ~a"
                           (printable-key  (honu:exp-bind-old (car export-binds)))
                           (printable-type cenv-entry)
                           (printable-key  (tenv:member-name matched))
                           (printable-type (tenv:member-type matched)))
                   (honu:exp-bind-old (car export-binds))))]
             ;; for fields, we just do invariance until we get read-only fields
             [else
              (if (type-equal? cenv-entry (tenv:member-type matched))
                  (loop non-matches (cdr export-binds))
                  (raise-read-error-with-stx
                   (format "Exported static member ~a has type ~a which is not the same type as ~a's type ~a"
                           (printable-key  (honu:exp-bind-old (car export-binds)))
                           (printable-type cenv-entry)
                           (printable-key  (tenv:member-name matched))
                           (printable-type (tenv:member-type matched)))
                   (honu:exp-bind-old (car export-binds))))]))])))
              

  
  (define (extend-cenv-with-type-members cenv type)
    (let ([type-entry (get-type-entry type)])
      (fold (lambda (m e)
              (extend-fenv (tenv:member-name m)
                           (tenv:member-type m)
                           e))
            cenv
            (tenv:type-members type-entry))))
  
  (define (typecheck-supernew cenv lenv withs supernew)
    (let loop ([withs        withs]
               [args         (honu:super-new-args supernew)]
               [checked-args '()])
      (cond
        [(and (null? withs)
              (null? args))
         (copy-struct honu:super-new supernew
           [honu:super-new-args (reverse checked-args)])]
        [(null? withs)
         (raise-read-error-with-stx
          (format "No expected init slot declaration for super arg ~a"
                  (printable-key (honu:name-arg-name (car args))))
          (honu:name-arg-name (car args)))]
        [(null? args)
         (raise-read-error-with-stx
          (format "Expected init slot ~a not used as super arg"
                  (printable-key (honu:formal-name (car withs))))
          (honu:formal-name (car withs)))]
        [else
         (let-values ([(matched non-matches) (partition-first (lambda (w)
                                                                (tenv-key=? (honu:formal-name w)
                                                                            (honu:name-arg-name (car args))))
                                                              withs)])
           (if (not matched)
               (raise-read-error-with-stx
                (format "No expected init slot declaration for super arg ~a"
                        (printable-key (honu:name-arg-name (car args))))
                (honu:name-arg-name (car args)))
               (let ([first-arg  (car args)])
                 (let-values ([(e1 t1) (parameterize ([current-class-environment cenv])
                                         (typecheck-expression lenv
                                                               (honu:formal-type matched)
                                                               (honu:name-arg-value first-arg)))])
                   (loop non-matches
                         (cdr args)
                         (cons (copy-struct honu:name-arg first-arg
                                 [honu:name-arg-value e1])
                               checked-args))))))])))
             
  
  (define (typecheck-members cenv lenv selftype members)
    (let loop ([members members]
               [cenv    cenv]
               [ret     '()])
      (cond
        [(null? members)
         (values (reverse ret) cenv)]
        [(or (honu:init-field? (car members))
             (honu:field?      (car members)))
         (let ([member (typecheck-member cenv lenv selftype (car members))])
           (loop (cdr members)
                 (extend-fenv (get-class-member-name (car members))
                              (get-class-member-type selftype (car members))
                              cenv)
                 (cons member ret)))]
        [(honu:method? (car members))
         (let-values ([(methods remainder) (span honu:method? members)])
           (let ([cenv (fold (lambda (m cenv)
                               (extend-fenv (get-class-member-name m)
                                            (get-class-member-type selftype m)
                                            cenv))
                             cenv
                             methods)])
             (loop remainder
                   cenv
                   ;; I only through the reverse in to keep the order the same.
                   ;; it doesn't really matter.
                   (append (reverse (map (lambda (m)
                                           (typecheck-member cenv lenv selftype m))
                                         methods))
                           ret))))])))
  
  (define (typecheck-member cenv lenv selftype member)
    (match member
      [(struct honu:init-field (stx name type value))
       (if (not (type-valid? type))
           (raise-read-error-with-stx
            "Type of init field is undefined"
            (honu:ast-stx type)))
       (if value
           (let-values ([(e1 t1) (parameterize ([current-class-environment cenv])
                                   (typecheck-expression lenv type value))])
             (copy-struct honu:init-field member
               [honu:init-field-value e1]))
           member)]
      [(struct honu:field (stx name type value))
       (if (not (type-valid? type))
           (raise-read-error-with-stx
            "Type of field is undefined"
            (honu:ast-stx type)))
       (let-values ([(e1 t1) (parameterize ([current-class-environment cenv])
                               (typecheck-expression cenv lenv type value))])
         (copy-struct honu:field member
           [honu:field-value e1]))]
      [(struct honu:method (stx name type args body))
       (if (not (type-valid? type))
           (raise-read-error-with-stx
            "Return type of method is undefined"
            (honu:ast-stx type)))
       (for-each (lambda (t)
                   (if (not (type-valid? t))
                       (raise-read-error-with-stx
                        "Type of method argument is undefined"
                        (honu:ast-stx t))))
                 (map honu:formal-type args))
       (let-values ([(e1 t1) (parameterize ([current-class-environment cenv]
                                            [current-return-type       type])
                               (typecheck-expression (fold (lambda (arg fenv)
                                                             (extend-fenv (honu:formal-name arg)
                                                                          (honu:formal-type arg)
                                                                          fenv))
                                                           lenv args)
                                                     type body))])
         (copy-struct honu:method member
           [honu:method-body e1]))]))

  (define (get-class-member-name member)
    (match member
      [(struct honu:init-field (stx name type value)) name]
      [(struct honu:field (stx name type value))      name]
      [(struct honu:method (stx name type args body)) name]))
  
  (define (get-class-member-type exptype member)
    (match member
      [(struct honu:init-field (stx name type value))
       type]
      [(struct honu:field (stx name type value))
       type]
      [(struct honu:method (stx name type args body))
       (make-method-type stx exptype 
                         (make-tuple-type stx (map honu:formal-type args))
                         type)]))
  
  )