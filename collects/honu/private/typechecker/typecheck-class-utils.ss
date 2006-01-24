(module typecheck-class-utils mzscheme
  
  (require (lib "list.ss" "srfi" "1")
           (lib "plt-match.ss")
           (lib "struct.ss")
           "../../ast.ss"
           "../../readerr.ss"
           "../../tenv.ss"
           "../tools/general.ss"
           "typecheck-expression.ss"
           "typecheck-parameters.ss"
           "type-utils.ss")
  
  (provide extend-lenv-with-type-members typecheck-members typecheck-supernew typecheck-exports)

  (define (typecheck-exports lenv selftype init-impls exports)
    (let loop ([impls   init-impls]
               [exports exports])
      (cond
        [(and (null? exports)
              (null? impls))
         (if (not (s:member selftype init-impls (lambda (t1 t2) (type-equal? t1 t2))))
             (raise-read-error-with-stx
              (format "No export statement for self type ~a"
                      (printable-type selftype))
              (ast-syntax selftype))
             ;; the selftype was already covered by one of the implemented types, so
             ;; we can just stop.
             (void))]
        [(null? exports)
         (raise-read-error-with-stx
          (format "No export statement for implemented type ~a"
                  (printable-type (car impls)))
          (ast-syntax (car impls)))]
        [(null? impls)
         (if (s:member selftype init-impls (lambda (t1 t2) (type-equal? t1 t2)))
             (raise-read-error-with-stx
              (format "Extra export statement for unimplemented type ~a"
                      (printable-type (ast:export-type (car exports))))
              (ast-syntax (car exports)))
             (let-values ([(matched non-matches) (partition-first (lambda (e)
                                                                    (type-equal? (ast:export-type e) selftype))
                                                                  exports)])
               (if (not matched)
                   (raise-read-error-with-stx
                    (format "No export statement for self type ~a"
                            (printable-type selftype))
                    (ast-syntax selftype))
                   (let ([type-entry (get-type-entry selftype)])
                     (typecheck-export lenv type-entry matched)
                     (if (not (null? non-matches))
                         (raise-read-error-with-stx
                          (format "Extra export statement for unimplemented type ~a"
                                  (printable-type (ast:export-type (car exports))))
                          (ast-syntax (car exports)))
                         (void))))))]
        [else
         (let-values ([(matched non-matches) (partition-first (lambda (t)
                                                                (type-equal? (ast:export-type (car exports)) t))
                                                              impls)])
           (if (not matched)
               (raise-read-error-with-stx
                (format "Extra export statement for unimplemented type ~a"
                        (ast:export-type (car exports)))
                (ast-syntax (car exports)))
               (let* ([type-entry (get-type-entry matched)]
                      [export (car exports)])
                 (typecheck-export lenv type-entry export)
                 (loop non-matches (cdr exports)))))])))
  
  (define (typecheck-export lenv type-entry export)
    ;; make sure to use both defined members and inherited members here
    (let loop ([type-members (append (tenv:type-members type-entry) (tenv:type-inherited type-entry))]
               [export-binds (ast:export-members export)])
      (cond
        [(and (null? type-members)
              (null? export-binds))
         (void)]
        [(null? type-members)
         (raise-read-error-with-stx
          (format "No member named ~a in type ~a"
                  (printable-key  (ast:export/member-external (car export-binds)))
                  (printable-type (ast:export-type export)))
          (ast:export/member-external (car export-binds)))]
        [(null? export-binds)
         (raise-read-error-with-stx
          (format "Member ~a of type ~a was not exported"
                  (printable-key  (tenv:member-name (car type-members)))
                  (printable-type (ast:export-type export)))
          (ast-syntax export))]
        [else
         (let-values ([(matched non-matches) (partition-first (lambda (m)
                                                                (tenv-key=? (tenv:member-name m)
                                                                            (ast:export/member-external (car export-binds))))
                                                              type-members)]
                      [(lenv-entry)          (lenv (ast:export/member-internal (car export-binds)))])
           (cond
             [(not lenv-entry)
              (raise-read-error-with-stx
               (format "No static member named ~a"
                       (printable-key (ast:export/member-internal (car export-binds))))
               (ast:export/member-internal (car export-binds)))]
             [(not matched)
              (raise-read-error-with-stx
               (format "No member named ~a in type ~a"
                       (printable-key  (ast:export/member-external (car export-binds)))
                       (printable-type (ast:export-type export)))
               (ast:export/member-external (car export-binds)))]
             ;; if it's a method, then allow exporting a subtype
             [(ast:type:method? (tenv:member-type matched))
              (if (<:_P lenv-entry (tenv:member-type matched))
                  (loop non-matches (cdr export-binds))
                  (raise-read-error-with-stx
                   (format "Exported static member ~a has type ~a which is not a subtype of ~a's type ~a"
                           (printable-key  (ast:export/member-internal (car export-binds)))
                           (printable-type lenv-entry)
                           (printable-key  (tenv:member-name matched))
                           (printable-type (tenv:member-type matched)))
                   (ast:export/member-internal (car export-binds))))]
             ;; for fields, we just do invariance until we get read-only fields
             [else
              (if (type-equal? lenv-entry (tenv:member-type matched))
                  (loop non-matches (cdr export-binds))
                  (raise-read-error-with-stx
                   (format "Exported static member ~a has type ~a which is not the same type as ~a's type ~a"
                           (printable-key  (ast:export/member-internal (car export-binds)))
                           (printable-type lenv-entry)
                           (printable-key  (tenv:member-name matched))
                           (printable-type (tenv:member-type matched)))
                   (ast:export/member-internal (car export-binds))))]))])))
              

  
  (define (extend-lenv-with-type-members lenv type)
    (let ([type-entry (get-type-entry type)])
      (fold (lambda (m e)
              (extend-fenv (tenv:member-name m)
                           (tenv:member-type m)
                           e))
            lenv
            (tenv:type-members type-entry))))
  
  (define (typecheck-supernew lenv withs supernew)
    (let loop ([withs        withs]
               [args         (ast:super-new-args supernew)]
               [checked-args '()])
      (cond
        [(and (null? withs)
              (null? args))
         (copy-struct ast:super-new supernew
           [ast:super-new-args (reverse checked-args)])]
        [(null? withs)
         (raise-read-error-with-stx
          (format "No expected init slot declaration for super arg ~a"
                  (printable-key (ast:named/arg-name (car args))))
          (ast:named/arg-name (car args)))]
        [(null? args)
         (raise-read-error-with-stx
          (format "Expected init slot ~a not used as super arg"
                  (printable-key (ast:formal-name (car withs))))
          (ast:formal-name (car withs)))]
        [else
         (let-values ([(matched non-matches) (partition-first (lambda (w)
                                                                (tenv-key=? (ast:formal-name w)
                                                                            (ast:named/arg-name (car args))))
                                                              withs)])
           (if (not matched)
               (raise-read-error-with-stx
                (format "No expected init slot declaration for super arg ~a"
                        (printable-key (ast:named/arg-name (car args))))
                (ast:named/arg-name (car args)))
               (let ([first-arg  (car args)])
                 (let-values ([(e1 t1) (typecheck-expression lenv
                                                             (ast:formal-type matched)
                                                             (ast:named/arg-actual first-arg))])
                   (loop non-matches
                         (cdr args)
                         (cons (copy-struct ast:named/arg first-arg
                                 [ast:named/arg-actual e1])
                               checked-args))))))])))
             
  
  (define (typecheck-members lenv selftype members)
    (let loop ([members members]
               [lenv    lenv]
               [ret     '()])
      (cond
        [(null? members)
         (values (reverse ret) lenv)]
        [(or (ast:class/member:field/formal? (car members))
             (ast:class/member:field?      (car members)))
         (let ([member (typecheck-member lenv selftype (car members))])
           (loop (cdr members)
                 (extend-fenv (get-class-member-name (car members))
                              (get-class-member-type selftype (car members))
                              lenv)
                 (cons member ret)))]
        [(ast:class/member:method? (car members))
         (let-values ([(methods remainder) (span ast:class/member:method? members)])
           (let ([lenv (fold (lambda (m lenv)
                               (extend-fenv (get-class-member-name m)
                                            (get-class-member-type selftype m)
                                            lenv))
                             lenv
                             methods)])
             (loop remainder
                   lenv
                   ;; I only through the reverse in to keep the order the same.
                   ;; it doesn't really matter.
                   (append (reverse (map (lambda (m)
                                           (typecheck-member lenv selftype m))
                                         methods))
                           ret))))])))
  
  (define (typecheck-member lenv selftype member)
    (match member
      [(struct ast:class/member:field/formal (stx name type value))
       (if (not (type-valid? type))
           (raise-read-error-with-stx
            "Type of init field is undefined"
            (ast-syntax type)))
       (if value
           (let-values ([(e1 t1) (typecheck-expression lenv type value)])
             (copy-struct ast:class/member:field/formal member
               [ast:class/member:field/formal-default e1]))
           member)]
      [(struct ast:class/member:field (stx name type value))
       (if (not (type-valid? type))
           (raise-read-error-with-stx
            "Type of field is undefined"
            (ast-syntax type)))
       (let-values ([(e1 t1) (typecheck-expression lenv type value)])
         (copy-struct ast:class/member:field member
           [ast:class/member:field-default e1]))]
      [(struct ast:class/member:method (stx name type args body))
       (if (not (type-valid? type))
           (raise-read-error-with-stx
            "Return type of method is undefined"
            (ast-syntax type)))
       (for-each (lambda (t)
                   (if (not (type-valid? t))
                       (raise-read-error-with-stx
                        "Type of method argument is undefined"
                        (ast-syntax t))))
                 (map ast:formal-type args))
       (let-values ([(e1 t1) (parameterize ([current-return-type       type])
                               (typecheck-expression (fold (lambda (arg fenv)
                                                             (extend-fenv (ast:formal-name arg)
                                                                          (ast:formal-type arg)
                                                                          fenv))
                                                           lenv args)
                                                     type body))])
         (copy-struct ast:class/member:method member
           [ast:class/member:method-body e1]))]))

  (define (get-class-member-name member)
    (match member
      [(struct ast:class/member:field/formal (stx name type value)) name]
      [(struct ast:class/member:field (stx name type value))      name]
      [(struct ast:class/member:method (stx name type args body)) name]))
  
  (define (get-class-member-type exptype member)
    (match member
      [(struct ast:class/member:field/formal (stx name type value))
       type]
      [(struct ast:class/member:field (stx name type value))
       type]
      [(struct ast:class/member:method (stx name type args body))
       (make-method-type stx exptype 
                         (make-tuple-type stx (map ast:formal-type args))
                         type)]))
  
  )
