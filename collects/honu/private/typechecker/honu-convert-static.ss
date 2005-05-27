(module honu-convert-static mzscheme
  
  (require (lib "list.ss" "srfi" "1")
           (lib "struct.ss")
           "../../ast.ss"
           "../../tenv.ss"
           "honu-type-utils.ss"
           "../../read-error-with-stx.ss")
  
  (define (tenv-filter-map tenv p)
    (filter values (tenv-map tenv p)))
  
  (define (default-bindings tenv)
    (append '(printStr printLine error readChar readLine strToInt strToFloat
              intToStr floatToStr charToStr strLen substr charAt)
            (tenv-filter-map tenv
               (lambda (k v)
                 (if (tenv-func? v) (printable-key k) #f)))))
  
  (define default-class-bindings '())
  
  (define (get-static-bindings defn)
    (define (get-slotdefn-name d)
      (cond
        [(honu-field? d) (printable-key (honu-field-name d))]
        [(honu-method? d) (printable-key (honu-method-name d))]
        [(honu-init-field? d) (printable-key (honu-init-field-name d))]))
    (cond
      [(honu-class? defn)
       (append default-class-bindings
               (map printable-key (honu-class-init-names defn))
               (filter-map get-slotdefn-name (honu-class-defns defn)))]
      [(honu-mixin? defn)
       (append default-class-bindings
               (map printable-key (honu-mixin-init-names defn))
               (filter-map get-slotdefn-name 
                           (append (honu-mixin-defns-before defn)
                                   (honu-mixin-defns-after defn))))]
      [else '()]))
  
  (provide honu-convert-static)
  (define (honu-convert-static tenv pgm)
    (let* ((env (default-bindings tenv))
           (defns (map (lambda (defn)
                         (honu-convert-defn tenv env defn))
                       (honu-program-defns pgm))))
      (copy-struct honu-program pgm
                   (honu-program-defns defns))))
  
  (define (honu-convert-defn tenv env defn)
    (cond
      [(honu-class? defn)
       (let* ((cenv (get-static-bindings defn))
              (new-mdidefns (map (lambda (d)
                                   (honu-convert-mdidefn d env cenv)) (honu-class-defns defn))))
         (copy-struct honu-class defn
                      (honu-class-defns new-mdidefns)))]
      [(honu-mixin? defn)
       (let* ((cenv (append (get-static-bindings defn)
                            ;; get-fields-and-methods returns (name . type) pairs, so map car
                            (map (lambda (p)
                                   (printable-key (car p)))
                                 (get-fields-and-methods tenv (honu-mixin-arg-type defn)))))
              (new-befores (map (lambda (d)
                                  (honu-convert-mdidefn d env cenv))
                                (honu-mixin-defns-before defn)))
              (new-super-new (honu-convert-super-new (honu-mixin-super-new defn) env cenv))
              (new-afters (map (lambda (d)
                                 (honu-convert-mdidefn d env cenv))
                               (honu-mixin-defns-after defn))))
         (copy-struct honu-mixin defn
                      (honu-mixin-defns-before new-befores)
                      (honu-mixin-super-new new-super-new)
                      (honu-mixin-defns-after new-afters)))]
      [else defn]))

  (define (honu-convert-mdidefn defn env cenv)
    (cond
      [(honu-init-field? defn)
       (if (not (honu-init-field-value defn))
           (copy-struct honu-init-field defn
                        (honu-init-field-value (honu-convert-exp (honu-init-field-value defn)
                                                                 env cenv)))
           defn)]
      [(honu-field? defn)
       (copy-struct honu-field defn
                    (honu-field-value (honu-convert-exp (honu-field-value defn) env cenv)))]
      [(honu-method? defn)
       (copy-struct honu-method defn
                    (honu-method-body (honu-convert-exp (honu-method-body defn)
                                                        (append (map printable-key (honu-method-arg-names defn))
                                                                env)
                                                        cenv)))]))

  (define (honu-convert-super-new defn env cenv)
    (let ((new-vals (map (lambda (e)
                           (honu-convert-exp e env cenv))
                         (honu-super-new-arg-vals defn))))
      (copy-struct honu-super-new defn
                   (honu-super-new-arg-vals new-vals))))
  
  ;; I should consider changing this to allowing through all names that aren't bound in the
  ;; cenv (those should be converted to my.foo).  The typechecker will already give appropriate
  ;; error messages for unbound variables.
  (define (honu-convert-exp exp bound-vars cenv)
    (cond
      [(honu-var? exp)
       (cond
         [(member (printable-key (honu-var-name exp)) bound-vars) exp]
         [(member (printable-key (honu-var-name exp)) cenv)
          (make-honu-facc (honu-ast-src-stx exp)
                          'my #f (honu-var-name exp))]
         [else (raise-read-error-with-stx
                (format "Unbound variable name ~a" (printable-key (honu-var-name exp)))
                (honu-var-name exp))])]
      [(honu-assn? exp)
       (let ((new-rhs (honu-convert-exp (honu-assn-rhs exp) bound-vars cenv)))
         (cond
           [(member (printable-key (honu-assn-name exp)) bound-vars)
            (copy-struct honu-assn exp (honu-assn-rhs new-rhs))]
           [(member (printable-key (honu-assn-name exp)) cenv)
            (make-honu-fassn (honu-ast-src-stx exp)
                             'my #f (honu-assn-name exp) new-rhs)]
           [else (raise-read-error-with-stx
                  (format "Unbound variable name ~a" (printable-key (honu-assn-name exp)))
                  (honu-assn-name exp))]))]
           
      [(honu-call? exp)
       (let ((new-args (map (lambda (e)
                              (honu-convert-exp e bound-vars cenv))
                            (honu-call-args exp))))
         (cond
           [(member (printable-key (honu-call-name exp)) bound-vars)
            (copy-struct honu-call exp (honu-call-args new-args))]
           [(member (printable-key (honu-call-name exp)) cenv)
            (make-honu-mcall (honu-ast-src-stx exp)
                             'my #f (honu-call-name exp) new-args)]
           [else (raise-read-error-with-stx
                  (format "Unbound function name ~a" (printable-key (honu-call-name exp)))
                  (honu-call-name exp))]))]
      [(honu-prim? exp)
       (copy-struct honu-prim exp
                    (honu-prim-left (honu-convert-exp (honu-prim-left exp) bound-vars cenv))
                    (honu-prim-right (honu-convert-exp (honu-prim-right exp) bound-vars cenv)))]
      [(honu-facc? exp)
       (if (eqv? (honu-facc-obj exp) 'my)
           exp
           (copy-struct honu-facc exp
                        (honu-facc-obj (honu-convert-exp (honu-facc-obj exp) bound-vars cenv))))]
      [(honu-fassn? exp)
       (copy-struct honu-fassn exp
                    (honu-fassn-obj (if (eqv? (honu-fassn-obj exp) 'my) 'my
                             (honu-convert-exp (honu-fassn-obj exp) bound-vars cenv)))
                    (honu-fassn-rhs (honu-convert-exp (honu-fassn-rhs exp) bound-vars cenv)))]
      [(honu-mcall? exp)
       (copy-struct honu-mcall exp
                    (honu-mcall-obj (if (eqv? (honu-mcall-obj exp) 'my) 'my
                             (honu-convert-exp (honu-mcall-obj exp) bound-vars cenv)))
                    (honu-mcall-args (map (lambda (e)
                                 (honu-convert-exp e bound-vars cenv))
                               (honu-mcall-args exp))))]
      [(honu-cast? exp)
       (copy-struct honu-cast exp
                    (honu-cast-obj (honu-convert-exp (honu-cast-obj exp) bound-vars cenv)))]
      [(honu-isa? exp)
       (copy-struct honu-isa exp
                    (honu-isa-obj (honu-convert-exp (honu-isa-obj exp) bound-vars cenv)))]
      [(honu-if? exp)
       (copy-struct honu-if exp
                    (honu-if-cond (honu-convert-exp (honu-if-cond exp) bound-vars cenv))
                    (honu-if-true (honu-convert-exp (honu-if-true exp) bound-vars cenv))
                    (honu-if-false (honu-convert-exp (honu-if-false exp) bound-vars cenv)))]
      [(honu-new? exp)
       (copy-struct honu-new exp
                    (honu-new-arg-vals (map (lambda (e)
                                     (honu-convert-exp e bound-vars cenv))
                                   (honu-new-arg-vals exp))))]
      [(honu-lambda? exp)
       (copy-struct honu-lambda exp
         (honu-lambda-body (honu-convert-exp (honu-lambda-body exp)
                                             (append (map printable-key (honu-lambda-arg-names exp))
                                                     bound-vars)
                                             cenv)))]
      [(honu-block? exp)
       (let loop ((binds (honu-block-binds exp))
                  (new-binds null)
                  (bound-vars bound-vars))
         (if (null? binds)
             (copy-struct honu-block exp
                          (honu-block-binds (reverse new-binds))
                          (honu-block-exps (map (lambda (e)
                                       (honu-convert-exp e bound-vars cenv))
                                     (honu-block-exps exp))))
             (let ((bind (car binds)))
               (loop (cdr binds)
                     (cons (copy-struct honu-binding bind
                            (honu-binding-rhs (honu-convert-exp (honu-binding-rhs bind) bound-vars cenv)))
                           new-binds)
                     (cons (printable-key (honu-binding-name bind)) bound-vars)))))]
      [(honu-return? exp)
       (copy-struct honu-return exp
                    (honu-return-body (honu-convert-exp (honu-return-body exp) bound-vars cenv)))]
      [else exp]))
  )
