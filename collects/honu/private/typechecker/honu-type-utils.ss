(module honu-type-utils mzscheme
  
  (require (all-except (lib "list.ss" "srfi" "1") any)
           (lib "plt-match.ss")
           (lib "contract.ss")
           "../../ast.ss"
           "../../tenv.ss"
           "../../read-error-with-stx.ss")
  
  (provide honu-bool-type honu-str-type honu-int-type honu-float-type
           honu-char-type 
           honu-null-type honu-error-type honu-any-type honu-void-type
           honu-func-type-from-exp)

  (define (honu-error-type exp)
    (if (syntax? exp)
        (make-honu-bottom-type exp)
        (make-honu-bottom-type (honu-ast-src-stx exp))))
  
  (define (honu-func-type-from-exp args return exp)
    (if (syntax? exp)
        (make-honu-func-type exp args return)
        (make-honu-func-type (honu-ast-src-stx exp)
                             args return)))
  
  (define (honu-dispatch-type-from-exp dispatches args return exp)
    (if (syntax? exp)
        (make-honu-dispatch-type exp dispatches args return)
        (make-honu-dispatch-type (honu-ast-src-stx exp)
                               dispatches args return)))
  
  (define (honu-null-type exp)
    (if (syntax? exp)
        (make-honu-iface-bottom-type exp)
        (make-honu-iface-bottom-type (honu-ast-src-stx exp))))

  (define (honu-any-type exp)
    (if (syntax? exp)
        (make-honu-iface-top-type exp)
        (make-honu-iface-top-type (honu-ast-src-stx exp))))

  (define (honu-void-type exp)
    (if (syntax? exp)
        (make-honu-top-type exp)
        (make-honu-top-type (honu-ast-src-stx exp))))
  
  (define (honu-int-type exp)
    (create-honu-prim-type exp 'int))
  (define (honu-bool-type exp)
    (create-honu-prim-type exp 'bool))
  (define (honu-str-type exp)
    (create-honu-prim-type exp 'str))
  (define (honu-float-type exp)
    (create-honu-prim-type exp 'float))
  (define (honu-char-type exp)
    (create-honu-prim-type exp 'char))

  (define (create-honu-prim-type exp name)
    (if (syntax? exp)
        (make-honu-prim-type exp name)
        (make-honu-prim-type (honu-ast-src-stx exp) name)))
  
  (provide printable-type)
  (define (printable-type typ)
    (match typ
      [(struct honu-bottom-type       (stx))          "<any type>"]
      [(struct honu-top-type          (stx))          "void"]
      [(struct honu-iface-bottom-type (stx))          "<interface type>"]
      [(struct honu-iface-top-type    (stx))          "Any"]
      [(struct honu-iface-type        (stx name))     (symbol->string (printable-key name))]
      [(struct honu-prim-type         (stx name))     (symbol->string name)]
      [(struct honu-func-type         (stx args ret)) 
       (if (null? args)
           (string-append "[]->" (printable-type ret))
           (string-append "[" (fold-right (lambda (t s)
                                            (string-append s ", " (printable-type t)))
                                          (printable-type (car args))
                                          (cdr args)) 
                          "]->" (printable-type ret)))]
      [(struct honu-dispatch-type         (stx dispatches args ret)) 
       (string-append "[" (fold-right (lambda (t s)
                                        (string-append s ", " (printable-type t)))
                                      (string-append "("
                                                     (fold-right (lambda (t s)
                                                                   (string-append s ", " (printable-type t)))
                                                                 (printable-type (car dispatches))
                                                                 (cdr dispatches))
                                                     ")")
                                      args)
                      "]->" (printable-type ret))]))

  (provide/contract [raise-type-error-with-stx (honu-type? honu-type? any/c . -> . any)])
  (define (raise-type-error-with-stx t1 t2 stx)
    (raise-read-error-with-stx
     (format "Expected type ~a, got type ~a"
             (printable-type t1)
             (printable-type t2))
     stx))
  
  (provide honu-type-equal? honu-iface-type-in-tenv? honu-type-in-tenv?)
  
  (define (honu-type-equal? t1 t2)
    (cond
      [(and (honu-bottom-type? t1)
            (honu-bottom-type? t2)) #t]
      [(and (honu-top-type? t1)
            (honu-top-type? t2)) #t]
      [(and (honu-iface-bottom-type? t1)
            (honu-iface-bottom-type? t2)) #t]
      [(and (honu-iface-top-type? t1)
            (honu-iface-top-type? t2)) #t]
;      [(and (honu-void-type? t1)
;            (honu-void-type? t2)) #t]
      [(and (honu-prim-type? t1)
            (honu-prim-type? t2))
       (eqv? (honu-prim-type-name t1)
             (honu-prim-type-name t2))]
      [(and (honu-iface-type? t1)
            (honu-iface-type? t2))
       (eq? (printable-key (honu-iface-type-name t1))
            (printable-key (honu-iface-type-name t2)))]
      [(and (honu-func-type? t1)
            (honu-func-type? t2))
       (and (honu-type-equal? (honu-func-type-return t1)
                              (honu-func-type-return t2))
            (equal? (length (honu-func-type-args t1))
                    (length (honu-func-type-args t2)))
            (andmap honu-type-equal?
                    (honu-func-type-args t1)
                    (honu-func-type-args t2)))]
      [(and (honu-dispatch-type? t1)
            (honu-dispatch-type? t2))
       (and (honu-type-equal? (honu-dispatch-type-return t1)
                              (honu-dispatch-type-return t2))
            (equal? (length (honu-dispatch-type-dispatches t1))
                    (length (honu-dispatch-type-dispatches t2)))
            (andmap honu-type-equal?
                    (honu-dispatch-type-dispatches t1)
                    (honu-dispatch-type-dispatches t2))
            (equal? (length (honu-dispatch-type-args t1))
                    (length (honu-dispatch-type-args t2)))
            (andmap honu-type-equal?
                    (honu-dispatch-type-args t1)
                    (honu-dispatch-type-args t2)))]
      [else #f]))

  (define (honu-iface-type-in-tenv? tenv t)
    (or (honu-iface-top-type? t)
        (and (honu-iface-type? t)
             (get-type-entry (honu-iface-type-name t) tenv))))
  
  (define (honu-prim-type-in-honu? t)
    (member (honu-prim-type-name t) '(int bool str float char)))
  
  (define (honu-func-type-in-tenv? tenv t)
    (and (or (honu-top-type? (honu-func-type-return t)) ;; take care of void here
             (honu-type-in-tenv? tenv (honu-func-type-return t)))
         (andmap (lambda (t)
                   (honu-type-in-tenv? tenv t))
                 (honu-func-type-args t))))

  (define (honu-dispatch-type-in-tenv? tenv t)
    (and (or (honu-top-type? (honu-dispatch-type-return t)) ;; take care of void here
             (honu-type-in-tenv? tenv (honu-dispatch-type-return t)))
         (andmap (lambda (t)
                   (honu-type-in-tenv? tenv t))
                 (honu-dispatch-type-dispatches t))
         (andmap (lambda (t)
                   (honu-type-in-tenv? tenv t))
                 (honu-dispatch-type-args t))))

  (define (honu-type-in-tenv? tenv t)
    (cond
      [(honu-dispatch-type? t) (honu-dispatch-type-in-tenv? tenv t)]
      [(honu-func-type? t)   (honu-func-type-in-tenv? tenv t)]
      [(honu-prim-type? t)   (honu-prim-type-in-honu? t)]
      [else                  (honu-iface-type-in-tenv? tenv t)]))
  
  (provide get-field-type get-method-type)

  (define (get-field-type tenv typ fd)
    (if (honu-iface-top-type? typ)
        (raise-read-error-with-stx
         "The Any type has no fields."
         fd))
    (if (not (honu-iface-type? typ))
        (raise-read-error-with-stx
         "Attempt to get field of a type that is not an interface type."
         fd))
    (let* ([type-def (get-type-entry (honu-iface-type-name typ) tenv)]
           [field-decl (find (lambda (d)
                               (and (honu-field-decl? d)
                                    (tenv-key=? (honu-field-decl-name d) fd)))
                             (tenv-type-members type-def))])
      (if field-decl
          (honu-field-decl-type field-decl)
          (let loop ([supers (tenv-type-supers type-def)])
            (cond
             ((null? supers) #f)
             ((get-field-type tenv (car supers) fd) => values)
             (else (loop (cdr supers))))))))

  (define (get-method-type tenv typ md)
    (if (honu-iface-top-type? typ)
        (raise-read-error-with-stx
         "The Any type has no methods."
         md))
    (if (not (honu-iface-type? typ))
        (raise-read-error-with-stx
         "Attempt to get method of a type that is not an interface type."
         md))
    (let* ([type-def (get-type-entry (honu-iface-type-name typ) tenv)]
           [method-decl (find (lambda (d)
                                (and (honu-method-decl? d)
                                     (tenv-key=? (honu-method-decl-name d) md)))
                              (tenv-type-members type-def))])
      (if method-decl
          (honu-dispatch-type-from-exp
           (list typ)
           (honu-method-decl-arg-types method-decl)
           (honu-method-decl-type method-decl)
           method-decl)
          (let loop ([supers (tenv-type-supers type-def)])
            (cond
             ((null? supers) #f)
             ((get-method-type tenv (car supers) md) => values)
             (else (loop (cdr supers))))))))
  
  (provide <:_P Implements_P)
  
  (define (Subtype_P tenv t1 t2)
    (if (and (honu-iface-type? t1)
             (honu-iface-type? t2))
        (let ([t1-def (get-type-entry (honu-iface-type-name t1) tenv)])
          (ormap (lambda (t)
                   (honu-type-equal? t t2))
                 (tenv-type-supers t1-def)))
        #f))
  
  (define (<:_P tenv t1 t2)
    (cond
      [(honu-type-equal? t1 t2) #t] ; Reflexive
      [(honu-bottom-type? t1) #t] ; bottom is a subtype of all
      [(honu-top-type? t2) #t] ; top is a supertype of all
      [(and (honu-iface-bottom-type? t1)
            (or (honu-iface-top-type? t2)
                (honu-iface-type? t2))) #t] ; iface bottom type <: any iface type
      [(and (honu-iface-top-type? t2)
            (or (honu-iface-bottom-type? t1)
                (honu-iface-type? t1))) #t] ; any iface type <: iface top type 
      [(Subtype_P tenv t1 t2) #t] ; Direct subtype is <:
      [(and (honu-iface-type? t1)
            (honu-iface-type? t2))
       (let ([t1-def (get-type-entry (honu-iface-type-name t1) tenv)])
         (ormap (lambda (t)
                  (<:_P tenv t t2))
              (tenv-type-supers t1-def)))]
      [(and (honu-func-type? t1)
            (honu-func-type? t2))
       (and (<:_P tenv
                  (honu-func-type-return t1)
                  (honu-func-type-return t2))
            (equal? (length (honu-func-type-args t1))
                    (length (honu-func-type-args t2)))
            (andmap (lambda (at bt)
                      (<:_P tenv bt at))
                    (honu-func-type-args t1)
                    (honu-func-type-args t2)))]
      [(and (honu-dispatch-type? t1)
            (honu-dispatch-type? t2))
       (and (<:_P tenv ;; return covariant
                  (honu-dispatch-type-return t1)
                  (honu-dispatch-type-return t2))
            (equal? (length (honu-dispatch-type-dispatches t1))
                    (length (honu-dispatch-type-dispatches t2)))
            (andmap (lambda (at bt) ;; dispatched args covariant
                      (<:_P tenv at bt))
                    (honu-dispatch-type-dispatches t1)
                    (honu-dispatch-type-dispatches t2))
            (equal? (length (honu-dispatch-type-args t1))
                    (length (honu-dispatch-type-args t2)))
            (andmap (lambda (at bt) ;; non-dispatched args contravariant
                      (<:_P tenv bt at))
                    (honu-dispatch-type-args t1)
                    (honu-dispatch-type-args t2)))]
      [else #f]))
  
  (define (Implements_P tenv c t)
    (let ([defn (get-class-entry c tenv)])
      (ormap (lambda (t1)
                  (<:_P tenv t1 t))
                (tenv-class-impls defn))))

  (provide get-init-names-and-types)
  (define (get-init-names-and-types tenv c)
    (let ((defn (get-class-entry c tenv)))
      (tenv-class-inits defn)))
  
  (provide check-init-type-for-name)
  (define (check-init-type-for-name tenv inits name type)
    (if (null? inits)
        (raise-read-error-with-stx
         "Initialization argument name not found in list of initialization field names."
         name)
        (let ((current (car inits)))
          (if (eq? (printable-key (tenv-init-name current)) (printable-key name))
              ;; The name matches, so check the types.
              (if (<:_P tenv type (tenv-init-type current))
                  ;; The types matched, so we're good so far.  Return the unused init args.
                  (cdr inits)
                  ;; The types didn't match, so we need to do some further checking.
                  (if (tenv-init-optional? current)
                      ;; This field had to be initialized, so this is a type error.
                      (raise-read-error-with-stx
                       "Type for initialization value does not match declared type."
                       name)
                      ;; It doesn't have to be initialized, so we assume that it was left
                      ;; out and continue with the rest of the list, dropping it.
                      (check-init-type-for-name tenv (cdr inits) name type)))
              ;; The current initialization argument doesn't match, so keep checking as they
              ;; can be ordered differently.
              (cons current (check-init-type-for-name tenv (cdr inits) name type))))))
  
  (provide get-field-names-for-type)
  (define (get-field-names-for-type tenv t)
    (if (honu-iface-top-type? t) '()
        (let ([type-defn (get-type-entry (honu-iface-type-name t) tenv)])
          (apply append (cons (filter-map (lambda (d)
                                            (cond
                                              [(honu-field-decl? d) (honu-field-decl-name d)]
                                              [else #f]))
                                          (tenv-type-members type-defn))
                              (map (lambda (t)
                                     (get-field-names-for-type tenv t))
                                   (tenv-type-supers type-defn)))))))
                 
  (provide get-method-names-for-type)
  (define (get-method-names-for-type tenv t)
    (if (honu-iface-top-type? t) '()
        (let ([type-defn (get-type-entry (honu-iface-type-name t) tenv)])
          (apply append (cons (filter-map (lambda (d)
                                            (cond
                                              [(honu-method-decl? d) (honu-method-decl-name d)]
                                              [else #f]))
                                          (tenv-type-members type-defn))
                              (map (lambda (t)
                                     (get-method-names-for-type tenv t))
                                   (tenv-type-supers type-defn)))))))

  (provide get-fields-and-methods)
  (define (get-fields-and-methods tenv t)
    (if (honu-iface-top-type? t) '() ; Any has no inherent fields or methods.
        (let ([type-defn (get-type-entry (honu-iface-type-name t) tenv)])
          (apply append
                 (cons (append (map (lambda (d)
                                      (cons (honu-field-decl-name d)
                                            (honu-field-decl-type d)))
                                    (filter honu-field-decl?
                                            (tenv-type-members type-defn)))
                               (map (lambda (d)
                                      (cons (honu-method-decl-name d)
                                            (honu-dispatch-type-from-exp
                                             (list t)
                                             (honu-method-decl-arg-types d)
                                             (honu-method-decl-type d)
                                             d)))
                                    (filter honu-method-decl?
                                            (tenv-type-members type-defn))))
                       (map (lambda (t)
                              (get-fields-and-methods tenv t))
                            (tenv-type-supers type-defn)))))))             
  
  (define (get-function-names tenv)
    (filter values
            (tenv-map tenv (lambda (k v)
                             (if (tenv-func? v) k #f)))))
  
  (provide get-initial-env)
  (define (get-initial-env tenv)
    (let ([function-names (get-function-names tenv)])
      (fold (lambda (f e)
              (let ([func-entry (get-func-entry f tenv)])
                (extend-env e
                            f
                            (honu-func-type-from-exp (tenv-func-arg-types func-entry)
                                                     (tenv-func-return-type func-entry)
                                                     (tenv-entry-src-stx func-entry)))))
            (empty-env)
            function-names)))
  
  (provide empty-env extend-env)
  
  (define (empty-env) (lambda (id) #f))
  
  (define (extend-env env name val)
    (lambda (id) 
      (if (eqv? (printable-key name) (printable-key id)) val (env id))))

  )
