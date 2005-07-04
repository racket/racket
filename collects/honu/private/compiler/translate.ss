(module translate mzscheme
  
  (require (all-except (lib "list.ss" "srfi" "1") any)
           (lib "contract.ss")
           (lib "plt-match.ss")
           "../../ast.ss"
           "../../readerr.ss"
           "../../tenv.ss"
           "../typechecker/type-utils.ss"
           "translate-class-utils.ss"
           "translate-expression.ss"
           "translate-utils.ss")
  
  (provide/contract [translate ((listof honu:defn?)
                                . -> .
                                (listof (syntax/c any/c)))]
                    [translate-defn (honu:defn?
                                     . -> .
                                     (syntax/c any/c))])
  (define (translate defns)
    (let loop ([defns-to-go defns]
               [syntaxes    '()])
      (cond
        [(null? defns-to-go) (reverse syntaxes)]
        [(honu:mixin? (car defns-to-go))
         (loop (cdr defns-to-go) syntaxes)]
        [(honu:subclass? (car defns-to-go))
         (let ([mixin (find (lambda (d)
                              (and (honu:mixin? d) 
                                   (tenv-key=? (honu:mixin-name d)
                                               (honu:subclass-mixin (car defns-to-go)))))
                            defns)])
           (loop (cdr defns-to-go) (cons (translate-subclass mixin (car defns-to-go)) syntaxes)))]
        [else
         (loop (cdr defns-to-go) (cons (translate-defn (car defns-to-go)) syntaxes))])))
  
  (define (translate-member-names name)
    (let* ([iface      (make-iface-type name name)]
           [type-entry (get-type-entry iface)])
      (let loop ([members (append (tenv:type-members type-entry) (tenv:type-inherited type-entry))]
                 [names   '()])
        (if (null? members)
            (reverse names)
            (if (honu:type-disp? (tenv:member-type (car members)))
                (loop (cdr members)
                      (cons (translate-method-name iface (tenv:member-name (car members)))
                            names))
                (loop (cdr members)
                      (cons (translate-field-setter-name iface (tenv:member-name (car members)))
                            (cons (translate-field-getter-name iface (tenv:member-name (car members)))
                                  names))))))))
                                                         
  (define (translate-defn defn)
    (match defn
      [(struct honu:bind-top (stx names _ value))
       (let-values ([(bound-names body) (translate-binding-clause names (translate-expression #f value))])
         (at stx `(define-values ,bound-names ,body)))]
      [(struct honu:function (stx name _ args body))
       (translate-function stx name args (translate-expression #f body))]
      [(struct honu:iface (stx name supers members))
       (at stx `(define ,(translate-iface-name (make-iface-type name name))
                  (interface ,(if (null? supers)
                                  (list (translate-iface-name (make-any-type #f)))
                                  (map translate-iface-name supers))
                    ,@(translate-member-names name))))]
      [(struct honu:class (stx name _ _ impls inits members exports))
       (at stx `(define ,(translate-class-name name)
                  (class* object% ,(map translate-iface-name impls)
                    (inspect #f)
                    ,(translate-inits inits)
                    ,@(map (lambda (m)
                             (translate-member #f m)) members)
                    ,@(translate-class-exports exports)
                    ,(translate-formatter name members #f)
                    (super-new))))]
      [else (raise-read-error-with-stx
             "Haven't translated that type of definition yet."
             (honu:ast-stx defn))]))
  
  (define (translate-subclass mixin-defn defn)
    (match (list mixin-defn defn)
      [(list (struct honu:mixin (mstx mname _ arg-type _ impls inits _ super-new members-before members-after exports))
             (struct honu:subclass (stx name base mixin)))
       (let* ([base-entry (get-class-entry base)]
              [base-types (cons (tenv:class-sub-type base-entry)
                                (tenv:class-impls base-entry))])
         (at stx `(define ,(translate-class-name name)
                    (class* ,(translate-class-name base) ,(map translate-iface-name impls)
                      (inspect #f)
                      ,(translate-inits inits)
                      ,@(map (lambda (m)
                               (translate-member arg-type m)) members-before)
                      ,(translate-super-new arg-type super-new)
                      ,@(map (lambda (m)
                               (translate-member arg-type m)) members-after)
                      ,@(translate-subclass-exports base-types arg-type exports)
                      ,(translate-formatter name (append members-before members-after) arg-type)))))]))
  
  (define (translate-formatter name members arg-type)
    (let ([right-define (if arg-type 'define/override 'define/public)])
      `(,right-define (format-class renderer indent print-fields?)
         (if print-fields?
             (format "~a {~a}" 
                     (quote ,(syntax-e name))
                     ,(cons 'string-append
                            (let ([printable-members (filter (lambda (m)
                                                               (not (honu:method? m)))
                                                             members)]
                                  [printable-smembers (if arg-type
                                                          (filter-map (lambda (m)
                                                                        (if (not (honu:type-disp? (tenv:member-type m)))
                                                                            (tenv:member-name m)
                                                                            #f))
                                                                      (tenv:type-members (get-type-entry arg-type)))
                                                          '())]
                                  ;; how much more do we want the members indented?  Let's try 2 spaces more.
                                  [indent-delta 2])
                              (if (and (null? printable-members)
                                       (null? printable-smembers))
                                  '("")
                                  (fold-right (lambda (m l)
                                                (list* "\n" (translate-super-member-formatter arg-type m indent-delta) l))
                                              (fold-right (lambda (m l)
                                                            (list* "\n" (translate-member-formatter m indent-delta) l))
                                                          '("\n" (make-string indent #\space))
                                                          printable-members)
                                              printable-smembers)))))
             (format "~a" (quote ,(syntax-e name)))))))
  
  (define (translate-member-formatter member indent-delta)
    (let ([name (if (honu:field? member)
                    (honu:field-name member)
                    (honu:init-field-name member))])
      `(format "~a~a = ~a;"
               (make-string (+ indent ,indent-delta) #\space)
               (quote ,(syntax-e name))
               ;; the 3 is for " = "
               (renderer ,name (+ indent ,(+ indent-delta (string-length (symbol->string (syntax-e name))) 3))))))
  
  (define (translate-super-member-formatter arg-type name indent-delta)
    `(format "~a~a = ~a;"
             (make-string (+ indent ,indent-delta) #\space)
             (quote ,(syntax-e name))
             ;; as before, the 3 is for " = "
             (renderer ,(translate-static-field-getter arg-type name)
                       (+ indent ,(+ indent-delta (string-length (symbol->string (syntax-e name))) 3)))))
  )