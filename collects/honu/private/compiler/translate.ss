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
           "translate-parameters.ss"
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
  
  (define (translate-iface-member-names name)
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

  (define (translate-iface-member-types members)
    (define (get-member-type-list m)
      (match m
        [(struct honu:field-decl (_ _ type))
         (list (translate-type-for-syntax type))]
        [(struct honu:method-decl (_ _ type arg-types))
         (cons (translate-type-for-syntax type)
               (map translate-type-for-syntax arg-types))]))
    (apply append (map get-member-type-list members)))
  
  (define (translate-defn defn)
    (match defn
      [(struct honu:bind-top (stx names types value))
       (let-values ([(bound-names body) (translate-binding-clause names (translate-expression value))])
         (at stx `(begin (honu:type ,@(map translate-type-for-syntax types))
                         (define-values ,bound-names ,body))))]
      [(struct honu:function (stx name type args body))
       (at stx `(begin (honu:type ,(translate-type-for-syntax type))
                       (honu:type ,@(map (lambda (a) (translate-type-for-syntax (honu:formal-type a))) args))
                       ,(translate-function stx name args (translate-expression body))))]
      [(struct honu:iface (stx name supers members))
       (at stx `(begin
                  (define ,(translate-iface-name (make-iface-type name name))
                    (interface ,(if (null? supers)
                                    (list (translate-iface-name (make-any-type #f)))
                                    (map translate-iface-name supers))
                      ,@(translate-iface-member-names name)))
                  (honu:type ,@(translate-iface-member-types members))))]
      [(struct honu:class (stx name selftype _ impls inits members exports))
       (at stx `(define ,(translate-class-name name)
                  (class* object% ,(map translate-iface-name impls)
                    (honu:type ,(translate-type-for-syntax selftype))
                    (inspect #f)
                    ,(translate-inits inits)
                    ,@(map translate-member members)
                    ,@(translate-class-exports exports)
                    ,(translate-impl-method impls)
                    ,(translate-formatter name members)
                    (super-new))))]
      [else (raise-read-error-with-stx
             "Haven't translated that type of definition yet."
             (honu:ast-stx defn))]))
  
  (define (translate-subclass mixin-defn defn)
    (match (list mixin-defn defn)
      [(list (struct honu:mixin (mstx mname selftype arg-type _ impls inits withs super-new members-before members-after exports))
             (struct honu:subclass (stx name base mixin)))
       (parameterize ([current-mixin-argument-type arg-type])
         (let* ([base-entry (get-class-entry base)]
                [base-types (cons (tenv:class-sub-type base-entry)
                                  (tenv:class-impls base-entry))])
           (at stx `(define ,(translate-class-name name)
                      (class* ,(translate-class-name base) ,(map translate-iface-name impls)
                        (honu:type ,(translate-type-for-syntax selftype))
                        (honu:type ,@(map (lambda (w) (translate-type-for-syntax (honu:formal-type w))) withs))
                        (inspect #f)
                        ,(translate-inits inits)
                        ,@(map translate-member members-before)
                        ,(translate-super-new super-new)
                        ,@(map translate-member members-after)
                        ,@(translate-subclass-exports base-types exports)
                        ,(translate-impl-method impls)
                        ,(translate-formatter name (append members-before members-after)))))))]))

  (define (translate-impl-method impls)
    (let ([right-define (if (current-mixin-argument-type) 'define/override 'define/public)])
      `(,right-define (implements? iface)
         (mz:ormap (lambda (i)
                     (interface-extension? i iface))
                   (list* ,@(map translate-iface-name impls) '())))))
  
  (define (translate-formatter name members)
    (let ([right-define (if (current-mixin-argument-type) 'define/override 'define/public)])
      `(begin
         (,right-define (format-class-name)
           ,(format "~a" (syntax-e name)))
         (,right-define (format-class renderer indent)
           (format "~a {~a}" 
                   (format-class-name)
                   ,(cons 'string-append
                          (let ([printable-members (filter (lambda (m)
                                                             (not (honu:method? m)))
                                                           members)]
                                [printable-smembers (if (current-mixin-argument-type)
                                                        (filter-map (lambda (m)
                                                                      (if (not (honu:type-disp? (tenv:member-type m)))
                                                                          (tenv:member-name m)
                                                                          #f))
                                                                    (tenv:type-members (get-type-entry (current-mixin-argument-type))))
                                                        '())]
                                ;; how much more do we want the members indented?  Let's try 2 spaces more.
                                [indent-delta 2])
                            (if (and (null? printable-members)
                                     (null? printable-smembers))
                                '("")
                                (fold-right (lambda (m l)
                                              (list* "\n" (translate-super-member-formatter m indent-delta) l))
                                            (fold-right (lambda (m l)
                                                          (list* "\n" (translate-member-formatter m indent-delta) l))
                                                        '("\n" (make-string indent #\space))
                                                        printable-members)
                                            printable-smembers)))))))))
  
  (define (translate-member-formatter member indent-delta)
    (let ([name (if (honu:field? member)
                    (honu:field-name member)
                    (honu:init-field-name member))])
      `(format "~a~a = ~a;"
               (make-string (+ indent ,indent-delta) #\space)
               (quote ,(syntax-e name))
               ;; the 3 is for " = "
               (renderer ,name (+ indent ,(+ indent-delta (string-length (symbol->string (syntax-e name))) 3))))))
  
  (define (translate-super-member-formatter name indent-delta)
    `(format "~a~a = ~a;"
             (make-string (+ indent ,indent-delta) #\space)
             (quote ,(syntax-e name))
             ;; as before, the 3 is for " = "
             (renderer ,(translate-static-field-getter name)
                       (+ indent ,(+ indent-delta (string-length (symbol->string (syntax-e name))) 3)))))
  )
