(module translate-utils mzscheme

  (require (all-except (lib "list.ss" "srfi" "1") any)
           (lib "contract.ss")
           "../../ast.ss"
           "../../tenv.ss")

  (provide current-compile-context)
  (define current-compile-context (make-parameter #f))

  (define stx-for-original-property (read-syntax #f (open-input-string "original")))

  (provide/contract [at      ((union (syntax/c any/c) false/c)
                              any/c
                              . -> .
                              (syntax/c any/c))]
                    [at-ctxt ((syntax/c any/c)       . -> . (syntax/c any/c))])
  (define (at stx expr)
    (datum->syntax-object (current-compile-context) expr stx stx-for-original-property))
  (define (at-ctxt stx)
    (datum->syntax-object (current-compile-context) (syntax-e stx) stx stx-for-original-property))

  (provide void-value)
  (define void-value '())
  
  (provide translate-function)
  (define (translate-function stx name args body)
    (define (wrapping-syntax arg body)
      (if name
          (at stx `(define (,(at-ctxt name) ,arg)
                     (let/ec last-k ,body)))
          (at stx `(lambda (,arg)
                     (let/ec last-k ,body)))))
    (if (= (length args) 1)
        (wrapping-syntax (at-ctxt (honu:formal-name (car args)))
                         body)
        (wrapping-syntax (at #f 'arg-tuple)
                         `(let-values ([,(map (lambda (a)
                                                (at-ctxt (honu:formal-name a)))
                                              args)
                                        (apply values ,(at #f 'arg-tuple))])
                            ,body))))
  
  (provide translate-binding-clause)
  (define (translate-binding-clause names value)
    (define (grab-indices names)
      (let loop ([names names]
                 [n     0]
                 [ret   '()])
        (cond
          [(null? names)
           (reverse ret)]
          [(car names)
           (loop (cdr names)
                 (+ n 1)
                 (cons `(list-ref arg-tuple ,n) ret))]
          [else
           (loop (cdr names)
                 (+ n 1)
                 ret)])))
    (values (filter (lambda (n) n) names)
            `(let ([arg-tuple ,(if (= (length names) 1)
                                   `(list* ,value ())
                                   value)])
               ,(cons 'values (grab-indices names)))))
  
  (provide translate-iface-name translate-class-name translate-method-name
           translate-field-getter-name translate-field-setter-name)
  (define (translate-iface-name type)
    (let ([name (if (honu:type-iface-top? type)
                    (datum->syntax-object #f 'Any #f)
                    (honu:type-iface-name type))])
      (at name (string->symbol (string-append (symbol->string (syntax-e name)) "<%>")))))
  
  (define (translate-class-name class)
    (at class (string->symbol (string-append (symbol->string (syntax-e class)) "%"))))
  
  (define (translate-method-name type name)
    (at name (string->symbol (string-append (symbol->string (syntax-e (translate-iface-name type)))
                                            "-"
                                            (symbol->string (syntax-e name))))))
  
  (define (translate-field-getter-name type name)
    (at name (string->symbol (string-append (symbol->string (syntax-e (translate-iface-name type)))
                                            "-"
                                            (symbol->string (syntax-e name))
                                            "-get"))))

  (define (translate-field-setter-name type name)
    (at name (string->symbol (string-append (symbol->string (syntax-e (translate-iface-name type)))
                                            "-"
                                            (symbol->string (syntax-e name))
                                            "-set!"))))

  (provide translate-static-method translate-static-field-getter translate-static-field-setter)
  (define (translate-static-method tenv arg-type name arg)
    (if arg-type
        (let ([type-entry (get-type-entry tenv arg-type)])
          (if (s:member name
                        (map tenv:member-name (append (tenv:type-members type-entry)
                                                      (tenv:type-inherited type-entry)))
                        tenv-key=?)
              (if arg
                  `(super ,(translate-method-name arg-type name) ,arg)
                  `(lambda (arg-tuple)
                     (super ,(translate-method-name arg-type name) arg-tuple)))
              (if arg
                  `(,(at-ctxt name) ,arg)
                  (at-ctxt name))))
        (if arg
            `(,(at-ctxt name) ,arg)
            (at-ctxt name))))
  
  (define (translate-static-field-getter tenv arg-type name)
    (if arg-type
        (let ([type-entry (get-type-entry tenv arg-type)])
          (if (s:member name
                        (map tenv:member-name (append (tenv:type-members type-entry)
                                                      (tenv:type-inherited type-entry)))
                        tenv-key=?)
              `(super ,(translate-field-getter-name arg-type name) ,void-value)
              (at-ctxt name)))
        (at-ctxt name)))
  
  (define (translate-static-field-setter tenv arg-type name arg)
    (if arg-type
        (let ([type-entry (get-type-entry tenv arg-type)])
          (if (s:member name
                        (map tenv:member-name (append (tenv:type-members type-entry)
                                                      (tenv:type-inherited type-entry)))
                        tenv-key=?)
              `(super ,(translate-field-setter-name arg-type name) ,arg)
              `(begin (set! ,(at-ctxt name) ,arg)
                      ,void-value)))
        `(begin (set! ,(at-ctxt name) ,arg)
                ,void-value))) 

  )
