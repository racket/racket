(module translate-expression mzscheme

  (require (lib "contract.ss")
           (lib "plt-match.ss")
           "../../ast.ss"
           "../../readerr.ss"
           "../../tenv.ss"
           "../typechecker/type-utils.ss"
           "translate-utils.ss")

  (provide/contract [translate-expression ((union honu:type? false/c) honu:expr?
                                           . -> .
                                           (syntax/c any/c))])
  (define (translate-expression arg-type expr)
    (match expr
      [(struct honu:lit (stx _ value))
       (at stx value)]
      [(struct honu:var (stx name))
       (at-ctxt name)]
      [(struct honu:tuple (stx args))
       ;; list is a bindable name in Honu, so... we use list*, which isn't.
       (at stx `(list* ,@(map (lambda (e)
                                (translate-expression arg-type e))
                              args)
                       ()))]
      [(struct honu:lambda (stx _ formals body))
       (translate-function stx #f formals (translate-expression arg-type body))]
      [(struct honu:call (stx func arg))
       (match func
         [(struct honu:member (stx 'my _ name #t))
          (at stx (translate-static-method arg-type name
                                     (translate-expression arg-type arg)))]
         [(struct honu:member (stx obj elab name #t))
          (at stx `(honu:send ,(translate-expression arg-type obj)
                              ,(translate-method-name elab name)
                              ,(translate-expression arg-type arg)))]
         [else 
          (at stx `(,(translate-expression arg-type func)
                    ,(translate-expression arg-type arg)))])]
      [(struct honu:select (stx slot arg))
       (at stx `(list-ref ,(translate-expression arg-type arg)
                          (- ,slot 1)))]
      [(struct honu:if (stx test then else))
       (if else
           (at stx `(if ,(translate-expression arg-type test)
                        ,(translate-expression arg-type then)
                        ,(translate-expression arg-type else)))
           (at stx `(if ,(translate-expression arg-type test)
                        ,(translate-expression arg-type then)
                        ,void-value)))]
      [(struct honu:cond (stx clauses else))
       (if else
           (at stx `(cond ,@(map (lambda (c)
                                   `(,(translate-expression arg-type (honu:cond-clause-pred c))
                                     ,(translate-expression arg-type (honu:cond-clause-rhs  c))))
                                 clauses)
                          (else ,(translate-expression arg-type else))))
           (at stx `(cond ,@(map (lambda (c)
                                   `(,(translate-expression arg-type (honu:cond-clause-pred c))
                                     ,(translate-expression arg-type (honu:cond-clause-rhs  c))))
                                 clauses)
                          (else ,void-value))))]
      [(struct honu:un-op (stx op op-stx op-type arg))
       (case op
         [(not)
          (at stx
              `(,(at op-stx 'not) ,(translate-expression arg-type arg)))]
         [(minus)
          (at stx
              `(,(at op-stx '-) ,(translate-expression arg-type arg)))]
         [else (raise-read-error-with-stx
                "Haven't translated unary operator yet."
                op-stx)])]
      [(struct honu:bin-op (stx op op-stx op-type larg rarg))
       (case op
         [(equal)
          (if (and (honu:type-prim? op-type)
                   (eqv? (honu:type-prim-name op-type) 'string))
              (at stx
                  `(,(at op-stx 'string=?)
                    ,(translate-expression arg-type larg)
                    ,(translate-expression arg-type rarg)))
              (at stx
                  `(,(at op-stx 'eqv?)
                    ,(translate-expression arg-type larg)
                    ,(translate-expression arg-type rarg))))]
         [(neq)
          (if (and (honu:type-prim? op-type)
                   (eqv? (honu:type-prim-name op-type) 'string))
              (at stx
                  `(,(at op-stx 'not)
                    (,(at op-stx 'string=?)
                     ,(translate-expression arg-type larg)
                     ,(translate-expression arg-type rarg))))
              (at stx
                  `(,(at op-stx 'not)
                    (,(at op-stx 'eqv?)
                     ,(translate-expression arg-type larg)
                     ,(translate-expression arg-type rarg)))))]
         [(clseq)
          (at stx
              `(,(at op-stx 'equal?)
                ,(translate-expression arg-type larg)
                ,(translate-expression arg-type rarg)))]
         [(and)
          (at stx
              `(,(at op-stx 'and)
                ,(translate-expression arg-type larg)
                ,(translate-expression arg-type rarg)))]
         [(or)
          (at stx
              `(,(at op-stx 'or)
                ,(translate-expression arg-type larg)
                ,(translate-expression arg-type rarg)))]
         [(lt)
          (case (honu:type-prim-name op-type)
            [(int float)
             (at stx
                 `(,(at op-stx '<)
                   ,(translate-expression arg-type larg)
                   ,(translate-expression arg-type rarg)))]
            [(string)
             (at stx
                 `(,(at op-stx 'string<?)
                   ,(translate-expression arg-type larg)
                   ,(translate-expression arg-type rarg)))]
            [(char)
             (at stx
                 `(,(at op-stx 'char<?)
                   ,(translate-expression arg-type larg)
                   ,(translate-expression arg-type rarg)))])]
         [(le)
          (case (honu:type-prim-name op-type)
            [(int float)
             (at stx
                 `(,(at op-stx '<=)
                   ,(translate-expression arg-type larg)
                   ,(translate-expression arg-type rarg)))]
            [(string)
             (at stx
                 `(,(at op-stx 'string<=?)
                   ,(translate-expression arg-type larg)
                   ,(translate-expression arg-type rarg)))]
            [(char)
             (at stx
                 `(,(at op-stx 'char<=?)
                   ,(translate-expression arg-type larg)
                   ,(translate-expression arg-type rarg)))])]
         [(gt)
          (case (honu:type-prim-name op-type)
            [(int float)
             (at stx
                 `(,(at op-stx '>)
                   ,(translate-expression arg-type larg)
                   ,(translate-expression arg-type rarg)))]
            [(string)
             (at stx
                 `(,(at op-stx 'string>?)
                   ,(translate-expression arg-type larg)
                   ,(translate-expression arg-type rarg)))]
            [(char)
             (at stx
                 `(,(at op-stx 'char>?)
                   ,(translate-expression arg-type larg)
                   ,(translate-expression arg-type rarg)))])]
         [(ge)
          (case (honu:type-prim-name op-type)
            [(int float)
             (at stx
                 `(,(at op-stx '>=)
                   ,(translate-expression arg-type larg)
                   ,(translate-expression arg-type rarg)))]
            [(string)
             (at stx
                 `(,(at op-stx 'string>=?)
                   ,(translate-expression arg-type larg)
                   ,(translate-expression arg-type rarg)))]
            [(char)
             (at stx
                 `(,(at op-stx 'char>=?)
                   ,(translate-expression arg-type larg)
                   ,(translate-expression arg-type rarg)))])]
         [(plus)
          (case (honu:type-prim-name op-type)
            [(int float)
             (at stx
                 `(,(at op-stx '+)
                   ,(translate-expression arg-type larg)
                   ,(translate-expression arg-type rarg)))]
            [(string)
             (at stx
                 `(,(at op-stx 'string-append)
                   ,(translate-expression arg-type larg)
                   ,(translate-expression arg-type rarg)))])]
         [(minus)
          (at stx
              `(,(at op-stx '-)
                ,(translate-expression arg-type larg)
                ,(translate-expression arg-type rarg)))]
         [(times)
          (at stx
              `(,(at op-stx '*)
                ,(translate-expression arg-type larg)
                ,(translate-expression arg-type rarg)))]
         [(div)
          (case (honu:type-prim-name op-type)
            [(int)
             (at stx
                 `(,(at op-stx 'quotient)
                   ,(translate-expression arg-type larg)
                   ,(translate-expression arg-type rarg)))]
            [(float)
             (at stx
                 `(,(at op-stx '/)
                   ,(translate-expression arg-type larg)
                   ,(translate-expression arg-type rarg)))])]
         [(mod)
          (at stx
              `(,(at op-stx 'remainder)
                ,(translate-expression arg-type larg)
                ,(translate-expression arg-type rarg)))]
         [else (raise-read-error-with-stx
                "Haven't translated binary operator yet."
                op-stx)])]
      [(struct honu:return (stx body))
       (at stx
           `(last-k ,(translate-expression arg-type body)))]
      [(struct honu:let (stx bindings body))
       (at stx
           `(let*-values ,(map (lambda (b)
                                 (let-values ([(bound-names body)
                                               (translate-binding-clause (honu:binding-names b)
                                                                         (translate-expression arg-type (honu:binding-value b)))])
                                   ;; make sure to give the let binding the appropriate syntax,
                                   ;; otherwise errors will highlight the entire let expression.
                                   (at (honu:ast-stx b) `(,bound-names ,body))))
                               bindings)
              ,(translate-expression arg-type body)))]
      [(struct honu:seq (stx effects value))
       (at stx
           `(begin ,@(map (lambda (e)
                            (translate-expression arg-type e))
                          effects)
                   ,(translate-expression arg-type value)))]
      [(struct honu:while (stx test body))
       (at stx
           `(let loop ()
              (if ,(translate-expression arg-type test)
                  (begin ,(translate-expression arg-type body) (loop))
                  ,void-value)))]
      [(struct honu:assn (stx lhs rhs))
       (match lhs
         [(struct honu:var (_ _))
          (at stx `(begin (set! ,(translate-expression arg-type lhs)
                                ,(translate-expression arg-type rhs))
                          ,void-value))]
         [(struct honu:member (mstx 'my _ name method?))
          (if method?
              (raise-read-error-with-stx
               "Left-hand side of assignment cannot be a method name"
               mstx)
              (at stx (translate-static-field-setter arg-type name
                                               (translate-expression arg-type rhs))))]
         [(struct honu:member (mstx obj elab name method?))
          (if method?
              (raise-read-error-with-stx
               "Left-hand side of assignment cannot be a method name"
               mstx)
              (at stx `(honu:send ,(translate-expression arg-type obj)
                                  ,(translate-field-setter-name elab name)
                                  ,(translate-expression arg-type rhs))))]
         [else
          (raise-read-error-with-stx
           "Left-hand side of assignment is invalid"
           stx)])]
      [(struct honu:member (stx 'my _ name method?))
       (if method?
           (at stx (translate-static-method arg-type name))
           (at stx (translate-static-field-getter arg-type name)))]
      [(struct honu:member (stx obj elab name method?))
       (if method?
           (at stx `(lambda (args)
                      (honu:send ,(translate-expression arg-type obj)
                                 ,(translate-method-name elab name)
                                 args)))
           (at stx `(honu:send ,(translate-expression arg-type obj)
                               ,(translate-field-getter-name elab name)
                               ,void-value)))]
      [(struct honu:new (stx class _ args))
       (at stx `(new ,(translate-class-name class)
                     ,@(map (lambda (a)
                              `(,(honu:name-arg-name a)
                                 ,(translate-expression arg-type (honu:name-arg-value a))))
                            args)))]
      [(struct honu:cast (stx obj type))
       (at stx `(let ([cast-obj ,(translate-expression arg-type obj)])
                  ;; you can always cast null to an interface type
                  (if (or (is-a? cast-obj null%)
                          (is-a? cast-obj ,(translate-iface-name type)))
                      cast-obj
                      ;; we can use object-info and class-info since we always set (inspect #f)
                      ;; we have to do that for the moment anyway for "extensional" class equality.
                      (let*-values ([(class dc-1) (object-info cast-obj)]
                                    [(class-name dc-1 dc-2 dc-3 dc-4 dc-5 dc-6) (class-info class)])
                        (error (format "Class ~a does not implement ~a" 
                                       (let ([class-string (symbol->string class-name)])
                                         (string->symbol (substring class-string 0 (- (string-length class-string) 1))))
                                       (quote ,(printable-type type))))))))]
      [(struct honu:isa (stx obj type))
       (at stx `(let ([cast-obj ,(translate-expression arg-type obj)])
                  ;; null is a member of any interface type
                  (or (is-a? cast-obj null%)
                      (is-a? cast-obj ,(translate-iface-name type)))))]
      [(struct honu:this (stx))
       (at stx 'this)]
      [else (raise-read-error-with-stx
             "Haven't translated that type of expression yet."
             (honu:ast-stx expr))]))

  )
