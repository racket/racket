(module translate-expression mzscheme

  (require (lib "contract.ss")
           (lib "plt-match.ss")
           "../../ast.ss"
           "../../readerr.ss"
           "../../tenv.ss"
           "../typechecker/type-utils.ss"
           "translate-utils.ss")

  (provide/contract [translate-expression (ast:expr?
                                           . -> .
                                           (syntax/c any/c))])
  (define (translate-expression expr)
    (match expr
      [(struct ast:expr:literal (stx _ value))
       (at stx value)]
      [(struct ast:expr:var (stx name))
       (at-ctxt name)]
      [(struct ast:expr:tuple (stx args))
       ;; list is a bindable name in Honu, so... we use list*, which isn't.
       (at stx `(list* ,@(map translate-expression args) ()))]
      [(struct ast:expr:function (stx _ formals body))
       (translate-function stx #f formals (translate-expression body))]
      [(struct ast:expr:apply (stx func arg))
       (match func
         [(struct ast:expr:member (stx 'my _ name #t))
          (at stx (translate-static-method name (translate-expression arg)))]
         [(struct ast:expr:member (stx obj elab name #t))
          (at stx `(honu:send ,(translate-expression obj)
                              ,(translate-method-name elab name)
                              ,(translate-expression arg)))]
         [else 
          (at stx `(,(translate-expression func)
                    ,(translate-expression arg)))])]
      [(struct ast:expr:tuple/select (stx slot arg))
       (at stx `(list-ref ,(translate-expression arg)
                          (- ,slot 1)))]
      [(struct ast:expr:if (stx test then else))
       (if else
           (at stx `(if ,(translate-expression test)
                        ,(translate-expression then)
                        ,(translate-expression else)))
           (at stx `(if ,(translate-expression test)
                        ,(translate-expression then)
                        ,void-value)))]
      [(struct ast:expr:cond (stx clauses else))
       (if else
           (at stx `(cond ,@(map (lambda (c)
                                   `(,(translate-expression (ast:cond/clause-test c))
                                     ,(translate-expression (ast:cond/clause-result  c))))
                                 clauses)
                          (else ,(translate-expression else))))
           (at stx `(cond ,@(map (lambda (c)
                                   `(,(translate-expression (ast:cond/clause-test c))
                                     ,(translate-expression (ast:cond/clause-result  c))))
                                 clauses)
                          (else ,void-value))))]
      [(struct ast:expr:unary/op (stx op op-stx op-type arg))
       (case op
         [(not)
          (at stx
              `(,(at op-stx 'not) ,(translate-expression arg)))]
         [(minus)
          (at stx
              `(,(at op-stx '-) ,(translate-expression arg)))]
         [else (raise-read-error-with-stx
                "Haven't translated unary operator yet."
                op-stx)])]
      [(struct ast:expr:binary/op (stx op op-stx op-type larg rarg))
       (case op
         [(equal)
          (if (and (ast:type:primitive? op-type)
                   (eqv? (ast:type:primitive-name op-type) 'string))
              (at stx
                  `(,(at op-stx 'string=?)
                    ,(translate-expression larg)
                    ,(translate-expression rarg)))
              (at stx
                  `(,(at op-stx 'eqv?)
                    ,(translate-expression larg)
                    ,(translate-expression rarg))))]
         [(neq)
          (if (and (ast:type:primitive? op-type)
                   (eqv? (ast:type:primitive-name op-type) 'string))
              (at stx
                  `(,(at op-stx 'not)
                    (,(at op-stx 'string=?)
                     ,(translate-expression larg)
                     ,(translate-expression rarg))))
              (at stx
                  `(,(at op-stx 'not)
                    (,(at op-stx 'eqv?)
                     ,(translate-expression larg)
                     ,(translate-expression rarg)))))]
         [(clseq)
          (at stx
              `(,(at op-stx 'equal?)
                ,(translate-expression larg)
                ,(translate-expression rarg)))]
         [(and)
          (at stx
              `(,(at op-stx 'and)
                ,(translate-expression larg)
                ,(translate-expression rarg)))]
         [(or)
          (at stx
              `(,(at op-stx 'or)
                ,(translate-expression larg)
                ,(translate-expression rarg)))]
         [(lt)
          (case (ast:type:primitive-name op-type)
            [(int float)
             (at stx
                 `(,(at op-stx '<)
                   ,(translate-expression larg)
                   ,(translate-expression rarg)))]
            [(string)
             (at stx
                 `(,(at op-stx 'string<?)
                   ,(translate-expression larg)
                   ,(translate-expression rarg)))]
            [(char)
             (at stx
                 `(,(at op-stx 'char<?)
                   ,(translate-expression larg)
                   ,(translate-expression rarg)))])]
         [(le)
          (case (ast:type:primitive-name op-type)
            [(int float)
             (at stx
                 `(,(at op-stx '<=)
                   ,(translate-expression larg)
                   ,(translate-expression rarg)))]
            [(string)
             (at stx
                 `(,(at op-stx 'string<=?)
                   ,(translate-expression larg)
                   ,(translate-expression rarg)))]
            [(char)
             (at stx
                 `(,(at op-stx 'char<=?)
                   ,(translate-expression larg)
                   ,(translate-expression rarg)))])]
         [(gt)
          (case (ast:type:primitive-name op-type)
            [(int float)
             (at stx
                 `(,(at op-stx '>)
                   ,(translate-expression larg)
                   ,(translate-expression rarg)))]
            [(string)
             (at stx
                 `(,(at op-stx 'string>?)
                   ,(translate-expression larg)
                   ,(translate-expression rarg)))]
            [(char)
             (at stx
                 `(,(at op-stx 'char>?)
                   ,(translate-expression larg)
                   ,(translate-expression rarg)))])]
         [(ge)
          (case (ast:type:primitive-name op-type)
            [(int float)
             (at stx
                 `(,(at op-stx '>=)
                   ,(translate-expression larg)
                   ,(translate-expression rarg)))]
            [(string)
             (at stx
                 `(,(at op-stx 'string>=?)
                   ,(translate-expression larg)
                   ,(translate-expression rarg)))]
            [(char)
             (at stx
                 `(,(at op-stx 'char>=?)
                   ,(translate-expression larg)
                   ,(translate-expression rarg)))])]
         [(plus)
          (case (ast:type:primitive-name op-type)
            [(int float)
             (at stx
                 `(,(at op-stx '+)
                   ,(translate-expression larg)
                   ,(translate-expression rarg)))]
            [(string)
             (at stx
                 `(,(at op-stx 'string-append)
                   ,(translate-expression larg)
                   ,(translate-expression rarg)))])]
         [(minus)
          (at stx
              `(,(at op-stx '-)
                ,(translate-expression larg)
                ,(translate-expression rarg)))]
         [(times)
          (at stx
              `(,(at op-stx '*)
                ,(translate-expression larg)
                ,(translate-expression rarg)))]
         [(div)
          (case (ast:type:primitive-name op-type)
            [(int)
             (at stx
                 `(,(at op-stx 'quotient)
                   ,(translate-expression larg)
                   ,(translate-expression rarg)))]
            [(float)
             (at stx
                 `(,(at op-stx '/)
                   ,(translate-expression larg)
                   ,(translate-expression rarg)))])]
         [(mod)
          (at stx
              `(,(at op-stx 'remainder)
                ,(translate-expression larg)
                ,(translate-expression rarg)))]
         [else (raise-read-error-with-stx
                "Haven't translated binary operator yet."
                op-stx)])]
      [(struct ast:expr:return (stx body))
       (at stx
           `(last-k ,(translate-expression body)))]
      [(struct ast:expr:let (stx bindings body))
       (at stx
           `(let*-values ,(map (lambda (b)
                                 (let-values ([(bound-names body)
                                               (translate-binding-clause (ast:defn:binding-names b)
                                                                         (translate-expression (ast:defn:binding-init b)))])
                                   ;; make sure to give the let binding the appropriate syntax,
                                   ;; otherwise errors will highlight the entire let expression.
                                   (at (ast-syntax b) `(,bound-names ,body))))
                               bindings)
              ,(translate-expression body)))]
      [(struct ast:expr:sequence (stx effects value))
       (at stx
           `(begin ,@(map translate-expression effects)
                   ,(translate-expression value)))]
      [(struct ast:expr:while (stx test body))
       (at stx
           `(let loop ()
              (if ,(translate-expression test)
                  (begin ,(translate-expression body) (loop))
                  ,void-value)))]
      [(struct ast:expr:assign (stx lhs rhs))
       (match lhs
         [(struct ast:expr:var (_ _))
          (at stx `(begin (set! ,(translate-expression lhs)
                                ,(translate-expression rhs))
                          ,void-value))]
         [(struct ast:expr:member (mstx 'my _ name method?))
          (if method?
              (raise-read-error-with-stx
               "Left-hand side of assignment cannot be a method name"
               mstx)
              (at stx (translate-static-field-setter name (translate-expression rhs))))]
         [(struct ast:expr:member (mstx obj elab name method?))
          (if method?
              (raise-read-error-with-stx
               "Left-hand side of assignment cannot be a method name"
               mstx)
              (at stx `(honu:send ,(translate-expression obj)
                                  ,(translate-field-setter-name elab name)
                                  ,(translate-expression rhs))))]
         [else
          (raise-read-error-with-stx
           "Left-hand side of assignment is invalid"
           stx)])]
      [(struct ast:expr:member (stx 'my _ name method?))
       (if method?
           (at stx (translate-static-method name))
           (at stx (translate-static-field-getter name)))]
      [(struct ast:expr:member (stx obj elab name method?))
       (if method?
           (at stx `(lambda (args)
                      (honu:send ,(translate-expression obj)
                                 ,(translate-method-name elab name)
                                 args)))
           (at stx `(honu:send ,(translate-expression obj)
                               ,(translate-field-getter-name elab name)
                               ,void-value)))]
      [(struct ast:expr:new (stx class _ args))
       (at stx `(new ,(translate-class-name class)
                     ,@(map (lambda (a)
                              `(,(ast:named/arg-name a)
                                 ,(translate-expression (ast:named/arg-actual a))))
                            args)))]
      [(struct ast:expr:cast (stx obj type))
       (at stx `(let ([cast-obj ,(translate-expression obj)])
                  ;; you can always cast null to an interface type
                  (if (or (is-a? cast-obj null%)
                          (honu:send cast-obj implements? ,(translate-iface-name type)))
                      cast-obj
                      (error (format "Class ~a does not implement ~a"
                                     (honu:send cast-obj format-class-name)
                                     (quote ,(syntax-e (iface-name type))))))))]
      [(struct ast:expr:isa (stx obj type))
       (at stx `(let ([cast-obj ,(translate-expression obj)])
                  ;; null is a member of any interface type
                  (or (is-a? cast-obj null%)
                      (honu:send cast-obj implements? ,(translate-iface-name type)))))]
      [(struct ast:expr:self (stx))
       (at stx 'this)]
      [else (raise-read-error-with-stx
             "Haven't translated that type of expression yet."
             (ast-syntax expr))]))

  )
