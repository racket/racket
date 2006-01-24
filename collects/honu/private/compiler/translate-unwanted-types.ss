(module translate-unwanted-types mzscheme
  
  (require (lib "list.ss" "srfi" "1")
           (lib "plt-match.ss")
           "../../ast.ss"
           "translate-utils.ss")
  
  (provide build-unwanted-type-syntax)
  (define (build-unwanted-type-syntax defns)
    (map build-unwanted-type-syntax-defn defns))
  
  ;; since we're never going to run the result anyway, it doesn't matter
  ;; how we build things -- no need to flatten.
  (define (build-unwanted-type-syntax-defn defn)
    (match defn
      [(struct ast:defn:binding (_ _ types value))
       (cons (build-unwanted-type-syntax-expression value)
             ;; remember to filter out the top types used whenever _ appears
             (map translate-type-for-syntax (filter (lambda (t) (not (ast:type:top? t))) types)))]
      [(struct ast:defn:function (_ _ type formals body))
       (list (translate-type-for-syntax type)
             (build-unwanted-type-syntax-expression body)
             (map (lambda (f)
                    (translate-type-for-syntax (ast:formal-type f)))
                  formals))]
      [(struct ast:defn:iface (_ _ _ members))
       (map build-unwanted-type-syntax-member-decl members)]
      [(struct ast:defn:class (_ _ selftype _ _ inits members exports))
       (list (translate-type-for-syntax selftype)
             (map (lambda (f)
                    (translate-type-for-syntax (ast:formal-type f)))
                  inits)
             (map build-unwanted-type-syntax-member members)
             (map (lambda (e)
                    (translate-type-for-syntax (ast:export-type e)))
                  exports))]
      [(struct ast:defn:mixin (_ _ selftype arg-type _ _ inits withs super-new
                           members-before members-after exports))
       (list (translate-type-for-syntax selftype)
             (translate-type-for-syntax arg-type)
             (map (lambda (f)
                    (translate-type-for-syntax (ast:formal-type f)))
                  inits)
             (map (lambda (f)
                    (translate-type-for-syntax (ast:formal-type f)))
                   withs)
             (map (lambda (a)
                    (build-unwanted-type-syntax-expression (ast:named/arg-actual a)))
                  (ast:super-new-args super-new))
             (map build-unwanted-type-syntax-member members-before)
             (map build-unwanted-type-syntax-member members-after)
             (map (lambda (e)
                    (translate-type-for-syntax (ast:export-type e)))
                  exports))]
      [(struct ast:defn:subclass (_ _ _ mixin))
       ;; okay, this isn't a type, but we still want to see it as a use
       ;; until we can translate mixins correctly.
       (list (translate-mixin-name mixin))]))
  
  (define (build-unwanted-type-syntax-member-decl member)
    (match member
      [(struct ast:iface/member:field (_ _ type))
       (translate-type-for-syntax type)]
      [(struct ast:iface/member:method (_ _ type arg-types))
       (list (translate-type-for-syntax type)
             (map translate-type-for-syntax arg-types))]))
        
  (define (build-unwanted-type-syntax-member member)
    (match member
      [(struct ast:class/member:field/formal (_ _ type value))
       (list (translate-type-for-syntax type)
             (if value (build-unwanted-type-syntax-expression value) '()))]
      [(struct ast:class/member:field (_ _ type value))
       (list (translate-type-for-syntax type)
             (build-unwanted-type-syntax-expression value))]
      [(struct ast:class/member:method (_ _ type formals body))
       (list (translate-type-for-syntax type)
             (map (lambda (f)
                    (translate-type-for-syntax (ast:formal-type f)))
                  formals)
             (build-unwanted-type-syntax-expression body))]))

  (define (build-unwanted-type-syntax-expression expr)
    (match expr
      [(struct ast:expr:function (_ type formals body))
       (list (translate-type-for-syntax type)
             (map (lambda (f)
                    (translate-type-for-syntax (ast:formal-type f)))
                  formals)
             (build-unwanted-type-syntax-expression body))]
      [(struct ast:expr:let (_ bindings body))
       (list (map (lambda (b)
                    ;; again, make sure to remove types corresponding to _
                    (list (map translate-type-for-syntax (filter (lambda (t)
                                                                   (not (ast:type:top? t)))
                                                                 (ast:defn:binding-types b)))
                          (build-unwanted-type-syntax-expression (ast:defn:binding-init b))))
                  bindings)
             (build-unwanted-type-syntax-expression body))]
      [(struct ast:expr:sequence (_ effects result))
       (list (map (lambda (e)
                    (build-unwanted-type-syntax-expression e))
                  effects)
             (build-unwanted-type-syntax-expression result))]
      [(struct ast:expr:apply (_ func arg))
       (list (build-unwanted-type-syntax-expression func)
             (build-unwanted-type-syntax-expression arg))]
      [(struct ast:expr:assign (_ lhs rhs))
       (list (build-unwanted-type-syntax-expression lhs)
             (build-unwanted-type-syntax-expression rhs))]
      [(struct ast:expr:return (_ body))
       (build-unwanted-type-syntax-expression body)]
      [(struct ast:expr:tuple/select (_ _ arg))
       (build-unwanted-type-syntax-expression arg)]
      [(struct ast:expr:tuple (_ args))
       (map build-unwanted-type-syntax-expression args)]
      [(struct ast:expr:member (_ obj _ _ _))
       (if (ast:expr? obj)
           (build-unwanted-type-syntax-expression obj)
           (list))]
      [(struct ast:expr:new (_ obj type args))
       (list (build-unwanted-type-syntax-expression obj)
             (translate-type-for-syntax type)
             (map (lambda (a)
                    (build-unwanted-type-syntax-expression (ast:named/arg-actual a)))
                  args))]
      ;; here are the two cases where the type already appears in the compiled code
      [(struct ast:expr:cast (_ obj _))
       (build-unwanted-type-syntax-expression obj)]
      [(struct ast:expr:isa (_ obj _))
       (build-unwanted-type-syntax-expression obj)]
      [(struct ast:expr:unary/op (_ _ _ _ arg))
       (build-unwanted-type-syntax-expression arg)]
      [(struct ast:expr:binary/op (_ _ _ _ larg rarg))
       (list (build-unwanted-type-syntax-expression larg)
             (build-unwanted-type-syntax-expression rarg))]
      [(struct ast:expr:if (_ cond then else))
       (list (build-unwanted-type-syntax-expression cond)
             (build-unwanted-type-syntax-expression then)
             (build-unwanted-type-syntax-expression else))]
      [(struct ast:expr:cond (_ clauses else))
       (list (map (lambda (c)
                    (list (build-unwanted-type-syntax-expression (ast:cond/clause-test c))
                          (build-unwanted-type-syntax-expression (ast:cond/clause-result c))))
                  clauses)
             (if else (build-unwanted-type-syntax-expression else) '()))]
      [else '()]))
        
  ;; Yes, this is just part of the hack that gives us Check Syntax-correctness on all the types that
  ;; are not otherwise used in the compiled code.
  (provide translate-type-for-syntax)
  (define (translate-type-for-syntax type)
    (define (real-translation type)
      (match type
        [(struct ast:type:object:iface (stx name))
         (translate-iface-name type)]
        [(struct ast:type:object:any (stx))
         (translate-iface-name type)]
        [(struct ast:type:primitive (stx name))
         '()]
        [(struct ast:type:function (stx arg ret))
         (list (real-translation arg)
                 (real-translation ret))]
        [(struct ast:type:tuple (stx args))
         (map real-translation args)]))
    (real-translation type))

  )