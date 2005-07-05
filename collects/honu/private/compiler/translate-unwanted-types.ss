(module translate-unwanted-types mzscheme
  
  (require (lib "plt-match.ss")
           "../../ast.ss"
           "translate-utils.ss")
  
  (provide build-unwanted-type-syntax)
  (define (build-unwanted-type-syntax defns)
    (map build-unwanted-type-syntax-defn defns))
  
  ;; since we're never going to run the result anyway, it doesn't matter
  ;; how we build things -- no need to flatten.
  (define (build-unwanted-type-syntax-defn defn)
    (match defn
      [(struct honu:bind-top (_ _ types value))
       (cons (build-unwanted-type-syntax-expression value)
             (map translate-type-for-syntax types))]
      [(struct honu:function (_ _ type formals body))
       (list (translate-type-for-syntax type)
             (build-unwanted-type-syntax-expression body)
             (map (lambda (f)
                    (translate-type-for-syntax (honu:formal-type f)))
                  formals))]
      [(struct honu:iface (_ _ _ members))
       (map build-unwanted-type-syntax-member-decl members)]
      [(struct honu:class (_ _ selftype _ _ inits members exports))
       (list (translate-type-for-syntax selftype)
             (map (lambda (f)
                    (translate-type-for-syntax (honu:formal-type f)))
                  inits)
             (map build-unwanted-type-syntax-member members)
             (map (lambda (e)
                    (translate-type-for-syntax (honu:export-type e)))
                  exports))]
      [(struct honu:mixin (_ _ selftype arg-type _ _ inits withs super-new
                           members-before members-after exports))
       (list (translate-type-for-syntax selftype)
             (translate-type-for-syntax arg-type)
             (map (lambda (f)
                    (translate-type-for-syntax (honu:formal-type f)))
                  inits)
             (map (lambda (f)
                    (translate-type-for-syntax (honu:formal-type f)))
                   withs)
             (map (lambda (a)
                    (build-unwanted-type-syntax-expression (honu:name-arg-value a)))
                  (honu:super-new-args super-new))
             (map build-unwanted-type-syntax-member members-before)
             (map build-unwanted-type-syntax-member members-after)
             (map (lambda (e)
                    (translate-type-for-syntax (honu:export-type e)))
                  exports))]
      [(struct honu:subclass (_ _ _ mixin))
       ;; okay, this isn't a type, but we still want to see it as a use
       ;; until we can translate mixins correctly.
       (list (translate-mixin-name mixin))]))
  
  (define (build-unwanted-type-syntax-member-decl member)
    (match member
      [(struct honu:field-decl (_ _ type))
       (translate-type-for-syntax type)]
      [(struct honu:method-decl (_ _ type arg-types))
       (list (translate-type-for-syntax type)
             (map translate-type-for-syntax arg-types))]))
        
  (define (build-unwanted-type-syntax-member member)
    (match member
      [(struct honu:init-field (_ _ type value))
       (list (translate-type-for-syntax type)
             (if value (build-unwanted-type-syntax-expression value) '()))]
      [(struct honu:field (_ _ type value))
       (list (translate-type-for-syntax type)
             (build-unwanted-type-syntax-expression value))]
      [(struct honu:method (_ _ type formals body))
       (list (translate-type-for-syntax type)
             (map (lambda (f)
                    (translate-type-for-syntax (honu:formal-type f)))
                  formals)
             (build-unwanted-type-syntax-expression body))]))

  (define (build-unwanted-type-syntax-expression expr)
    (match expr
      [(struct honu:lambda (_ type formals body))
       (list (translate-type-for-syntax type)
             (map (lambda (f)
                    (translate-type-for-syntax (honu:formal-type f)))
                  formals)
             (build-unwanted-type-syntax-expression body))]
      [(struct honu:let (_ bindings body))
       (list (map (lambda (b)
                    (list (map translate-type-for-syntax (honu:binding-types b))
                          (build-unwanted-type-syntax-expression (honu:binding-value b))))
                  bindings)
             (build-unwanted-type-syntax-expression body))]
      [(struct honu:seq (_ effects result))
       (list (map (lambda (e)
                    (build-unwanted-type-syntax-expression e))
                  effects)
             (build-unwanted-type-syntax-expression result))]
      [(struct honu:call (_ func arg))
       (list (build-unwanted-type-syntax-expression func)
             (build-unwanted-type-syntax-expression arg))]
      [(struct honu:assn (_ lhs rhs))
       (list (build-unwanted-type-syntax-expression lhs)
             (build-unwanted-type-syntax-expression rhs))]
      [(struct honu:return (_ body))
       (build-unwanted-type-syntax-expression body)]
      [(struct honu:select (_ _ arg))
       (build-unwanted-type-syntax-expression arg)]
      [(struct honu:tuple (_ args))
       (map build-unwanted-type-syntax-expression args)]
      [(struct honu:member (_ obj _ _ _))
       (if (honu:expr? obj)
           (build-unwanted-type-syntax-expression obj)
           (list))]
      [(struct honu:new (_ obj type args))
       (list (build-unwanted-type-syntax-expression obj)
             (translate-type-for-syntax type)
             (map (lambda (a)
                    (build-unwanted-type-syntax-expression (honu:name-arg-value a)))
                  args))]
      ;; here are the two cases where the type already appears in the compiled code
      [(struct honu:cast (_ obj _))
       (build-unwanted-type-syntax-expression obj)]
      [(struct honu:isa (_ obj _))
       (build-unwanted-type-syntax-expression obj)]
      [(struct honu:un-op (_ _ _ _ arg))
       (build-unwanted-type-syntax-expression arg)]
      [(struct honu:bin-op (_ _ _ _ larg rarg))
       (list (build-unwanted-type-syntax-expression larg)
             (build-unwanted-type-syntax-expression rarg))]
      [(struct honu:if (_ cond then else))
       (list (build-unwanted-type-syntax-expression cond)
             (build-unwanted-type-syntax-expression then)
             (build-unwanted-type-syntax-expression else))]
      [(struct honu:cond (_ clauses else))
       (list (map (lambda (c)
                    (list (build-unwanted-type-syntax-expression (honu:cond-clause-pred c))
                          (build-unwanted-type-syntax-expression (honu:cond-clause-rhs c))))
                  clauses)
             (if else (build-unwanted-type-syntax-expression else) '()))]
      [else '()]))
        
  ;; Yes, this is just part of the hack that gives us Check Syntax-correctness on all the types that
  ;; are not otherwise used in the compiled code.
  (provide translate-type-for-syntax)
  (define (translate-type-for-syntax type)
    (define (real-translation type)
      (match type
        [(struct honu:type-iface (stx name))
         (translate-iface-name type)]
        [(struct honu:type-iface-top (stx))
         (translate-iface-name type)]
        [(struct honu:type-prim (stx name))
         '()]
        [(struct honu:type-func (stx arg ret))
         (list (real-translation arg)
                 (real-translation ret))]
        [(struct honu:type-tuple (stx args))
         (map real-translation args)]))
    (real-translation type))

  )