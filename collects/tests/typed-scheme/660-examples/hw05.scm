#reader(planet "typed-reader.ss" ("plt" "typed-scheme.plt"))
(module hw05 "../../typed-scheme.ss"
  
  (require "support.ss")
  
  #|
The grammar:
  <BRANG> ::= <num>
            | { + <BRANG> <BRANG> }
            | { - <BRANG> <BRANG> }
            | { * <BRANG> <BRANG> }
            | { / <BRANG> <BRANG> }
            | { with { <id> <BRANG> } <BRANG> }
            | <id>
            | { fun { <id> } <BRANG> }
            | { call <BRANG> <BRANG> }

Evaluation rules:
  eval(N,env)                = N
  eval({+ E1 E2},env)        = eval(E1,env) + eval(E2,env)
  eval({- E1 E2},env)        = eval(E1,env) - eval(E2,env)
  eval({* E1 E2},env)        = eval(E1,env) * eval(E2,env)
  eval({/ E1 E2},env)        = eval(E1,env) / eval(E2,env)
  eval(Ref(N),env)           = list-ref(env,N)
  eval({with {x E1} E2},env) = eval(E2,cons(eval(E1,env),env))
  eval({fun {x} E},env)      = <{fun {x} E},env>
  eval({call E1 E2},env1)    = eval(Ef,cons(eval(E2,env1),env2))
                                    if eval(E1,env1)=<{fun {x} Ef},env2>
                             = error!  otherwise
|#
  
  ;; input syntax
  (define-type BRANG
    [Num  (n number)]
    [Add  (lhs BRANG) (rhs BRANG)]
    [Sub  (lhs BRANG) (rhs BRANG)]
    [Mul  (lhs BRANG) (rhs BRANG)]
    [Div  (lhs BRANG) (rhs BRANG)]
    [Id   (name symbol)]
    [With (name symbol) (named BRANG) (body BRANG)]
    [Fun  (name symbol) (body BRANG)]
    [Call (fun-expr BRANG) (arg-expr BRANG)])
  
  ;; preprocessed syntax
  (define-type BRANG*
    [Num*  (n number)]
    [Add*  (lhs BRANG*) (rhs BRANG*)]
    [Sub*  (lhs BRANG*) (rhs BRANG*)]
    [Mul*  (lhs BRANG*) (rhs BRANG*)]
    [Div*  (lhs BRANG*) (rhs BRANG*)]
    [Ref*  (idx Number)]
    [With* (named BRANG*) (body BRANG*)]
    [Fun*  (body BRANG*)]
    [Call* (fun-expr BRANG*) (arg-expr BRANG*)])
  
  ;; parse-sexpr : s-expr -> BRANG
  #;(define (parse-sexpr sexpr)
    (cond [(number? sexpr) (Num sexpr)]
          [(symbol? sexpr) (Id sexpr)]
          [(and (list? sexpr)
                (not (null? sexpr))
                (eq? 'with (first sexpr)))
           (if (and (= 3 (length sexpr))
                    (list? (second sexpr))
                    (= 2 (length (second sexpr)))
                    (symbol? (first (second sexpr))))
               (With (first (second sexpr))
                     (parse-sexpr (second (second sexpr)))
                     (parse-sexpr (third sexpr)))
               (error 'parse-sexpr "bad `with' syntax"))]
          [(and (list? sexpr)
                (not (null? sexpr))
                (eq? 'fun (first sexpr)))
           (if (and (= 3 (length sexpr))
                    (list? (second sexpr))
                    (= 1 (length (second sexpr)))
                    (symbol? (first (second sexpr))))
               (Fun (first (second sexpr))
                    (parse-sexpr (third sexpr)))
               (error 'parse-sexpr "bad `fun' syntax"))]
          [(and (list? sexpr) (= 3 (length sexpr)))
           (let ([make-node
                  (case (first sexpr)
                    [(+) Add]
                    [(-) Sub]
                    [(*) Mul]
                    [(/) Div]
                    [(call) Call]
                    [else (error 'parse-sexpr "don't know about ~s"
                                 (first sexpr))])])
             (make-node (parse-sexpr (second sexpr))
                        (parse-sexpr (third sexpr))))]
          [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))
  
  ;; parse : string -> BRANG
  ;; parses a string containing an BRANG expression to a BRANG AST
  #;(define (parse str)
    (parse-sexpr (string->sexpr str)))
  
  
  ;; These are the values of our language
  (define-type VAL
    [NumV (n number)]
    [FunV (body BRANG*) (env ENV)])

  ;; NEW
  (define-type-alias ENV (Listof VAL))

  ;; An environment is a simple list of values
  ;(define ENV? (list-of VAL?))
  
  ;; Syntactic environments for the de-Bruijn preprocessing:
  ;; define a type and an empty environment
  
  ;; this is represented by procedures, but the type should be:
  ;; DE-ENV := symbol -> integer
  ;; NEW
  (define-type-alias DE-ENV (Symbol -> Number))
  
  ;; de-empty-env : DE-ENV
  ;; the empty syntactic environment, always throws an error
  (define: (de-empty-env [id : Symbol]) : Number
    (error 'de-env "Free identifier: ~s" id))
  
  ;; de-extend : DE-ENV symbol -> DE-ENV
  ;; extends a given de-env for a new identifier  
  (define: (de-extend [env : DE-ENV] [id : Symbol]) : DE-ENV
    (lambda: ([name : Symbol])
      (if (eq? id name)
          0
          (+ 1 (env name)))))
  ;; test
  #;(test (let ([e (de-extend (de-extend de-empty-env 'b) 'a)])
          (map (lambda (id) (e id))
               '(a b)))
        => '(0 1))
  
  ;; preprocess : BRANG DE-ENV -> BRANG*
  ;; replaces identifier expressions into Ref AST values
  (define: (preprocess [expr : BRANG] [de-env : DE-ENV]) : BRANG*
    (let ([sub (lambda: ([expr : BRANG]) (preprocess expr de-env))])
      (cases expr
        [(Num n)   (Num* n)]
        [(Add l r) (Add* (sub l) (sub r))]
        [(Sub l r) (Sub* (sub l) (sub r))]
        [(Mul l r) (Mul* (sub l) (sub r))]
        [(Div l r) (Div* (sub l) (sub r))]
        [(With bound-id named-expr bound-body)
         (With* (sub named-expr)
                (preprocess bound-body (de-extend de-env bound-id)))]
        [(Id id) (Ref* (de-env id))]
        [(Fun bound-id bound-body)
         (Fun* (preprocess bound-body (de-extend de-env bound-id)))]
        [(Call fun-expr arg-expr)
         (Call* (sub fun-expr) (sub arg-expr))])))
  
  ;; arith-op : (num num -> num) VAL VAL -> VAL
  ;; gets a Scheme numeric binary operator, and uses it within a NumV
  ;; wrapper
  (define: (arith-op [op : (Number Number -> Number)] [val1 : VAL] [val2 : VAL]) : VAL
    (define: (NumV->number [v : VAL]) : Number
      (cases v
        [(NumV n) n]
        [else (error 'arith-op "expects a number, got: ~s" v)]))
    (NumV (op (NumV->number val1) (NumV->number val2))))
  
  ;; eval : BRANG* env -> VAL
  ;; evaluates BRANG* expressions by reducing them to values
  (define: (-eval [expr : BRANG*] [env : ENV]) : VAL
    (cases expr
      [(Num* n) (NumV n)]
      [(Add* l r) (arith-op + (-eval l env) (-eval r env))]
      [(Sub* l r) (arith-op - (-eval l env) (-eval r env))]
      [(Mul* l r) (arith-op * (-eval l env) (-eval r env))]
      [(Div* l r) (arith-op / (-eval l env) (-eval r env))]
      [(With* named-expr bound-body)
       (-eval bound-body (cons (-eval named-expr env) env))]
      [(Ref* n) (list-ref env n)]
      [(Fun* bound-body) (FunV bound-body env)]
      [(Call* fun-expr arg-expr)
       (let ([fval (-eval fun-expr env)])
         (cases fval
           [(FunV bound-body f-env)
            (-eval bound-body (cons (-eval arg-expr env) f-env))]
           [else (error '-eval "`call' expects a function, got: ~s"
                        fval)]))]))
  #|
  ;; run : string -> number
  ;; evaluate a BRANG program contained in a string
  (define (run str)
    (let ([result (-eval (preprocess (parse str) de-empty-env) null)])
      (cases result
        [(NumV n) n]
        [else (error 'run
                     "evaluation returned a non-number: ~s" result)])))
  
  ;; tests
  
  (test (run "{call {fun {x} {+ x 1}} 4}")
        => 5)
  (test (run "{with {add3 {fun {x} {+ x 3}}}
              {call add3 1}}")
        => 4)
  (test (run "{with {add3 {fun {x} {+ x 3}}}
              {with {add1 {fun {x} {+ x 1}}}
                {with {x 3}
                  {call add1 {call add3 x}}}}}")
        => 7)
  (test (run "{with {identity {fun {x} x}}
              {with {foo {fun {x} {+ x 1}}}
                {call {call identity foo} 123}}}")
        => 124)
  (test (run "{with {x 3}
              {with {f {fun {y} {+ x y}}}
                {with {x 5}
                  {call f 4}}}}")
        => 7)
  (test (run "{call {call {fun {x} {call x 1}}
                        {fun {x} {fun {y} {+ x y}}}}
                  123}")
        => 124)
  |#)
