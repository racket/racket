#reader (planet "typed-reader.ss" ("plt" "typed-scheme.plt"))
(module hw03 "../../typed-scheme.ss"
  
  (require "support.ss")
  
  #| This is the updated Algae BNF definition:
  <ALGAE> ::= <num>
            | { + <ALGAE> ... }
            | { - <ALGAE> <ALGAE> ... }
            | { * <ALGAE> ... }
            | { / <ALGAE> <ALGAE> ... }
            | { = <ALGAE> <ALGAE> }
            | { < <ALGAE> <ALGAE> }
            | { <= <ALGAE> <ALGAE> }
            | { if <ALGAE> <ALGAE> <ALGAE> }
            | { with { <id> <ALGAE> } <ALGAE>}
            | <id>
|#
  
  (define-type ALGAE
    [Num     (n number)]
    [Add     (args (list-of ALGAE))]
    ;; note how Sub & Div match the corresponding BNF derivation
    [Sub     (fst ALGAE) (args (list-of ALGAE))]
    [Mul     (args (list-of ALGAE))]
    [Div     (fst ALGAE) (args (list-of ALGAE))]
    [Eql     (lhs ALGAE) (rhs ALGAE)]
    [Less    (lhs ALGAE) (rhs ALGAE)]
    [LessEql (lhs ALGAE) (rhs ALGAE)]
    [If      (cond-expr ALGAE) (then-expr ALGAE) (else-expr ALGAE)]
    [Id      (name symbol)]
    [With    (name symbol) (named ALGAE) (body ALGAE)])
  
  
  ;; parse-sexpr : s-expr -> ALGAE
  #;(define: (parse-sexpr [sexpr : Sexp]) : ALGAE
    (cond
      [(number? sexpr) (Num sexpr)]
      [(symbol? sexpr) (Id sexpr)]
      ;; new code (needed because not doesn't work)
      [(null? sexpr) (error 'parse-sexpr "bad syntax in ~s" sexpr)]
      ;; end new code
      ;; these next two have the horrid and trick.
      [(and (list? sexpr) (not (null? sexpr))
            (eq? 'with (first sexpr))
            (let ([s (second sexpr)])
              (if (list? s)
                  (if (= 2 (length s))
                      (let ([sym (first s)])
                        (if (symbol? sym)                            
                            (With sym
                                  (parse-sexpr (second s))
                                  (parse-sexpr (third sexpr)))
                            (error 'parse-sexpr "bad `with' syntax")))
                      (error 'parse-sexpr "bad `with' syntax"))
                  (error 'parse-sexpr "bad `with' syntax"))))]
      [(and (list? sexpr) (not (null? sexpr))
            (let ([subs (map parse-sexpr (rest sexpr))])
              (case (first sexpr)
                [(+) (Add subs)]
                [(-) (if (null? subs)
                         (error 'parse-sexpr "need at least one arg for `-'")
                         (Sub (first subs) (rest subs)))]
                [(*) (Mul subs)]
                [(/) (if (null? subs)
                         (error 'parse-sexpr "need at least one arg for `/'")
                         (Div (first subs) (rest subs)))]
                [(=) (if (= 2 (length subs))
                         (Eql (first subs) (second subs))
                         (error 'parse-sexpr "need two args for `='"))]
                [(<) (if (= 2 (length subs))
                         (Less (first subs) (second subs))
                         (error 'parse-sexpr "need two args for `<'"))]
                [(<=) (if (= 2 (length subs))
                          (LessEql (first subs) (second subs))
                          (error 'parse-sexpr "need two args for `<='"))]
                [(if) (if (= 3 (length subs))
                          (If (first subs) (second subs) (third subs))
                          (error 'parse-sexpr "need three exprs for `if'"))]
                [else (error 'parse-sexpr "don't know about ~s"
                             (first sexpr))])))]
      [else (error 'parse-sexpr "bad syntax in ~s" sexpr)]))
  
  ;; parse : string -> ALGAE
  ;; parses a string containing an ALGAE expression to an ALGAE AST
  #;(define: (parse [str : String]) : ALGAE
    (parse-sexpr (string->sexpr str)))
  
  #| Formal specs for `subst':
   (`N' is a <num>, `E1', `E2' are <ALGAE>s, `x' is some <id>, `y' is a
   *different* <id>)
      N[v/x]                = N
      {+ E ...}[v/x]        = {+ E[v/x] ...}
      {- E1 E ...}[v/x]     = {- E1[v/x] E[v/x] ...}
      {* E ...}[v/x]        = {* E[v/x] ...}
      {/ E1 E ...}[v/x]     = {/ E1[v/x] E[v/x] ...}
      {= E1 E2}[v/x]        = {= E1[v/x] E2[v/x]}
      {< E1 E2}[v/x]        = {< E1[v/x] E2[v/x]}
      {<= E1 E2}[v/x]       = {<= E1[v/x] E2[v/x]}
      {if E1 E2 E3}[v/x]    = {if E1[v/x] E2[v/x] E3[v/x]}
      y[v/x]                = y
      x[v/x]                = v
      {with {y E1} E2}[v/x] = {with {y E1[v/x]} E2[v/x]}
      {with {x E1} E2}[v/x] = {with {x E1[v/x]} E2}
|#
  
  ;; subst : ALGAE symbol ALGAE -> ALGAE
  ;; substitutes the second argument with the third argument in the
  ;; first argument, as per the rules of substitution; the resulting
  ;; expression contains no free instances of the second argument
  (define: (subst [expr : ALGAE] [from : Symbol] [to : ALGAE]) : ALGAE
    (let ([subst-list (lambda: ([exprs : (Listof ALGAE)])
                        (map (lambda: ([x : ALGAE]) (subst x from to)) exprs))])
      (cases expr
        [(Num n) expr]
        [(Add args)     (Add (subst-list args))]
        [(Mul args)     (Mul (subst-list args))]
        [(Sub fst args) (Sub (subst fst from to) (subst-list args))]
        [(Div fst args) (Div (subst fst from to) (subst-list args))]
        [(Eql     l r)  (Eql     (subst l from to) (subst r from to))]
        [(Less    l r)  (Less    (subst l from to) (subst r from to))]
        [(LessEql l r)  (LessEql (subst l from to) (subst r from to))]
        [(If c t e) (If (subst c from to)
                        (subst t from to)
                        (subst e from to))]
        [(Id id) (if (eq? id from) to expr)]
        [(With bound-id named-expr bound-body)
         (With bound-id
               (subst named-expr from to)
               (if (eq? bound-id from)
                   bound-body
                   (subst bound-body from to)))])))
  
  (define: (subst2 [expr : ALGAE] [from : Symbol] [to : ALGAE]) : ALGAE
    (let ([subst-list (lambda: ([exprs : (Listof ALGAE)])
                        (map (lambda: ([x : ALGAE]) (subst2 x from to)) exprs))])
      (cond
        [(Num? expr) expr]
        [(Add? expr) (Add (subst-list (Add-args expr)))]
        [(Mul? expr) (Mul (subst-list (Mul-args expr)))]
        [(Sub? expr) (Sub (subst2 (Sub-fst expr) from to) (subst-list (Sub-args expr)))]
        [(Div? expr) (Div (subst2 (Div-fst expr) from to) (subst-list (Div-args expr)))]
        [(Eql? expr) (Eql (subst2 (Eql-lhs expr) from to) (subst2 (Eql-rhs expr) from to))]
        [(Less? expr)  (Less    (subst2 (Less-lhs expr) from to) (subst2 (Less-rhs expr) from to))]
        [(LessEql? expr)  (LessEql (subst2 (LessEql-lhs expr) from to) (subst2 (LessEql-rhs expr) from to))]
        [(If? expr) (If (subst2 (If-cond-expr expr) from to)
                        (subst2 (If-then-expr expr) from to)
                        (subst2 (If-else-expr expr) from to))]
        [(Id? expr) (if (eq? (Id-name expr) from) to expr)]
        [(With? expr)
         (With (With-name expr)
               (subst2 (With-named expr) from to)
               (if (eq? (With-name expr) from)
                   (With-body expr)
                   (subst2 (With-body expr) from to)))])))
  
  #| Formal specs for `eval':
     eval(N)             = N
     eval({+ E ...})     = eval(E) + ...
     eval({- E1})        = -eval(E1)
     eval({- E1 E ...})  = eval(E1) - (eval(E) + ...)
     eval({* E ...})     = eval(E1) * ...
     eval({/ E1})        = 1/eval(E1)
     eval({/ E1 E ...})  = eval(E1) / (eval(E) * ...)
     eval({= E1 E2})     = 1 if eval(E1)=eval(E2), 0 otherwise
     eval({< E1 E2})     = 1 if eval(E1)<eval(E2), 0 otherwise
     eval({<= E1 E2})    = 1 if eval(E1)<=eval(E2), 0 otherwise
     eval({if E1 E2 E3}) = eval(E3) if eval(E1)=0, eval(E2) otherwise
     eval(id)            = error!
     eval({with {x E1} E2}) = eval(E2[eval(E1)/x])
|#
  
  ;; -eval : ALGAE -> number
  ;; evaluates ALGAE expressions by reducing them to numbers
  (define: (-eval [expr : ALGAE]) : Number
    (cases expr
      [(Num n) n]
      [(Add args)     (foldl #{+ : (Number Number -> Number)} 0 (map -eval args))]
      [(Mul args)     (foldl #{* : (Number Number -> Number)} 1 (map -eval args))]
      [(Sub fst args) (if (null? args)
                          (- (-eval fst))
                          (- (-eval fst) (foldl #{+ : (Number Number -> Number)} 0 (map -eval args))))]
      [(Div fst args) (if (null? args)
                          (/ (-eval fst))
                          (/ (-eval fst) (foldl #{* : (Number Number -> Number)} 1 (map -eval args))))]
      [(Eql     l r) (if (=  (-eval l) (-eval r)) 1 0)]
      [(Less    l r) (if (<  (-eval l) (-eval r)) 1 0)]
      [(LessEql l r) (if (<= (-eval l) (-eval r)) 1 0)]
      [(If cond then else) (-eval (if (= 0 (-eval cond)) else then))]
      [(With bound-id named-expr bound-body)
       (-eval (subst bound-body bound-id (Num (-eval named-expr))))]
      [(Id id) (error '-eval "free identifier: ~s" id)]))
  
  ;; run : string -> number
  ;; evaluate an ALGAE program contained in a string
  #;(define: (run [str : String]) : Number
    (-eval (parse str)))
  
  ;; previous tests
  (test   5 <= (run "5"))
  (test  10 <= (run "{+ 5 5}"))
  (test  20 <= (run "{with {x {+ 5 5}} {+ x x}}"))
  (test  10 <= (run "{with {x 5} {+ x x}}"))
  (test  14 <= (run "{with {x {+ 5 5}} {with {y {- x 3}} {+ y y}}}"))
  (test   4 <= (run "{with {x 5} {with {y {- x 3}} {+ y y}}}"))
  (test  15 <= (run "{with {x 5} {+ x {with {x 3} 10}}}"))
  (test   8 <= (run "{with {x 5} {+ x {with {x 3} x}}}"))
  (test  10 <= (run "{with {x 5} {+ x {with {y 3} x}}}"))
  (test   5 <= (run "{with {x 5} {with {y x} y}}"))
  (test   5 <= (run "{with {x 5} {with {x x} x}}"))
  ;; new tests
  (test   0 <= (run "{+}"))
  (test   1 <= (run "{*}"))
  (test  -2 <= (run "{- 2}"))
  (test 1/2 <= (run "{/ 2}"))
  (test 1/2 <= (run "{/ 1 2}"))
  (test  10 <= (run "{+ 1 2 3 4}"))
  (test   2 <= (run "{if {< 2 3} 2 3}"))
  (test   2 <= (run "{if {<= 3 3} 2 3}"))
  (test   3 <= (run "{if {= 2 3} {/ 2 0} 3}"))
  (test   1 <= (run "{+ {= 3 3} {< 3 2} {<= 3 2}}"))
  (test   1 <= (run "{with {x 2} {= 1/8 {/ {* x 4}}}}"))
  (test   1 <= (run "{with {x 2} {if {< 1 2} {<= 1 2} 3}}"))
  ;; test errors
  (test (run "{-}")            =error> "need at least")
  (test (run "{/}")            =error> "need at least")
  (test (run "{= 1 2 3}")      =error> "need two args")
  (test (run "{< 1}")          =error> "need two args")
  (test (run "{<=}")           =error> "need two args")
  (test (run "{with 1}")       =error> "bad * syntax")
  (test (run "{with {x 1} y}") =error> "free identifier")
  (test (run "{if 1}")         =error> "need three")
  (test (run "{foo 1}")        =error> "don't know")
  (test (run "{}")             =error> "bad syntax in")
  
  #| Dessert answer:

   Adding `...' (or Kleene star) to our BNF language does not make it
   more expressive.  An informal proof: say that you have a BNF with
   some use of `...' ("?" indicates unknown parts):

     <FOO> ::= ? | ? <BAR> ... ? | ?

   we can translate that to a BNF that does not use `...' by inventing a
   fresh non-terminal (say that `<FOO1>' is not used elsewhere) and
   rewriting the above derivation as follows:

     <FOO>  ::= ? | ? <FOO1> ? | ?
     <FOO1> ::= <BAR> <FOO1>
              |             <-- an empty derivation

   This can be systematically repeated, and the result will be an
   ellipsis-free BNF that is equivalent to the original.

|#
  
  #| Bonus answer

   Yes, we could simulate `and' and `or' using arithmetics:

   * use {* x y} instead of {and x y}

   * use {+ {* x x} {* y y}} instead of {or x y}

   ... but that wouldn't be enough to do short circuiting and simulating
   Scheme's `and' and `or', because these forms will evaluate *all* of
   their subexpressions.  To do that properly, we need more than
   arithmetics: we need conditionals.  For example:

   * use {if x y 0} instead of {and x y}

   * use {if x 1 y} instead of {or x y}

|#
  )
