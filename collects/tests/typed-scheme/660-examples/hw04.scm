#reader (planet "typed-reader.ss" ("plt" "typed-scheme.plt"))
(module hw04 "../../typed-scheme.ss"
  
  (require "support.ss")

#| This is the updated Algae BNF definition:
  <PROGRAM> ::= { program <FUN> ... }
  <FUN>     ::= { fun <id> { <id> } <ALGAE> }
  <ALGAE>   ::= <num>
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
              | { call <id> <ALGAE> }
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
  [With    (name symbol) (named ALGAE) (body ALGAE)]
  [Call    (fun symbol) (arg ALGAE)])

(define-type FUN
  [Fun (name symbol) (arg symbol) (body ALGAE)])

(define-type PROGRAM
  [Funs (funs (list-of FUN))])


;; parse-program : s-expr -> PROGRAM
;; parses a whole program s-expression into a PROGRAM
#;(define: (parse-program [sexpr : Sexp]) : PROGRAM  
  (if (and (list? sexpr)
           (not (null? sexpr))
           (eq? 'program (first sexpr)))
      (Funs (map parse-fun (rest sexpr)))
    (error 'parse-program "bad program syntax: ~s" sexpr)))

;; parse-fun : s-expr -> FUN
;; parses a function s-expression syntax to an instance of FUN
#;(define: (parse-fun [sexpr : Sexp]) : FUN
  (if (and ;; check overall structure
           (list? sexpr)
           (= 4 (length sexpr))
           (eq? 'fun (first sexpr))
           ;; check function name
           (symbol? (second sexpr))
           ;; check argument in a sublist
           (list? (third sexpr))
           (= 1 (length (third sexpr)))
           (symbol? (first (third sexpr))))
    ;; assemble the needed Fun parts
    (Fun (second sexpr)
         (first (third sexpr))
         (parse-expr (fourth sexpr)))
    (error 'parse-program "bad function syntax: ~s" sexpr)))

;; parse-expr : s-expr -> ALGAE
;; parses an s-expression into an ALGAE abstract syntax tree
#;(define: (parse-expr [sexpr : Sexp]) : ALGAE
  (cond
    [(number? sexpr) (Num sexpr)]
    [(symbol? sexpr) (Id sexpr)]
    [(and (list? sexpr) (not (null? sexpr))
          (eq? 'with (first sexpr)))
     (if (and (list? (second sexpr))
              (= 2 (length (second sexpr)))
              (symbol? (first (second sexpr))))
       (With (first (second sexpr))
             (parse-expr (second (second sexpr)))
             (parse-expr (third sexpr)))
       (error 'parse-expr "bad `with' syntax"))]
    ;; and trick
    [(and (list? sexpr) (not (null? sexpr))
          (eq? 'call (first sexpr)))
     (if (and (= 3 (length sexpr)) (symbol? (second sexpr)))
       (Call (second sexpr)
             (parse-expr (third sexpr)))
       (error 'parse-expr "bad `call' syntax"))]
    ;; and trick
    [(and (list? sexpr) (not (null? sexpr)))
     (let ([subs (map parse-expr (rest sexpr))])
       (case (first sexpr)
         [(+) (Add subs)]
         [(-) (if (null? subs)
                (error 'parse-expr "need at least one arg for `-'")
                (Sub (first subs) (rest subs)))]
         [(*) (Mul subs)]
         [(/) (if (null? subs)
                (error 'parse-expr "need at least one arg for `/'")
                (Div (first subs) (rest subs)))]
         [(=) (if (= 2 (length subs))
                (Eql (first subs) (second subs))
                (error 'parse-expr "need two args for `='"))]
         [(<) (if (= 2 (length subs))
                (Less (first subs) (second subs))
                (error 'parse-expr "need two args for `<'"))]
         [(<=) (if (= 2 (length subs))
                 (LessEql (first subs) (second subs))
                 (error 'parse-expr "need two args for `<='"))]
         [(if) (if (= 3 (length subs))
                 (If (first subs) (second subs) (third subs))
                 (error 'parse-expr "need three exprs for `if'"))]
         [else (error 'parse-expr "don't know about ~s"
                      (first sexpr))]))]
    [else (error 'parse-expr "bad syntax in ~s" sexpr)]))

;; Bonus:
;; verify-functions : PROGRAM -> void
;; this function verifies the list of functions, and doesn't return any
;; useful value.
(define: (verify-functions [prog : PROGRAM]) : Any
  ;; this will fail if there is no `main' definition
  (lookup-fun 'main prog)
  ;; check for repeating names, see helper below
  (check-duplicates (map Fun-name (Funs-funs prog)) '())
  ;; finally, scan `Call' syntaxes
  (check-calls-list (map Fun-body (Funs-funs prog)) prog))

;; check-duplicates : (list-of symbol) (list-of symbol) -> void
;; helper for `verify-functions'
(define: (check-duplicates [symbols : (Listof Symbol)] [seen : (Listof Symbol)]) : Any
  ;; `symbols' is what we check, `seen' is names we've already seen
  (cond [(null? symbols) 'ok]
        [(member (first symbols) seen)
         (error 'verify-functions
                "duplicate definition: ~s" (first symbols))]
        [else (check-duplicates (rest symbols) ;; CHANGE
                                (cons (first symbols) seen))]))

;; helper for `verify-functions'
(define: (check-calls-list [funs : (Listof ALGAE)] [prog : PROGRAM]) : Any
  (if (null? funs)
    'ok
    ;; note that `and' is not really needed below, we just want to use
    ;; both expressions so everything is checked.  Also in
    ;; `check-calls-expr'.
    (and (check-calls-expr (first funs) prog)
         (check-calls-list (rest funs) prog))))

(define: (check-calls-expr [expr : ALGAE] [prog : PROGRAM]) : Any
  (cases expr
    [(Num n) 'ok]
    [(Add args)     (check-calls-list args prog)]
    [(Mul args)     (check-calls-list args prog)]
    [(Sub fst args) (check-calls-list (cons fst args) prog)]
    [(Div fst args) (check-calls-list (cons fst args) prog)]
    [(Eql     l r)  (and (check-calls-expr l prog)
                         (check-calls-expr r prog))]
    [(Less    l r)  (and (check-calls-expr l prog)
                         (check-calls-expr r prog))]
    [(LessEql l r)  (and (check-calls-expr l prog)
                         (check-calls-expr r prog))]
    [(If c t e)     (and (check-calls-expr c prog)
                         (check-calls-expr t prog)
                         (check-calls-expr e prog))]
    [(Id id) 'ok]
    [(With bound-id named-expr bound-body)
     (and (check-calls-expr named-expr prog)
          (check-calls-expr bound-body prog))]
    [(Call fun-name arg)
     (and (lookup-fun fun-name prog)
          (check-calls-expr arg prog))]))

;; parse : string -> PROGRAM
;; parses a string containing an ALGAE program to a PROGRAM instance
#;(define (parse str)
  (let ([prog (parse-program (string->sexpr str))])
    ;; Bonus answer: the reason we use two expressions is that
    ;; `verify-functions' can only signal errors, so it is used only for
    ;; its side effect.
    (verify-functions prog)
    prog))

;; subst : ALGAE symbol ALGAE -> ALGAE
;; substitutes the second argument with the third argument in the
;; first argument, as per the rules of substitution; the resulting
;; expression contains no free instances of the second argument
(define: (subst [expr : ALGAE] [from : symbol] [to : ALGAE]) : ALGAE
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
               (subst bound-body from to)))]
      [(Call fun-name arg) (Call fun-name (subst arg from to))])))

;; lookup-fun : symbol PROGRAM -> FUN
;; looks up a FUN instance in a PROGRAM given its name
(define: (lookup-fun [name : Symbol] [prog : PROGRAM]) : FUN
  (cases prog
    [(Funs funs)
     (or (ormap (lambda: ([fun : FUN])
                  ;; `ormap' will return the first true (= non-#f)
                  ;; result, so this is both a predicate and returning
                  ;; the value that is used
                  (cases fun
                    [(Fun fname arg expr) (and (eq? fname name) fun)]))
                funs)
         (error 'lookup-fun
                "missing function definition for: ~s" name))]))

;; eval : ALGAE PROGRAM -> number
;; evaluates ALGAE expressions by reducing them to numbers
;; `prog' is provided for function lookup
(define: (-eval [expr : ALGAE] [prog : PROGRAM]) : Number
  ;; note the scoping rules: the following function will call the real
  ;; eval, but it expects a single argument, and always uses `prog'
  (let ([-eval (lambda: ([expr : ALGAE]) (-eval expr prog))])
    (cases expr
      [(Num n) n]
      [(Add args)     (foldl #{+ :: (Number Number -> Number)} 0 (map -eval args))]
      [(Mul args)     (foldl #{* :: (Number Number -> Number)} 1 (map -eval args))]
      [(Sub fst args) (if (null? args)
                        (- (-eval fst))
                        (- (-eval fst) (foldl #{+ :: (Number Number -> Number)} 0 (map -eval args))))]
      [(Div fst args) (if (null? args)
                        (/ (-eval fst))
                        (/ (-eval fst) (foldl #{* :: (Number Number -> Number)} 1 (map -eval args))))]
      [(Eql     l r) (if (=  (-eval l) (-eval r)) 1 0)]
      [(Less    l r) (if (<  (-eval l) (-eval r)) 1 0)]
      [(LessEql l r) (if (<= (-eval l) (-eval r)) 1 0)]
      [(If cond then else) (-eval (if (= 0 (-eval cond)) else then))]
      [(With bound-id named-expr bound-body)
       (-eval (subst bound-body bound-id (Num (-eval named-expr))))]
      [(Id id) (error '-eval "free identifier: ~s" id)]
      [(Call fun-name arg)
       (cases (lookup-fun fun-name prog)
         [(Fun name bound-id body)
          (-eval (subst body bound-id (Num (-eval arg))))])])))

;; run : string number -> number
;; evaluate an ALGAE complete program contained in a string using a
;; given value
#;(define: (run [str : String] [arg : Number]) : Number
  (let ([prog (parse str)])
    (-eval (Call 'main (Num arg)) prog)))

;; big test
(test (run "{program
              {fun even? {n}
                {if {= 0 n} 1 {call odd? {- n 1}}}}
              {fun odd? {n}
                {if {= 0 n} 0 {call even? {- n 1}}}}
              {fun main {n}
                {if {= n 1}
                  1
                  {+ 1 {call main
                             {if {call even? n}
                               {/ n 2}
                               {+ 1 {* n 3}}}}}}}}"
           3)
      => 8)
;; test cases for full coverage
(test (run "1" 1)
      =error> "bad program syntax")
(test (run "{program 1}" 1)
      =error> "bad function syntax")
(test (run "{program {fun main {x} {with {y 1} {+ x y}}}}" 1)
      => 2)
(test (run "{program {fun main {x} {with {foo 1} {call foo foo}}}
                     {fun foo {x} {- x -1}}}"
           1)
      => 2)
(test (run "{program {fun main {x} {with y {+ x y}}}}" 1)
      =error> "bad `with' syntax")
(test (run "{program {fun main {x} {call 1 2}}}" 1)
      =error> "bad `call' syntax")
(test (run "{program {fun main {x} {-}}}" 1)
      =error> "need at least one")
(test (run "{program {fun main {x} {/}}}" 1)
      =error> "need at least one")
(test (run "{program {fun main {x} {=}}}" 1)
      =error> "need two args")
(test (run "{program {fun main {x} {< 1}}}" 1)
      =error> "need two args")
(test (run "{program {fun main {x} {<=}}}" 1)
      =error> "need two args")
(test (run "{program {fun main {x} {if 1 2 3 4}}}" 1)
      =error> "need three exprs")
(test (run "{program {fun main {x} {main 1}}}" 1)
      =error> "don't know about")
(test (run "{program {fun main {x} {}}}" 1)
      =error> "bad syntax in")
(test (run "{program {fun main {x} x} {fun main {x} x}}" 1)
      =error> "duplicate definition")
(test (run "{program {fun main {x} {call foo x}}}" 1)
      =error> "missing function definition")
(test (run "{program {fun main {x} y}}" 1)
      =error> "free identifier")
(test (run "{program
              {fun main {x}
                {*{+{*{+{*}{*}}{+{*}{*}{*}{*}}{+{*}{*}{*}{*}}}{*}}
                  {+{*}{*}{*}{*}{*}}
                  {+{*}{*}{*}{*}}}}}" 1)
      => 660)
(test (run "{program {fun main {x} {+ {< x 3} {<= x 3} {= x 3}}}}" 1)
      => 2)
(test (run "{program {fun main {x} {+ {< x 3} {<= x 3} {= x 3}}}}" 3)
      => 2)
(test (run "{program {fun main {x} {+ {< x 3} {<= x 3} {= x 3}}}}" 4)
      => 0)
(test (run "{program {fun main {x} {* {- x} {/ x}}}}" 2)
      => -1)
(test (run "{program {fun main {x} {with {x 2} x}}}" 1)
      => 2)
;; can't check `run' since we won't check that the error happend when
;; parsing
(test (parse "{program {fun foo {x} x}}")
      =error> "missing function definition for: main")
(test (parse "{program {fun main {x} {call bar x}} {fun foo {x} x}}")
      =error> "missing function definition for: bar")
;; test that the language is not higher order
(test 1 <= (run "{program {fun foo {foo} foo}
                          {fun main {foo} {call foo foo}}}"
                1))

)
