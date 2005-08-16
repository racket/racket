(define-type WAE
  [num (n number?)]
  [add (lhs WAE?) (rhs WAE?)]
  [sub (lhs WAE?) (rhs WAE?)]
  [id (name symbol?)]
  [with (name symbol?) (named-expr WAE?) (body WAE?)])

;; parse : sexp $\longrightarrow$ AE
;; to convert s-expressions into AEs

(define (parse sexp)
  (cond
   [(symbol? sexp) (id sexp)]
   [(number? sexp) (num sexp)]
   [(list? sexp)
    (case (first sexp)
      [(+) (add (parse (second sexp))
                     (parse (third sexp)))]
      [(-) (sub (parse (second sexp))
                     (parse (third sexp)))]
      [(with) (with (first (second sexp))
                         (parse (second (second sexp)))
                         (parse (third sexp)))])]))

;; subst : \scheme|WAE| symbol \scheme|WAE| $\rightarrow$ \scheme|WAE|
;; substitutes second argument with third argument in first argument,
;; as per the rules of substitution; the resulting expression contains
;; no free instances of the second argument

(define (subst expr sub-id val)
  (type-case WAE expr
             [num (n) expr]
             [add (l r) (add (subst l sub-id val)
                                  (subst r sub-id val))]
             [sub (l r) (sub (subst l sub-id val)
                                  (subst r sub-id val))]
             [id (v) (if (symbol=? v sub-id) val expr)]
             [with (bound-id named-expr bound-body)
                   (if (symbol=? bound-id sub-id)
                       (with bound-id
                                  (subst named-expr sub-id val)
                                  bound-body)
                       (with bound-id
                                  (subst named-expr sub-id val)
                                  (subst bound-body sub-id val)))]))


(test (subst (add (id 'x) (id 'x)) 'x (num 5))
      (add (num 5) (num 5)))

(test (subst (with 'x (num 5) (add (id 'x) (id 'x))) 'x (num 3))
      (with 'x (num 5) (add (id 'x) (id 'x))))

(test (subst (add (id 'x) (with 'x (num 3) (num 10))) 'x (num 5))
      (add (num 5) (with 'x (num 3) (num 10))))

(test (subst (add (id 'x) (with 'x (num 3) (id 'x))) 'x (num 5))
      (add (num 5) (with 'x (num 3) (id 'x))))

(define (calc expr)
  (type-case WAE expr
             [num (n) n]
             [add (l r) (+ (calc l) (calc r))]
             [sub (l r) (- (calc l) (calc r))]
             [with (bound-id named-expr bound-body)
                   (calc (subst bound-body
                                bound-id
                                (num (calc named-expr))))]
             [id (v) (error 'calc "free identifier")]))

(test (calc (parse '5)) 5)
(test (calc (parse '{+ 5 5})) 10)
(test (calc (parse '{with {x {+ 5 5}} {+ x x}})) 20)
(test (calc (parse '{with {x 5} {+ x x}})) 10)
(test (calc (parse '{with {x {+ 5 5}} {with {y {- x 3}} {+ y y}}})) 14)
(test (calc (parse '{with {x 5} {with {y {- x 3}} {+ y y}}})) 4)
(test (calc (parse '{with {x 5} {+ x {with {x 3} 10}}})) 15)
(test (calc (parse '{with {x 5} {+ x {with {x 3} x}}})) 8)
(test (calc (parse '{with {x 5} {+ x {with {y 3} x}}})) 10)
(test (calc (parse '{with {x 5} {with {y x} y}})) 5)
(test (calc (parse '{with {x 5} {with {x x} x}})) 5)
(test/exn (lambda () (calc (parse '{with {x x} x}))) "free identifier")
