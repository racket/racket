(define-type FWAE
  [num (n number?)]
  [add (lhs FWAE?) (rhs FWAE?)]
  [sub (lhs FWAE?) (rhs FWAE?)]
  [id (name symbol?)]
  [with (name symbol?) (named-expr FWAE?) (body FWAE?)]
  [fun (param symbol?) (body FWAE?)]
  [app (fun-expr FWAE?) (arg-expr FWAE?)])

;; parse : sexp $\longrightarrow$ FWAE
;; to convert s-expressions into FWAEs

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
                         (parse (third sexp)))]
      [(fun) (fun (first (second sexp)) (parse (third sexp)))]
      [else (app (parse (first sexp)) (parse (second sexp)))])]))

;; subst : \scheme|FWAE| symbol \scheme|FWAE| $\rightarrow$ \scheme|FWAE|
;; substitutes second argument with third argument in first argument,
;; as per the rules of substitution; the resulting expression contains
;; no free instances of the second argument

(define (subst expr sub-id val)
  (type-case FWAE expr
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
                                  (subst bound-body sub-id val)))]
             [fun (bound-id bound-body)
                  (if (symbol=? bound-id sub-id)
                      expr
                      (fun bound-id
                                (subst bound-body sub-id val)))]
             [app (fun-expr arg-expr)
                  (app (subst fun-expr sub-id val)
                            (subst arg-expr sub-id val))]))


(test (subst (add (id 'x) (id 'x)) 'x (num 5))
      (add (num 5) (num 5)))

(test (subst (with 'x (num 5) (add (id 'x) (id 'x))) 'x (num 3))
      (with 'x (num 5) (add (id 'x) (id 'x))))

(test (subst (add (id 'x) (with 'x (num 3) (num 10))) 'x (num 5))
      (add (num 5) (with 'x (num 3) (num 10))))

(test (subst (add (id 'x) (with 'x (num 3) (id 'x))) 'x (num 5))
      (add (num 5) (with 'x (num 3) (id 'x))))

(test (subst (parse '{fun {x} {+ x y}}) 'x (num 5))
      (parse '{fun {x} {+ x y}}))

(test (subst (parse '{fun {x} {+ x y}}) 'y (num 5))
      (parse '{fun {x} {+ x 5}}))

(test (subst (parse '{{fun {x} {+ x y}} {fun {y} {+ x y}}}) 'y (num 3))
      (parse '{{fun {x} {+ x 3}} {fun {y} {+ x y}}}))


;; num+ : \scheme|num| \scheme|num| -> \scheme|num|

(define (num+ n1 n2)
  (num (+ (num-n n1) (num-n n2))))

;; num- : \scheme|num| \scheme|num| -> \scheme|num|

(define (num- n1 n2)
  (num (- (num-n n1) (num-n n2))))

;; interp : \scheme|FWAE| $\rightarrow$ \scheme|FWAE|
;; evaluates \scheme|FWAE| expressions by reducing them to their corresponding values
;; return values are either \scheme|num| or \scheme|fun|

(define (interp expr)
  (type-case FWAE expr
             [num (n) expr]
             [add (l r) (num+ (interp l) (interp r))]
             [sub (l r) (num- (interp l) (interp r))]
             [with (bound-id named-expr bound-body)
                   (interp (subst bound-body
                                  bound-id
                                  (interp named-expr)))]
             [id (v) (error 'interp "free identifier")]
             [fun (bound-id bound-body)
                  expr]
             [app (fun-expr arg-expr)
                  (local ([define fun-val (interp fun-expr)])
                         (interp (subst (fun-body fun-val)
                                        (fun-param fun-val)
                                        (interp arg-expr))))]))

(define (interp-test expr ans)
  (test (interp (parse expr)) (num ans)))

(define (interp-test-error expr expected-exception-msg)
  (test-exn (lambda () (interp (parse expr))) expected-exception-msg))

(interp-test '5 5)
(interp-test '{+ 5 5} 10)
(interp-test '{with {x {+ 5 5}} {+ x x}} 20)
(interp-test '{with {x 5} {+ x x}} 10)
(interp-test '{with {x {+ 5 5}} {with {y {- x 3}} {+ y y}}} 14)
(interp-test '{with {x 5} {with {y {- x 3}} {+ y y}}} 4)
(interp-test '{with {x 5} {+ x {with {x 3} 10}}} 15)
(interp-test '{with {x 5} {+ x {with {x 3} x}}} 8)
(interp-test '{with {x 5} {+ x {with {y 3} x}}} 10)
(interp-test '{with {x 5} {with {y x} y}} 5)
(interp-test '{with {x 5} {with {x x} x}} 5)
(interp-test '{{fun {x} {+ x 5}} 5} 10)
(interp-test '{with {double {fun {x} {+ x x}}} {+ {double 5} {double 10}}} 30)
(interp-test '{{{fun {x} x} {fun {x} {+ x 5}}} 3} 8)
(interp-test '{with {f {with {x 3} {fun {y} {+ x y}}}}
                    {with {x 5} {f 4}}} 7)

(test/pred (interp (parse '{fun {x} x})) fun?)
(test/pred (interp (parse '{with {x 3} {fun {y} {+ x y}}})) fun?)

(test/exn (lambda () (interp (parse '{with {x x} x}))) "free identifier")
