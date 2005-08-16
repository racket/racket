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

;; num+ : \scheme|num| \scheme|num| -> \scheme|num|

(define (num+ n1 n2)
  (num (+ (num-n n1) (num-n n2))))

;; num- : \scheme|num| \scheme|num| -> \scheme|num|

(define (num- n1 n2)
  (num (- (num-n n1) (num-n n2))))

(define-type Subcache
  [mtSub]
  [aSub (name symbol?) (value FWAE?) (sc Subcache?)])

;; lookup : symbol \scheme|SubCache| -> \scheme|FWAE|

(define (lookup name sc)
  (type-case Subcache sc
             [mtSub () (error 'lookup "no binding for identifier")]
             [aSub (bound-name bound-value rest-sc)
                   (if (symbol=? bound-name name)
                       bound-value
                       (lookup name rest-sc))]))

;; interp : \scheme|FWAE| \scheme|SubCache| $\rightarrow$ \scheme|FWAE|
;; evaluates \scheme|FWAE| expressions by reducing them to their corresponding values
;; return values are either \scheme|num| or \scheme|fun|

(define (interp expr sc)
  (type-case FWAE expr
             [num (n) expr]
             [add (l r) (num+ (interp l sc) (interp r sc))]
             [sub (l r) (num- (interp l sc) (interp r sc))]
             [with (bound-id named-expr bound-body)
                   (interp bound-body
                           (aSub bound-id
                                      (interp named-expr sc)
                                      sc))]
             [id (v) (lookup v sc)]
             [fun (bound-id bound-body)
                  expr]
             [app (fun-expr arg-expr)
                  (local ([define fun-val (interp fun-expr sc)])
                         (interp (fun-body fun-val)
                                 (aSub (fun-param fun-val)
                                            (interp arg-expr sc)
                                            sc)))]))

(define (interp-test expr ans)
  (test (interp (parse expr) (mtSub)) (num ans)))

(define (interp-test-error expr expected-exception-msg)
  (test/exn (lambda () (interp (parse expr) (mtSub))) expected-exception-msg))


(interp-test 5 5)
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
"expect error: should yield 7, dyn scope yields 9"
(interp-test '{with {f {with {x 3} {fun {y} {+ x y}}}}
                    {with {x 5} {f 4}}} 7)

(test/pred (interp (parse '{fun {x} x}) (mtSub)) fun?)

(test/pred (interp (parse '{with {x 3} {fun {y} {+ x y}}}) (mtSub)) fun?)

(interp-test-error '{with {x x} x} "no binding")

