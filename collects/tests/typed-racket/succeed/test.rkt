#lang typed-scheme

(define: x : (U Number #f) 1)
(if x #{x :: Number} 1)
(lambda () 1)
(lambda: ([y : Number]) (if #t y y))

(plambda: (a) ([y : Number]) (if y #t #f))
(plambda: (a) ([y : a]) y)
(plambda: (a) ([y : a]) y)
(plambda: () ([y : Boolean]) (if y #t #f))

#{(if #t #t #t) :: Boolean}

(let () 3)
(let ([x 1] [y 2]) x)
#{(let ([x 1] [y 2]) x) :: Number}
(let: ([x : Number 1] [y : Integer 2]) x)
#{(let: ([x : Integer 1] [y : Integer 2]) x) :: Integer}

#{(let*: ([x : Number 1] [x : Integer 2]) x) :: Integer}
#{(let*: ([x : Number 1] [x : Integer 2]) #{x :: Integer}) :: Integer}

#{(letrec: ([x : Integer 1] [y : Integer 2]) #{x :: Integer}) :: Integer}
(letrec: ([x : Integer 1] [y : Integer 2]) #{x :: Integer})
(let ()
  (define x 1)
  (define y 2)
  x)
(letrec: ([z : (-> Any) (lambda () z)]) 1)
(letrec: ([z : (-> Any) (lambda () w)]
          [w : (-> Any) (lambda () z)]) z)
(let ()
  (define: (z) : Any w)
  (define: (w) : Any z)
  z)
(let ()
  (define: (z [x : Number]) : Any w)
  (define: (w) : Any z)
  z)
(case-lambda: [() 1]
              [([x : Number]) x])
;; Error
#;#{(case-lambda: [() 1]
                [([x : Number]) x]) :: String}
#{(lambda: ([x : Number]) 1) :: (Number -> Number)}
#{(lambda: ([x : Number]) 1) :: Any}
#{(lambda: ([x : Number]) 1) :: (Integer -> Any)}
#{(lambda: ([x : Number]) x) :: (Number -> Number)}
#{(lambda: ([x : Number]) x) :: (Integer -> Any)}
(define zzz 1)
(set! zzz 2)

(define-struct: xxx ())
(define-struct: xxx2 ([y : Number]))
(define-struct: xxx3 ([y : Number] [z : Number]))
(define-struct: xxx4 ([y : Number] [z : xxx4]))
(define-struct: xxx5 ([y : Number] [z : xxx4]))
(define-struct: (A) xxx6 ([y : A] [z : xxx4]))
xxx6-y
(with-continuation-mark ((inst make-continuation-mark-key Integer)) 1 1)
'foo
'(foo foo foo)
(define-type-alias NNN Number)
(define-type-alias (NNN2 A) (Listof Number))
(define-type-alias (NNN3 A) (Listof A))
(define-syntax-rule (m x) 1)
(m 2)
#{1 :: 1}
(lambda: ([x : String]) (lambda () (set! x "foo")))
#'(x y z)
(begin0 1 1 1)
(begin 1 1 1)
(#%expression (begin 1 1 1))

(values 1)
(values 1 1)
(values)

(: ff (Number -> Number))
(define (ff x) x)
(ff 1)

(lambda: ([y : String][x : Number]) (values 1 x 1))
(lambda: ([x : Number]) (values 1 x 1))
(lambda () (values 1 1))
(lambda () 1)
#{(lambda (x) x) :: (Number -> Number)}

{ann (values (lambda (x) x) (lambda (x) x)) (values (Number -> Number) (String -> String))}

(list 1 2 3)
(ann (list 1 2 3) (Pair Number (Listof Integer)))
(ann (list 1 2 3) (Listof Integer))
(ann (list 1 2 3) (Listof Number))

(list* 1 2 3)
(ann (list* 1 2 3 (list)) (Pair Number (Listof Integer)))

((lambda (x) 1) 1)
((lambda (x y) 1) 1 2)
