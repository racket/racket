#lang racket/base
(require racket/contract)

#;
(->i ([x number?]
      [y (x) (<=/c x)])
     any)

#;
(define (coerce-proj x)
  ...)

#;
(build-->i 
 (list number?)
 (list (λ (x pos neg blame info) (coerce-proj (<=/c x) pos neg blame info)))
 (λ (x/c y/proc)  ;; <= arguments are in strange order: first the non-dependent things, then the dependent things
   (λ (pos neg blame info)
     (let ([here ...])
       (let ([x/proj (x/c neg pos blame info)]
             [x/proj/i (x/c here pos blame info)])
         (λ (f)
           (λ (x y)
             (let ([x (x/proj x)]
                   [xi (x/proj/i x)])
               (let ([y (y/proc xi neg pos blame info)]
                     [y (y/proc xi here pos blame info)])
                 (f x y))))))))))

(syntax->datum (expand #'(-> number? (<=/c 10) any)))

#|
test cases:

(->i (#:kwd1 [x number?]
      #:kwd2 [x number?])
     (values [y number?]
             [z number?]))
=> duplicate identifier 'x'


(->i (#:kwd1 [w number?]
      #:kwd1 [x number?])
     (values [y number?]
             [z number?]))
=> duplicate keyword


(->i (#:kwd1 [w number?]
      #:kwd2 [x number?])
     (values [y number?]
             [w number?]))
=> duplicate variable 'w'


(->i (#:kwd1 [w number?]
      #:kwd2 [x number?])
     (values [y number?]
             [y number?]))
=> duplicate variable 'y'


(->i (#:kwd1 [w number?]
      #:kwd2 [x number?])
     (values [y number?]
             [w number?]))
=> duplicate identifier 'w'

(let ([values (λ (x) x)])
  (->i (#:kwd1 [w number?]
        #:kwd2 [x number?])
       (values number?)))
;=>  no error(?)

(->i (#:kwd1 [x number?]
      #:kwd2 [y number?])
     [x number?])
;=> duplicate identifier 'x'

(->i (#:kwd1 [x number?]
      #:kwd2 [y number?])
     #:rest [x any/c]
     any)
;=> duplicate identifier 'x'


(let ([c integer?])
  (->i ((arg any/c)) () (values (_ (arg) c) (x (arg) c) (_ (arg) c))))
; => all or none _s

(->i ([x (y) number?])
     any)
; => unknown dependent variable

(->i ([x (y) number?])
     [y number?])
; => domain cannot depend on a range variable

(->i ()
     #:rest [x (y) number?]
     [y number?])
; => domain cannot depend on a range variable

(->i ([x (x) number?])
     any)
; => cyclic dependencies not allowed

(->i ([x (y) number?]
      [y (x) number?])
     any)
; => cyclic dependencies not allowed

(->i ([in number?])
     (values [x (y) number?]
             [y (z) number?]
             [z (x) number?]))

;; => cyclic depenencies

(->i ()
     #:rest [x (x) number?]
     any)
; => error cyclic dependencies

(->i ([x (y) number?]
      [y number?])
     any)
; => no syntax error

(->i ()
     (values [x (y) number?]
             [y number?]))
; => no syntax error

(->i ()
     #:rest [x number?]
     [y (x) number?])
;; => no syntax error


|#
