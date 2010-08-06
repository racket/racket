#lang racket/base
(require racket/contract
         racket/pretty)

(pretty-print
 (syntax->datum (expand-once
                 #'(->i ([i (box/c (listof integer?))])
                        (values [_ (i)
                                   (begin 
                                     (set-box! i (cons 1 (unbox i)))
                                     (λ (x) 
                                       (set-box! i (cons 4 (unbox i)))
                                       #t))]
                                [_ (i)
                                   (begin 
                                     (set-box! i (cons 2 (unbox i)))
                                     (λ (x) 
                                       (set-box! i (cons 5 (unbox i)))
                                       #t))])))))


(let ([b (box '())])
  ((contract (->i ([i (box/c (listof integer?))])
                  (values [_ (i)
                             (begin 
                               (set-box! i (cons 1 (unbox i)))
                               (λ (x) 
                                 (set-box! i (cons 4 (unbox i)))
                                 #t))]
                          [_ (i)
                             (begin 
                               (set-box! i (cons 2 (unbox i)))
                               (λ (x) 
                                 (set-box! i (cons 5 (unbox i)))
                                 #t))]))
             (λ (i) 
               (set-box! i (cons 3 (unbox i)))
               (values 2 2))
             (quote pos)
             (quote neg))
   b)
  (unbox b))
;; ==> 

#|
;; timing tests:

(define f1
  (contract (-> number? number? (or/c (<=/c 1) (<=/c 2)) any)
            (λ (x y z) (+ x y z))
            'pos 'neg))

(define f2
  (contract (->i ([x number?] [y number?] [z (x y) (or/c (<=/c x) (<=/c y))]) any)
            (λ (x y z) (+ x y z))
            'pos 'neg))


(define (tme f)
  (time
   (let loop ([n 100000])
     (unless (zero? n)
       (f -1 -1 -1) (f -1 -1 -1) (f -1 -1 -1) (f -1 -1 -1) (f -1 -1 -1) 
       (f -1 -1 -1) (f -1 -1 -1) (f -1 -1 -1) (f -1 -1 -1) (f -1 -1 -1) 
       (f -1 -1 -1) (f -1 -1 -1) (f -1 -1 -1) (f -1 -1 -1) (f -1 -1 -1) 
       (f -1 -1 -1) (f -1 -1 -1) (f -1 -1 -1) (f -1 -1 -1) (f -1 -1 -1) 
       (f -1 -1 -1) (f -1 -1 -1) (f -1 -1 -1) (f -1 -1 -1) (f -1 -1 -1) 
       (f -1 -1 -1) (f -1 -1 -1) (f -1 -1 -1) (f -1 -1 -1) (f -1 -1 -1) 
       (loop (- n 1))))))

'ignore: (tme f1)

'f1 (tme f1)
'f2 (tme f2)
|#

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


(->i ([x number?]) #:pre (y) #t any)
;; => unknown dependent variable


(->i ([x number?]) #:pre (x) #t [res any/c] #:post (y) #t)
;; => unknown dependent variable

(->i ([x (y) number?])
     [y number?])
; => domain cannot depend on a range variable

(->i ()
     #:rest [x (y) number?]
     [y number?])
; => domain cannot depend on a range variable

(->i ([x number?]) #:pre (res) #t [res any/c] #:post (x) #t)
;; => pre cannot depend on a range variables

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

((contract (->i ([x number?]
                 [y (x z) (between/c x z)]
                 [z number?])
                any)
           (λ (x y z) (+ x y z))
           'pos 'neg)
 1 2 3)
;; => 6

((contract (->i ([x number?]) #:pre () (= 1 2) any)
           (λ (x) 1)
           'pos 'neg) 2)
;; => pre-condition violation


((contract (->i ([f (-> number? number?)]) [res number?])
             (λ (f) (f 1))
             'pos 'neg) 
   (λ (n) (+ n 1)))
;; => 2


((contract (->i ([f (-> number? number?)]) [res number?])
             (λ (f) #f)
             'pos 'neg) 
   (λ (n) (+ n 1)))
;; => pos violation

((contract (->i ([x integer?]) () #:rest [rst (listof number?)] [r any/c]) (lambda w w) 'pos 'neg) 1 2)
;; => '(1 2)

((contract (->i (#:x [x integer?]) () #:rest [rst (listof number?)] [r any/c]) (lambda (#:x x . w) (cons x w)) 'pos 'neg) #:x 1 2)
;; => '(1 2)

((contract (->i () ([x integer?]) #:rest [rst (listof number?)] [r any/c]) (lambda w w) 'pos 'neg) 1 2)
;; => '(1 2)

((contract (->i () (#:x [x integer?]) #:rest [rst (listof number?)] [r any/c]) (lambda (#:x [x 1] . w) (cons x w)) 'pos 'neg) #:x 2 3)
;; => '(2 3)

((contract (->i () (#:x [x integer?]) #:rest [rst (listof number?)] [r any/c]) (lambda (#:x [x 1] . w) (cons x w)) 'pos 'neg)  2 3)
;; => '(1 2 3)

(let ([b (box '())])
  ((contract (->i ([i (box/c (listof integer?))])
                  (values [_ (i)
                             (begin 
                               (set-box! i (cons 1 (unbox i)))
                               (λ (x) 
                                 (set-box! i (cons 4 (unbox i)))
                                 #t))]
                          [_ (i)
                             (begin 
                               (set-box! i (cons 2 (unbox i)))
                               (λ (x) 
                                 (set-box! i (cons 5 (unbox i)))
                                 #t))]))
             (λ (i) 
               (set-box! i (cons 3 (unbox i)))
               (values 2 2))
             (quote pos)
             (quote neg))
   b)
  (unbox b))

;; => '(5 4 3 2 1)

|#
