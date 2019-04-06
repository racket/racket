#lang racket/base
(require rackunit racket/dict racket/list racket/string racket/math)

(define (sorted-assoc-dict alst) (sort alst keyword<? #:key car))
(define (unsorted-assoc-dict alst)
  (cond [(or (empty? alst) (empty? (rest alst))) alst]
        [else
         (define shf (shuffle alst))
         (if (apply keyword<? (map car shf)) (reverse shf) shf)]))
(define assoc-dicts (list sorted-assoc-dict unsorted-assoc-dict))

(define ihash-dict make-immutable-hash)
(define ihasheqv-dict make-immutable-hasheqv)
(define ihasheq-dict make-immutable-hasheq)
(define ihash-dicts (list ihash-dict ihasheqv-dict ihasheq-dict))

(define mhash-dict make-hash)
(define mhasheqv-dict make-hasheqv)
(define mhasheq-dict make-hasheq)
(define mhash-dicts (list mhash-dict mhasheqv-dict mhasheq-dict))

(define dicts (append assoc-dicts ihash-dicts mhash-dicts))

(for ([d (in-list dicts)])
  (define (name x)
    (format "keyword-apply/dict with ~a: ~a" (object-name d) x))

  (test-case (name "go")
    (define (go #:mode mode target) (list target mode))
    (check-equal? (keyword-apply/dict go (d '((#:mode . fast))) '("super.rkt"))
                  '("super.rkt" fast)))

  (test-case (name "sundae")
    (define (sundae #:ice-cream [ice-cream '("vanilla")]
                    #:toppings [toppings '("brownie-bits")]
                    #:sprinkles [sprinkles "chocolate"]
                    #:syrup [syrup "caramel"])
      (format "A sundae with ~a ice cream, ~a, ~a sprinkles, and ~a syrup."
              (string-join ice-cream #:before-last " and ")
              (string-join toppings #:before-last " and ")
              sprinkles
              syrup))
    (check-equal? (keyword-apply/dict sundae
                                      (d '((#:ice-cream . ("chocolate"))))
                                      '())
                  "A sundae with chocolate ice cream, brownie-bits, chocolate sprinkles, and caramel syrup.")
    (check-equal? (keyword-apply/dict sundae
                                      (d '((#:toppings . ("cookie-dough"))
                                           (#:sprinkles . "rainbow")
                                           (#:syrup . "chocolate")))
                                      '())
                  "A sundae with vanilla ice cream, cookie-dough, rainbow sprinkles, and chocolate syrup.")
    (check-equal? (keyword-apply/dict sundae
                                      #:sprinkles "rainbow"
                                      (d '((#:toppings . ("cookie-dough"))
                                           (#:syrup . "chocolate")))
                                      '())
                  "A sundae with vanilla ice cream, cookie-dough, rainbow sprinkles, and chocolate syrup."))

  (test-case (name "f mand y opt z")
    (define (f x #:y y #:z [z 10])
      (list x y z))

    (check-equal? (keyword-apply/dict f (d '((#:y . 2))) '(1)) '(1 2 10))
    (check-equal? (keyword-apply/dict f (d '((#:y . 2) (#:z . 3))) '(1)) '(1 2 3))
    (check-equal? (keyword-apply/dict f #:z 7 (d '((#:y . 2))) '(1)) '(1 2 7)))

  (test-case (name "dotted-h")
    (define (dotted-h x #:y [y 12])
      (list x y))

    (check-equal? (keyword-apply/dict dotted-h (d '()) (list 2)) '(2 12))
    (check-equal? (keyword-apply/dict dotted-h (d '((#:y . 8))) (list 3)) '(3 8))
    (check-equal? (keyword-apply/dict dotted-h (d '((#:y . 14))) '(g)) '(g 14)))

  (test-case (name "f mand a b c")
    (define (f #:a a #:b b #:c c d e f) (list a b c d e f))

    (check-equal? (keyword-apply/dict
                   f
                   (d '((#:b . "b") (#:c . "c") (#:a . "a")))
                   '("d" "e" "f"))
                  (list "a" "b" "c" "d" "e" "f")))

  (test-case (name "kinetic-energy")
    (define (kinetic-energy #:mass m #:velocity v)
      (* 1/2 m (sqr v)))
    (check-equal? (keyword-apply/dict kinetic-energy '((#:mass . 2) (#:velocity . 1)) '())
                  1)
    (check-equal? (keyword-apply/dict kinetic-energy '((#:mass . 5) (#:velocity . 3)) '())
                  (+ 22 1/2)))

  (test-case (name "error keyword duplicated")
    (check-exn
     #rx"keyword-apply/dict: keyword duplicated in dict and direct keyword arguments: '#:color"
     (λ ()
       (keyword-apply/dict void (d '((#:color . "green"))) #:color "red" '())))))

