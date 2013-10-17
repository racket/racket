#lang meta/web

(provide (all-defined-out))

(define (css url) @link[href: url rel: "stylesheet" type: "text/css"]{})
(define (icon name) @i[class: name]{})
(define (row . content) (div class: "row" content))

(define (panetitle . l) @div[class: "panetitle" l])

(define (print-num n)
 (list-ref 
  '("one" "two" "three" "four" "five" "six"
    "seven" "eight" "nine" "ten" "eleven" "twelve" "thirteen")
  (sub1 n)))

(define (columns n #:row? [row? #f] #:center-text? [center-text? #f]
                 #:center? [center? #f] #:tag [tag div]
                 #:push [push #f] . body)
  (define d (apply tag class: (list (print-num n) " columns" 
                                    (and center? " centered")
                                    (and center-text? " center-text")
                                    (and push 
                                         (list " push_" (print-num push))))
                   body))
  (if row? (row d) d))