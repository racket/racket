#lang plai/gc2/mutator
(allocator-setup "../good-collectors/good-collector.rkt" 104)
(halt-on-errors)

;(check-temps1 temps) -> boolean?
; temps : (listof number?)
;Consumes a list of temperature measures and checks whether all measurements are between 5 and 95 degrees celsius (inclusively.)

(define (check-temps1 temps)
  (if (empty? temps) #t ;divide end of the list / empty list from not-the-end
    (and ; ? this temp is between 5 & 95
     (>= (first temps) 5)
     (<= (first temps) 95)
     (check-temps1 (rest temps)) ; ? and so are all the rest
    )
  )
)

(check-temps1 '(7 10 32 87))
(check-temps1 '(7 4 32 87))
(check-temps1 '(7 98 32 87))

;(check-temps temps low high) -> boolean?
;  temps : (listof number?)
;  low : number?
;  high : number?
;Consumes a list of temperature measures and checks whether all measurements are between low and high degrees celsius (inclusively.)

(define (check-temps temps low high)
  (if (empty? temps) #t ;divide end of the list / empty list from not-the-end
    (and ; ? this temp is between low & high
     (>= (first temps) low)
     (<= (first temps) high)
     (check-temps (rest temps) low high) ; ? and so are all the rest
    )
  )
)

(check-temps '(7 10 32 87) 5 90)
(check-temps '(7 10 32 87) 8 90)
(check-temps '(7 10 32 87) 5 80)

;(convert digits) -> number?
;  digits : (listof number?)
;Consumes a list of digits (numbers between 0 and 9) and produces the corresponding number. The first digit is the least significant, and so on.

(define (convert digits)
  (if (empty? digits) 0 ;at the end, don't add anything!
    (+ (first digits) (* 10 (convert (rest digits))))
  )
)

(convert '(1 2 3))

;(average-price prices) -> number?
;  prices : (listof number?)
;Consumes a list of toy prices and computes the average price of a toy. The average is total of all prices divided by the number of toys.

(define (count-prices prices sum count) ;need to introduce some extra parameters to hold the count & sum for averaging
  (if (empty? prices) (/ sum count) ;end of the list- calculate the average
    (count-prices (rest prices) (+ sum (first prices)) (+ count 1)) ;increment the count & update the total sum
  )
)
(define (average-price prices)
  (if (empty? prices) 0 ;divide special case empty list from everything else
    (count-prices prices 0 0)
  )
)

(average-price '())
(average-price '(5 15 32 6))

;(convertFC fahrenheit) -> (listof number?)
;  fahrenheit : (listof number?)
;Converts a list of of Fahrenheit measurements to a list of Celsius measurements.

(define (convertFC fahrenheit)
  (if (empty? fahrenheit) empty
    (cons (* (- (first fahrenheit) 32) 5/9) (convertFC (rest fahrenheit)))
  )
)

(convertFC '(-40 15 32 50 60 85))

;(eliminate-exp ua lotp) -> (listof number?)
;  ua : number?
;  lotp : (listof number?)
;Eliminates from lotp all toys whose price is greater than ua.

(define (eliminate-exp ua lotp)
  (cond
    [(empty? lotp) empty] ; end of the list
    [(> (first lotp) ua) (eliminate-exp ua (rest lotp))] ;skip this element
    [else (cons (first lotp) (eliminate-exp ua (rest lotp)))] ;keep this element
  )
)

(eliminate-exp 15 '(1 16 3 5 22 8))
