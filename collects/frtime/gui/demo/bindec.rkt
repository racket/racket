#lang frtime
(require frtime/gui/simple)
(require (rename-in mred [horizontal-panel% horizontal-panel%]))

; just change this to change the range of the binary/decimal converter
(define SIZE 10) 

(define (bool-lst->num bool-lst)
  (let loop ([lst bool-lst] [sum 0] [pow 0])
    (if (empty? lst)
        sum
        (loop (cdr lst) 
              (+ sum
                 (if (car lst)
                     (expt 2 pow)
                     0))
              (add1 pow)))))

(define (place-num->bool loc num)
  (if (= 0 loc)
      (odd? num)
      (place-num->bool (sub1 loc) (quotient num 2))))


(current-widget-parent (new ft-frame% (label "Binary<-->Decimal")))

(define-values-rec
  [sld (mode value-b ft-slider%
             (stretchable-width #t)
             (min-value 0)
             (max-value (sub1 (expt 2 SIZE)))
             (value-set (changes 
                           (bool-lst->num
                            boxes))))]
  
  [boxes (parameterize ([current-widget-parent
                         (mode widget horizontal-panel%)])
           (build-list SIZE ; build-list is right associative.
                       (lambda (i)
                         (mode value-b ft-check-box%
                               (label (number->string (expt 2 i)))
                               (value-set 
                                (changes (place-num->bool i sld)))))))])

(send (current-widget-parent) show #t)
