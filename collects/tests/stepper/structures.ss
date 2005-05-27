(define-struct item (name price))

(define inventory (cons (make-item 'rabbit 32.42)
                        (cons (make-item 'twiggy 3.18)
                              (cons (make-item 'richard-nixon 0.45)
                                    empty))))

(+ 3 4)

(define (sum-up-prices item-list)
  (cond ([null? item-list] 0)
        (else (+ (item-price (car item-list))
                 (sum-up-prices (cdr item-list))))))

(sum-up-prices inventory)
