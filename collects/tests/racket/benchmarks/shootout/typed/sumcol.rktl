(for/fold: : Number
           ([acc : Number 0])
    ([n : String (in-lines)])
  (+ acc (assert (string->number n))))
