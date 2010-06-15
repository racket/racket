(for/fold: : Number
           ([acc : Number 0])
    ([n : (U String EOF) (in-lines)])
  (+ acc (assert (string->number n))))
