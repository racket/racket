(define EO (new EvenOddC%))

(list (interface? EvenOdd<%>)
      (class? EvenOddC%)
      (send EO EvenOdd<%>-even 4)
      (not (send EO EvenOdd<%>-even 5))
      (send EO EvenOdd<%>-odd 7)
      (not (send EO EvenOdd<%>-odd 6)))
