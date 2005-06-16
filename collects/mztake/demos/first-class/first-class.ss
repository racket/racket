(module first-class mzscheme
  (map (lambda (x)
         (let* ([x (* 2 (+ 1 x))]
                [x (sub1 x)])
           x))
       '(2 4 6 7)))