(module embed-me7 mzscheme
  (require (lib "plot.ss" "plot")
           (lib "mred.ss" "mred")
           (lib "class.ss"))
  
  (define img (plot (line (lambda (x) x))))

  (define e (new text%))

  (send e insert img)
  
  (with-output-to-file "stdout"
    (lambda () 
      (printf "plotted\n"))
    'append))
