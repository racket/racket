(module embed-me7 mzscheme
  (require plot
           mred
           mzlib/class)
  
  (define img (plot (line (lambda (x) x))))

  (define e (new text%))

  (send e insert img)
  
  (with-output-to-file "stdout"
    (lambda () 
      (printf "plotted\n"))
    'append))
