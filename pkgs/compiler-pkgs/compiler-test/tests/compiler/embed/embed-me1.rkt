(module embed-me1 mzscheme
  (with-output-to-file "stdout"
    (lambda () (printf "This is 1\n"))
    'append))

