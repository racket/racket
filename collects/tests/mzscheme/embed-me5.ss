(module embed-me5 mzscheme
  (require (lib "mred.ss" "mred"))
  (with-output-to-file "stdout"
    (lambda () (printf "This is 5: ~s\n" button%))
    'append))

