(module embed-me3 mzscheme
  (require mzlib/etc)
  (with-output-to-file "stdout"
    (lambda () 
      (printf "3 is here, too? ~a\n" true))
    'append))

