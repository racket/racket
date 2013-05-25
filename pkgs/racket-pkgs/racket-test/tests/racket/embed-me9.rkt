(module embed-me9 mzscheme
  (require "embed-me8.ss")
  (with-output-to-file "stdout"
    (lambda () 
      (printf "~a\n" (ex)))
    'append))
