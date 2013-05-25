(module embed-me2 mzscheme
  (require "embed-me1.ss"
	   mzlib/etc)
  (with-output-to-file "stdout"
    (lambda () (printf "This is 2: ~a\n" true))
    'append))
