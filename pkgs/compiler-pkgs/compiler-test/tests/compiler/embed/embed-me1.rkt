(module embed-me1 mzscheme
  (with-output-to-file (build-path (find-system-path 'temp-dir) "stdout")
    (lambda () (printf "This is 1\n"))
    'append))

