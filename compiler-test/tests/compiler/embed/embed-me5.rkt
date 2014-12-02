(module embed-me5 mzscheme
  (require mred)
  (with-output-to-file (build-path (find-system-path 'temp-dir) "stdout")
    (lambda () (printf "This is 5: ~s\n" button%))
    'append))

