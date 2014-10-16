(module embed-me3 mzscheme
  (require mzlib/etc)
  (with-output-to-file (build-path (find-system-path 'temp-dir) "stdout")
    (lambda () 
      (printf "3 is here, too? ~a\n" true))
    'append))

