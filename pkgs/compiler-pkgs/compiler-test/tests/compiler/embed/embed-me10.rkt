(module embed-me10 mzscheme
  (require openssl/mzssl)

  (with-output-to-file (build-path (find-system-path 'temp-dir) "stdout")
    (lambda () 
      (printf "~a\n" ssl-available?))
    'append))


