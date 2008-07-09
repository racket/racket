(module embed-me10 mzscheme
  (require openssl/mzssl)

  (with-output-to-file "stdout"
    (lambda () 
      (printf "~a\n" ssl-available?))
    'append))


