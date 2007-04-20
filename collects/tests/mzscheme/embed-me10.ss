(module embed-me10 mzscheme
  (require (lib "mzssl.ss" "openssl"))

  (with-output-to-file "stdout"
    (lambda () 
      (printf "~a\n" ssl-available?))
    'append))


