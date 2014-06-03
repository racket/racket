(module embed-me9 mzscheme
  (require "embed-me8.ss")
  (with-output-to-file (build-path (find-system-path 'temp-dir) "stdout")
    (lambda () 
      (printf "~a\n" (ex)))
    'append)
  
  (module test racket/base))

