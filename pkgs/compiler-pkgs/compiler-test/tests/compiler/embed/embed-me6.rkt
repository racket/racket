(module embed-me6 mzscheme
  (with-output-to-file (build-path (find-system-path 'temp-dir) "stdout")
    (lambda () 
      (printf "This is 6\n")
      (with-handlers ([void (lambda (exn) (printf "no etc.ss\n"))])
	(printf "~a\n" (dynamic-require 'mzlib/etc 'true))))
    'append))

