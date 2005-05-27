(module installer mzscheme
  (require (lib "compile.ss" "profj"))
  (provide installer)

  (define (installer plthome)
    (let ([java.util (build-path
		      (collection-path "profj" "libs" "java" "util"))])
      (let ([javac
	     (lambda (file)
	       (parameterize ([current-load-relative-directory
			       java.util])
		 (compile-java 'file
			       'file
			       'full
			       (build-path java.util file)
			       #f
			       #f)))])
	(javac "Random.java")))))