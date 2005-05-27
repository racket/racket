(module installer mzscheme
  (require (lib "compile.ss" "profj"))
  (provide installer)

  (define (installer plthome)
    (let ([java.lang (build-path
		      (collection-path "profj" "libs" "java" "lang"))])
      (let ([javac
	     (lambda (file)
	       (parameterize ([current-load-relative-directory
			       java.lang])
		 (compile-java 'file
			       'file
			       'full
			       (build-path java.lang file)
			       #f
			       #f)))])
	(javac "Math.java")
        (javac "System.java")
	(javac "Number.java")
        (javac "Double.java")
        (javac "Float.java")
        (javac "Boolean.java")
        (javac "CharSequence.java")))))
