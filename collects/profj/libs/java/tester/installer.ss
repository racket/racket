(module installer mzscheme
  (require profj/compile)
  (provide installer)

  (define (installer plthome)
    (let ([java.test (build-path
		      (collection-path "profj" "libs" "java" "tester"))])
      (let ([javac
	     (lambda (file)
	       (parameterize ([current-load-relative-directory
			       java.test])
		 (compile-java 'file
			       'file
			       'full
			       (build-path java.test file)
			       #f
			       #f)))])
	(javac "TestBase.djava")))))
