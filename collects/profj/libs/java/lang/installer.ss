(module installer mzscheme
  (require (lib "compile.ss" "profj"))
  (provide installer)

  (define (installer plthome)
    (let ([java.lang (build-path
		      (collection-path "profj" "libs" "java" "lang"))])
      (let ([javac
	     (lambda (file)
	       (parameterize ([current-load-relative-directory
			       java.lang]
                              [current-directory java.lang])
		 (compile-java 'file
			       'file
			       'full
			       (build-path java.lang file)
			       #f
			       #f)))])
        (javac "Util.djava")
        ;(printf "util~n")
	(javac "Math.java")
        ;(printf "math~n")
        (javac "System.java")
        ;(printf "sys~n")
	(javac "Number.java")
        ;(printf "num~n")
        (javac "Boolean.java")
        ;(printf "boolean ~n")
        (javac "Character.djava")
        ;(printf "character ~n")
        (javac "Integer.java")
        ;(printf "integer~n")
        (javac "Short.java")
        (javac "Byte.java")
        (javac "Long.java")
        (javac "Double.java")
        (javac "Float.java")
        (javac "CharSequence.java")))))
