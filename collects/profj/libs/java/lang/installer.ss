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
        ;Exceptions
        (javac "ClassNotFoundException.java")
        (javac "CloneNotSupportedException.java")
        (javac "IllegalAccessException.java")
        (javac "IllegalArgumentException.java")
        (javac "IllegalMonitorStateException.java")
        (javac "IllegalStateException.java")
        (javac "IllegalThreadStateException.java")
        (javac "InstantiationException.java")
        (javac "InterruptedException.java")
        (javac "NoSuchFieldException.java")
        (javac "NoSuchMethodException.java")
        (javac "NumberFormatException.java")
        (javac "SecurityException.java")
        (javac "StringIndexOutOfBoundsException.java")
        (javac "UnsupportedOperationException.java")

        ;Misc
        (javac "Util.djava")
	(javac "Math.java")
        (javac "System.java")
        
        ;Wrappers
	(javac "Number.java")
        (javac "Boolean.java")
        (javac "Character.djava")
        (javac "Integer.java")
        (javac "Short.java")
        (javac "Byte.java")
        (javac "Long.java")
        (javac "Double.java")
        (javac "Float.java")
        (javac "CharSequence.java")))))
