(module installer mzscheme
  (require profj/compile)
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
	(javac "Random.java")
        (javac "RandomAccess.java")
        (javac "Iterator.java")
        (javac "ListIterator.java")
        (javac "Collection.java")
        (javac "List.java")
        (javac "AbstractCollection.java")
        (javac "ConcurrentModificationException.java")
        (javac "NoSuchElementException.java")
        (javac "AbstractList.java")
        (javac "AbstractSequentialList.java")
        ))))
