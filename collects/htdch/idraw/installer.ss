(module installer mzscheme
  (require (lib "compile.ss" "profj"))
  (provide installer)

  (define (installer plthome)
    (let ((draw-path (build-path (collection-path "htdch" "idraw"))))
      (let ((javac
             (lambda (file)
               (parameterize ([current-load-relative-directory draw-path]
                              [current-directory draw-path] )
                 (compile-java 'file 'file 'full
                               (build-path draw-path file)
                               #f #f)))))
        (javac "Canvas.java")
        (javac "World.java")
	#|
        (javac "Posn.java")
        (javac "Color.java")
        (javac "Red.java")
        (javac "White.java")
        (javac "Blue.java")
        (javac "Black.java")
        (javac "Green.java")
        (javac "Yellow.java")
	|#
        ))))
