(module installer mzscheme
  (require (lib "compile.ss" "profj"))
  (provide installer)
  
  (define (installer plthome)
    (let ((draw-path (build-path (collection-path "htdch" "draw"))))
      (let ((javac
             (lambda (file)
               (parameterize ([current-load-relative-directory draw-path])
                 (compile-java 'file 'file 'full
                               (build-path draw-path file)
                               #f #f)))))
        (javac "Posn.java")
        (javac "Color.java")
;        (javac "ICanvas.java")
        (javac "World.java")
        (javac "Red.java")
        (javac "White.java")
        (javac "Blue.java")
        (javac "Black.java")
        (javac "Green.java")
        (javac "Yellow.java")))))
           
