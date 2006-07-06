(module installer mzscheme
  (require (lib "compile.ss" "profj")
           (prefix colors: (lib "installer.ss" "htdch" "colors"))
           (prefix geometry: (lib "installer.ss" "htdch" "geometry")))
  (provide installer)
  
  (define (mprintf . a)
    (fprintf a (current-error-port)))
  
  (define (installer plthome)
    (colors:installer plthome)
    (geometry:installer plthome)
    (let ((draw-path (build-path (collection-path "htdch" "draw"))))
      (let ((javac
             (lambda (file)
               (parameterize ([current-load-relative-directory draw-path]
                              [current-directory draw-path] )
                 (compile-java 'file 'file 'full
                               (build-path draw-path file)
                               #f #f)))))
        (javac "Canvas.java")
        (javac "World.java")))))
           
