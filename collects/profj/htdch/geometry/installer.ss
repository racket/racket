(module installer mzscheme
  (require profj/compile)
  (provide installer)
  
  (define (mprintf . a)
    (fprintf a (current-error-port)))
  
  (define (installer plthome)
    (let ((draw-path (build-path (collection-path "profj" "htdch" "geometry"))))
      (let ((javac
             (lambda (file)
               (parameterize ([current-load-relative-directory draw-path]
                              [current-directory draw-path] )
                 (compile-java 'file 'file 'full
                               (build-path draw-path file)
                               #f #f)))))
        (javac "Posn.java")))))
           
