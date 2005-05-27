(module pre-installer mzscheme
  (require (lib "compile.ss" "profj"))
  (provide pre-installer)
  
  (define (pre-installer plthome)
    (let ((draw-path (build-path (collection-path "htdch" "draw"))))
      (let ((javac
             (lambda (file)
               (parameterize ([current-load-relative-directory draw-path])
                 (compile-java 'file 'file 'full
                               (build-path draw-path file)
                               #f #f)))))
        (javac "Posn.java")
        ))))
           