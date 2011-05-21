#lang racket
(require racket/runtime-path
         tests/eli-tester
         scriblib/bibtex)

(define-runtime-path example.bib "example.bib")

(test
 (let ()
   (define-bibtex-cite example.bib
     ~cite-id citet-id generate-bibliography-id)
   (test
    (~cite-id "cryptoeprint:2000:067")
    (~cite-id "Tobin-Hochstadt:2011fk")
    (~cite-id "cryptoeprint:2000:067" "Tobin-Hochstadt:2011fk")
    (~cite-id "cryptoeprint:2000:067 Tobin-Hochstadt:2011fk")
    
    (citet-id "cryptoeprint:2000:067")
    (citet-id "Tobin-Hochstadt:2011fk")
    (citet-id "Tobin-Hochstadt:2011fk" "Tobin-Hochstadt:2011fk")
    (citet-id "Tobin-Hochstadt:2011fk Tobin-Hochstadt:2011fk")
    
    (generate-bibliography-id))))