#lang racket
(require racket/runtime-path
         tests/eli-tester
         scriblib/bibtex)

(define-runtime-path example.bib "example.bib")

(test
 (let ()
   (define example (path->bibdb example.bib))
   (define raw (bibdb-raw example))

   (test
    (hash-ref (hash-ref raw "sweig42") "month") => "march"
    (hash-ref (hash-ref raw "sweig42a") "month") => "1~mar"
    (hash-ref (hash-ref raw "sweig42b") "month") => "1~march"
    (hash-ref (hash-ref raw "sweig42c") "month") => "1~marcha"))
 (let ()
   (define-bibtex-cite example.bib
     ~cite-id citet-id generate-bibliography-id)

   (~cite-id "cryptoeprint:2000:067")
   (~cite-id "Tobin-Hochstadt:2011fk")
   (~cite-id "cryptoeprint:2000:067" "Tobin-Hochstadt:2011fk")
   (~cite-id "cryptoeprint:2000:067 Tobin-Hochstadt:2011fk")
   
   (citet-id "cryptoeprint:2000:067")
   (citet-id "Tobin-Hochstadt:2011fk")
   (citet-id "Tobin-Hochstadt:2011fk" "Tobin-Hochstadt:2011fk")
   (citet-id "Tobin-Hochstadt:2011fk Tobin-Hochstadt:2011fk")
   
   (generate-bibliography-id)))

