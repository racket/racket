#lang racket
(require racket/runtime-path
         tests/eli-tester
         scriblib/bibtex
         scriblib/autobib
         scribble/render
         (prefix-in text: scribble/text-render))

(define-runtime-path example.bib "example.bib")

(define-runtime-path normal-expected-path "bibtex.normal.txt")
(define-runtime-path number-expected-path "bibtex.number.txt")

(define-syntax-rule (test-render* definer expected-path body generate-bibliography-id)
  (let ()
    definer

    body

    (define actual-path
      (make-temporary-file "~a-bibtex.txt"))

    (render (list (generate-bibliography-id))
            (list actual-path)
            #:dest-dir (path-only actual-path)
            #:render-mixin text:render-mixin)

    (test
     (file->string actual-path) => (file->string expected-path))))

(define-syntax-rule (test-render expected-path options body)
  (begin
    (test-render* (define-bibtex-cite example.bib
                    ~cite-id citet-id generate-bibliography-id . options)
                  expected-path
                  (body ~cite-id citet-id)
                  generate-bibliography-id)
    (test-render* (begin
                    (define-cite autobib-cite autobib-citet
                      generate-bibliography-id . options)
                    (define-bibtex-cite* example.bib
                      autobib-cite autobib-citet
                      ~cite-id citet-id))
                  expected-path
                  (body ~cite-id citet-id)
                  generate-bibliography-id)))

(test
 (let ()
   (define example (path->bibdb example.bib))
   (define raw (bibdb-raw example))

   (test
    (hash-ref (hash-ref raw "sweig42") "month") => "march"
    (hash-ref (hash-ref raw "sweig42a") "month") => "1~mar"
    (hash-ref (hash-ref raw "sweig42b") "month") => "1~march"
    (hash-ref (hash-ref raw "sweig42c") "month") => "1~marcha"))

 (test-render normal-expected-path ()
              (λ (~cite-id citet-id)
                (~cite-id "salib:starkiller")
                (~cite-id "cryptoeprint:2000:067")
                (~cite-id "Tobin-Hochstadt:2011fk")
                (~cite-id "cryptoeprint:2000:067" "Tobin-Hochstadt:2011fk")
                (~cite-id "cryptoeprint:2000:067 Tobin-Hochstadt:2011fk")

                (citet-id "salib:starkiller")
                (citet-id "cryptoeprint:2000:067")
                (citet-id "Tobin-Hochstadt:2011fk")
                (citet-id "Tobin-Hochstadt:2011fk" "Tobin-Hochstadt:2011fk")
                (citet-id "Tobin-Hochstadt:2011fk Tobin-Hochstadt:2011fk")))
 (test-render number-expected-path (#:style number-style)
              (λ (~cite-id citet-id)
                (citet-id "salib:starkiller")
                (citet-id "cryptoeprint:2000:067")
                (citet-id "Tobin-Hochstadt:2011fk"))))
