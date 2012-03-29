#lang racket/base
(require rackunit
         scribble/render
         scriblib/autobib)

(check-not-exn
 (λ ()
   (define-cite ~cite citet generate-bib)

   (~cite (make-bib #:title "Test with nothing"))
   (generate-bib)))

(check-not-exn
 (λ ()
   (define-cite ~cite citet generate-bib)

   (~cite (make-bib #:title "Test with nothing"
                    #:author (author-name "Quick" "Checkerson")))
   (render (list (generate-bib))
           (list "test.scrbl"))))
