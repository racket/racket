#lang racket

(require plot/doc
         scribble/manual
         scribble/render
         scribble/text-render
         scribble/decode)

(define (render-doc doc-part)
  (define path (make-temporary-file "racket-doc-~a.txt" #f (current-directory)))
  (dynamic-wind
   (λ () (void))
   (λ () 
     (render (list (decode (list (declare-exporting) doc-part)))
             (list path)
             #:render-mixin render-mixin)
     (file->lines path))
   (λ () (delete-file path))))

(define (display-doc doc-part)
  (for ([line  (in-list (render-doc doc-part))])
    (displayln line)))

(display-doc (plot/dc:doc))
(newline)
(display-doc (treeof:doc))
(newline)
(display-doc (plot-background:doc))
(newline)
(display-doc (known-point-symbols:doc))
(newline)
