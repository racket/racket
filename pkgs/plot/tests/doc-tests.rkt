#lang racket

(require plot/private/doc
         scribble/manual
         scribble/render
         scribble/text-render
         scribble/decode
         (for-syntax racket/base racket/syntax))

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

(define-syntax (doc stx)
  (syntax-case stx ()
    [(_ name)  (with-syntax ([name:doc  (format-id #'name "~a:doc" #'name)])
                 (syntax/loc stx
                   (display-doc (name:doc))))]))

(doc plot/dc)
(newline)
(doc plot-background)
(newline)
(doc known-point-symbols)
(newline)
