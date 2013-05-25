#lang racket/base
(require scribble/render
         scribble/text-render
         scribble/core
         racket/file
         tests/eli-tester)

(provide render-tests)
(module+ main (render-tests))
(define (render-tests)
  (dynamic-wind
    (λ() (render (list (part #f
                             null
                             (list "Render")
                             (style #f null)
                             null
                             (list (paragraph (style #f null) "The content."))
                             null))
                 (list "example-for-render-test")
                 #:render-mixin render-mixin))
    (λ() (test (file->string "example-for-render-test.txt")
               => "Render\n\nThe content.\n"))
    (λ() (delete-file "example-for-render-test.txt"))))
