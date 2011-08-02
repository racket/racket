#lang racket/base
(require scribble/render
         scribble/text-render
         scribble/core
         racket/file)

(render (list (part #f
                    null
                    (list "Render")
                    (style #f null)
                    null
                    (list (paragraph (style #f null) "The content."))
                    null))
        (list "example")
        #:render-mixin render-mixin)

(unless (string=? "Render\n\nThe content.\n"
                  (file->string "example.txt"))
  (error "render test failed"))

(delete-file "example.txt")

