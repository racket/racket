#lang scribble/manual

@(require scribble/eval)

@title{Pretty-Print-Handler Bug Example}

@(define the-eval (make-base-eval))
@(interaction-eval
  #:eval the-eval
  (begin
    (require racket/pretty)
    (current-print pretty-print-handler)))

@examples[#:eval the-eval
'((x "positional 1") 
    (rest ("positional 2" "positional 3"))
    (a ())
    (b ("b-arg"))
    (c (("first c1" "second c1") ("first c2" "second c2")))
    (d #f)
    (e ()))]
