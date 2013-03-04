#lang scribble/base
@(require scriblib/render-cond)

@(cond-element
  [text "Text!"]
  [html "HTML!"]
  [latex "Latex!"]
  [markdown "Markdown!"])

@(cond-element
  [(or text html) "Text or HTML!"]
  [else "Latex!"])

@(cond-element
  [(and text html) "Text and HTML?!"]
  [else "Other!"])

@(cond-element
  [(not text) "Not Text!"]
  [else "Text!"])
