#lang scribble/doc
@(require scribble/basic
          scribble/decode
          "../private/front-toc.ss")

@title{Master Index}

@front-toc['index]

@(make-splice (index-blocks))
