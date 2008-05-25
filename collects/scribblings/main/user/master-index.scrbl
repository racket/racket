#lang scribble/doc
@(require scribble/basic
          scribble/decode
          "../private/front-toc.ss")

@title[#:style '(no-toc)]{Master Index}

@front-toc['index #f]

@(make-splice (index-blocks))
