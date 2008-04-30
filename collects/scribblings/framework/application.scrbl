#lang scribble/doc
@(require scribble/manual scribble/extract)
@(require (for-label framework))
@(require (for-label scheme/gui))
@title{Application}


@(include-extracted (lib "main.ss" "framework") #rx"^application:")
