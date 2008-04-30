#lang scribble/doc
@(require scribble/manual scribble/extract)
@(require (for-label framework))
@(require (for-label scheme/gui))
@title{Color Model}


@(include-extracted (lib "main.ss" "framework") #rx"^color-model:")
