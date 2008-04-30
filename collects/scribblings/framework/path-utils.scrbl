#lang scribble/doc
@(require scribble/manual scribble/extract)
@(require (for-label framework))
@(require (for-label scheme/gui))
@title{Path Utils}


@(include-extracted (lib "main.ss" "framework") #rx"^path-utils:")
