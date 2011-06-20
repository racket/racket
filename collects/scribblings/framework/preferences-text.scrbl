#lang scribble/doc
@(require scribble/manual scribble/extract)
@(require (for-label framework))
@(require (for-label scheme/gui))
@title{Preferences, Textual}

@defmodule*/no-declare[(framework/preferences)]
@declare-exporting[framework/preferences framework]

@(include-extracted (lib "preferences.rkt" "framework"))
