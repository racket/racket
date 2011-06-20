#lang scribble/doc
@(require scribble/manual scribble/extract
          (for-label framework racket/gui))
@title{GUI Utilities}

@defmodule*/no-declare[(framework/gui-utils)]
@declare-exporting[framework/gui-utils framework]

@(include-extracted (lib "gui-utils.rkt" "framework"))
