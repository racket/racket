#lang scribble/doc
@(require scribble/manual scribble/extract)
@(require (for-label framework/framework))
@(require (for-label scheme/gui))
@title{GUI Utilities}

@(require framework/framework-docs)
@(defmodule framework/gui-utils)

@(include-extracted (lib "gui-utils.ss" "framework"))
