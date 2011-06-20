#lang scribble/doc
@(require "common.rkt")

@title[#:tag "control-snips" #:style 'toc]{Control Snips}

To allow the buttons to be pushed, the editor in which they appear
must forward clicks to them properly.

@local-table-of-contents[]

@include-section["snip-wrapper.scrbl"]
@include-section["text-button-snip.scrbl"]
@include-section["button-snip.scrbl"]
@include-section["toggle-button-snip.scrbl"]
