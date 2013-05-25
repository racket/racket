#lang scribble/doc
@(require "../common.rkt" (for-label mrlib/hierlist))

@title[#:style 'toc]{Hierarchical List Control}

@defmodule[mrlib/hierlist]

A @racket[hierarchical-list%] control is a list of items, some of
which can themselves be hierarchical lists. Each such sub-list has an
arrow that the user can click to hide or show the sub-list's items.

The list control supports the following default keystrokes:

@itemize[
 
 @item{Down: move to the next entry at the current level (skipping lower levels).}

 @item{Up: move to the previous entry at the current level (skipping lower levels).}

 @item{Left: move to the enclosing level (only valid at embedded levels).}

 @item{Right: move down in one level (only valid for lists).}

 @item{Return:  open/close the current selected level (only valid for lists).}

]


@local-table-of-contents[]

@include-section["list.scrbl"]
@include-section["item.scrbl"]
@include-section["compound-item.scrbl"]
@include-section["snips.scrbl"]

