#lang scribble/manual
@(require "utils.ss" 
          (for-label (only-in scribble/basic span-class)))

@(define (compat)
   @italic{For backward compatibility.})

@title[#:tag "basic"]{Compatibility Basic Functions}

@defmodule[scribble/basic]{The @schememodname[scribble/basic]
compatibility library mostly just re-exports
@schememodname[scribble/base].}

@defproc[(span-class [style-name string?] [pre-content any/c] ...)
         element?]{

@compat[] Wraps the @tech{decode}d
@scheme[pre-content] as an element with style @scheme[style-name].}



@defproc[(itemize [itm (or/c whitespace? an-item?)] ...
                  [#:style style (or/c style? string? symbol? #f) #f]) 
         itemization?]{

@compat[] Like @scheme[itemlist], but whitespace strings among the
@scheme[itm]s are ignored.}
