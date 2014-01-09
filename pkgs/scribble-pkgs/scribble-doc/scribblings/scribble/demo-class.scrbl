#lang scribble/base
@(require scribble/manual
          (for-label racket/base
                     racket/class
                     racket/contract/base
                     racket/draw))

@(define css tt)

@defclass/title[#:link-target? #f bitmap-dc% object% (dc<%>)]{

In multi-page mode, this class definition gets its own page, and
there's an ``inherited methods'' table in the margin.  The table has
style class @css{inherited}, and the words ``inherited methods:'' and
``from'' have style class @css{inheritedlbl}.

@defmethod[#:link-target? #f (set-bitmap [bm any/c]) any]{

A method example; nothing new here, but note how the defined
identifier is not at the start of the box.}

}

