#lang scribble/doc
@(require scribble/manual "guide-utils.rkt")

@title[#:tag "graphics"]{Graphics and GUIs}

Racket provides many libraries for graphics and graphical user
interfaces (GUIs):

@itemlist[

 @item{The @racketmodname[racket/draw] library provides basic drawing
       tools, including drawing contexts such as bitmaps and
       PostScript files.

       See @other-doc['(lib "scribblings/draw/draw.scrbl")]
       for more information.}

 @item{The @racketmodname[racket/gui] library provides GUI widgets
       such as windows, buttons, checkboxes, and text fields. The
       library also includes a sophisticated and extensible text
       editor.

       See @other-doc['(lib "scribblings/gui/gui.scrbl")]
       for more information.}

 @item{The @racketmodname[slideshow/pict] library provides a more
       functional abstraction layer over @racketmodname[racket/draw].
       This layer is especially useful for creating slide
       presentations with @seclink[#:doc '(lib
       "scribblings/slideshow/slideshow.scrbl") "top"]{Slideshow}, but
       it is also useful for creating images for @seclink[#:doc '(lib
       "scribblings/scribble/scribble.scrbl") "top"]{Scribble}
       documents or other drawing tasks. Pictures created with the
       @racketmodname[slideshow/pict] library can be rendered to any
       drawing context.

       See @other-doc['(lib "scribblings/slideshow/slideshow.scrbl")]
       for more information.}

 @item{The @racket[2htdp/image] library is similar to
       @racketmodname[slideshow/pict]. It is more streamlined for
       pedagogical use, but also slightly more specific to screen and
       bitmap drawing.

       See @racket[2htdp/image] for more information.}

 @item{The @racketmodname[sgl] library provides OpenGL for 3-D
       graphics. The context for rendering OpenGL can be a window or
       bitmap created with @racketmodname[racket/gui].

       See @other-doc['(lib "sgl/scribblings/sgl.scrbl")] for more
       information.}

]
