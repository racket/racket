#lang scribble/doc
@(require scribble/manual "guide-utils.rkt" scribblings/private/docname)

@title[#:tag "graphics"]{Graphics and GUIs}

Racket provides many libraries for graphics and graphical user
interfaces (GUIs):

@itemlist[

 @item{The @racketmodname[racket/draw #:indirect] library provides basic drawing
       tools, including drawing contexts such as bitmaps and
       PostScript files.

       See @Draw[] for more information.}

 @item{The @racketmodname[racket/gui #:indirect] library provides GUI widgets
       such as windows, buttons, checkboxes, and text fields. The
       library also includes a sophisticated and extensible text
       editor.

       See @GUI[] for more information.}

 @item{The @racketmodname[pict #:indirect] library provides a more
       functional abstraction layer over @racketmodname[racket/draw].
       This layer is especially useful for creating slide
       presentations with @Slideshow{Slideshow}, but
       it is also useful for creating images for @seclink[#:doc '(lib
       "scribblings/scribble/scribble.scrbl") "top"]{Scribble}
       documents or other drawing tasks. Pictures created with the
       @racketmodname[pict #:indirect] library can be rendered to any
       drawing context.

       See @Slideshow[] for more information.}

 @item{The @racketmodname[2htdp/image #:indirect] library is similar to
       @racketmodname[pict #:indirect]. It is more streamlined for
       pedagogical use, but also slightly more specific to screen and
       bitmap drawing.

       See @racketmodname[2htdp/image #:indirect] for more information.}

 @item{The @racketmodname[sgl #:indirect] library provides OpenGL for 3-D
       graphics. The context for rendering OpenGL can be a window or
       bitmap created with @racketmodname[racket/gui #:indirect].

       See @other-doc['(lib "sgl/scribblings/sgl.scrbl") #:indirect "SGL"] for more
       information.}

]
