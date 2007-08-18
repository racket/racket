#reader(lib "docreader.ss" "scribble")
@require["common.ss"]

@title[#:style '(toc reveal)]{Guide}

For documentation purposes, the MrEd toolbox is organized into three
parts:

@itemize{

 @item{The @deftech{windowing} toolbox, for implementing form-filling
 GUI programs (such as a database query window) using buttons, menus,
 text fields, and events. The windowing toolbox is described in
 @secref["mr:windowing-overview"].}

 @item{The @deftech{drawing} toolbox, for drawing pictures or
 implementing dynamic GUI programs (such as a video game) using
 drawing canvases, pens, and brushes. The drawing toolbox is
 described in @secref["mr:drawing-overview"].}

 @item{The @deftech{editor} toolbox, for developing traditional text
 editors, editors that mix text and graphics, or free-form layout
 editors (such as a word processor, HTML editor, or icon-based file
 browser).  The editor toolbox is described in
 @secref["mr:editor-overview"].}

}

These three parts roughly represent layers of increasing
 sophistication. Simple GUI programs access only the windowing toolbox
 directly, more complex programs use both the windowing and drawing
 toolboxes, and large-scale applications rely on all three
 toolboxes. This three-layer view of the toolbox breaks down under
 close scrutiny, because the windowing, drawing, and editor toolboxes
 are actually interdependent and intertwined. Nevertheless, the
 layered separation is a good approximation.

All three parts are immediately available when MrEd is started, as
 well as the base class system from MzLib. The @indexed-file{mred.ss}
 library module of the @file{mred} collection provides all of the
 class, interface, and procedure names defined in this manual.  When
 MrEd starts up, it imports the @file{mred.ss} module and MzLib's
 @indexed-file{class.ss} module into the initial namespace (so no
 knowledge about modules is required to understand this manual).

@local-table-of-contents[]

@;------------------------------------------------------------------------

@include-section["win-overview.scrbl"]

@;------------------------------------------------------------------------

@include-section["draw-overview.scrbl"]

@;------------------------------------------------------------------------

@include-section["editor-overview.scrbl"]
