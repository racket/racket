#reader(lib "docreader.ss" "scribble")
@require["common.ss"]

@title{PLT Scheme GUI Reference}

This manual describes MrEd.

@bold{This reference describes a potential future version of PLT Scheme.
      It does not match the current implementation.}

@table-of-contents[]

@;------------------------------------------------------------------------

@section{GUI Toolbox Organization}

For documentation purposes, the MrEd toolbox is organized into three
parts:

@itemize{

 @item{The @deftech{windowing} toolbox, for implementing form-filling
 GUI programs (such as a database query window) using buttons, menus,
 text fields, and events. The windowing toolbox is documented in
 @secref["mr:windowing"].}

 @item{The @deftech{drawing} toolbox, for drawing pictures or
 implementing dynamic GUI programs (such as a video game) using
 drawing canvases, pens, and brushes. The drawing toolbox is
 documented in @secref["mr:drawing"].}

 @item{The @deftech{editor} toolbox, for developing traditional text
 editors, editors that mix text and graphics, or free-form layout
 editors (such as a word processor, HTML editor, or icon-based file
 browser).  The editor toolbox is documented in @secref["mr:editor"].}

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

The module @indexed-scheme[#%mred-kernel] is built into the MrEd
 executable, and intended for use only by @file{mred.ss}.  Attempting
 to require @file{mred.ss} in a plain MzScheme executable will result
 in a run-time error, because @scheme[#%mred-kernel] will not be
 available.

To create a namespace in which the @file{mred.ss} module will be used,
 call the @scheme[make-namespace-with-mred] procedure. That
 procedure attaches the @file{mred.ss} instance of the current
 namespace to the created namespace. Otherwise, different namespaces
 create different instances of the @file{mred.ss} module, which in
 turn generate distinct classes.

@;------------------------------------------------------------------------

@include-section["windowing.scrbl"]

@;------------------------------------------------------------------------

@include-section["drawing.scrbl"]

@;------------------------------------------------------------------------

@index-section["mred-index"]
