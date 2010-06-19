#lang scribble/doc
@(begin
(require scribble/manual
         "common.rkt"
         (for-label scheme/gui/base)
         (for-label drracket/tool-lib)
         (for-label scheme/unit scheme/contract scheme/class)
         (for-label scheme/base)
         (for-label framework/framework)
         (for-label drracket/syncheck-drracket-button))

(define (File x) @tt[x])
(define (FileFirst x) @tt[x]) ;; indexing missing

(define-syntax-rule (item/cap x . ys)
  (item (indexed-scheme x) ": " . ys)) ;; indexing missing
)

@title{@bold{Plugins}: Extending DrRacket}

@author["Robert Bruce Findler"]

@defmodule*[(drracket/tool-lib drscheme/tool-lib)]

This manual describes DrRacket's tools interface. It assumes
familiarity with 
Racket, as described in 
@(other-manual '(lib "scribblings/guide/guide.scrbl")),
DrRacket, as described in
@(other-manual '(lib "scribblings/drracket/drracket.scrbl")),
and the Framework, as described in
@(other-manual '(lib "scribblings/framework/framework.scrbl")).

The @racketmodname[drscheme/tool-lib] library is for backward
compatibility; it exports all of the bindings of
@racketmodname[drracket/tool-lib].

@table-of-contents[]

@bold{Thanks}

Thanks especially to 
Eli Barzilay, 
John Clements, 
Matthias Felleisen,
Cormac Flanagan,
Matthew Flatt, 
Max Hailperin, 
Philippe Meunier, 
Christian Queinnec,
PLT at large, and many others for
their feedback and help.

@section[#:tag "implementing-tools"]{Implementing DrRacket Tools}

Tools are designed for major extensions in DrRacket's
functionality.  To extend the appearance
or the functionality the DrRacket window (say, to annotate
programs in certain ways, to add buttons to the DrRacket
frame or to add additional languages to DrRacket) use a
tool. The Macro Stepper, the Syntax Checker, the Stepper,
and the teaching languages are all implemented as tools.

When DrRacket starts up, it looks for tools by reading
fields in the @File{info.rkt} file of each collection and the
newest version of each PLaneT package installed on the
system.  (Technically, DrRacket looks in a cache of the
@filepath{info.rkt} files contents created by @tt{raco setup}. Be sure to
re-run @tt{raco setup} if you change the contents of
the @File{info.rkt} files).  DrRacket checks for these
fields:
@itemize[
@item/cap[drracket-tools]{
  @scheme[(listof (listof string[subcollection-name]))]
}
@item/cap[drracket-tool-names]{@scheme[(listof (or/c #f string))]}
@item/cap[drracket-tool-icons]{
@schemeblock[(listof (or/c #f
                           string[relative-pathname] 
                           (cons string[filename] 
                                 (listof string[collection-name]))))]
}
@item/cap[drracket-tool-urls]{
@scheme[(listof (or/c #f string[url]))]
}]

The @scheme[drracket-tools] field names a list of tools in this
collection. Each tool is specified as a collection path,
relative to the collection where the @File{info.rkt} file
resides. As an example, if there is only one tool named
@File{tool.rkt}, this suffices:
@schemeblock[
(define drracket-tools (list (list "tool.rkt")))
]
If the @scheme[drracket-tool-icons] or @scheme[drracket-tool-names] fields are
present, they must be the same length as @scheme[drracket-tools]. The
@scheme[drracket-tool-icons] field specifies the path to an icon for each
tool and the name of each tool. If it is @scheme[#f], no
tool is shown. If it is a relative pathname, it must refer
to a bitmap and if it is a list of strings, it is treated
the same as the arguments to @scheme[lib], inside
@scheme[require].

This bitmap and the name show up in the about box, the
bug report form, and the splash screen as the tool is
loaded at DrRacket's startup.

@index{phase1}
@index{phase2}
Each of the @scheme[drracket-tools] files must contain a module that
@scheme[provide]s @scheme[tool@], which must be bound to a
@scheme[unit]. The unit
must import the @scheme[drracket:tool^] signature, which is
provided by the @FileFirst{tool.rkt} library in the
@scheme[drscheme] collection. The @scheme[drracket:tool^]
signature contains all of the names listed in this manual.
The unit must export the @scheme[drracket:tool-exports^]
signature. 

The @scheme[drracket:tool-exports^] signature contains two
names: @scheme[phase1] and @scheme[phase2]. These names must
be bound to thunks. After all of the tools are loaded, all of
the @tt{phase1} functions are called and then all of the
@tt{phase2} functions are called. Certain primitives can
only be called during the dynamic extent of those calls.

This mechanism is designed to support DrRacket's
@scheme[drracket:language:language<%>] extension
capabilities. That is, this mechanism enables two tools to
cooperate via new capabilities of languages. The first phase
is used for adding functionality that each language must
support and the second is used for creating instances of
languages. As an example, a tool may require certain
specialized language-specific information. It uses phase1 to
extend the @scheme[drracket:language:language<%>] interface
and supply a default implementation of the interface
extension. Then, other languages that are aware of the
extension can supply non-default implementations of the
additional functionality.

Phase 1 functions:
@itemize[
@item{@scheme[drracket:language:extend-language-interface]}
@item{@scheme[drracket:unit:add-to-program-editor-mixin]}
]

Phase 2 functions:
@itemize[
@item{@scheme[drracket:language-configuration:add-language]}
@item{@scheme[drracket:language:get-default-mixin]}
@item{@scheme[drracket:language:get-language-extensions]}
]

If the tool raises an error as it is loaded, invoked, or as
the @scheme[phase1] or @scheme[phase2] thunks are called,
DrRacket catches the error and displays a message box. Then,
DrRacket continues to start up, without the tool.

For example, if the @File{info.rkt} file in a collection
contains:
@schememod[
setup/infotab
(define drracket-name "Tool Name")
(define drracket-tools (list (list "tool.rkt")))
]
then the same collection would be expected to contain a
@File{tool.rkt} file. It might contain something like this:
@schememod[
scheme/gui
(require drracket/tool)

(provide tool@)

(define tool@
  (unit
    (import drracket:tool^)
    (export drracket:tool-exports^)
    (define (phase1) (message-box "tool example" "phase1"))
    (define (phase2) (message-box "tool example" "phase2"))
    (message-box "tool example" "unit invoked")))
]
This tool just opens a few windows to indicate that it has
been loaded and that the @scheme[phase1] and @scheme[phase2]
functions have been called.

@section[#:tag "adding-languages"]{Adding Languages to DrRacket}
@index{adding languages to DrRacket}

@subsection{Adding Module-based Languages to DrRacket}
If a language can be implemented as a module
(see @scheme[module] for details), then the simplest and
best way to use the language is via the ``Use the language
declared the in source'' checkbox in the @onscreen{Language} dialog.

For backwards compatibility, DrRacket also supports
and 
@File{info.rkt} file-based method for specifying
such languages. Include these definitions:
@itemize[
@item/cap[drscheme-language-modules]{
  This must be bound to a
  list of collection path specifications or strings, one for
  each language in the collection. Each collection path
  specification is the quoted form of what might appear as
  an argument to @scheme[require], using the
  @tt{lib} argument (but without the @tt{lib}). The
  strings represent relative paths starting at the directory
  containing the @File{info.rkt} file.  They are interpreted
  like string arguments to @scheme[require].
}
@item/cap[drscheme-language-positions]{
This must be bound to a
  list of language positions. Each language position
  corresponds to the position of the language in language
  dialog. Each language position is a list of strings whose
  length must be at least two.
}  
@item/cap[drscheme-language-numbers]{
This is optional. If
  present, it must be a list of a list of numbers. Each list
  corresponds to a single language from this collection.
  Each number indicates a sorting order in the language
  dialog for the corresponding string in 
  @racket[drscheme-language-positions]. If absent, it defaults
  to a list of zeros that has the same length as
  @racket[drscheme-language-positions]. This will rarely be correct.
}
  
@item/cap[drscheme-language-one-line-summaries]{
This is
  optional. If present, it must be a list of strings. Each
  string is displayed at the bottom of the language dialog
  when the corresponding language is selected.
}
@item/cap[drscheme-language-urls]{
This is
  optional. If present, it must be a list whose elements are
  either strings or @scheme[#f].
  Clicking the corresponding language's name in
  the interactions window opens a web browser to the url.
}
@item/cap[drscheme-language-readers]{
This is optional. If
  present, it must be bound to a quoted list of module
  specifications (that is, a quoted version of the argument
  to @scheme[require]). Each
  specification must be a module that exports a function
  named @scheme[read-syntax].  Each of these
  @scheme[read-syntax] functions must match Racket's
  @scheme[read-syntax] primitive's contract, but may
  read different concrete syntax.

  If the module specification is a plain string, it 
  represents a relative path starting at the directory
  containing the @File{info.rkt} file.  It is interpreted
  like the string arguments to @scheme[require].
}]
The lists must have the same length.

As an example, the @italic{Essentials of Programming Languages}
language specification's @File{info.rkt} used to look like this:
@schememod[
setup/infotab
(require string-constants)
(define name "EoPL Support")
(define drscheme-language-modules
  (list "eopl-lang.rkt"))
(define drscheme-language-positions
  (list (list (string-constant teaching-languages)
              "Essentials of Programming Languages")))
]
This @File{info.rkt} file indicates that there is a single
language in this collection. The module that implements the
language is the @File{eopl-lang.rkt} file in the same directory as
the @File{info.rkt} file. Additionally, the language dialog will contain
@tt{Essentials of Programming Languages} as a potential
language. The use of the string constant
@scheme[teaching-languages] ensures that EoPL's language is
placed properly in foreign language versions of DrRacket.

For collections that define multiple (related) languages, if
the language-positions contain multiple strings, the
languages whose leading strings match are grouped together.
That is, if two languages have strings:
@schemeblock[
  '("My Text" "First Language")
]
and
@schemeblock[
  '("My Text" "Second Language")
]
the two languages will be grouped together in the language
dialog.

@subsection{Adding Arbitrary Languages to DrRacket}
With some additional work, any language that can be compiled
to Racket is supported by the tools interface,
not just those that use standard configurations and
@scheme[module].

Each language is a class that implement the
@scheme[drracket:language:language<%>] interface.  DrRacket also
  provides two simpler interfaces:
  @scheme[drracket:language:module-based-language<%>] and
  @scheme[drracket:language:simple-module-based-language<%>],
  and 
  @scheme[mixin]s
  @scheme[drracket:language:simple-module-based-language->module-based-language-mixin]
  and
  @scheme[drracket:language:module-based-language->language-mixin]
  that build implementations of @scheme[drracket:language:language<%>]s from these simpler interfaces.

Once you have an implementation of the
@scheme[drracket:language:language<%>] interface, call
@scheme[drracket:language-configuration:add-language] to add the language
to DrRacket.

Each language comes with its own type, called
@tt{settings}. This can be any type the language
designer chooses, but to aid documentation, we call it
@tt{settings} here. The settings type is expected to
contain parameters of the language, such as case
sensitivity, etc. The implementor of the language provides a
GUI so the user can configure the settings and all of the
language's operations accept a setting. DrRacket maintains
the current settings for each language.

@subsection{Language Extensions}

Some tools may require additional functionality from the
@scheme[drracket:language:language<%>] interface. The
@scheme[drracket:language:extend-language-interface]
function and the
@scheme[drracket:language:get-default-mixin]
mixin make this possible.

For example, the MrFlow tool expands a program, analyzes it
and then displays sets of values for each program point.
These sets of values should be rendered in the syntax of the
language that MrFlow analyzes. Since MrFlow doesn't 
know which languages are available, it can call
@scheme[drracket:language:extend-language-interface]
to extend the @scheme[drracket:language:language<%>]
interface with a method for rendering sets of values and
provide a default implementation of that method. Tools that
know about MrFlow can then override the value rendering
method to provide a language-specific implementation of
value rendering.  Additionally, since the
@scheme[drracket:language:get-default-mixin]
adds the default implementation for the value-set rendering
method, all languages at least have some form of value-set
rendering.

In some cases, it is important for one tool to avoid
depending on another in the manner above. For example, if a
tool that provides a new language provides an implementation
for the MrFlow-specific method, that tool may fail to load
if MrFlow is not present (Indeed, with the tool manager,
this can happen to any tool that depends on another in this
manner.)

To avoid this problem, consider writing your tool to first
check to see if the base method is available before
extending it. For example, if the MrFlow tool provides the
@tt{render-value<%>} interface, then a tool that overrides
that method can first test to see if the superclass
implements that method before overriding it:
@schemeblock[
(define (my-language-mixin %)
  (if (implementation? % mrflow:render-value<%>)
      (class % 
        (define/override ...)
        (super-new))
      %))
]

To help test your tool, use the 
@seclink["environment-variables" #:doc '(lib "scribblings/drracket/drracket.scrbl")]{@tt{PLTONLYTOOL}}
environment variable to load it in isolation.

@section{Creating New Kinds of DrRacket Frames}

Each frame in DrRacket has certain menus and functionality,
most of which is achieved by using the framework.
Additionally, there is one mixin that DrRacket provides to
augment that. It is @scheme[drracket:frame:basics-mixin].
Be sure to mix it into any new frame class that you add to
DrRacket.

@section{Extending the Existing DrRacket Classes}

Each of the names:
@itemize[
@item{@scheme[drracket:get/extend:extend-interactions-text]}
@item{@scheme[drracket:get/extend:extend-definitions-text]}
@item{@scheme[drracket:get/extend:extend-interactions-canvas]}
@item{@scheme[drracket:get/extend:extend-definitions-canvas]}
@item{@scheme[drracket:get/extend:extend-unit-frame]}
@item{@scheme[drracket:get/extend:extend-tab]}]
is bound to an extender function. In order to change the
behavior of DrRacket, you can derive new classes from the
standard classes for the frame, texts, canvases. Each
extender accepts a function as input. The function it
accepts must take a class as it's argument and return a
classes derived from that class as its result. For example:

@schemeblock[
(drracket:get/extend:extend-interactions-text
  (lambda (super%)
    (class super%
      (define/public (method1 x) ...)
      (super-new))))
]
extends the interactions text class with a method named @tt{method1}.

@section[#:tag "Expanding and Breaking"]{Expanding the User's Program Text and Breaking}
@index{expanding user programs}
@index{breaking}
@index{break button}

Macro-expanding a program may involve arbitrary computation
and requires the setup of the correct language. To aid this,
DrRacket's tool interface provides
@scheme[drracket:eval:expand-program] to help. Use
this method to extract the fully expanded program text in a
particular language.

Because expanding the user's program may require DrRacket to
evaluate arbitrary code that the user wrote, tools that
expand the user's program should also allow the user to break
the expansion. To help with this, the tools interfaces
provides these methods:
@method[drracket:rep:context<%> enable-evaluation]
and
@method[drracket:rep:context<%> disable-evaluation].
Since your tool will be expanding the program text, you
should be both overriding 
@method[drracket:rep:context<%> enable-evaluation]
and
@method[drracket:rep:context<%> disable-evaluation]
to disable your tool and calling them
to ensure that only one expansion is happening
at a time.

Finally, DrRacket provides the
@method[drracket:rep:context<%> set-breakables]
method. This method controls what behavior the Break button
has.

@section{Editor Modes}
@index{modes}
@index{scheme mode}

DrRacket provides support for multiple editor modes. Tools
register modes via
@scheme[drracket:modes:add-mode]. Each mode is
visible in the @onscreen{Modes} submenu of the @onscreen{Edit}
menu. Initially, DrRacket only supports two modes: Racket
mode and text mode.

DrRacket automatically selects a mode for each open
file based on the file's extension. If the file ends with
@File{.txt}, DrRacket uses text mode. Otherwise, DrRacket
uses Racket mode.

@section{Language-specific capabilities}

@subsection{Customizing DrRacket's behavior}

When using the language declared in the source, DrRacket queries  that
language via @racket[module-compiled-language-info] to determine
if an expression in the interactions window is ready to be submitted
to the evaluator (when the user types return).
The info procedure is passed @racket['drracket:submit-predicate] 
and should return a function with this contract:
@racketblock[(-> (is-a?/c text%)
                 number?
                 boolean?)]
This function is called with the interactions window's editor object
the first position in the editor after the prompt and should return
a boolean indicating if the expression should be evaluated.
This function is called in sandbox, but with no filesystem or networking 
limits.

@subsection{Customizing DrRacket's GUI}

DrRacket's capability interface provides a mechanism for
tools to allow languages to hide their GUI interface, if the
tool does not apply to the language. Tools register
capabilities keyed with symbols via.
@scheme[drracket:language:register-capability]. Once
registered, a tool can query a language, via the 
@method[drracket:language:language<%> capability-value]
method. The result from this method controls whether or not
the tool shows this part of the GUI for DrRacket.

See @scheme[drracket:language:register-capability]
for a list of the capabilities registered by default.

@section{Check Syntax}

Check Syntax is a part of the DrRacket collection, but is implemented via the tools API.

@defmodule[drracket/syncheck-drracket-button]

@defthing[syncheck-drracket-button
          (list/c 
           string?
           (is-a?/c bitmap%)
           (-> (is-a?/c
                top-level-window<%>)
               any))]{
   This is meant to be used with the @scheme['drscheme:toolbar-buttons] 
   argument to the info proc returned
   from @scheme[read-language].
}

@defidform[syncheck:button-callback]{
  This is defined with @scheme[define-local-member-name] and
  is bound to a method of no arguments of the DrRacket frame that runs Check
  Syntax.
}
          
@defthing[syncheck-bitmap (is-a?/c bitmap%)]{
  The bitmap in the Check Syntax button on the DrRacket frame.
}

@include-section["get-slash-extend.scrbl"]
@include-section["unit.scrbl"]
@include-section["language.scrbl"]
@include-section["language-configuration.scrbl"]
@include-section["debug.scrbl"]
@include-section["rep.scrbl"]
@include-section["frame.scrbl"]
@include-section["help-desk.scrbl"]
@include-section["eval.scrbl"]
@include-section["modes.scrbl"]
@include-section["module-language-tools.scrbl"]
@include-section["module-language.scrbl"]

@section{Backwards compatibility}

This section lists the bindings that begin with @tt{drscheme:} provided by the tools
library; they are here for backwards compatibility and to provide links to the
@tt{drracket:} versions of the names.

@(require drracket/private/drsig
          (for-syntax racket/base
                      racket/unit-exptime))
@(define-syntax (drs-compat stx)
   (let-values ([(drs-parent drs-vars drs-var-defs-in-sig drs-stx-defs-in-sig) (signature-members #'drscheme:tool-cm^ #'here)]
                [(drr-parent drr-vars drr-var-defs-in-sig drr-stx-defs-in-sig) (signature-members #'drracket:tool-cm^ #'here)])
     (with-syntax ([(drs-id ...) drs-vars]
                   [(drr-id ...) drr-vars])
       #'(begin 
           (defthing drs-id any/c
             "This is provided for backwards compatibility; new code should use " (scheme drr-id) " instead.") 
           ...))))
@drs-compat[]

@(tools-include/drs "debug")
@(tools-include/drs "eval")
@(tools-include/drs "frame")
@(tools-include/drs "get/extend")
@(tools-include/drs "help-desk")
@(tools-include/drs "language-configuration")
@(tools-include/drs "language")
@(tools-include/drs "modes")
@(tools-include/drs "module-language-tools")
@(tools-include/drs "module-language")
@(tools-include/drs "rep")
@(tools-include/drs "unit")

@index-section[]
