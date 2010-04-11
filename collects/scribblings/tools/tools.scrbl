#lang scribble/doc
@(begin
(require scribble/manual
         (for-label scheme/gui/base)
         (for-label drscheme/tool-lib)
         (for-label scheme/unit scheme/contract scheme/class)
         (for-label scheme/base)
         (for-label framework/framework))

(define (File x) @tt[x])
(define (FileFirst x) @tt[x]) ;; indexing missing

(define (item/cap x . ys)
  (apply item (bold (format "~a" x)) ": " ys)) ;; indexing missing
)

@title{@bold{Plugins}: Extending DrScheme}

@author["Robert Bruce Findler"]

@(defmodule drscheme/tool-lib)

This manual describes DrScheme's tools interface. It assumes
familiarity with 
PLT Scheme, as described in 
@(other-manual '(lib "scribblings/guide/guide.scrbl")),
DrScheme, as described in
@(other-manual '(lib "scribblings/drscheme/drscheme.scrbl")),
and the Framework, as described in
@(other-manual '(lib "scribblings/framework/framework.scrbl")).

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

@section[#:tag "implementing-tools"]{Implementing DrScheme Tools}

Tools are designed for major extensions in DrScheme's
functionality.  To extend the appearance
or the functionality the DrScheme window (say, to annotate
programs in certain ways, to add buttons to the DrScheme
frame or to add additional languages to DrScheme) use a
tool. The Macro Stepper, the Syntax Checker, the Stepper,
and the teaching languages are all implemented as tools.

When DrScheme starts up, it looks for tools by reading
fields in the @File{info.ss} file of each collection and the
newest version of each PLaneT package installed on the
system.  (Technically, DrScheme looks in a cache of the
info.ss files contents created by setup-plt. Be sure to
re-run setup-plt if you change the contents of
the @File{info.ss} files).  DrScheme checks for these
fields:
@itemize[
@item/cap['tools]{
  @scheme[(listof (listof string[subcollection-name]))]
}
@item/cap['tool-names]{@scheme[(listof (union #f string))]}
@item/cap['tool-icons]{
@schemeblock[
(listof (union #f
               string[relative-pathname] 
               (cons string[filename] 
                     (listof string[collection-name]))))]
}
@item/cap['tool-urls]{
@scheme[(listof (union #f string[url]))]
}]

The @scheme[tools] field names a list of tools in this
collection. Each tool is specified as a collection path,
relative to the collection where the @File{info.ss} file
resides. As an example, if there is only one tool named
@File{tool.ss}, this suffices:
@schemeblock[
(define tools (list (list "tool.ss")))
]
If the @scheme[tool-icons] or @scheme[tool-names] fields are
present, they must be the same length as @scheme[tools]. The
@scheme[tool-icons] specifies the path to an icon for each
tool and the name of each tool. If it is @scheme[#f], no
tool is shown. If it is a relative pathname, it must refer
to a bitmap and if it is a list of strings, it is treated
the same as the arguments to @scheme[lib], inside
@scheme[require].

This bitmap and the name show up in the about box, Help
Desk's bug report form, and the splash screen as the tool is
loaded at DrScheme's startup.

@index{phase1}
@index{phase2}
Each of @scheme[tools] files must contain a module that
@scheme[provide]s @scheme[tool@], which must be bound to a
@scheme[unit]. The unit
must import the @scheme[drscheme:tool^] signature, which is
provided by the @FileFirst{tool.ss} library in the
@scheme[drscheme] collection. The @scheme[drscheme:tool^]
signature contains all of the names listed in this manual.
The unit must export the @scheme[drscheme:tool-exports^]
signature. 

The @scheme[drscheme:tool-exports^] signature contains two
names: @scheme[phase1] and @scheme[phase2]. These names must
be bound to thunks. After all of the tools are loaded, all of
the @tt{phase1} functions are called and then all of the
@tt{phase2} functions are called. Certain primitives can
only be called during the dynamic extent of those calls.

This mechanism is designed to support DrScheme's
@scheme[drscheme:language:language<%>] extension
capabilities. That is, this mechanism enables two tools to
cooperate via new capabilities of languages. The first phase
is used for adding functionality that each language must
support and the second is used for creating instances of
languages. As an example, a tool may require certain
specialized language-specific information. It uses phase1 to
extend the @scheme[drscheme:language:language<%>] interface
and supply a default implementation of the interface
extension. Then, other languages that are aware of the
extension can supply non-default implementations of the
additional functionality.

Phase 1 functions:
@itemize[
@item{@scheme[drscheme:language:extend-language-interface]}
@item{@scheme[drscheme:unit:add-to-program-editor-mixin]}
]

Phase 2 functions:
@itemize[
@item{@scheme[drscheme:language-configuration:add-language]}
@item{@scheme[drscheme:language:get-default-mixin]}
@item{@scheme[drscheme:language:get-language-extensions]}
]

If the tool raises an error as it is loaded, invoked, or as
the @scheme[phase1] or @scheme[phase2] thunks are called,
DrScheme catches the error and displays a message box. Then,
DrScheme continues to start up, without the tool.

For example, if the @File{info.ss} file in a collection
contains:
@schememod[
setup/infotab
(define name "Tool Name")
(define tools (list (list "tool.ss")))
]
then the same collection would be expected to contain a
@File{tool.ss} file. It might contain something like this:
@schememod[
scheme/gui
(require drscheme/tool)

(provide tool@)

(define tool@
  (unit
    (import drscheme:tool^)
    (export drscheme:tool-exports^)
    (define (phase1) (message-box "tool example" "phase1"))
    (define (phase2) (message-box "tool example" "phase2"))
    (message-box "tool example" "unit invoked")))
]
This tool just opens a few windows to indicate that it has
been loaded and that the @scheme[phase1] and @scheme[phase2]
functions have been called.

@section[#:tag "adding-languages"]{Adding Languages to DrScheme}
@index{adding languages to DrScheme}

@subsection{Adding Module-based Languages to DrScheme}
If a language can be implemented as a module
(see @scheme[module] for details)
and the standard language settings are
sufficient, simply create an
@File{info.ss} file in the collection
where the module is saved. Include these
definitions:
@itemize[
@item/cap['drscheme-language-modules]{
  @index{drscheme-language-modules} This must be bound to a
  list of collection path specifications or strings, one for
  each language in the collection. Each collection path
  specification is the quoted form of what might appear as
  an argument to @scheme[require], using the
  @tt{lib} argument (but without the @tt{lib}). The
  strings represent relative paths starting at the directory
  containing the @File{info.ss} file.  They are interpreted
  like string arguments to @scheme[require].
}
@item/cap['drscheme-language-positions]{
@index{drscheme-language-positions}
This must be bound to a
  list of language positions. Each language position
  corresponds to the position of the language in language
  dialog. Each language position is a list of strings whose
  length must be at least two.
}  
@item/cap['drscheme-language-numbers]{
@index{drscheme-language-numbers}
This is optional. If
  present, it must be a list of a list of numbers. Each list
  corresponds to a single language from this collection.
  Each number indicates a sorting order in the language
  dialog for the corresponding string in 
  @bold{drscheme-language-positions}. If absent, it defaults
  to a list of zeros that has the same length as
  @bold{drscheme-language-positions}. This will rarely be correct.
}
  
@item/cap['drscheme-language-one-line-summaries]{
@index{drscheme-language-one-line-summaries}
This is
  optional. If present, it must be a list of strings. Each
  string is displayed at the bottom of the language dialog
  when the corresponding language is selected.
}
@item/cap['drscheme-language-urls]{
@index{drscheme-language-urls}
This is
  optional. If present, it must be a list whose elements are
  either strings or @scheme[#f].
  Clicking the corresponding language's name in
  the interactions window opens a web browser to the url.
}
@item/cap['drscheme-language-readers]{
@index{drscheme-language-readers}
This is optional. If
  present, it must be bound to a quoted list of module
  specifications (that is, a quoted version of the argument
  to @scheme[require]). Each
  specification must be a module that exports a function
  named @scheme[read-syntax].  Each of these
  @scheme[read-syntax] functions must match MzScheme's
  @scheme[read-syntax] primitive's contract, but may
  read different concrete syntax.

  If the module specification is a plain string, it 
  represents a relative path starting at the directory
  containing the @File{info.ss} file.  It is interpreted
  like the string arguments to @scheme[require].
}]
The lists must have the same length.

As an example, the @italic{Essentials of Programming Languages}
language specification's @File{info.ss} looks like this:
@schememod[
setup/infotab
(require string-constants)
(define name "EoPL Support")
(define drscheme-language-modules
  (list "eopl-lang.ss"))
(define drscheme-language-positions
  (list (list (string-constant teaching-languages)
              "Essentials of Programming Languages")))
]
This @File{info.ss} file indicates that there is a single
language in this collection. The module that implements the
language is the @File{eopl-lang.ss} file in the same directory as
the @File{info.ss} file. Additionally, the language dialog will contain
@tt{Essentials of Programming Languages} as a potential
language. The use of the string constant
@scheme[teaching-languages] ensures that EoPL's language is
placed properly in foreign language versions of DrScheme.

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

@subsection{Adding Arbitrary Languages to DrScheme}
With some additional work, any language that can be compiled
to PLT Scheme is supported by the tools interface,
not just those that use standard configurations and
@scheme[module].

Each language is a class that implement the
@scheme[drscheme:language:language<%>] interface.  DrScheme also
  provides two simpler interfaces:
  @scheme[drscheme:language:module-based-language<%>] and
  @scheme[drscheme:language:simple-module-based-language<%>],
  and 
  @scheme[mixins]
  @scheme[drscheme:language:simple-module-based-language->module-based-language-mixin]
  and
  @scheme[drscheme:language:module-based-language->language-mixin]
  that build implementations of @scheme[language^]s from these simpler interfaces.

Once you have an implementation of the
@scheme[drscheme:language:language^] interface, call
@scheme[drscheme:language-configuration:add-language] to add the language
to DrScheme.

Each language comes with its own type, called
@tt{settings}. This can be any type the language
designer chooses, but to aid documentation, we call it
@tt{settings} here. The settings type is expected to
contain parameters of the language, such as case
sensitivity, etc. The implementor of the language provides a
GUI so the user can configure the settings and all of the
language's operations accept a setting. DrScheme maintains
the current settings for each language.

@subsection{Language Extensions}

Some tools may require additional functionality from the
@scheme[drscheme:language:language] interface. The
@scheme[drscheme:language:extend-language-interface]
function and the
@scheme[drscheme:language:get-default-mixin]
mixin make this possible.

For example, the MrFlow tool expands a program, analyzes it
and then displays sets of values for each program point.
These sets of values should be rendered in the syntax of the
language that MrFlow analyzes. Since MrFlow doesn't 
know which languages are available, it can call
@scheme[drscheme:language:extend-language-interface]
to extend the @scheme[drscheme:language:language<%>]
interface with a method for rendering sets of values and
provide a default implementation of that method. Tools that
know about MrFlow can then override the value rendering
method to provide a language-specific implementation of
value rendering.  Additionally, since the
@scheme[drscheme:language:get-default-mixin]
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
@seclink["environment-variables" #:doc '(lib "scribblings/drscheme/drscheme.scrbl")]{@tt{PLTONLYTOOL}}
environment variable to load it in isolation.

@section{Creating New Kinds of DrScheme Frames}

Each frame in DrScheme has certain menus and functionality,
most of which is achieved by using the framework.
Additionally, there is one mixin that DrScheme provides to
augment that. It is @scheme[drscheme:frame:basics-mixin].
Be sure to mix it into any new frame class that you add to
DrScheme.

@section{Extending the Existing DrScheme Classes}

Each of the names:
@itemize[
@item{@scheme[drscheme:get/extend:extend-interactions-text]}
@item{@scheme[drscheme:get/extend:extend-definitions-text]}
@item{@scheme[drscheme:get/extend:extend-interactions-canvas]}
@item{@scheme[drscheme:get/extend:extend-definitions-canvas]}
@item{@scheme[drscheme:get/extend:extend-unit-frame]}
@item{@scheme[drscheme:get/extend:extend-tab]}]
is bound to an extender function. In order to change the
behavior of drscheme, you can derive new classes from the
standard classes for the frame, texts, canvases. Each
extender accepts a function as input. The function it
accepts must take a class as it's argument and return a
classes derived from that class as its result. For example:

@schemeblock[
(drscheme:get/extend:extend-interactions-text
  (lambda (super%)
    (class super%
      (public method1)
      (define (method1 x) ...)
      ...)))
]
extends the interactions text class with a method named @tt{method1}.

@section[#:tag "Expanding and Breaking"]{Expanding the User's Program Text and Breaking}
@index{expanding user programs}
@index{breaking}
@index{break button}

Macro-expanding a program may involve arbitrary computation
and requires the setup of the correct language. To aid this,
DrScheme's tool interface provides
@scheme[drscheme:eval:expand-program] to help. Use
this method to extract the fully expanded program text in a
particular language.

Because expanding the user's program may require DrScheme to
evaluate arbitrary code that the user wrote, tools that
expand the user's program should also allow the user to break
the expansion. To help with this, the tools interfaces
provides these methods:
@method[drscheme:rep:context<%> enable-evaluation]
and
@method[drscheme:rep:context<%> disable-evaluation].
Since your tool will be expanding the program text, you
should be both overriding 
@method[drscheme:rep:context<%> enable-evaluation]
and
@method[drscheme:rep:context<%> disable-evaluation]
to disable your tool and calling them
to ensure that only one expansion is happening
at a time.

Finally, DrScheme provides the
@method[drscheme:rep:context<%> set-breakables]
method. This method controls what behavior the Break button
has.

@section{Editor Modes}
@index{modes}
@index{scheme mode}

DrScheme provides support for multiple editor modes. Tools
register modes via
@scheme[drscheme:modes:add-mode]. Each mode is
visible in the @onscreen{Modes} submenu of the @onscreen{Edit}
menu. Initially, DrScheme only supports two modes: scheme
mode and text mode.

DrScheme automatically selects a mode for each open
file based on the file's extension. If the file ends with
@File{.txt}, DrScheme uses text mode. Otherwise, DrScheme
uses Scheme mode.

@section{Language-specific capabilities}

Drscheme's capability interface provides a mechanism for
tools to allow languages to hide their GUI interface, if the
tool does not apply to the language. Tools register
capabilities keyed with symbols via.
@scheme[drscheme:language:register-capability]. Once
registered, a tool can query a language, via the 
@method[drscheme:language:language<%> capability-value]
method. The result from this method controls whether or not
the tool shows this part of the GUI for DrScheme.

See @scheme[drscheme:language:register-capability]
for a list of the capabilities registered by default.

@section{Check Syntax}

Check Syntax is a part of the DrScheme collection, but is implemented via the tools api, i.e.,
not taking any advantage of 

@defmodule[drscheme/syncheck-drscheme-button]

@defthing[syncheck-drscheme-button
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
  is bound to a method of no arguments of the DrScheme frame that runs Check
  Syntax.
}
          
@defthing[syncheck-bitmap (is-a?/c bitmap%)]{
  The bitmap in the Check Syntax button on the DrScheme frame.
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

@index-section[]
