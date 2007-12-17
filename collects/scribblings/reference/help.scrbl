#lang scribble/doc
@(require "mz.ss"
          (for-label scheme/help
                     net/url
                     scheme/gui))

@title{Interactive Help}

@declare-exporting[scheme/help scheme/init]
                   
@defmodule*/no-declare[(scheme/help)]{The @scheme[help] form documented
in this section is provided by the @schememodname[scheme/help] and
@schememodname[scheme/init] libraries, which means that it is
available when @exec{mzscheme} is started with no command-line
arguments. It is not provided by @scheme[scheme] or
@scheme[scheme/base].}

@deftogether[(
@defidform[help]
@defform/none[#:literals (help) (help id)]
@defform/none[#:literals (help) (help id #:from module-path)]
@defform/none[#:literals (help) (help #:search datum ...)]
)]{

Searches the documentation, and opens a web browser (using the user's
selected browser) to display the results. See
@schememodname[net/sendurl] for information on how the user's browser
is launched.

A simple @scheme[help] or @scheme[(help)] form opens this page.

A @scheme[(help id)] form looks for documentation specific to the
current binding of @scheme[id]. For example, 

@schemeblock[
(require net/url)
(help url->string)
]

opens a web browser to show the documentation for @scheme[url->string]
from the @schememodname[net/url] library.

For the purposes of @scheme[help], a @scheme[for-label] require
introduces a binding without actually executing the
@schememodname[net/url] library---for cases when you want to check
documentation, but cannot or do not want to run the providing module.

@schemeblock[
(require scheme/gui) (code:comment #, @t{does not work in @exec{mzscheme}})
(require (for-label scheme/gui)) (code:comment #, @t{ok in @exec{mzscheme}})
(help frame%)
]

If @scheme[id] has no for-label and normal binding, then @scheme[help]
lists all libraries that are known to export a binding for
@scheme[id].

The @scheme[(help id #:from module-path)] variant is similar to
@scheme[(help id)], but using only the exports of
@scheme[module-path]. (The @scheme[module-path] module is required
@scheme[for-label] in a temporary namespace.)

@schemeblock[
(help frame% #:from scheme/gui) (code:comment #, @t{equivalent to the above})
]

The @scheme[(help #:search datum ...)] form performs a general
search. Searching uses strings; each string @scheme[datum] is used
as-is, and any other form of @scheme[datum] is converted to a string
using @scheme[display]. No @scheme[datum] is evaluated as an
expression.

For example,

@schemeblock[
(help #:search "web browser" firefox)
]

searches the documentation index for references that include the
phrase ``web browser'' or ``firefox.''

}
