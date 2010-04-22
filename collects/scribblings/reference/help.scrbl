#lang scribble/doc
@(require "mz.ss"
          scribble/core
          scribble/html-properties
          (for-label racket/help
                     net/url
                     racket/gui/base))

@; Beware of this hard-wired link to the main doc page:
@(define main-doc-page
   (hyperlink "../index.html"
              #:style (make-style
                       "plainlink" 
                       (list
                        (make-attributes 
                         `((onclick . ,(format "return GotoPLTRoot(\"~a\");" (version)))))))
              "main documentation page"))

@title{Interactive Help}

@note-init-lib[racket/help]

@deftogether[(
@defidform[help]
@defform/none[#:literals (help) (help string ...)]
@defform/none[#:literals (help) (help id)]
@defform/none[#:literals (help) (help id #:from module-path)]
@defform/none[#:literals (help) (help #:search datum ...)]
)]{

@emph{For general help, see the @|main-doc-page|.}

The @scheme[help] form searches the documentation and opens a web
browser (using the user's selected browser) to display the results.

@margin-note{See @schememodname[net/sendurl] for information on how
the user's browser is launched to display help information.}

A simple @scheme[help] or @scheme[(help)] form opens the main
documentation page.

The @scheme[(help string ...)] form---using literal strings, as
opposed to expressions that produce strings---performs a
string-matching search. For example,

@schemeblock[
(help "web browser" "firefox")
]

searches the documentation index for references that include the
phrase ``web browser'' or ``firefox.''

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
(require racket/gui) (code:comment @#,t{does not work in @exec{mzscheme}})
(require (for-label racket/gui)) (code:comment @#,t{ok in @exec{mzscheme}})
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
(help frame% #:from racket/gui) (code:comment @#,t{equivalent to the above})
]

The @scheme[(help #:search datum ...)] form is similar to
@scheme[(help string ...)], where any non-string form of
@scheme[datum] is converted to a string using @scheme[display]. No
@scheme[datum] is evaluated as an expression.

For example,

@schemeblock[
(help #:search "web browser" firefox)
]

also searches the documentation index for references that include the
phrase ``web browser'' or ``firefox.''

}
