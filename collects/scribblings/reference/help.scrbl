#lang scribble/doc
@(require "mz.rkt" scribble/core scribble/html-properties
          (for-label racket/help net/url racket/gui/base))

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

@defform*[#:id help
          [help
           (help string ...)
           (help id)
           (help id #:from module-path)
           (help #:search datum ...)]]{

@emph{For general help, see the @|main-doc-page|.}

The @racket[help] form searches the documentation and opens a web
browser (using the user's selected browser) to display the results.

@margin-note{See @racketmodname[net/sendurl] for information on how
the user's browser is launched to display help information.}

A simple @racket[help] or @racket[(help)] form opens the main
documentation page.

The @racket[(help string ...)] form---using literal strings, as
opposed to expressions that produce strings---performs a
string-matching search. For example,

@racketblock[
(help "web browser" "firefox")
]

searches the documentation index for references that include the
phrase ``web browser'' or ``firefox.''

A @racket[(help id)] form looks for documentation specific to the
current binding of @racket[id]. For example, 

@racketblock[
(require net/url)
(help url->string)
]

opens a web browser to show the documentation for @racket[url->string]
from the @racketmodname[net/url] library.

For the purposes of @racket[help], a @racket[for-label] require
introduces a binding without actually executing the
@racketmodname[net/url] library---for cases when you want to check
documentation, but cannot or do not want to run the providing module.

@racketblock[
(require racket/gui) (code:comment @#,t{does not work in @exec{racket}})
(require (for-label racket/gui)) (code:comment @#,t{ok in @exec{racket}})
(help frame%)
]

If @racket[id] has no for-label and normal binding, then @racket[help]
lists all libraries that are known to export a binding for
@racket[id].

The @racket[(help id #:from module-path)] variant is similar to
@racket[(help id)], but using only the exports of
@racket[module-path]. (The @racket[module-path] module is required
@racket[for-label] in a temporary namespace.)

@racketblock[
(help frame% #:from racket/gui) (code:comment @#,t{equivalent to the above})
]

The @racket[(help #:search datum ...)] form is similar to
@racket[(help string ...)], where any non-string form of
@racket[datum] is converted to a string using @racket[display]. No
@racket[datum] is evaluated as an expression.

For example,

@racketblock[
(help #:search "web browser" firefox)
]

also searches the documentation index for references that include the
phrase ``web browser'' or ``firefox.''

}
