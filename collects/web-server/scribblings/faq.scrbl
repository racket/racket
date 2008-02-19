#lang scribble/doc
@(require "web-server.ss")

@title{Troubleshooting}

@section{General}

@subsection{IE ignores my CSS or behaves strange in other ways}

In quirks mode, IE does not parse your page as XML, in particular it will not recognize many instances of
"empty tag shorthand", e.g. "<img src='...' />", whereas the @web-server uses @scheme[(lib "xml.ss" "xml")]
to format XML, which uses empty tag shorthand by default. You can change the default with the @scheme[empty-tag-shorthand]
parameter: @scheme[(empty-tag-shorthand 'never)].
