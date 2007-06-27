#reader(lib "docreader.ss" "scribble")
@require["../web-server.ss"]

@title{Web Server User Guide}

@table-of-contents[]

@section{General}

@subsection{IE ignores my CSS or behaves strange in other ways}

In quirks mode, IE does not parse your page as XML, in particular it will not recognize many instances of
"empty tag shorthand", e.g. "<img src='...' />", whereas the @web-server uses @scheme[(lib "xml.ss" "xml")]
to format XML, which uses empty tag shorthand by default. You can change the default with the @scheme[empty-tag-shorthand]
parameter: @scheme[(empty-tag-shorthand 'never)].

@index-section["web-server-guide-index"]