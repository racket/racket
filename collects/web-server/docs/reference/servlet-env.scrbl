#reader(lib "docreader.ss" "scribble")
@require["../web-server.ss"]

@title[#:tag "servlet-env.ss"
       #:style 'toc]{Environment}

The @web-server provides a means of running Scheme servlets
from within DrScheme, or any other REPL.

@file{servlet-env.ss} provides the servlet API from @file{servlet.ss}
as well as the following:

@defthing[send-url (parameter/c ([url string?] [separate-window? boolean?] . -> . void))]{
 Should open @scheme[url]. In another window if @scheme[separate-window?] is true.
 By default this is from @scheme[(lib "sendurl.ss" "net")].
}

@defform[(on-web servlet-expr)]{This expands to @scheme[(on-web 8000 servlet-expr)].}

@defform[(on-web port servlet-expr)]{
 This constructs a small servlet, where the body of the @scheme[start] procedure is
 @scheme[servlet-expr], runs the @web-server on port @scheme[port], and calls
 @scheme[send-url] with a URL for the constructed servlet. The call blocks until the
 servlet finishes its computation, i.e. @scheme[servlet-expr] is evaluated, and
 returns its result. @scheme[servlet-expr] may use the entire Scheme servlet API.
}
