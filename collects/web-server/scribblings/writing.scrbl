#lang scribble/doc
@(require "web-server.ss")

@title[#:style 'toc]{Writing Servlets}

@local-table-of-contents[]

@include-section["servlet-definition.scrbl"]

@; ------------------------------------------------------------
@section{APIs}

There are two API sets provided by the Web Server. One is for standard servlets, the other is for stateless servlets.

@subsection{Standard API}

@defmodule[web-server/servlet]{

This API provides: 
@itemize{
         @item{@schememodname[web-server/servlet/web-cells],}
         @item{@schememodname[web-server/http/bindings],}
         @item{@schememodname[web-server/http],}
         @item{@schememodname[web-server/servlet/servlet-structs], and}
         @item{@schememodname[web-server/servlet/web].}}
}

@subsection{Stateless API}

@defmodule[web-server/lang/lang-api]{

This API provides:
@itemize{
         @item{@schememodname[net/url],}
         @item{@schememodname[web-server/http],}
         @item{@schememodname[web-server/lang/abort-resume],}
         @item{@schememodname[web-server/lang/web],}
         @item{@schememodname[web-server/lang/web-cells],}
         @item{@schememodname[web-server/lang/web-param], and}
         @item{@schememodname[web-server/lang/file-box].}}
}

@; ------------------------------------------------------------
@section[#:tag "servlet-structs.ss"]{Common Contracts}
@(require (for-label web-server/servlet/servlet-structs
                     web-server/servlet))

@defmodule[web-server/servlet/servlet-structs]{

@filepath{servlet/servlet-structs.ss} provides a number of contracts
for use in servlets.

@defthing[k-url? contract?]{
Equivalent to @scheme[string?]. 
                                       
Example: @scheme["http://localhost:8080/servlets;1*1*20131636/examples/add.ss"]}

@defthing[response-generator/c contract?]{
Equivalent to @scheme[(k-url? . -> . response?)].
           
Example: @schemeblock[(lambda (k-url)
                        `(html 
                          (body 
                           (a ([href ,k-url])
                              "Click Me to Invoke the Continuation!"))))]
}

@defthing[expiration-handler/c contract?]{
Equivalent to @scheme[(or/c false/c (request? . -> . response?))].
           
Example: @schemeblock[(lambda (req)
                        `(html (head (title "Expired"))
                               (body (h1 "Expired")
                                     (p "This URL has expired. "
                                        "Please return to the home page."))))]
}

@defthing[embed/url/c contract?]{
Equivalent to @scheme[(((request? . -> . any/c)) (expiration-handler/c) . opt-> . string?)].

This is what @scheme[send/suspend/dispatch] gives to its function argument.
}

}

@; ------------------------------------------------------------
@include-section["http.scrbl"]
@include-section["web.scrbl"]
@include-section["lang.scrbl"]
@include-section["web-cells.scrbl"]

@; ------------------------------------------------------------
@section[#:tag "lang/file-box.ss"]{File Boxes}
@(require (for-label web-server/lang/file-box
                     scheme/serialize))

@defmodule[web-server/lang/file-box]{

As mentioned earlier, it is dangerous to rely on the store in
Web Language servlets, due to the deployment scenarios available
to them. @filepath{lang/file-box.ss} provides a simple API to replace
boxes in a safe way.

@defproc[(file-box? [v any/c])
         boolean?]{Checks if @scheme[v] is a file-box.}

@defproc[(file-box [p path?]
                   [v serializable?])
         file-box?]{
 Creates a file-box that is stored at @scheme[p], with the default
 contents of @scheme[v].
}

@defproc[(file-unbox [fb file-box?])
         serializable?]{
 Returns the value inside @scheme[fb]
}

@defproc[(file-box-set? [fb file-box?])
         boolean?]{
 Returns @scheme[#t] if @scheme[fb] contains a value.
}

@defproc[(file-box-set! [fb file-box?]
                        [v serializable?])
         void]{
 Saves @scheme[v] in the file represented by @scheme[fb].
}

@warning{If you plan on using a load-balancer, make sure your file-boxes
are on a shared medium.}

}

@; ------------------------------------------------------------
@section[#:tag "lang/web-param.ss"]{Stateless Web Parameters}
@(require (for-label web-server/lang/web-param))

@defmodule[web-server/lang/web-param]{

It is not easy to use @scheme[parameterize] in the
Web Language. @filepath{lang/web-param.ss} provides (roughly) the same
functionality in a way that is serializable. Like other serializable
things in the Web Language, they are sensitive to source code modification.

@defform[(make-web-parameter default)]{
 Expands to the definition of a web-parameter with
 @scheme[default] as the default value. A web-parameter is
 a procedure that, when called with zero arguments, returns @scheme[default]
 or the last value @scheme[web-parameterize]d in the dynamic context
 of the call.
}

@defproc[(web-parameter? [v any/c])
         boolean?]{
 Checks if @scheme[v] appears to be a web-parameter.
}

@defform[(web-parameterize ([web-parameter-expr value-expr] ...) expr ...)]{
 Runs @scheme[(begin expr ...)] such that the web-parameters that
 the @scheme[web-parameter-expr]s evaluate to are bound to the @scheme[value-expr]s.
 From the perspective of the @scheme[value-expr]s, this is like @scheme[let].
}
}

@; ------------------------------------------------------------
@include-section["formlets.scrbl"]
@include-section["templates.scrbl"]
@include-section["managers.scrbl"]
