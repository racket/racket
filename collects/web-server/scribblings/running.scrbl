#lang scribble/doc
@(require "web-server.ss")

@title[#:tag "run.ss"]{Running Web Servlets}

There are a number of ways to run Web servlets.

@; ------------------------------------------------------------
@section[#:tag "insta"]{Instant Servlets}
@(require (for-label (only-in web-server/insta/insta
                              no-web-browser static-files-path)
                     web-server/servlet-env))

@defmodulelang[web-server/insta #:use-sources (web-server/insta/insta)]

The fastest way to get a servlet running in the Web server is to use the 
"Insta" language in DrScheme. Enter the following into DrScheme:

@schememod[
web-server/insta

(define (start request)
  `(html (head (title "Hello world!"))
         (body (p "Hey out there!"))))
]

And press @onscreen["Run"]. A Web browser will open up showing your new servlet.
This servlet will only be accessible from your local machine.

Behind the scenes, DrScheme has used @scheme[serve/servlet] to start a new server
that uses your @scheme[start] function as the servlet. 
You are given the entire @schememodname[web-server/servlet] API.

The following API is provided to customize the server instance:

@defproc[(no-web-browser) void]{
 Calling this will instruct DrScheme to @emph{not} start a Web browser when you press
  @onscreen["Run"].
}

@defproc[(static-files-path [path path-string?]) void]{
 This instructs the Web server to serve static files, such as stylesheet and images, from @scheme[path].
}


@; ------------------------------------------------------------
@include-section["servlet-env.scrbl"]

@; ------------------------------------------------------------
@section[#:tag "command-line-tools"]{Command-line Tools}
@(require (for-label web-server/configuration/configuration-table
                     web-server/web-server
                     web-server/web-config-sig
                     web-server/web-config-unit
                     web-server/web-server-unit))

@section-index{plt-web-server}

One command-line utility is provided with the @|web-server|:

@commandline{plt-web-server [-f <file-name> -p <port> -a <ip-address> --ssl]}

The optional file-name argument specifies the path to a
@scheme[configuration-table] S-expression (see @scheme[configuration-table->sexpr] for the syntax documentation.)
If this is not provided, the
default configuration shipped with the server is used. The optional
port and ip-address arguments override the corresponding portions of
the @scheme[configuration-table]. If the SSL option is provided, then 
the server uses HTTPS with @filepath{server-cert.pem} and @filepath{private-key.pem}
in the current directory, with 443 as the default port. (See the @schememodname[openssl] 
module for details on the SSL implementation.)

The @scheme[configuration-table] is given to
@scheme[configuration-table->web-config@] and used to construct a
@scheme[web-config^] unit, and is linked with the
@scheme[web-server@] unit. The resulting unit is invoked, and the
server runs until the process is killed.
