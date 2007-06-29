#reader(lib "docreader.ss" "scribble")
@require["../web-server.ss"]

@title[#:tag "web-server-unit.ss"
       #:style 'toc]{Web Server Unit}

The @web-server offers a unit-based approach to running the server.

@file{web-server-sig.ss} provides the @defthing[web-server^ signature?] signature
with two elements:

 @defproc[(serve) (-> void)]{
  Runs the server and returns a procedure that shuts down the server.
 }

 @defproc[(serve-ports [ip input-port?]
                       [op output-port?])
          void]{
 Serves a single connection represented by the ports @scheme[ip] and
 @scheme[op].
 }

@file{web-server-unit.ss} provides the @defthing[web-server\@ unit?] unit. It
imports a @scheme[web-config^] unit and a @scheme[tcp^] unit. It uses the
@scheme[web-config^] to construct a @scheme[dispatcher?] function that
sets up one virtual host dispatcher, for each virtual host in the @scheme[web-config^],
that sequences the following operations:
@itemize[
 @item{Logs the incoming request with the given format to the given file}
 @item{Performs HTTP Basic Authentication with the given password file}
 @item{Allows the @scheme["/conf/refresh-passwords"] URL to refresh the password file.}
 @item{Allows the @scheme["/conf/collect-garbage"] URL to call the garbage collector.}
 @item{Allows the @scheme["/conf/refresh-servlets"] URL to refresh the servlets cache.}
 @item{Execute servlets under the @scheme["/servlets/"] URL in the given servlet root directory.}
 @item{Serves files under the @scheme["/"] URL in the given htdocs directory.}
]

Using this @scheme[dispatcher?], it loads a dispatching server that provides @scheme[serve]
and @scheme[serve-ports] functions that operate as expected.
