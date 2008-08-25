#lang scribble/doc
@(require "web-server.ss")

@title[#:tag "servlet-env.ss"
       #:style 'toc]{Simple Single Servlet Servers}
@(require (for-label web-server/servlet-env))

@defmodule[web-server/servlet-env]

The @web-server provides a way to quickly configure and start a server instance.

@defproc[(serve/servlet [servlet (request? . -> . response?)]
                       [#:launch-browser? launch-browser? boolean? #t]
                       [#:quit? quit? boolean? #t]
                       [#:listen-ip listen-ip string? "127.0.0.1"]
                       [#:port port number? 8000]
                       [#:manager manager manager? default-threshold-LRU-manager]
                       [#:servlet-namespace servlet-namespace (listof module-path?) empty]
                       [#:server-root-path server-root-path path? default-server-root-path]
                       [#:extra-files-path extra-files-path path? (build-path server-root-path "htdocs")]
                       [#:servlets-root servlets-root path? (build-path server-root-path ".")]
                       [#:file-not-found-path file-not-found-path  path?
                                              (build-path server-root-path "conf" "not-found.html")]
                       [#:mime-types-path mime-types-path path?
                                          (build-path server-root-path "mime.types")]
                       [#:servlet-path servlet-path path?
                                       "servlets/standalone.ss"])
                       void]{
 This sets up and starts a fairly default server instance.
      
 @scheme[servlet] is installed as a server at @scheme[servlet-path] with @scheme[manager]
 as its continuation manager. (The default manager limits the amount of memory to 64 MB and
 deals with memory pressure as discussed in the @scheme[make-threshold-LRU-manager] documentation.)
 
 If @scheme[launch-browser?] is true, then a web browser is opened to the servlet's start page.
 
 If @scheme[quit?] is true, then the URL @filepath["/quit"] ends the server.

 Advanced users may need the following options:
 
 The server listens on @scheme[listen-ip] and port @scheme[port].
 
 The modules specified by @scheme[servlet-namespace] are shared with other servlets.
 
 The server files are rooted at @scheme[server-root-path] (which is defaultly the distribution root.)
 A file path, in addition to the @filepath["htdocs"] directory under @scheme[server-root-path] may be
 provided with @scheme[extra-files-path]. These files are checked first.
 The @filepath["servlets"] directory is expected at @scheme[servlets-root].
 
 If a file cannot be found, @scheme[file-not-found-path] is used as an error response.
 
 MIME types are looked up at @scheme[mime-types-path].
}
