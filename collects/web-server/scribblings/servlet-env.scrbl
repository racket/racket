#lang scribble/doc
@(require "web-server.ss")

@title[#:tag "servlet-env.ss"
       #:style 'toc]{Simple Single Servlet Servers}
@(require (for-label web-server/servlet-env
                     web-server/http
                     web-server/managers/lru
                     web-server/configuration/responders
                     scheme/list))

@defmodule[web-server/servlet-env]{

The @web-server provides a way to quickly configure and start a server instance.

Here's a simple example:
@schememod[
scheme
(require web-server/servlet
         web-server/servlet-env)

(define (my-app request)
  `(html (head (title "Hello world!"))
         (body (p "Hey out there!"))))

(serve/servlet my-app)
]

Suppose you'd like to change the port to something else, change the last line to:
@schemeblock[
(serve/servlet my-app 
               #:port 8080)
]

By default the URL for your servlet is @filepath{http://localhost:8000/servlets/standalone.ss}, 
suppose you wanted it to be @filepath{http://localhost:8000/hello.ss}:
@schemeblock[
(serve/servlet my-app
               #:servlet-path "/hello.ss")
]

Suppose you wanted it to capture top-level requests:
@schemeblock[
(serve/servlet my-app
               #:servlet-path "/")
]
Or, perhaps just some nice top-level name:
@schemeblock[
(serve/servlet my-app
               #:servlet-path "/main")
]

Suppose you wanted to use a style-sheet (@filepath{style.css}) found on your Desktop (@filepath{/Users/jay/Desktop/}):
@schemeblock[
(serve/servlet my-app
               #:extra-files-paths 
               (list
                (build-path "/Users/jay/Desktop")))
]
These files are served @emph{in addition} to those from the @scheme[#:server-root-path] @filepath{htdocs} directory.
Notice that you may pass any number of extra paths.

Suppose you would like to start a server for a stateless Web servlet @filepath{servlet.ss} that provides @schemeid[start]:
@schememod[
 scheme
 (require "servlet.ss"
          web-server/servlet-env)

 (serve/servlet start #:stateless? #t)
]
Note: If you put the call to @scheme[serve/servlet] in the module like normal, strange things will happen because of the way
the top-level interacts with continuations. (Read: Don't do it.)

If you want to use @scheme[serve/servlet] in a start up script for a Web server, and don't want a browser opened or the DrScheme banner printed, then you can write:
@schemeblock[
(serve/servlet my-app
               #:command-line? #t)
]

@defproc[(serve/servlet [start (request? . -> . response?)]
                        [#:command-line? command-line? boolean? #f]
                        [#:launch-browser? launch-browser? boolean? (not command-line?)]
                        [#:quit? quit? boolean? (not command-line?)]
                        [#:banner? banner? boolean? (not command-line?)]
                        [#:listen-ip listen-ip string? "127.0.0.1"]
                        [#:port port number? 8000]
                        [#:servlet-path servlet-path string?
                                        "/servlets/standalone.ss"]
                        [#:servlet-regexp servlet-regexp regexp?
                                          (regexp (format "^~a$" (regexp-quote servlet-path)))]
                        [#:stateless? stateless? boolean? #f]
                        [#:manager manager manager? (make-threshold-LRU-manager #f (* 1024 1024 64))]
                        [#:servlet-namespace servlet-namespace (listof module-path?) empty]
                        [#:server-root-path server-root-path path? default-server-root-path]
                        [#:extra-files-paths extra-files-paths (listof path?) (list (build-path server-root-path "htdocs"))]
                        [#:servlets-root servlets-root path? (build-path server-root-path "htdocs")]
                        [#:servlet-current-directory servlet-current-directory path? servlets-root]
                        [#:file-not-found-responder file-not-found-responder
                                                    (gen-file-not-found-responder (build-path server-root-path "conf" "not-found.html"))]
                        [#:mime-types-path mime-types-path path?
                                           (build-path server-root-path "mime.types")])
                       void]{
 This sets up and starts a fairly default server instance.
      
 @scheme[start] is loaded as a servlet and responds to requests that match @scheme[servlet-regexp]. The current directory
 of servlet execution is @scheme[servlet-current-directory].

 If @scheme[launch-browser?] is true, then a web browser is opened to @filepath{http://localhost:<port><servlet-path>}.
 
 If @scheme[quit?] is true, then the URL @filepath["/quit"] ends the server.
  
 If @scheme[stateless?] is true, then the servlet is run as a stateless @schememodname[web-server] module.
 
 Advanced users may need the following options:
 
 The server listens on @scheme[listen-ip] and port @scheme[port].
 
 The servlet is loaded with @scheme[manager]
 as its continuation manager. (The default manager limits the amount of memory to 64 MB and
 deals with memory pressure as discussed in the @scheme[make-threshold-LRU-manager] documentation.)
 
 The modules specified by @scheme[servlet-namespace] are shared with other servlets.
 
 The server files are rooted at @scheme[server-root-path] (which is defaultly the distribution root.)
 File paths, in addition to the @filepath["htdocs"] directory under @scheme[server-root-path] may be
 provided with @scheme[extra-files-paths]. These paths are checked first, in the order they appear in the list.
 
 Other servlets are served from @scheme[servlets-root].
 
 If a file cannot be found, @scheme[file-not-found-responder] is used to generate an error response.
 
 If @scheme[banner?] is true, then an informative banner is printed. You may want to use this when
 running from the command line, in which case the @scheme[command-line?] option controls similar options.
 
 MIME types are looked up at @scheme[mime-types-path].
}

}