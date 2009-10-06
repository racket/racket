#lang scribble/doc
@(require "web-server.ss")

@title[#:tag "servlet-env.ss"
       #:style 'toc]{Simple Single Servlet Servers}
@(require (for-label web-server/servlet-env
                     web-server/http
                     web-server/managers/lru
                     web-server/lang/lang-api
                     web-server/private/util
                     web-server/dispatchers/dispatch
                     web-server/configuration/configuration-table
                     web-server/configuration/responders
                     web-server/dispatchers/dispatch-log
                     scheme/serialize
                     web-server/stuffers
                     scheme/list))

@defmodule[web-server/servlet-env]{

The @web-server provides a way to quickly configure and start a servlet with more customizability than @schememodname[web-server/insta] provides. This is provided by the @schememodname[web-server/servlet-env] module. Here is a simple example of its use:
@schememod[
scheme
(require web-server/servlet
         web-server/servlet-env)

(define (start request)
  `(html (head (title "Hello world!"))
         (body (p "Hey out there!"))))
                                      
(serve/servlet start)
]

Unlike the @schememodname[web-server/insta] language, @scheme[start] is not a special identifier, so we could just as well have written the example as:
@schememod[
scheme
(require web-server/servlet
         web-server/servlet-env)

(define (my-app request)
  `(html (head (title "Hello world!"))
         (body (p "Hey out there!"))))

(serve/servlet my-app)
]

Let's look at some of the customizations @scheme[serve/servlet] allows.

Suppose you'd like to change the port to something else, change the last line to:
@schemeblock[
(serve/servlet my-app 
               #:port 8080)
]

Suppose you want to accept connections from external machines:
@schemeblock[
(serve/servlet my-app 
               #:listen-ip #f)
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
               #:servlet-regexp #rx"")
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
You may pass any number of extra paths.

If you want to use @scheme[serve/servlet] in a start up script for a Web application,
and don't want a browser opened or the DrScheme banner printed, then you can write:
@schemeblock[
(serve/servlet my-app
               #:command-line? #t)
]

Suppose you would like to start a server for a stateless Web servlet @filepath{servlet.ss} that provides @schemeid[start]:
@schememod[
 scheme
 (require "servlet.ss"
          web-server/servlet-env)

 (serve/servlet start #:stateless? #t)
]

@bold{Warning:} If you put the call to @scheme[serve/servlet] in a @schememodname[web-server] module directly it will not work correctly. 
Consider the following module:
@schememod[
 web-server
 (require web-server/servlet-env)
 
 (define (start req)
   (start
    (send/suspend
     (lambda (k-url)
       `(html (body (a ([href ,k-url]) "Hello world!")))))))
 
 (serve/servlet start #:stateless? #t)
]
First, if this module is not saved in a file (e.g., @filepath{servlet.ss}), then the serialization layer cannot locate the definitions of the 
serialized continuations. Second, due to an unfortunately subtle bug that we have not yet corrected,
every time the continuation link is clicked, @scheme[serve/servlet] will
run and attempt to start a Web server instance and open a browser window. These problems do not occur if your servlet is saved in a file
and if @scheme[serve/servlet] is run in another module.

@defproc[(serve/servlet [start (request? . -> . response/c)]
                        [#:command-line? command-line? boolean? #f]
                        [#:launch-browser? launch-browser? boolean? (not command-line?)]
                        [#:quit? quit? boolean? (not command-line?)]
                        [#:banner? banner? boolean? (not command-line?)]
                        [#:listen-ip listen-ip (or/c false/c string?) "127.0.0.1"]
                        [#:port port number? 8000]
                        [#:servlet-path servlet-path string?
                                        "/servlets/standalone.ss"]
                        [#:servlet-regexp servlet-regexp regexp?
                                          (regexp 
                                           (format 
                                            "^~a$"
                                            (regexp-quote servlet-path)))]
                        [#:stateless? stateless? boolean? #f]
                        [#:stuffer stuffer (stuffer/c serializable? bytes?) default-stuffer]
                        [#:manager manager manager? (make-threshold-LRU-manager #f (* 128 1024 1024))]
                        [#:servlet-namespace servlet-namespace (listof module-path?) empty]
                        [#:server-root-path server-root-path path-string? default-server-root-path]
                        [#:extra-files-paths extra-files-paths (listof path-string?) (list (build-path server-root-path "htdocs"))]
                        [#:servlets-root servlets-root path-string? (build-path server-root-path "htdocs")]
                        [#:servlet-current-directory servlet-current-directory path-string? servlets-root]
                        [#:file-not-found-responder file-not-found-responder
                                                    (request? . -> . response/c)
                                                    (gen-file-not-found-responder 
                                                     (build-path
                                                      server-root-path
                                                      "conf"
                                                      "not-found.html"))]
                        [#:mime-types-path mime-types-path path-string?
                                           ....]
                        [#:ssl? ssl? boolean? #f]
                        [#:ssl-cert ssl-cert (or/c false/c path-string?) (and ssl? (build-path server-root-path "server-cert.pem"))]
                        [#:ssl-key ssl-key (or/c false/c path-string?) (and ssl? (build-path server-root-path "private-key.pem"))]

                        [#:log-file log-file (or/c false/c path-string?) #f]
                        [#:log-format log-format log-format/c 'apache-default])
                       void]{
 This sets up and starts a fairly default server instance.
      
 @scheme[start] is loaded as a servlet and responds to requests that match @scheme[servlet-regexp]. The current directory
 of servlet execution is @scheme[servlet-current-directory].

 If @scheme[launch-browser?] is true, then a web browser is opened to @filepath{http://localhost:<port><servlet-path>}.
 
 If @scheme[quit?] is true, then the URL @filepath["/quit"] ends the server.
  
 If @scheme[stateless?] is true, then the servlet is run as a stateless @schememod[web-server] module and @scheme[stuffer] is used
 as the @tech{stuffer}.
 
 Advanced users may need the following options:
 
 The server listens on @scheme[listen-ip] and port @scheme[port]. If @scheme[listen-ip] is @scheme[#f], then the server accepts 
 connections to all of the listening machine's addresses. Otherwise, the server accepts connections only at the interface(s) associated with the given string.
 For example, providing @scheme["127.0.0.1"] (the default) as @scheme[listen-ip] creates a server that accepts only connections to @scheme["127.0.0.1"] (the loopback interface) from the local machine.
 
 If @scheme[ssl-cert] and @scheme[ssl-key] are not false, then the server runs in HTTPS mode with @scheme[ssl-cert]
 and @scheme[ssl-key] as the certificates and private keys.
 
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
 
 MIME types are looked up at @scheme[mime-types-path]. By default the @filepath{mime.types} file in the
 @scheme[server-root-path] is used, but if that file does not exist, then the file that ships with the
 Web Server is used instead. Of course, if a path is given, then it overrides this behavior.

 If @scheme[log-file] is given, then it used to log requests using @scheme[log-format] as the format. Allowable formats
 are those allowed by @scheme[log-format->format].
}
              
}
