#lang scribble/doc
@(require "web-server.ss")

@title{Troubleshooting}

@section{General}

@subsection{IE ignores my CSS or behaves strange in other ways}

@(require (for-label xml))

In quirks mode, IE does not parse your page as XML, in particular it will not recognize many instances of
"empty tag shorthand", e.g. "<img src='...' />", whereas the @web-server uses @schememodname[xml]
to format XML, which uses empty tag shorthand by default. You can change the default with the @scheme[empty-tag-shorthand]
parameter: @scheme[(empty-tag-shorthand 'never)].


@subsection{How do I set up the server to use HTTPS?}

The essence of the solution to this problem is to use an SSL TCP implementation as provided by @schememodname[net/ssl-tcp-unit]. Many of the functions that start the Web Server are parameterized by a @scheme[tcp@] unit. If you pass an SSL unit, then the server will be serving HTTPS. However, to do this, you must write your own start up script. Here's a simple example:

@(require (for-label scheme/unit)
          (for-label net/ssl-tcp-unit)
          (for-label net/tcp-sig)
          (for-label net/tcp-unit)
          (for-label web-server/web-server)
          (for-label web-server/web-server-unit)
          (for-label web-server/web-server-sig)
          (for-label web-server/web-config-sig)
          (for-label web-server/web-config-unit)
          (for-label web-server/configuration/namespace))

@schememod[
scheme

@code:comment{Load the appropriate libraries to reimplement server}
(require scheme/unit
         net/ssl-tcp-unit
         net/tcp-sig
         net/tcp-unit
         web-server/web-server
         web-server/web-server-unit
         web-server/web-server-sig
         web-server/web-config-sig
         web-server/web-config-unit
         web-server/configuration/namespace)

@code:comment{Define the necessary parameters.}
(define port-no 8443)
(define SSL-path (find-system-path 'home-dir))

@code:comment{Load the standard configuration file, but augment the port.}
(define configuration
  (configuration-table-sexpr->web-config@
   (build-path (collection-path "web-server")
               "default-web-root"
               "configuration-table.ss")
   #:port port-no))

@code:comment{The configuration is a unit and this lets us treat it as one.}
(define-unit-binding config@ configuration
  (import) (export web-config^))

@code:comment{This loads the SSL TCP interface with the appropriate keys.}
(define-unit-binding ssl-tcp@
  (make-ssl-tcp@ (build-path SSL-path "server-cert.pem")
                 (build-path SSL-path "private-key.pem")
                 #f #f #f #f #f)
  (import) (export tcp^))

@code:comment{Combine the configuration with the TCP interface to get a server!}
(define-compound-unit/infer ssl-server@
  (import)
  (link ssl-tcp@ config@ web-server@)
  (export web-server^))

@code:comment{Invoke the server to get at what it provides.}
(define-values/invoke-unit/infer ssl-server@)

@code:comment{Run the server.}
(serve)
(do-not-return)
]

Running this script, rather than @exec{plt-web-server}, runs the server using SSL on port @scheme[port-no].
The certificate and private key are located in the @scheme[SSL-path] directory.