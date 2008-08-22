#lang scribble/doc
@(require "web-server.ss")

@title{Troubleshooting}

@section{What special considerations are there for security with the Web Server?}

The biggest problem is that a naive usage of continuations will allow continuations to subvert
authentication mechanisms. Typically, all that is necessary to execute a continuation is its URL.
Thus, URLs must be as protected as the information in the continuation.

Consider if you link to a public site from a private continuation URL: the @exec{Referrer} field in
the new HTTP request will contain the private URL. Furthermore, if your HTTP traffic is in the clear,
then these URLs can be easily poached.

One solution to this is to use a special cookie as an authenticator. This way, if a URL escapes, it will
not be able to be used, unless the cookie is present. For advice about how to do this well, see
@link["http://cookies.lcs.mit.edu/pubs/webauth.html"]{Dos and Don'ts of Client Authentication on the Web}
from the MIT Cookie Eaters.

Note: It may be considered a great feature that URLs can be shared this way, because delegation is
easily built into an application via URLs.

@section{How do I use Apache with the PLT Web Server?}

You may want to put Apache in front of your PLT Web Server application. 
Apache can rewrite and proxy requests for a private (or public) PLT Web Server:

@verbatim{
RewriteRule ^(.*)$ http://localhost:8080/$1 [P]
}

The first argument to @exec{RewriteRule} is a match pattern. The second is how to rewrite the URL.
The @exec{[P]} flag instructs Apache to proxy the request. If you do not include this, Apache will
return an HTTP Redirect response and the client should make a second request.

See Apache's documentation for more details on @link["http://httpd.apache.org/docs/2.0/mod/mod_rewrite.html#rewriterule"]{RewriteRule}.

@section{IE ignores my CSS or behaves strange in other ways}

@(require (for-label xml))

In quirks mode, IE does not parse your page as XML, in particular it will not recognize many instances of
"empty tag shorthand", e.g. "<img src='...' />", whereas the @web-server uses @schememodname[xml]
to format XML, which uses empty tag shorthand by default. You can change the default with the @scheme[empty-tag-shorthand]
parameter: @scheme[(empty-tag-shorthand 'never)].

@section{Can the server create a PID file?}

The server has no option for this, but you can add it very easily. There's two techniques.

First, if you use a UNIX platform, in your shell startup script you can use
@verbatim{
echo $$ > PID
exec run-web-server
}

Using @exec{exec} will reuse the same process, and therefore, the PID file will be accurate.

Second, if you want to make your own Scheme start-up script, you can write:
@(require (for-label mzlib/os))
@schemeblock[
(require mzlib/os)
(with-output-to-file _pid-file (lambda () (write (getpid))))
(_start-server)
]

@section[#:tag "faq:https"]{How do I set up the server to use HTTPS?}

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
         (only-in web-server/web-server do-not-return)
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
  (configuration-table->web-config@
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