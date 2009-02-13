#lang scribble/doc
@(require "web-server.ss")
@(require (for-label web-server/dispatchers/dispatch-servlets))

@title{Troubleshooting and Tips}

@section{Why are my servlets not updating on the server when I change the code on disk?}

By default, the server uses @scheme[make-cached-url->servlet] to load servlets
from the disk. As it loads them, they are cached and the disk is not referred to for future
requests. This ensures that there is a single namespace for each servlet, so that different instances
can share resources, such as database connections, and communicate through the store. The default
configuration of the server (meaning the dispatcher sequence used when you load a configuration file)
provides a special URL to localhost that will reset the cache: @filepath{/conf/refresh-servlets}. If
you want the server to reload your changed servlet code, then GET this URL and the server will reload the
servlet on the next request.

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
(with-output-to-file _your-pid-file (lambda () (write (getpid))))
(_start-server)
]

@section[#:tag "faq:https"]{How do I set up the server to use HTTPS?}

This requires an SSL certificate and private key. This is very platform specific, but we will provide
the details for using OpenSSL on UNIX:

@commandline{openssl genrsa -des3 -out private-key.pem 1024}

This will generate a new private key, but it will have a passphrase on it. You can remove this via:

@commandline{openssl rsa -in private-key.pem -out private-key.pem}
@commandline{chmod 400 private-key.pem}

Now, we generate a self-signed certificate:

@commandline{openssl req -new -x509 -nodes -sha1 -days 365 -key private-key.pem > server-cert.pem}

(Each certificate authority has different instructions for generating certificate signing requests.)

We can now start the server with:

@commandline{plt-web-server --ssl}

The Web Server will start on port 443 (which can be overridden with the @exec{-p} option) using the
@filepath{private-key.pem} and @filepath{server-cert.pem} we've created.

@section{How do I limit the number of requests serviced at once by the Web Server?}

Refer to @secref["limit.ss"].
