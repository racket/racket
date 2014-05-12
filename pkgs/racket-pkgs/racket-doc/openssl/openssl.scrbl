#lang scribble/doc
@(require scribble/manual
          racket/list
          (for-label openssl
                     racket
                     openssl/sha1
                     openssl/md5))

@title{OpenSSL: Secure Communication}

@defmodule[openssl]

The @racketmodname[openssl] library provides glue for the OpenSSL
library with the Racket port system. It provides functions nearly
identically to the standard TCP subsystem in Racket, plus a
generic @racket[ports->ssl-ports] interface.

To use this library, you will need OpenSSL installed on your machine,
but on many platforms the necessary libraries are included with the OS
or with the Racket distribution. In particular:

@itemize[

@item{For Windows, @racketmodname[openssl] depends on
@filepath{libeay32.dll} and @filepath{ssleay32.dll}, which are
included in the Racket distribution for Windows.}

@item{For Mac OS X, @racketmodname[openssl] depends on
@filepath{libssl.dylib} and @filepath{libcrypto.dylib}, which are
provided by Mac OS X 10.2 and later.}

@item{For Unix, @racketmodname[openssl] depends on
@filepath{libssl.so} and @filepath{libcrypto.so}, which must be
installed in a standard library location or in a directory listed by
@envvar{LD_LIBRARY_PATH}. These libraries are included in many OS
distributions.}

]

@defthing[ssl-available? boolean?]{

A boolean value that reports whether the system OpenSSL library was
successfully loaded. Calling @racket[ssl-connect], @|etc| when this
value is @racket[#f] (library not loaded) will raise an exception.}


@defthing[ssl-load-fail-reason (or/c #f string?)]{

Either @racket[#f] (when @racket[ssl-available?] is @racket[#t]) or an
error string (when @racket[ssl-available?] is @racket[#f]).}

@; ----------------------------------------------------------------------

@section{TCP-like Client Procedures}

Use @racket[ssl-connect] or @racket[ssl-connect/enable-break] to
create an SSL connection over TCP. To create a secure connection,
supply the result of @racket[ssl-secure-client-context] or create a
client context with @racket[ssl-make-client-context] and configure it
using the functions described in @secref["cert-procs"].

@defproc[(ssl-connect [hostname string?]
                      [port-no (integer-in 1 65535)]
                      [client-protocol
                       (or/c ssl-client-context?
                             'sslv2-or-v3 'sslv2 'sslv3 'tls 'tls11 'tls12)
                       'sslv2-or-v3])
         (values input-port? output-port?)]{

Connect to the host given by @racket[hostname], on the port given by
@racket[port-no]. This connection will be encrypted using SSL.  The
return values are as for @racket[tcp-connect]: an input port and an
output port.

The optional @racket[client-protocol] argument determines which
encryption protocol is used, whether the server's certificate is
checked, etc. The argument can be either a client context created by
@racket[ssl-make-client-context], or one of the following symbols:
@racket['sslv2-or-v3] (the default), @racket['sslv2], @racket['sslv3],
@racket['tls], @racket['tls11], or @racket['tls12]; see
@racket[ssl-make-client-context] for further details (including the
meanings of the protocol symbols).

Closing the resulting output port does not send a shutdown message to
the server. See also @racket[ports->ssl-ports].

If hostname verification is enabled (see
@racket[ssl-set-verify-hostname!]), the peer's certificate is checked
against @racket[hostname].

@;{
See `enforce-retry?' in "mzssl.rkt", currently set to #f so that this
paragraph does not apply:
Beware that the SSL protocol allows reading or writing in only one
direction at a time. If you request data from the input port, then
data cannot be written to the output port (i.e., attempting to write
will block) until the other end of the connection responds to the
read. Even merely checking for input data --- using
@racket[byte-ready?], for example --- commits the connection to
reading, and the other end must respond with a (possibly zero-length)
answer. Protocols that work with SSL, such as IMAP, have a
well-defined communication pattern, where theres no question of
whether the other end is supposed to be sending or reading data.
}

}

@defproc[(ssl-connect/enable-break
          [hostname string?]
	  [port-no (integer-in 1 65535)]
	  [client-protocol
	   (or/c ssl-client-context?
                 'sslv2-or-v3 'sslv2 'sslv3 'tls 'tls11 'tls12)
           'sslv2-or-v3])
         (values input-port? output-port?)]{

Like @racket[ssl-connect], but breaking is enabled while trying to
connect.}


@defproc[(ssl-secure-client-context)
         ssl-client-context?]{

Returns a client context (using the @racket['tls] protocol) that
verifies certificates using the default verification sources from
@racket[(ssl-default-verify-sources)], verifies hostnames, and avoids
using weak ciphers. The result is essentially equivalent to the
following:

@racketblock[
(let ([ctx (ssl-make-client-context 'tls)])
  (code:comment "Load default verification sources (root certificates)")
  (ssl-load-default-verify-sources! ctx)
  (code:comment "Require certificate verification")
  (ssl-set-verify! ctx #t)
  (code:comment "Require hostname verification")
  (ssl-set-verify-hostname! ctx #t)
  (code:comment "No weak cipher suites")
  (ssl-set-ciphers! ctx "DEFAULT:!aNULL:!eNULL:!LOW:!EXPORT:!SSLv2")
  (code:comment "Seal context so further changes cannot weaken it")
  (ssl-seal-context! ctx)
  ctx)
]

The context is cached, so different calls to
@racket[ssl-secure-client-context] return the same context unless
@racket[(ssl-default-verify-sources)] has changed.
}


@defproc[(ssl-make-client-context
          [protocol (or/c 'sslv2-or-v3 'sslv2 'sslv3 'tls 'tls11 'tls12) 'sslv2-or-v3])
         ssl-client-context?]{

Creates a context to be supplied to @racket[ssl-connect]. The context
identifies a communication protocol (as selected by
@racket[protocol]), and also holds certificate information (i.e., the
client's identity, its trusted certificate authorities, etc.). See the
section @secref["cert-procs"] below for more information on
certificates.

The @racket[protocol] must be one of the following:
@itemize[
  @item{@racket['sslv2-or-v3] : SSL protocol versions 2 or 3, as
  appropriate (this is the default)}
  @item{@racket['sslv2] : SSL protocol version 2}
  @item{@racket['sslv3] : SSL protocol version 3}
  @item{@racket['tls] : the TLS protocol version 1}
  @item{@racket['tls11] : the TLS protocol version 1.1}
  @item{@racket['tls12] : the TLS protocol version 1.2}
]

Note that SSL protocol version 2 is deprecated on some platforms and may not be
present in your system libraries. The use of SSLv2 may also compromise security; 
thus, using SSLv3 is recommended. TLS 1.1 and 1.2 are relatively new and not
always available. See also @racket[supported-client-protocols] and
@racket[supported-server-protocols].

@history[#:changed "6.1" @elem{Added @racket['tls11] and @racket['tls12].}]}


@defproc[(supported-client-protocols)
         (listof (or/c 'sslv2-or-v3 'sslv2 'sslv3 'tls 'tls11 'tls12))]{

Returns a list of symbols representing protocols that are supported
for clients on the current platform.}


@defproc[(ssl-client-context? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a value produced by
@racket[ssl-make-client-context], @racket[#f] otherwise.

@history[#:added "6.0.1.3"]}


@; ----------------------------------------------------------------------

@section{TCP-like Server Procedures}

@defproc[(ssl-listen
	  [port-no (integer-in 1 65535)]
	  [queue-k exact-nonnegative-integer? 5]
	  [reuse? any/c #f]
	  [hostname-or-#f (or/c string? #f) #f]
	  [server-protocol
	   (or/c ssl-server-context? 
                 'sslv2-or-v3 'sslv2 'sslv3 'tls 'tls11 'tls12)
           'sslv2-or-v3])
	 ssl-listener?]{

Like @racket[tcp-listen], but the result is an SSL listener. The extra optional
@racket[server-protocol] is as for @racket[ssl-connect], except that a
context must be a server context instead of a client context.

Call @racket[ssl-load-certificate-chain!] and
@racket[ssl-load-private-key!] to avoid a @emph{no shared cipher}
error on accepting connections. The file @filepath{test.pem} in the
@filepath{openssl} collection is a suitable argument for both calls
when testing. Since @filepath{test.pem} is public, however, such a
test configuration obviously provides no security.

An SSL listener is a synchronizable value (see @racket[sync]). It is
ready---with itself as its value---when the underlying TCP listener is
ready. At that point, however, accepting a connection with
@racket[ssl-accept] may not complete immediately, because
further communication is needed to establish the connection.}


@deftogether[(
  @defproc[(ssl-close [listener ssl-listener?]) void?]
  @defproc[(ssl-listener? [v any/c]) boolean?])]{

Analogous to @racket[tcp-close] and @racket[tcp-listener?].}

@deftogether[(
  @defproc[(ssl-accept [listener ssl-listener?])
           (values input-port? output-port?)]
  @defproc[(ssl-accept/enable-break [listener ssl-listener?])
           (values input-port? output-port?)])]{

Analogous to @racket[tcp-accept].

Closing the resulting output port does not send a shutdown message to
the client. See also @racket[ports->ssl-ports].

See also @racket[ssl-connect] about the limitations of reading and
writing to an SSL connection (i.e., one direction at a time).

The @racket[ssl-accept/enable-break] procedure is analogous to
@racket[tcp-accept/enable-break].}


@defproc[(ssl-abandon-port [p ssl-port?]) void?]{

Analogous to @racket[tcp-abandon-port].}


@defproc[(ssl-addresses [p (or/c ssl-port? ssl-listener?)]
                        [port-numbers? any/c #f])
         void?]{

Analogous to @racket[tcp-addresses].}


@defproc[(ssl-port? [v any/c]) boolean?]{

Returns @racket[#t] of @racket[v] is an SSL port produced by
@racket[ssl-connect], @racket[ssl-connect/enable-break],
@racket[ssl-accept], @racket[ssl-accept/enable-break], or
@racket[ports->ssl-ports].}


@defproc[(ssl-make-server-context [protocol (or/c 'sslv2-or-v3 'sslv2 'sslv3 'tls 'tls11 'tls12)])
         ssl-server-context?]{

Like @racket[ssl-make-client-context], but creates a server context.}


@defproc[(ssl-server-context? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a value produced by
@racket[ssl-make-server-context], @racket[#f] otherwise.}

@defproc[(supported-server-protocols)
         (listof (or/c 'sslv2-or-v3 'sslv2 'sslv3 'tls 'tls11 'tls12))]{

Returns a list of symbols representing protocols that are supported
for servers on the current platform.

@history[#:added "6.0.1.3"]}

@; ----------------------------------------------------------------------

@section{SSL-wrapper Interface}

@defproc[(ports->ssl-ports
           [input-port input-port?]
	   [output-port output-port?]
           [#:mode mode symbol? 'accept]
	   [#:context context
                      (or/c ssl-client-context? ssl-server-context?)
                      ((if (eq? mode 'accept)
                           ssl-make-server-context 
                           ssl-make-client-context)
                       protocol)]
	   [#:encrypt protocol (or/c 'sslv2-or-v3 'sslv2 'sslv3 'tls 'tls11 'tls12) 'sslv2-or-v3]
	   [#:close-original? close-original? boolean? #f]
	   [#:shutdown-on-close? shutdown-on-close? boolean? #f]
	   [#:error/ssl error procedure? error]
           [#:hostname hostname (or/c string? #f) #f])
         (values input-port? output-port?)]{

Returns two values---an input port and an output port---that
implement the SSL protocol over the given input and output port. (The
given ports should be connected to another process that runs the SSL
protocol.)

The @racket[mode] argument can be @racket['connect] or
@racket['accept]. The mode determines how the SSL protocol is
initialized over the ports, either as a client or as a server. As with
@racket[ssl-listen], in @racket['accept] mode, supply a
@racket[context] that has been initialized with
@racket[ssl-load-certificate-chain!] and
@racket[ssl-load-private-key!] to avoid a @emph{no shared cipher}
error.

The @racket[context] argument should be a client context for
@racket['connect] mode or a server context for @racket['accept]
mode. If it is not supplied, a context is created using the protocol
specified by a @racket[protocol] argument.

If the @racket[protocol] argument is not supplied, it defaults to
@racket['sslv2-or-v3]. See @racket[ssl-make-client-context] for
further details (including all options and the meanings of the
protocol symbols).  This argument is ignored if a @racket[context]
argument is supplied.

If @racket[close-original?] is true, then when both SSL ports are
closed, the given input and output ports are automatically closed.

If @racket[shutdown-on-close?] is true, then when the output SSL port
is closed, it sends a shutdown message to the other end of the SSL
connection. When shutdown is enabled, closing the
output port can fail if the given output port becomes unwritable
(e.g., because the other end of the given port has been closed by
another process).

The @racket[error] argument is an error procedure to use for raising
communication errors. The default is @racket[error], which raises
@racket[exn:fail]; in contrast, @racket[ssl-accept] and
@racket[ssl-connect] use an error function that raises
@racket[exn:fail:network].

See also @racket[ssl-connect] about the limitations of reading and
writing to an SSL connection (i.e., one direction at a time).

If hostname verification is enabled (see
@racket[ssl-set-verify-hostname!]), the peer's certificate is checked
against @racket[hostname].
}

@; ----------------------------------------------------------------------

@section[#:tag "cert-procs"]{Context Procedures}

@defproc[(ssl-load-verify-source!
	    [context (or/c ssl-client-context? ssl-server-context?)]
            [src (or/c path-string?
                       (list/c 'directory path-string?)
                       (list/c 'win32-store string?)
                       (list/c 'macosx-keychain path-string?))]
            [#:try? try? any/c #f])
         void?]{

Loads verification sources from @racket[src] into
@racket[context]. Currently, only certificates are loaded; the
certificates are used to verify the certificates of a connection
peer. Call this procedure multiple times to load multiple sets of
trusted certificates.

The following kinds of verification sources are supported:

@itemlist[

@item{If @racket[src] is a path or string, it is treated as a PEM file
containing root certificates. The file is loaded immediately.}

@item{If @racket[src] is @racket[(list 'directory _dir)], then
@racket[_dir] should contain PEM files with hashed symbolic links (see
the @tt{openssl c_rehash} utility). The directory contents are not
loaded immediately; rather, they are searched only when a certificate
needs verification.}

@item{If @racket[src] is @racket[(list 'win32-store _store)], then the
certificates from the store named @racket[_store] are loaded
immediately. Only supported on Windows.}

@item{If @racket[src] is @racket[(list 'macosx-keychain _path)], then
the certificates from the keychain stored at @racket[_path] are loaded
immediately. Only supported on Mac OS X.}

]

If @racket[try?] is @racket[#f] and loading @racket[src] fails (for
example, because the file or directory does not exist), then an
exception is raised. If @racket[try?] is a true value, then a load
failure is ignored.

You can use the file @filepath{test.pem} of the @filepath{openssl}
collection for testing purposes. Since @filepath{test.pem} is public,
such a test configuration obviously provides no security.
}

@defparam[ssl-default-verify-sources srcs
          (let ([source/c (or/c path-string?
                                (list/c 'directory path-string?)
                                (list/c 'win32-store string?)
                                (list/c 'macosx-keychain path-string?))])
            (listof source/c))]{

Holds a list of verification sources, used by
@racket[ssl-load-default-verify-sources!]. The default sources depend
on the platform:

@itemlist[

@item{On Linux, the default sources are determined by the
@tt{SSL_CERT_FILE} and @tt{SSL_CERT_DIR} environment variables, if the
variables are set, or the system-wide default locations otherwise.}

@item{On Mac OS X, the default sources consist of the system keychain
for root certificates: @racket['(macosx-keychain
"/System/Library/Keychains/SystemRootCertificates.keychain")].}

@item{On Windows, the default sources consist of the system
certificate store for root certificates: @racket['(win32-store
"ROOT")].}

]
}

@defproc[(ssl-load-default-verify-sources!
           [context (or/c ssl-client-context? ssl-server-context?)])
         void?]{

Loads the default verification sources, as determined by
@racket[(ssl-default-verify-sources)], into @racket[context]. Load
failures are ignored, since some default sources may refer to
nonexistent paths.
}

@defproc[(ssl-load-verify-root-certificates!
            [context-or-listener (or/c ssl-client-conntext? ssl-server-context?
                                       ssl-listener?)]
            [pathname path-string?])
         void?]{

Deprecated; like @racket[ssl-load-verify-source!], but only supports
loading certificate files in PEM format.
}

@defproc[(ssl-set-ciphers! [context (or/c ssl-client-context? ssl-server-context?)]
                           [cipher-spec string?])
         void?]{

Specifies the cipher suites that can be used in connections created
with @racket[context]. The meaning of @racket[cipher-spec] is the same
as for the
@hyperlink["http://www.openssl.org/docs/apps/ciphers.html"]{@tt{openssl
ciphers} command}.
}

@defproc[(ssl-seal-context! [context (or/c ssl-client-context? ssl-server-context?)])
         void?]{

Seals @racket[context], preventing further modifications. After a
context is sealed, passing it to functions such as
@racket[ssl-set-verify!] and
@racket[ssl-load-verify-root-certificates!] results in an error.}

@defproc[(ssl-load-certificate-chain!
           [context-or-listener (or/c ssl-client-context? ssl-server-context?
				      ssl-listener?)]
	   [pathname path-string?])
         void?]{

Loads a PEM-format certification chain file for connections to made
with the given server context (created by
@racket[ssl-make-server-context]) or listener (created by
@racket[ssl-listen]). A certificate chain can also be loaded into a
client context (created by @racket[ssl-make-client-context]) when
connecting to a server requiring client credentials, but that
situation is uncommon.

This chain is used to identify the client or server when it connects
or accepts connections. Loading a chain overwrites the old chain. Also
call @racket[ssl-load-private-key!] to load the certificate's
corresponding key.

You can use the file @filepath{test.pem} of the @filepath{openssl}
collection for testing purposes. Since @filepath{test.pem} is public,
such a test configuration obviously provides no security.
}

@defproc[(ssl-load-private-key!
	  [context-or-listener (or/c ssl-client-context? ssl-server-context?
				     ssl-listener?)]
	  [pathname path-string?]
	  [rsa? boolean? #t]
	  [asn1? boolean? #f])
         void?]{

Loads the first private key from @racket[pathname] for the given
context or listener. The key goes with the certificate that identifies
the client or server. Like @racket[ssl-load-certificate-chain!], this
procedure is usually used with server contexts or listeners, seldom
with client contexts.

If @racket[rsa?] is @racket[#t] (the default), the first RSA key is
read (i.e., non-RSA keys are skipped). If @racket[asn1?] is
@racket[#t], the file is parsed as ASN1 format instead of PEM.

You can use the file @filepath{test.pem} of the @filepath{openssl}
collection for testing purposes. Since @filepath{test.pem} is public,
such a test configuration obviously provides no security.
}

@defproc[(ssl-load-suggested-certificate-authorities!
	  [context-or-listener (or/c ssl-client-context? ssl-server-context?
				     ssl-listener?)]
	  [pathname path-string?])
          void?]{

Loads a PEM-format file containing certificates that are used by a
server. The certificate list is sent to a client when the server
requests a certificate as an indication of which certificates the
server trusts.

Loading the suggested certificates does not imply trust, however; any
certificate presented by the client will be checked using the trusted
roots loaded by @racket[ssl-load-verify-root-certificates!].

You can use the file @filepath{test.pem} of the @filepath{openssl}
collection for testing purposes where the peer identifies itself using
@filepath{test.pem}.}

@deftogether[[
@defproc[(ssl-server-context-enable-dhe!
           [context ssl-server-context?]
           [dh-param-path path-string? ssl-dh4096-param-path])
         void?]
@defproc[(ssl-server-context-enable-ecdhe!
           [context ssl-server-context?]
           [curve-name symbol? 'secp521r1])
         void?]
]]{

Enables cipher suites that provide
@hyperlink["http://en.wikipedia.org/wiki/Forward_secrecy"]{perfect
forward secrecy} via ephemeral Diffie-Hellman (DHE) or ephemeral
elliptic-curve Diffie-Hellman (ECDHE) key exchange, respectively.

For DHE, the @racket[dh-param-path] must be a path to a PEM file
containing DH parameters.

For ECDHE, the @racket[curve-name] must be one of the following
symbols naming a standard elliptic curve:
@(add-between
  (map (lambda (s) (racket '@#,(racketvalfont (symbol->string s))))
       '(sect163k1 sect163r1 sect163r2 sect193r1 sect193r2 sect233k1 sect233r1
         sect239k1 sect283k1 sect283r1 sect409k1 sect409r1 sect571k1 sect571r1
         secp160k1 secp160r1 secp160r2 secp192k1 secp224k1 secp224r1 secp256k1
         secp384r1 secp521r1 prime192v prime256v))
  ", ").
}

@defthing[ssl-dh4096-param-path path?]{

Path for 4096-bit Diffie-Hellman parameters.
}

@defproc[(ssl-set-server-name-identification-callback!
           [context ssl-server-context?]
           [callback (string? . -> . (or/c ssl-server-context? #f))])
         void?]{

Provides an SSL server context with a procedure it can use for switching
to alternative contexts on a per-connection basis. The procedure is given
the hostname the client was attempting to connect to, to use as the basis
for its decision.

The client sends this information via the TLS
@hyperlink["http://en.wikipedia.org/wiki/Server_Name_Indication"]{Server Name Identification}
extension, which was created to allow @hyperlink["http://en.wikipedia.org/wiki/Virtual_hosting"]{virtual hosting}
for secure servers.

The suggested use it to prepare the appropriate server contexts, 
define a single callback which can dispatch between them, and then
apply it to all the contexts before sealing them. A minimal example:

@racketblock[
  (define ctx-a (ssl-make-server-context 'tls))
  (define ctx-b (ssl-make-server-context 'tls))
  ...
  (ssl-load-certificate-chain! ctx-a "cert-a.pem")
  (ssl-load-certificate-chain! ctx-b "cert-b.pem")
  ...
  (ssl-load-private-key! ctx-a "key-a.pem")
  (ssl-load-private-key! ctx-b "key-b.pem")
  ...
  (define (callback hostname)
    (cond [(equal? hostname "a") ctx-a]
          [(equal? hostname "b") ctx-b]
          ...
          [else #f]))
  (ssl-set-server-name-identification-callback! ctx-a callback)
  (ssl-set-server-name-identification-callback! ctx-b callback)
  ...
  (ssl-seal-context! ctx-a)
  (ssl-seal-context! ctx-b)
  ...
  (ssl-listen 443 5 #t #f ctx-a)
]

If the callback returns @racket[#f], the connection attempt will continue,
using the original server context.

}

@; ----------------------------------------------------------------------
@section[#:tag "peer-verif"]{Peer Verification}

@defproc[(ssl-set-verify! [clp (or/c ssl-client-context? ssl-server-context?
                                     ssl-listener? ssl-port?)] 
                          [on? any/c])
         void?]{

Requires certificate verification on the peer SSL connection when
@racket[on?] is @racket[#t]. If @racket[clp] is an SSL port, then the
connection is immediately renegotiated, and an exception is raised
immediately if certificate verification fails. If @racket[clp] is a
context or listener, certification verification happens on each
subsequent connection using the context or listener.

Enabling verification also requires, at a minimum, designating trusted
certificate authorities with @racket[ssl-load-verify-source!].

Verifying the certificate is not sufficient to prevent attacks by
active adversaries, such as
@hyperlink["http://en.wikipedia.org/wiki/Man-in-the-middle_attack"]{man-in-the-middle
attacks}.  See also @racket[ssl-set-verify-hostname!].
}


@defproc[(ssl-try-verify! [clp (or/c ssl-client-context? ssl-server-context?
                                     ssl-listener? ssl-port?)] 
                          [on? any/c])
         void?]{

Like @racket[ssl-set-verify!], but when peer certificate verification fails,
then connection continues to work. Use @racket[ssl-peer-verified?] to determine
whether verification succeeded.}


@defproc[(ssl-peer-verified? [p ssl-port?]) boolean?]{

Returns @racket[#t] if the peer of SSL port @racket[p] has presented a
valid and verified certificate, @racket[#f] otherwise.}

@defproc[(ssl-set-verify-hostname! [ctx (or/c ssl-client-context? ssl-server-context?)]
                                   [on? any/c])
         void?]{

Requires hostname verification of SSL peers of connections made using
@racket[ctx] when @racket[on?] is @racket[#t]. When hostname
verification is enabled, the hostname associated with a connection
(see @racket[ssl-connect] or @racket[ports->ssl-ports]) is checked
against the hostnames listed in the peer's certificate. If the peer
certificate does not contain an entry matching the hostname, or if the
peer does not present a certificate, the connection is rejected and an
exception is raised.

Hostname verification does not imply certificate verification. To
verify the certificate itself, also call @racket[ssl-set-verify!].
}

@defproc[(ssl-peer-certificate-hostnames [p ssl-port?])
         (listof string?)]{

Returns the list of hostnames for which the certificate of
@racket[p]'s peer is valid according to
@hyperlink["http://www.ietf.org/rfc/rfc2818.txt"]{RFC 2818}. If the
peer has not presented a certificate, @racket['()] is returned.

The result list may contain both hostnames such as
@racket["www.racket-lang.org"] and hostname patterns such as
@racket["*.racket-lang.org"].
}

@defproc[(ssl-peer-check-hostname [p ssl-port?] [hostname string?])
         boolean?]{

Returns @racket[#t] if the peer certificate of @racket[p] is valid for
@racket[hostname] according to
@hyperlink["http://www.ietf.org/rfc/rfc2818.txt"]{RFC 2818}.
}

@defproc[(ssl-peer-subject-name [p ssl-port?]) (or/c bytes? #f)]{

If @racket[ssl-peer-verified?] would return @racket[#t] for
@racket[p], the result is a byte string for the subject field of
the certificate presented by the SSL port's peer, otherwise the result
is @racket[#f].

Use @racket[ssl-peer-check-hostname] or
@racket[ssl-peer-certificate-hostnames] instead to check the validity
of an SSL connection.
}

@defproc[(ssl-peer-issuer-name [p ssl-port?]) (or/c bytes? #f)]{

If @racket[ssl-peer-verified?] would return @racket[#t] for
@racket[p], the result is a byte string for the issuer field of
the certificate presented by the SSL port's peer, otherwise the result
is @racket[#f].}

@; ----------------------------------------------------------------------

@section{SHA-1 Hashing}

@defmodule[openssl/sha1]{The @racketmodname[openssl/sha1] library
provides a Racket wrapper for the OpenSSL library's SHA-1 hashing
functions. If the OpenSSL library cannot be opened, this library logs
a warning and falls back to the implementation in
@racketmodname[file/sha1].}

@defproc[(sha1 [in input-port?]) string?]{

Returns a 40-character string that represents the SHA-1 hash (in
hexadecimal notation) of the content from @racket[in], consuming all
of the input from @racket[in] until an end-of-file.

The @racket[sha1] function composes @racket[bytes->hex-string] with
@racket[sha1-bytes].}

@defproc[(sha1-bytes [in input-port?]) bytes?]{

Returns a 20-byte byte string that represents the SHA-1 hash of the
content from @racket[in], consuming all of the input from @racket[in]
until an end-of-file.}

@defproc[(bytes->hex-string [bstr bytes?]) string?]{

Converts the given byte string to a string representation, where each
byte in @racket[bstr] is converted to its two-digit hexadecimal
representation in the resulting string.}

@defproc[(hex-string->bytes [str string?]) bytes?]{

The inverse of @racket[bytes->hex-string].}

@; ----------------------------------------------------------------------

@section{MD5 Hashing}

@defmodule[openssl/md5]{The @racketmodname[openssl/md5] library
provides a Racket wrapper for the OpenSSL library's MD5 hashing
functions. If the OpenSSL library cannot be opened, this library logs
a warning and falls back to the implementation in
@racketmodname[file/md5].}

@history[#:added "6.0.0.3"]

@defproc[(md5 [in input-port?]) string?]{

Returns a 32-character string that represents the MD5 hash (in
hexadecimal notation) of the content from @racket[in], consuming all
of the input from @racket[in] until an end-of-file.

The @racket[md5] function composes @racket[bytes->hex-string] with
@racket[md5-bytes].}

@defproc[(md5-bytes [in input-port?]) bytes?]{

Returns a 16-byte byte string that represents the MD5 hash of the
content from @racket[in], consuming all of the input from @racket[in]
until an end-of-file.}
