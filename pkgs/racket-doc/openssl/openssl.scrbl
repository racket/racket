#lang scribble/doc
@(require scribble/manual
          racket/list
          (for-label openssl
                     (except-in racket sha1-bytes)
                     openssl/sha1
                     openssl/md5
                     openssl/libcrypto
                     openssl/libssl
                     (only-in ffi/unsafe ffi-lib ffi-lib?)))

@(define-syntax-rule (define-racket/base sha1-bytes-id)
   (begin
     (require (for-label (only-in racket/base sha1-bytes)))
     (define sha1-bytes-id @racket[sha1-bytes])))
@(define-racket/base racket:sha1-bytes)

@(define alpn-url
   "https://en.wikipedia.org/wiki/Application-Layer_Protocol_Negotiation")

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

@item{For Mac OS, @racketmodname[openssl] depends on
@filepath{libssl.dylib} and @filepath{libcrypto.dylib}. Although those
libraries are provided by Mac OS 10.2 and later, their use is
deprecated, so the Racket distribution for Mac OS includes newer
versions.}

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
                       (or/c ssl-client-context? ssl-protocol-symbol/c)
                       'auto]
                      [#:alpn alpn-protocols (listof bytes?) null])
         (values input-port? output-port?)]{

Connect to the host given by @racket[hostname], on the port given by
@racket[port-no]. This connection will be encrypted using SSL.  The
return values are as for @racket[tcp-connect]: an input port and an
output port.

The default @racket['auto] protocol is @bold{insecure}. Use
@racket['secure] for a secure connection. See
@racket[ssl-secure-client-context] for details.

The optional @racket[client-protocol] argument determines which
encryption protocol is used, whether the server's certificate is
checked, etc. The argument can be either a client context created by
@racket[ssl-make-client-context] a symbol specifying the protocol to
use; see @racket[ssl-make-client-context] for further details,
including the meanings of the protocol symbols.

Closing the resulting output port does not send a shutdown message to
the server. See also @racket[ports->ssl-ports].

If hostname verification is enabled (see
@racket[ssl-set-verify-hostname!]), the peer's certificate is checked
against @racket[hostname].

If @racket[alpn-protocols] is not empty, the client attempts to use
@hyperlink[alpn-url]{ALPN} to negotiate the application-level
protocol. The protocols should be listed in order of preference, and
each protocol must be a byte string with a length between 1 and 255
(inclusive). See also @racket[ssl-get-alpn-selected].

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

@history[#:changed "6.3.0.12" @elem{Added @racket['secure] for
                                    @racket[client-protocol].}
         #:changed "8.0.0.13" @elem{Added @racket[#:alpn] argument.}]}

@defproc[(ssl-connect/enable-break
          [hostname string?]
	  [port-no (integer-in 1 65535)]
	  [client-protocol
	   (or/c ssl-client-context? ssl-protocol-symbol/c)
           'auto])
         (values input-port? output-port?)]{

Like @racket[ssl-connect], but breaking is enabled while trying to
connect.}


@defproc[(ssl-secure-client-context)
         ssl-client-context?]{

Returns a client context that verifies certificates using the default
verification sources from @racket[(ssl-default-verify-sources)],
verifies hostnames, and avoids using weak ciphers. The result is
essentially equivalent to the following:

@racketblock[
(let ([ctx (ssl-make-client-context 'auto)])
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

Note that @racket[(ssl-secure-client-context)] returns a sealed
context, so it is not possible to add a private key and certificate
chain to it. If client credentials are required, use
@racket[ssl-make-client-context] instead.
}


@defproc[(ssl-make-client-context
          [protocol ssl-protocol-symbol/c 'auto]
          [#:private-key private-key
                         (or/c (list/c 'pem path-string?)
                               (list/c 'der path-string?)
                               #f)
                         #f]
          [#:certificate-chain certificate-chain (or/c path-string? #f) #f])
         ssl-client-context?]{

Creates a context to be supplied to @racket[ssl-connect]. The context
is @bold{insecure} unless @racket['secure] is supplied or additional steps are taken; see
@racket[ssl-secure-client-context] for details.

The client context identifies a communication protocol (as selected by
@racket[protocol]), and also holds certificate information (i.e., the
client's identity, its trusted certificate authorities, etc.). See the
section @secref["cert-procs"] below for more information on
certificates.

The @racket[protocol] should be one of the following:
@itemize[
@item{@racket['secure] : Equivalent to @racket[(ssl-secure-client-context)].}
@item{@racket['auto] : Automatically negotiates the protocol version
from those that this library considers sufficiently secure---currently
TLS versions 1.0 and higher, but subject to change.}
@item{@racket['tls12] : Only TLS protocol version 1.2.}
@item{@racket['tls13] : Only TLS protocol version 1.3.}
]
The following @racket[protocol] symbols are deprecated but still supported:
@itemlist[
@item{@racket['sslv2-or-v3] : Alias for @racket['auto]. Note that
despite the name, neither SSL 2.0 nor 3.0 are considered sufficiently
secure, so this @racket[protocol] no longer allows either of them.}
@item{@racket['sslv2] : SSL protocol version 2.0. @bold{Deprecated by RFC 6176 (2011).}
Note that SSL 2.0 support has been removed from many platforms.}
@item{@racket['sslv3] : SSL protocol version 3.0. @bold{Deprecated by RFC 7568 (2015).}}
@item{@racket['tls] : Only TLS protocol version 1.0. @bold{Deprecated by RFC 8996 (2021).}}
@item{@racket['tls11] : Only TLS protocol version 1.1. @bold{Deprecated by RFC 8996 (2021).}}
]

Not all protocol versions are supported by all servers. The
@racket['secure] and @racket['auto] options offer broad compatibility at a reasonable level
of security. Note that the security of connections depends on more
than the protocol version; see @racket[ssl-secure-client-context] for
details. See also
@racket[supported-client-protocols] and
@racket[supported-server-protocols].

If @racket[private-key] and @racket[certificate-chain] are provided,
they are loaded into the context using @racket[ssl-load-private-key!]
and @racket[ssl-load-certificate-chain!], respectively. Client
credentials are rarely used with HTTPS, but they are occasionally used
in other kind of servers.

@history[
#:changed "6.1" @elem{Added @racket['tls11] and @racket['tls12].}
#:changed "6.1.1.3" @elem{Default to new @racket['auto] and disabled SSL
                          2.0 and 3.0 by default.}
#:changed "6.3.0.12" @elem{Added @racket['secure].}
#:changed "7.3.0.10" @elem{Added @racket[#:private-key] and @racket[#:certificate-chain]
arguments.}
]}

@defthing[ssl-protocol-symbol/c contract?
          #:value (or/c 'secure 'auto 'sslv2-or-v3
                        'sslv2 'sslv3 'tls 'tls11 'tls12 'tls13)]{

Contract for symbols representing SSL/TLS protocol versions. See
@racket[ssl-make-client-context] for an explanation of how the symbols are
interpreted.

@history[
#:added "8.6.0.4"
#:changed "8.6.0.4" @elem{Added @racket['tls13].}
]}

@defproc[(supported-client-protocols)
         (listof ssl-protocol-symbol/c)]{

Returns a list of symbols representing protocols that are supported
for clients on the current platform.

@history[#:changed "6.3.0.12" @elem{Added @racket['secure].}]}


@defproc[(ssl-client-context? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a value produced by
@racket[ssl-make-client-context], @racket[#f] otherwise.

@history[#:added "6.0.1.3"]}

@defproc[(ssl-max-client-protocol) (or/c ssl-protocol-symbol/c #f)]{

Returns the most recent SSL/TLS protocol version supported by the
current platform for client connections.

@history[#:added "6.1.1.3"]
}

@defproc[(ssl-protocol-version [p ssl-port?])
         ssl-protocol-symbol/c]{

Returns a symbol representing the SSL/TLS protocol version negotiated for the
connection represented by @racket[p].

@history[#:added "8.6.0.4"]}

@; ----------------------------------------------------------------------

@section{TCP-like Server Procedures}

@defproc[(ssl-listen
	  [port-no listen-port-number?]
	  [queue-k exact-nonnegative-integer? 5]
	  [reuse? any/c #f]
	  [hostname-or-#f (or/c string? #f) #f]
	  [server-protocol
	   (or/c ssl-server-context?  ssl-protocol-symbol/c)
           'auto])
	 ssl-listener?]{

Like @racket[tcp-listen], but the result is an SSL listener. The extra optional
@racket[server-protocol] is as for @racket[ssl-connect], except that a
context must be a server context instead of a client context, and
@racket['secure] is simply an alias for @racket['auto].

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
further communication is needed to establish the connection.

@history[#:changed "6.3.0.12" @elem{Added @racket['secure].}]}


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
         (or/c (values string? string?)
               (values string? port-number? string? listen-port-number?))]{

Analogous to @racket[tcp-addresses].}


@defproc[(ssl-port? [v any/c]) boolean?]{

Returns @racket[#t] of @racket[v] is an SSL port produced by
@racket[ssl-connect], @racket[ssl-connect/enable-break],
@racket[ssl-accept], @racket[ssl-accept/enable-break], or
@racket[ports->ssl-ports].}


@defproc[(ssl-make-server-context
          [protocol ssl-protocol-symbol/c 'auto]
          [#:private-key private-key
                         (or/c (list/c 'pem path-string?)
                               (list/c 'der path-string?)
                               #f)
                         #f]
          [#:certificate-chain certificate-chain (or/c path-string? #f) #f])
         ssl-server-context?]{

Like @racket[ssl-make-client-context], but creates a server context.
For a server context, the @racket['secure] protocol is the same as
@racket['auto].

If @racket[private-key] and @racket[certificate-chain] are provided,
they are loaded into the context using @racket[ssl-load-private-key!]
and @racket[ssl-load-certificate-chain!], respectively.

@history[
#:changed "6.3.0.12" @elem{Added @racket['secure].}
#:changed "7.3.0.10" @elem{Added @racket[#:private-key] and @racket[#:certificate-chain]
arguments.}
]}


@defproc[(ssl-server-context? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a value produced by
@racket[ssl-make-server-context], @racket[#f] otherwise.}

@defproc[(supported-server-protocols)
         (listof ssl-protocol-symbol/c)]{

Returns a list of symbols representing protocols that are supported
for servers on the current platform.

@history[#:added "6.0.1.3"
         #:changed "6.3.0.12" @elem{Added @racket['secure].}]}

@defproc[(ssl-max-server-protocol) (or/c ssl-protocol-symbol/c #f)]{

Returns the most recent SSL/TLS protocol version supported by the
current platform for server connections.

@history[#:added "6.1.1.3"]
}

@; ----------------------------------------------------------------------

@section{SSL-wrapper Interface}

@defproc[(ports->ssl-ports
           [input-port input-port?]
	   [output-port output-port?]
           [#:mode mode (or/c 'connect 'accept) 'accept]
	   [#:context context
                      (or/c ssl-client-context? ssl-server-context?)
                      ((if (eq? mode 'accept)
                           ssl-make-server-context 
                           ssl-make-client-context)
                       protocol)]
	   [#:encrypt protocol ssl-protocol-symbol/c 'auto]
	   [#:close-original? close-original? boolean? #f]
	   [#:shutdown-on-close? shutdown-on-close? boolean? #f]
	   [#:error/ssl error procedure? error]
           [#:hostname hostname (or/c string? #f) #f]
           [#:alpn alpn-protocols (listof bytes?) null])
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
@racket['auto]. See @racket[ssl-make-client-context] for
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

If @racket[alpn-protocols] is not empty and @racket[mode] is
@racket['connect], then the client attempts to use
@hyperlink[alpn-url]{ALPN}; see also @racket[ssl-connect] and
@racket[ssl-get-alpn-selected]. If @racket[alpn-protocols] is not
empty and @racket[mode] is @racket['accept], an exception
(@racket[exn:fail]) is raised; use @racket[ssl-set-server-alpn!] to set
the ALPN protocols for a server context.

@history[#:changed "8.0.0.13" @elem{Added @racket[#:alpn] argument.}]}

@; ----------------------------------------------------------------------

@section[#:tag "cert-procs"]{Context Procedures}

@defproc[(ssl-load-verify-source!
	    [context (or/c ssl-client-context? ssl-server-context?)]
            [src (or/c path-string?
                       (list/c 'directory path-string?)
                       (list/c 'win32-store string?)
                       (list/c 'macosx-keychain (or/c #f path-string?)))]
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

@item{If @racket[src] is @racket[(list 'macosx-keychain #f)], then the
certificates from the Mac OS trust anchor (root) certificates (as
returned by @hyperlink["https://developer.apple.com/documentation/security/1401507-sectrustcopyanchorcertificates"]{@tt{SecTrustCopyAnchorCertificates}})
are loaded immediately. Only supported on Mac OS.}

@item{If @racket[src] is @racket[(list 'macosx-keychain _path)], then
the certificates from the keychain stored at @racket[_path] are loaded
immediately. Only supported on Mac OS.}

]

If @racket[try?] is @racket[#f] and loading @racket[src] fails (for
example, because the file or directory does not exist), then an
exception is raised. If @racket[try?] is a true value, then a load
failure is ignored.

You can use the file @filepath{test.pem} of the @filepath{openssl}
collection for testing purposes. Since @filepath{test.pem} is public,
such a test configuration obviously provides no security.

@history[#:changed "8.4.0.5" @elem{Added @racket[(list 'macosx-keychain #f)]
                             variant.}]}

@defparam[ssl-default-verify-sources srcs
          (let ([source/c (or/c path-string?
                                (list/c 'directory path-string?)
                                (list/c 'win32-store string?)
                                (list/c 'macosx-keychain (or/c #f path-string?)))])
            (listof source/c))]{

Holds a list of verification sources, used by
@racket[ssl-load-default-verify-sources!]. The default sources depend
on the platform:

@itemlist[

@item{On Linux, the default sources are determined by the
@tt{SSL_CERT_FILE} and @tt{SSL_CERT_DIR} environment variables, if the
variables are set, or the system-wide default locations otherwise.}

@item{On Mac OS, the default sources consist of the OS trust anchor
(root) certificates: @racket['(macosx-keychain #f)].}

@item{On Windows, the default sources consist of the system
certificate store for root certificates: @racket['(win32-store
"ROOT")].}

]

@history[#:changed "8.4.0.5" @elem{Changed default source on Mac OS.}]}

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
           [dh-param (or/c path-string? bytes?) ssl-dh4096-param-bytes])
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

For DHE, the @racket[dh-param] must be a path to a @filepath{.pem}
file containing DH parameters or the content of such a file as a byte
string.

For ECDHE, the @racket[curve-name] must be one of the following
symbols naming a standard elliptic curve:
@(add-between
  (map (lambda (s) (racket '@#,(racketvalfont (symbol->string s))))
       '(sect163k1 sect163r1 sect163r2 sect193r1 sect193r2 sect233k1 sect233r1
         sect239k1 sect283k1 sect283r1 sect409k1 sect409r1 sect571k1 sect571r1
         secp160k1 secp160r1 secp160r2 secp192k1 secp224k1 secp224r1 secp256k1
         secp384r1 secp521r1 prime192v prime256v))
  ", ").

@history[#:changed "7.7.0.4" @elem{Allow a byte string as the @racket[dh-param]
                                   argument to @racket[ssl-server-context-enable-dhe!].}]}

@defthing[ssl-dh4096-param-bytes bytes?]{

Byte string describing 4096-bit Diffie-Hellman parameters in @filepath{.pem} format.

@history[#:changed "7.7.0.4" @elem{Added as a replacement for
                                   @racketidfont{ssl-dh4096-param-path}.}]}

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

The suggested use is to prepare the appropriate server contexts,
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

@defproc[(ssl-set-server-alpn! [context ssl-server-context?]
                               [alpn-protocols (listof bytes?)]
                               [allow-no-match? boolean? #t])
         void?]{

Sets the @hyperlink[alpn-url]{ALPN} protocols supported by the server
context. The protocols are listed in order of preference,
most-preferred first. That is, when a client connects, the server
selects the first protocol in its @racket[alpn-protocols] that is
supported by the client. If the client does not use ALPN, then the
connection is accepted and no protocol is selected. If the client uses
ALPN but has no protocols in common with the server, then if
@racket[allow-no-match?] is true, the connection is accepted and no
protocol is selected; if @racket[allow-no-match?]  is false, then the
connection is refused.

@history[#:added "8.4.0.5"]}

@defproc[(ssl-set-keylogger! [context (or/c ssl-server-context? ssl-client-context?)]
                             [logger (or/c #f logger?)]) void?]{

Instructs the @racket[context] to log a message to @racket[logger]
whenever TLS key material is generated or received.  The message is
logged with its level set to @racket['debug], its topic set to
@racket['openssl-keylogger], and its associated data is a byte string
representing the key material.  When @racket[logger] is @racket[#f],
the context is instructed to stop logging this information.

@bold{Warning:} if @racket[logger] has any ancestors, then this
information may also be available to them, depending on the logger's
propagation settings.

Debugging is the typical use case for this functionality.  The owner
of a context can use it to write key material to a file to be consumed
by tools such as Wireshark.  In the following example, anyone with
access to @filepath{keylogfile.txt} is able to decrypt connections made via
@racket[ctx]:

@racketblock[
  (define out
    (open-output-file
     #:exists 'append
     "keylogfile.txt"))
  (define logger
    (make-logger))
  (void
   (thread
    (lambda ()
      (define receiver
        (make-log-receiver logger 'debug 'openssl-keylogger))
      (let loop ()
        (match-define (vector _ _ key-data _)
          (sync receiver))
        (write-bytes key-data out)
        (newline out)
        (flush-output out)
        (loop)))))

  (define ctx (ssl-make-client-context 'auto))
  (ssl-set-keylogger! ctx logger)
]

@history[#:added "8.7.0.8"]}

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

@defproc[(ssl-default-channel-binding [p ssl-port?])
         (list/c symbol? bytes?)]{

Returns the default channel binding type and value for @racket[p], based on the
connection's TLS protocol version. Following
@hyperlink["https://datatracker.ietf.org/doc/html/rfc9266#section-3"]{RFC 9266 Section 3},
the result uses @racket['tls-exporter] for TLS 1.3 and later;
it uses @racket['tls-unique] for TLS 1.2 and earlier.

@history[#:added "8.6.0.4"]}

@defproc[(ssl-channel-binding [p ssl-port?]
                              [type (or/c 'tls-exporter
                                          'tls-unique
                                          'tls-server-end-point)])
         bytes?]{

Returns channel binding information for the TLS connection of
@racket[p]. An authentication protocol run over TLS can incorporate
information identifying the TLS connection (@racket['tls-exporter] or
@racket['tls-unique]) or
server certificate (@racket['tls-server-end-point]) into the
authentication process, thus preventing the authentication steps from
being replayed on another channel. Channel binding is described in
general in @hyperlink["https://tools.ietf.org/html/rfc5056"]{RFC 5056};
channel binding for TLS is described in
@hyperlink["https://tools.ietf.org/html/rfc5929"]{RFC 5929} and
@hyperlink["https://datatracker.ietf.org/doc/html/rfc9266"]{RFC 9266}.

If the channel binding cannot be retrieved (for example, if the
connection is closed), an exception is raised.

@history[
#:added "7.7.0.9"
#:changed "8.6.0.4" @elem{Added @racket['tls-exporter]. An exception is raised
for @racket['tls-unique] with a TLS 1.3 connection.}
]}


@defproc[(ssl-get-alpn-selected [p ssl-port?])
         (or/c bytes? #f)]{

Returns the ALPN protocol selected during negotiation, or @racket[#f]
if no protocol was selected.

If a server does not support any of the protocols proposed by a
client, it might reject the connection or it might accept the
connection without selecting an application protocol. So it is
recommended to always check the selected protocol after making a
connection.

@history[#:added "8.0.0.13"]}

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
until an end-of-file.

The @racket:sha1-bytes function from @racketmodname[racket/base]
computes the same result and is only slightly slower.}

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

@; ----------------------------------------------------------------------

@(define foreign-doc '(lib "scribblings/foreign/foreign.scrbl"))

@section[#:tag "libcrypto"]{The @filepath{libcrypto} Shared Library}

@defmodule[openssl/libcrypto]{The @racketmodname[openssl/libcrypto]
library provides a @tech[#:doc foreign-doc]{foreign-library value} for
the @filepath{libcrypto} shared library.}


@defthing[libcrypto (or/c #f ffi-lib?)]{

Returns a @tech[#:doc foreign-doc]{foreign-library value} for
@filepath{libcrypto}, or @racket[#f] if the library could not be found
or loaded. The load attempt uses the versions specified by
@racket[openssl-lib-versions].}

@defthing[libcrypto-load-fail-reason (or/c #f string?)]{

Either @racket[#f] when @racket[libcrypto] is non-@racket[#f], or a
string when @racket[libcrypto] is @racket[#f]. In the latter case, the
string provides an error message for the attempt to load
@filepath{libcrypto}.}


@defthing[openssl-lib-versions (listof string?)]{

A list of versions that are tried for loading @filepath{libcrypto}.
The list of version strings is suitable as a second argument to
@racket[ffi-lib].}

@; ----------------------------------------------------------------------

@section[#:tag "libssl"]{The @filepath{libssl} Shared Library}

@defmodule[openssl/libssl]{The @racketmodname[openssl/libssl]
library provides a @tech[#:doc foreign-doc]{foreign-library value} for
the @filepath{libssl} shared library.}


@defthing[libssl (or/c #f ffi-lib?)]{

Returns a @tech[#:doc foreign-doc]{foreign-library value} for
@filepath{libssl}, or @racket[#f] if the library could not be found
or loaded. The load attempt uses the versions specified by
@racket[openssl-lib-versions].}

@defthing[libssl-load-fail-reason (or/c #f string?)]{

Either @racket[#f] when @racket[libssl] is non-@racket[#f], or a
string when @racket[libssl] is @racket[#f]. In the latter case, the
string provides an error message for the attempt to load
@filepath{libssl}.}
