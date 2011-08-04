#lang scribble/doc
@(require scribble/manual
          scribble/bnf
          (for-label openssl
                     scheme
                     openssl/sha1))

@title{OpenSSL: Secure Communication}

@defmodule[openssl]

The @racketmodname[openssl] library provides glue for the OpenSSL
library with the Racket port system. It provides functions nearly
identically to the standard TCP subsystem in Racket, plus a
generic @racket[ports->ssl-ports] interface.

To use this library, you will need OpenSSL installed on your machine,
but

@itemize[
  @item{for Windows, the Racket distribution for Windows includes
  the necessary DLLs.}

  @item{for Mac OS X, version 10.2 and later provides the necessary
  OpenSSL libraries.}

  @item{for Unix, @filepath{libssl.so} and @filepath{libcrypto.so} are
  likely to be installed on your machine, already.}

]


@defthing[ssl-available? boolean?]{

A boolean value which says whether the system openssl library was
successfully loaded. Calling @racket[ssl-connect], @|etc| when this
value is @racket[#f] (library not loaded) will raise an exception.}


@defthing[ssl-load-fail-reason (or/c false/c string?)]{

Either @racket[#f] (when @racket[ssl-available?] is @racket[#t]) or an
error string (when @racket[ssl-available?] is @racket[#f]).}

@; ----------------------------------------------------------------------

@section{TCP-like Client Procedures}

@defproc[(ssl-connect (hostname string?)
                      (port-no (integer-in 1 65535))
                      (client-protocol
                       (or/c ssl-client-context? symbol?) 'sslv2-or-v3))
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
or @racket['tls]; see @racket[ssl-make-client-context] for further
details (including the meanings of the protocol symbols).

Closing the resulting output port does not send a shutdown message to
the server. See also @racket[ports->ssl-ports].

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
          (hostname string?)
	  (port-no (integer-in 1 65535))
	  (client-protocol
	   (or/c ssl-client-context? symbol?) 'sslv2-or-v3))
         (values input-port? output-port?)]{

Like @racket[ssl-connect], but breaking is enabled while trying to
connect.}


@defproc[(ssl-make-client-context (protocol symbol? 'sslv2-or-v3))
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
]

Note that SSL protocol version 2 is deprecated on some platforms and may not be
present in your system libraries. The use of SSLv2 may also compromise security; 
thus, using SSLv3 is recommended.

By default, the context returned by @racket[ssl-make-client-context] does not
request verification of a server's certificate. Use @racket[ssl-set-verify!]
to enable such verification.}


@defproc[(ssl-client-context? (v any/c)) boolean?]{

Returns @racket[#t] if @racket[v] is a value produced by
@racket[ssl-make-client-context], @racket[#f] otherwise.}


@; ----------------------------------------------------------------------

@section{TCP-like Server Procedures}

@defproc[(ssl-listen
	  (port-no (integer-in 1 65535))
	  [queue-k exact-nonnegative-integer?]
	  [reuse? any/c #f]
	  [hostname-or-#f (or/c string? false/c) #f]
	  [server-protocol
	   (or/c ssl-server-context? symbol?) 'sslv2-or-v3])
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
  @defproc[(ssl-close (listener ssl-listener?)) void?]
  @defproc[(ssl-listener? (v any/c)) boolean?])]{

Analogous to @racket[tcp-close] and @racket[tcp-listener?].}

@deftogether[(
  @defproc[(ssl-accept (listener ssl-listener?))
           (values input-port? output-port?)]
  @defproc[(ssl-accept/enable-break (listener ssl-listener?))
           (values input-port? output-port?)])]{

Analogous to @racket[tcp-accept].

Closing the resulting output port does not send a shutdown message to
the client. See also @racket[ports->ssl-ports].

See also @racket[ssl-connect] about the limitations of reading and
writing to an SSL connection (i.e., one direction at a time).

The @racket[ssl-accept/enable-break] procedure is analogous to
@racket[tcp-accept/enable-break].}


@defproc[(ssl-abandon-port [in (and/c ssl-port? output-port?)]) void?]{

Analogous to @racket[tcp-abandon-port].}


@defproc[(ssl-addresses [p (or/c ssl-port? ssl-listener?)][port-numbers? any/c #f]) void?]{

Analogous to @racket[tcp-addresses].}


@defproc[(ssl-port? [v any/c]) boolean?]{

Returns @racket[#t] of @racket[v] is an SSL port produced by
@racket[ssl-connect], @racket[ssl-connect/enable-break],
@racket[ssl-accept], @racket[ssl-accept/enable-break], or
@racket[ports->ssl-ports].}


@defproc[(ssl-make-server-context (protocol symbol?))
         ssl-server-context?]{

Like @racket[ssl-make-client-context], but creates a server context.}

@defproc[(ssl-server-context? (v any/c)) boolean?]{

Returns @racket[#t] if @racket[v] is a value produced by
@racket[ssl-make-server-context], @racket[#f] otherwise.}


@; ----------------------------------------------------------------------

@section{SSL-wrapper Interface}

@defproc[(ports->ssl-ports
           (input-port input-port?)
	   (output-port output-port?)
           [#:mode mode symbol? 'accept]
	   [#:context context (or/c ssl-client-context? ssl-server-context?)
                      ((if (eq? mode 'accept)
                           ssl-make-server-context 
                           ssl-make-client-context)
                       protocol)]
	   [#:encrypt protocol symbol? 'sslv2-or-v3]
	   [#:close-original? close-original? boolean? #f]
	   [#:shutdown-on-close? shutdown-on-close? boolean? #f]
	   [#:error/ssl error procedure? error])
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
writing to an SSL connection (i.e., one direction at a time).}

@; ----------------------------------------------------------------------

@section[#:tag "cert-procs"]{Context Procedures}

@defproc[(ssl-load-certificate-chain!
           (context-or-listener (or/c ssl-client-context? ssl-server-context?
				      ssl-listener?))
	   (pathname path-string?))
         void?]{

Loads a PEM-format certification chain file for connections to made
with the given context (created by @racket[ssl-make-client-context] or
@racket[ssl-make-server-context]) or listener (created by
@racket[ssl-listen]).

This chain is used to identify the client or server when it connects
or accepts connections. Loading a chain overwrites the old chain. Also
call @racket[ssl-load-private-key!] to load the certificate's
corresponding key.

You can use the file @filepath{test.pem} of the @filepath{openssl}
collection for testing purposes. Since @filepath{test.pem} is public,
such a test configuration obviously provides no security.}

@defproc[(ssl-load-private-key!
	  (context-or-listener (or/c ssl-client-context? ssl-server-context?
				     ssl-listener?))
	  (pathname path-string?)
	  [rsa? boolean? #t]
	  [asn1? boolean? #f])
         void?]{

Loads the first private key from @racket[pathname] for the given
context or listener. The key goes with the certificate that identifies
the client or server.

If @racket[rsa?] is @racket[#t] (the default), the first RSA key is
read (i.e., non-RSA keys are skipped). If @racket[asn1?] is
@racket[#t], the file is parsed as ASN1 format instead of PEM.

You can use the file @filepath{test.pem} of the @filepath{openssl}
collection for testing purposes. Since @filepath{test.pem} is public,
such a test configuration obviously provides no security.}

@defproc[(ssl-set-verify!
	  (context-or-listener (or/c ssl-client-context? ssl-server-context?
				      ssl-listener?))
	  (verify? boolean?))
         void?]{

Enables or disables verification of a connection peer's certificates.
By default, verification is disabled.

Enabling verification also requires, at a minimum, designating trusted
certificate authorities with
@racket[ssl-load-verify-root-certificates!].}

@defproc[(ssl-load-verify-root-certificates!
	  (context-or-listener (or/c ssl-client-context? ssl-server-context?
				      ssl-listener?))
	  (pathname path-string?))
         void?]{

Loads a PEM-format file containing trusted certificates that are used
to verify the certificates of a connection peer. Call this procedure
multiple times to load multiple sets of trusted certificates.

You can use the file @filepath{test.pem} of the @filepath{openssl}
collection for testing purposes. Since @filepath{test.pem} is public,
such a test configuration obviously provides no security.}

@defproc[(ssl-load-suggested-certificate-authorities!
	  (context-or-listener (or/c ssl-client-context? ssl-server-context?
				      ssl-listener?))
	  (pathname path-string?))
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

@; ----------------------------------------------------------------------
@section[#:tag "peer-verif"]{Peer Verification}

@defproc[(ssl-peer-verified? [p ssl-port?]) boolean?]{

Returns @racket[#t] if the peer of SSL port @racket[p] has presented a
valid and verified certificate, @racket[#f] otherwise.}

@defproc[(ssl-peer-subject-name [p ssl-port?]) (or/c bytes? #f)]{

If @racket[ssl-peer-verified?] would return @racket[#t] for
@racket[p], the result is a byte string for the subject field of
the certificate presented by the SSL port's peer, otherwise the result
is @racket[#f].}

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

@defproc[(sha1 [in input-port]) string?]{

Returns a 40-character string that represents the SHA-1 hash (in
hexadecimal notation) of the content from @racket[in], consuming all
of the input from @racket[in] until an end-of-file.

The @racket[sha1] function composes @racket[bytes->hex-string] with
@racket[sha1-bytes].}

@defproc[(sha1-bytes [in input-port]) bytes?]{

Returns a 20-byte byte string that represents the SHA-1 hash of the
content from @racket[in], consuming all of the input from @racket[in]
until an end-of-file.}

@defproc[(bytes->hex-string [bstr bytes?]) string?]{

Converts the given byte string to a string representation, where each
byte in @racket[bstr] is converted to its two-digit hexadecimal
representation in the resulting string.}

@; ----------------------------------------------------------------------

@section{Implementation Notes}

For Windows, @racketmodname[openssl] relies on @filepath{libeay32.dll}
and @filepath{ssleay32.dll}, where the DLLs are located in the same
place as @filepath{libmzsch@nonterm{vers}.dll} (where @nonterm{vers}
is either @tt{xxxxxxx} or a mangling of Racket's version
number). The DLLs are distributed as part of Racket.

For Unix variants, @racketmodname[openssl] relies on
@filepath{libcrypto.so} and @filepath{libssl.so}, which must be
installed in a standard library location, or in a directory listed by
@envvar{LD_LIBRARY_PATH}.

For Mac OS X, @racketmodname[openssl] relies on
@filepath{libssl.dylib} and @filepath{libcrypto.dylib}, which are part
of the OS distribution for Mac OS X 10.2 and later.
