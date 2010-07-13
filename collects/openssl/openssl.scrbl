#lang scribble/doc
@(require scribble/manual
          scribble/bnf
          (for-label openssl
		     scheme
                     openssl/sha1))

@title{@bold{OpenSSL}}

@defmodule[openssl]

The @schememodname[openssl] library provides glue for the OpenSSL
library with the Racket port system. It provides functions nearly
identically to the standard TCP subsystem in Racket, plus a
generic @scheme[ports->ssl-ports] interface.

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
successfully loaded. Calling @scheme[ssl-connect], @|etc| when this
value is @scheme[#f] (library not loaded) will raise an exception.}


@defthing[ssl-load-fail-reason (or/c false/c string?)]{

Either @scheme[#f] (when @scheme[ssl-available?] is @scheme[#t]) or an
error string (when @scheme[ssl-available?] is @scheme[#f]).}

@; ----------------------------------------------------------------------

@section{TCP-like Client Procedures}

@defproc[(ssl-connect (hostname string?)
                      (port-no (integer-in 1 65535))
                      (client-protocol
                       (or/c ssl-client-context? symbol?) 'sslv2-or-v3))
         (values input-port? output-port?)]{

Connect to the host given by @scheme[hostname], on the port given by
@scheme[port-no]. This connection will be encrypted using SSL.  The
return values are as for @scheme[tcp-connect]: an input port and an
output port.

The optional @scheme[client-protocol] argument determines which
encryption protocol is used, whether the server's certificate is
checked, etc. The argument can be either a client context created by
@scheme[ssl-make-client-context], or one of the following symbols:
@scheme['sslv2-or-v3] (the default), @scheme['sslv2], @scheme['sslv3],
or @scheme['tls]; see @scheme[ssl-make-client-context] for further
details (including the meanings of the protocol symbols).

Closing the resulting output port does not send a shutdown message to
the server. See also @scheme[ports->ssl-ports].

@;{
See `enforce-retry?' in "mzssl.ss", currently set to #f so that this
paragraph does not apply:
Beware that the SSL protocol allows reading or writing in only one
direction at a time. If you request data from the input port, then
data cannot be written to the output port (i.e., attempting to write
will block) until the other end of the connection responds to the
read. Even merely checking for input data --- using
@scheme[byte-ready?], for example --- commits the connection to
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

Like @scheme[ssl-connect], but breaking is enabled while trying to
connect.}


@defproc[(ssl-make-client-context (protocol symbol? 'sslv2-or-v3))
         ssl-client-context?]{

Creates a context to be supplied to @scheme[ssl-connect]. The context
identifies a communication protocol (as selected by
@scheme[protocol]), and also holds certificate information (i.e., the
client's identity, its trusted certificate authorities, etc.). See the
section @secref["cert-procs"] below for more information on
certificates.

The @scheme[protocol] must be one of the following:
@itemize[
  @item{@scheme['sslv2-or-v3] : SSL protocol versions 2 or 3, as
  appropriate (this is the default)}
  @item{@scheme['sslv2] : SSL protocol version 2}
  @item{@scheme['sslv3] : SSL protocol version 3}
  @item{@scheme['tls] : the TLS protocol version 1}
]

By default, the context returned by @scheme[ssl-make-client-context] does not
request verification of a server's certificate. Use @scheme[ssl-set-verify!]
to enable such verification.}


@defproc[(ssl-client-context? (v any/c)) boolean?]{

Returns @scheme[#t] if @scheme[v] is a value produced by
@scheme[ssl-make-client-context], @scheme[#f] otherwise.}


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

Like @scheme[tcp-listen], but the result is an SSL listener (which is
a synchronizable value; see @scheme[sync]). The extra optional
@scheme[server-protocol] is as for @scheme[ssl-connect], except that a
context must be a server context instead of a client context.

Call @scheme[ssl-load-certificate-chain!] and
@scheme[ssl-load-private-key!] to avoid a @emph{no shared cipher}
error on accepting connections. The file @filepath{test.pem} in the
@filepath{openssl} collection is a suitable argument for both calls
when testing. Since @filepath{test.pem} is public, however, such a
test configuration obviously provides no security.}

@deftogether[(
  @defproc[(ssl-close (listener ssl-listener?)) void?]
  @defproc[(ssl-listener? (v any/c)) boolean?])]{

Analogous to @scheme[tcp-close] and @scheme[tcp-listener?].}

@deftogether[(
  @defproc[(ssl-accept (listener ssl-listener?))
           (values input-port? output-port?)]
  @defproc[(ssl-accept/enable-break (listener ssl-listener?))
           (values input-port? output-port?)])]{

Analogous to @scheme[tcp-accept].

Closing the resulting output port does not send a shutdown message to
the client. See also @scheme[ports->ssl-ports].

See also @scheme[ssl-connect] about the limitations of reading and
writing to an SSL connection (i.e., one direction at a time).

The @scheme[ssl-accept/enable-break] procedure is analogous to
@scheme[tcp-accept/enable-break].}


@defproc[(ssl-make-server-context (protocol symbol?))
         ssl-server-context?]{

Like @scheme[ssl-make-client-context], but creates a server context.}

@defproc[(ssl-server-context? (v any/c)) boolean?]{

Returns @scheme[#t] if @scheme[v] is a value produced by
@scheme[ssl-make-server-context], @scheme[#f] otherwise.}

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

The @scheme[mode] argument can be @scheme['connect] or
@scheme['accept]. The mode determines how the SSL protocol is
initialized over the ports, either as a client or as a server. As with
@scheme[ssl-listen], in @scheme['accept] mode, supply a
@scheme[context] that has been initialized with
@scheme[ssl-load-certificate-chain!] and
@scheme[ssl-load-private-key!] to avoid a @emph{no shared cipher}
error.

The @scheme[context] argument should be a client context for
@scheme['connect] mode or a server context for @scheme['accept]
mode. If it is not supplied, a context is created using the protocol
specified by a @scheme[protocol] argument.

If the @scheme[protocol] argument is not supplied, it defaults to
@scheme['sslv2-or-v3]. See @scheme[ssl-make-client-context] for
further details (including all options and the meanings of the
protocol symbols).  This argument is ignored if a @scheme[context]
argument is supplied.

If @scheme[close-original?] is true, then when both SSL ports are
closed, the given input and output ports are automatically closed.

If @scheme[shutdown-on-close?] is true, then when the output SSL port
is closed, it sends a shutdown message to the other end of the SSL
connection. When shutdown is enabled, closing the
output port can fail if the given output port becomes unwritable
(e.g., because the other end of the given port has been closed by
another process).

The @scheme[error] argument is an error procedure to use for raising
communication errors. The default is @scheme[error], which raises
@scheme[exn:fail]; in contrast, @scheme[ssl-accept] and
@scheme[ssl-connect] use an error function that raises
@scheme[exn:fail:network].

See also @scheme[ssl-connect] about the limitations of reading and
writing to an SSL connection (i.e., one direction at a time).}

@; ----------------------------------------------------------------------

@section[#:tag "cert-procs"]{Context Procedures}

@defproc[(ssl-load-certificate-chain!
           (context-or-listener (or/c ssl-client-context? ssl-server-context?
				      ssl-listener?))
	   (pathname path-string?))
         void?]{

Loads a PEM-format certification chain file for connections to made
with the given context (created by @scheme[ssl-make-client-context] or
@scheme[ssl-make-server-context]) or listener (created by
@scheme[ssl-listen]).

This chain is used to identify the client or server when it connects
or accepts connections. Loading a chain overwrites the old chain. Also
call @scheme[ssl-load-private-key!] to load the certificate's
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

Loads the first private key from @scheme[pathname] for the given
context or listener. The key goes with the certificate that identifies
the client or server.

If @scheme[rsa?] is @scheme[#t] (the default), the first RSA key is
read (i.e., non-RSA keys are skipped). If @scheme[asn1?] is
@scheme[#t], the file is parsed as ASN1 format instead of PEM.

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
@scheme[ssl-load-verify-root-certificates!].}

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
roots loaded by @scheme[ssl-load-verify-root-certificates!].

You can use the file @filepath{test.pem} of the @filepath{openssl}
collection for testing purposes where the peer identifies itself using
@filepath{test.pem}.}

@; ----------------------------------------------------------------------

@section{SHA-1 Hashing}

@defmodule[openssl/sha1]{The @schememodname[openssl/sha1] library
provides a Racket wrapper for the OpenSSL library's SHA-1 hashing
functions. If the OpenSSL library cannot be opened, this library logs
a warning and falls back to the implementation in
@racketmodname[file/sha1].}

@defproc[(sha1 [in input-port]) string?]{

Returns a 40-character string that represents the SHA-1 hash (in
hexadecimal notation) of the content from @scheme[in], consuming all
of the input from @scheme[in] until an end-of-file.

The @scheme[sha1] function composes @scheme[bytes->hex-string] with
@racket[sha1-bytes].}

@defproc[(sha1-bytes [in input-port]) bytes?]{

Returns a 20-byte byte string that represents the SHA-1 hash of the
content from @scheme[in], consuming all of the input from @scheme[in]
until an end-of-file.}

@defproc[(bytes->hex-string [bstr bytes?]) string?]{

Converts the given byte string to a string representation, where each
byte in @scheme[bstr] is converted to its two-digit hexadecimal
representation in the resulting string.}

@; ----------------------------------------------------------------------

@section{Implementation Notes}

For Windows, @schememodname[openssl] relies on @filepath{libeay32.dll}
and @filepath{ssleay32.dll}, where the DLLs are located in the same
place as @filepath{libmzsch@nonterm{vers}.dll} (where @nonterm{vers}
is either @tt{xxxxxxx} or a mangling of Racket's version
number). The DLLs are distributed as part of Racket.

For Unix variants, @schememodname[openssl] relies on
@filepath{libcryto.so} and @filepath{libssl.so}, which must be
installed in a standard library location, or in a directory listed by
@envvar{LD_LIBRARY_PATH}.

For Mac OS X, @schememodname[openssl] relies on
@filepath{libssl.dylib} and @filepath{libcryto.dylib}, which are part
of the OS distribution for Mac OS X 10.2 and later.
