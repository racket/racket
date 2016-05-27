#lang scribble/doc
@(require "common.rkt"
          (for-label net/smtp net/smtp-unit net/smtp-sig racket/tcp openssl))

@title[#:tag "smtp"]{SMTP: Sending E-Mail}

@defmodule[net/smtp]{The @racketmodname[net/smtp] module provides
tools for sending electronic mail messages using SMTP. The client must
provide the address of an SMTP server; in contrast, the
@racketmodname[net/sendmail] module uses a pre-configured
@exec{sendmail} on the local system.}

The @racketmodname[net/head] library defines the format of a
@tech{header} string, which is used by @racket[send-smtp-message]. The
@racketmodname[net/head] module also provides utilities to verify the
formatting of a mail address. The procedures of the @racket[net/smtp]
module assume that the given string arguments are well-formed.


@section{SMTP Functions}

@defproc[(smtp-send-message [server-address string?]
                            [from string?]
                            [to (listof string?)]
                            [header string?]
                            [message (listof (or/c string? bytes?))]
                            [#:port-no port-no/k (integer-in 0 65535) 25]
                            [#:auth-user user (or/c string? false/c) #f]
                            [#:auth-passwd pw (or/c string? false/c) #f]
                            [#:tcp-connect connect
                                           ((string? (integer-in 0 65535))
                                            . ->* . (input-port? output-port?))
                                           tcp-connect]
                            [#:tls-encode encode
                                          (or/c false/c
                                                ((input-port? output-port?
                                                  #:mode (one-of/c 'connect)
                                                  #:encrypt (one-of/c 'tls)
                                                  #:close-original? (one-of/c #t))
                                                 . ->* . (input-port? output-port?)))
                                          #f]
                            [port-no (integer-in 0 65535) port-no/k])
         void?]{

Connects to the server at @racket[server-address] and @racket[port-no]
to send a message. The @racket[from] argument specifies the mail
address of the sender, and @racket[to] is a list of recipient
addresses (including ``To:'', ``CC'', and ``BCC'' recipients).

The @racket[header] argument is the complete message header, which
should already include ``From:'', ``To:'', and ``CC:'' fields
consistent with the given sender and recipients. See also the
@racketmodname[net/head] library for header-creating utilities.

The @racket[message] argument is the body of the message, where each
string or byte string in the list corresponds to a single line of
message text. No string in @racket[message] should contain a carriage
return or linefeed character.

The optional @racket[port-no] argument---which can be specified either
with the @racket[#:port-no] keyword or, for backward compatibility, as
an extra argument after keywords---specifies the IP port to use in
contacting the SMTP server.

The optional @racket[#:auth-user] and @racket[#:auth-passwd] keyword
argument supply a username and password for authenticated SMTP (using
the AUTH PLAIN protocol).

The optional @racket[#:tcp-connect] keyword argument supplies a
connection procedure to be used in place of @racket[tcp-connect]. For
example, use @racket[ssl-connect] to connect to the server via SSL.

If the optional @racket[#:tls-encode] keyword argument supplies a
procedure instead of #f, then the ESMTP STARTTLS protocol is used to
initiate SSL communication with the server. The procedure given as the
#:tls-encode argument should be like @racket[ports->ssl-ports]; it
will be called as

@racketblock[
(encode r w #:mode 'connect #:encrypt 'tls #:close-original? #t)
]

and it should return two values: an input port and an export port.
All further SMTP communication uses the returned ports.

For encrypted communication, normally either @racket[ssl-connect]
should be supplied for @racket[#:tcp-connect], or
@racket[ports->ssl-ports] should be supplied for
@racket[#:tls-encode]---one or the other (depending on what the server
expects), rather than both.}

@defparam[smtp-sending-end-of-message proc (-> any)]{

A parameter that determines a send-done procedure to be called after
@racket[smtp-send-message] has completely sent the message. Before the
send-done procedure is called, breaking the thread that is executing
@racket[smtp-send-message] cancels the send. After the send-done
procedure is called, breaking may or may not cancel the send (and
probably will not).}

@; ----------------------------------------

@section{SMTP Unit}

@margin-note{@racket[smtp@] and @racket[smtp^] are deprecated.
They exist for backward-compatibility and will likely be removed in
the future. New code should use the @racketmodname[net/smtp] module.}

@defmodule[net/smtp-unit]

@defthing[smtp@ unit?]{

Imports nothing, exports @racket[smtp^].}

@; ----------------------------------------

@section{SMTP Signature}

@defmodule[net/smtp-sig]

@defsignature[smtp^ ()]{}

Includes everything exported by the @racketmodname[net/smtp] module.
