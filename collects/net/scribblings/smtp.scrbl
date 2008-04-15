#lang scribble/doc
@(require "common.ss"
          (for-label net/smtp
                     net/smtp-unit
                     net/smtp-sig
                     scheme/tcp
                     openssl))

@title[#:tag "smtp"]{SMTP: Sending E-Mail}

@defmodule[net/smtp]{The @schememodname[net/smtp] module provides
tools for sending electronic mail messages using SMTP. The client must
provide the address of an SMTP server; in contrast, the
@schememodname[net/sendmail] module uses a pre-configured
@exec{sendmail} on the local system.}

The @schememodname[net/head] library defines the format of a
@tech{header} string, which is used by @scheme[send-smtp-message]. The
@schememodname[net/head] module also provides utilities to verify the
formatting of a mail address. The procedures of the @scheme[net/smtp]
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

Connects to the server at @scheme[server-address] and @scheme[port-no]
to send a message. The @scheme[from] argument specifies the mail
address of the sender, and @scheme[to] is a list of recipient
addresses (including ``To:'', ``CC'', and ``BCC'' recipients).

The @scheme[header] argument is the complete message header, which
should already include ``From:'', ``To:'', and ``CC:'' fields
consistent with the given sender and recipients. See also the
@schememodname[net/head] library for header-creating utilities.

The @scheme[message] argument is the body of the message, where each
string or byte string in the list corresponds to a single line of
message text. No string in @scheme[message] should contain a carriage
return or linefeed character.

The optional @scheme[port-no] argument---which can be specified either
with the @scheme[#:port-no] keyword or, for backward compatibility, as
an extra argument after keywords---specifies the IP port to use in
contacting the SMTP server.

The optional @scheme[#:auth-user] and @scheme[#:auth-passwd] keyword
argument supply a username and password for authenticated SMTP (using
the AUTH PLAIN protocol).

The optional @scheme[#:tcp-connect] keyword argument supplies a
connection procedure to be used in place of @scheme[tcp-connect]. For
example, use @scheme[ssl-connect] to connect to the server via SSL.

If the optional @scheme[#:tls-encode] keyword argument supplies a
procedure instead of #f, then the ESMTP STARTTLS protocol is used to
initiate SSL communication with the server. The procedure given as the
#:tls-encode argument should be like @scheme[ports->ssl-ports]; it
will be called as

@schemeblock[
(encode r w #:mode 'connect #:encrypt 'tls #:close-original? #t)
]

and it should return two values: an input port and an export port.
All further SMTP communication uses the returned ports.

For encrypted communication, normally either @scheme[ssl-connect]
should be supplied for @scheme[#:tcp-connect], or
@scheme[ports->ssl-ports] should be supplied for
@scheme[#:tls-encode]---one or the other (depending on what the server
expects), rather than both.}

@defparam[smtp-sending-end-of-message proc (-> any)]{

A parameter that determines a send-done procedure to be called after
@scheme[smtp-send-message] has completely sent the message. Before the
send-done procedure is called, breaking the thread that is executing
@scheme[smtp-send-message] cancels the send. After the send-done
procedure is called, breaking may or may not cancel the send (and
probably will not).}

@; ----------------------------------------

@section{SMTP Unit}

@defmodule[net/smtp-unit]

@defthing[smtp@ unit?]{

Imports nothing, exports @scheme[smtp^].}

@; ----------------------------------------

@section{SMTP Signature}

@defmodule[net/smtp-sig]

@defsignature[smtp^ ()]{}

Includes everything exported by the @schememodname[net/smtp] module.
