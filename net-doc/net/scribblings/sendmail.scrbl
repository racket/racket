#lang scribble/doc
@(require "common.rkt"
          (for-label net/sendmail net/sendmail-unit net/sendmail-sig))

@title[#:tag "sendmail"]{@exec{sendmail}: Sending E-Mail}

@defmodule[net/sendmail]{The @racketmodname[net/sendmail] module
provides tools for sending electronic mail messages using a
@exec{sendmail} program on the local system. See also the
@racketmodname[net/smtp] package, which sends mail via SMTP.}

All strings used in mail messages are assumed to conform to their
corresponding SMTP specifications, except as noted otherwise.

@section{Sendmail Functions}

@defproc[(send-mail-message/port [from (or/c string? false/c)]
                                 [subject string?]
                                 [to (listof string?)]
                                 [cc (listof string?)]
                                 [bcc (listof string?)]
                                 [extra-header string?] ...)
         output-port?]{

The first argument is the header for the sender, the second is the
subject line, the third a list of ``To:'' recipients, the fourth a list
of ``CC:'' recipients, and the fifth a list of ``BCC:'' recipients.  All
of these are quoted if they contain non-ASCII characters.
@margin-note{Note that passing already-quoted strings would be fine,
  since then there are no non-ASCII characters.}
Additional arguments argument supply other mail headers, which must be
provided as lines (not terminated by a linefeed or carriage return) to
include verbatim in the header.

The return value is an output port into which the client must write
the message.  Clients are urged to use @racket[close-output-port] on
the return value as soon as the necessary text has been written, so
that the sendmail process can complete.

The @racket[from] argument can be any value; of course, spoofing should
be used with care.  If it is @racket[#f], no ``From:'' header is
generated, which usually means that your sendmail program will fill in
the right value based on the user.}

@defproc[(send-mail-message [from string?]
                            [subject string?]
                            [to (listof string?)]
                            [cc (listof string?)]
                            [bcc (listof string?)]
                            [body (listof string?)]
                            [extra-header string?] ...)
         void?]{

Like @racket[send-mail-message/port], but with @racket[body] as a list
of strings, each providing a line of the message body.

Lines that contain a single period do not need to be quoted.}


@; ----------------------------------------

@section{Sendmail Unit}

@margin-note{@racket[sendmail@] and @racket[sendmail^] are deprecated.
They exist for backward-compatibility and will likely be removed in
the future. New code should use the @racketmodname[net/sendmail] module.}

@defmodule[net/sendmail-unit]

@defthing[sendmail@ unit?]{

Imports nothing, exports @racket[sendmail^].}

@; ----------------------------------------

@section{Sendmail Signature}

@defmodule[net/sendmail-sig]

@defsignature[sendmail^ ()]{}

Includes everything exported by the @racketmodname[net/sendmail] module.
