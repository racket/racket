#lang scribble/doc
@(require "common.ss"
          (for-label net/sendmail
                     net/sendmail-unit
                     net/sendmail-sig))

@title[#:tag "sendmail"]{@exec{sendmail}: Sending E-Mail}

@defmodule[net/sendmail]{The @schememodname[net/sendmail] module
provides tools for sending electronic mail messages using a
@exec{sendmail} program on the local system. See also the
@schememodname[net/smtp] package, which sends mail via SMTP.}

All strings used in mail messages are assumed to conform to their
corresponding SMTP specifications, except as noted otherwise.

@section{Sendmail Functions}

@defproc[(send-mail-message/port [from string?]
                                 [subject string?]
                                 [to (listof string?)]
                                 [cc (listof string?)]
                                 [bcc (listof string?)]
                                 [extra-header string?] ...)
         output-port?]{

The first argument is the header for the sender, the second is the
subject line, the third a list of ``To:'' recipients, the fourth a
list of ``CC:'' recipients, and the fifth a list of ``BCC:''
recipients.  Additional arguments argument supply other mail headers,
which must be provided as lines (not terminated by a linefeed or
carriage return) to include verbatim in the header.

The return value is an output port into which the client must write
the message.  Clients are urged to use @scheme[close-output-port] on
the return value as soon as the necessary text has been written, so
that the sendmail process can complete.

The @scheme[from] argument can be any value; of course, spoofing
should be used with care.}

@defproc[(send-mail-message [from string?]
                            [subject string?]
                            [to (listof string?)]
                            [cc (listof string?)]
                            [bcc (listof string?)]
                            [body (listof string?)]
                            [extra-header string?] ...)
         void?]{

Like @scheme[send-mail-message/port], but with @scheme[body] as a list
of strings, each providing a line of the message body.

Lines that contain a single period do not need to be quoted.}


@defstruct[(no-mail-recipients exn) ()]{

Raised when no mail recipients were specified for
@scheme[send-mail-message/port].}



@; ----------------------------------------

@section{Sendmail Unit}

@defmodule[net/sendmail-unit]

@defthing[sendmail@ unit?]{

Imports nothing, exports @scheme[sendmail^].}

@; ----------------------------------------

@section{Sendmail Signature}

@defmodule[net/sendmail-sig]

@defsignature[sendmail^ ()]{}

Includes everything exported by the @schememodname[net/sendmail] module.
