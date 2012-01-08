#lang scribble/doc
@(require "common.rkt" scribble/struct
          (for-label net/mime net/mime-unit net/mime-sig))

@(define-syntax-rule (mime-table (type (sub-type0 ref0) (sub-type ref) ...) ...)
  (let ([spacer (hspace 1)]
        [to-flow (lambda (e)
                  (make-flow (list (make-paragraph (list e)))))])
   (make-table
    #f
    (append
     (list
      (list (to-flow (racket 'type))
            (to-flow spacer)
            (to-flow (racket 'sub-type0))
            (to-flow spacer)
            (to-flow ref0))
      (list (to-flow spacer)
            (to-flow spacer)
            (to-flow (racket 'sub-type))
            (to-flow spacer)
            (to-flow ref))
      ...)
     ...))))

@title[#:tag "mime"]{MIME: Decoding Internet Data}

@defmodule[net/mime]{The @racketmodname[net/mime] library provides
utilities for parsing and creating MIME encodings as described in RFC
2045 through RFC 2049.

The library was written by Francisco Solsona.}

@; ----------------------------------------

@section{Message Decoding}

@defproc[(mime-analyze [message-in (or/c bytes? input-port)]
                       [part? any/c #f])
         message?]{

Parses @racket[message-in] and returns the parsed result as a
@racket[message] instance.

If @racket[part?] is @racket[#f], then @racket[message-in] should
start with the header for a full message; otherwise,
@racket[message-in] should start with the header for a part within a
message.}

@defstruct[message ([version real?]
                    [entity entity]
                    [fields (listof string?)])]{

A decoded MIME message. The version is @racket[1.0] by default. The
@racket[entity] field represents the message data. The @racket[fields]
field contains one string for each field in the message header.}

@defstruct[entity ([type symbol?]
                   [subtype symbol?]
                   [charset symbol?]
                   [encoding symbol?]
                   [disposition disposition?]
                   [params (listof (cons/c symbol? string?))]
                   [id string?]
                   [description string?]
                   [other (listof string?)]
                   [fields (listof string?)]
                   [parts (listof message?)]
                   [body (or/c (output-port? . -> . void?) null?)])]{

Represents the content of a message or a sub-part. The
@racket[mime-analyze] function chooses default values for fields
when they are not specified in input.

Standard values for the @racket[type] field include @racket['text],
@racket['image], @racket['audio], @racket['video],
@racket['application], @racket['message], and @racket['multipart].

Standard values for the @racket[subtype] field depend on the
@racket[type] field, and include the following, but any
@racket[subtype] is allowed as a downcased version of the
specification from the header.

@mime-table[
(
 text            (plain                                   "[RFC1521, NSB]")
                 (richtext                                "[RFC1521, NSB]")
                 (tab-separated-values                    "[Lindner]")
)(
 multipart       (mixed                                   "[RFC1521, NSB]")
                 (alternative                             "[RFC1521, NSB]")
                 (digest                                  "[RFC1521, NSB]")
                 (parallel                                "[RFC1521, NSB]")
                 (appledouble                "[MacMime, Faltstrom]")
                 (header-set                             "[Crocker]")
)(
 message         (rfc822                                  "[RFC1521, NSB]")
                 (partial                                 "[RFC1521, NSB]")
                 (external-body                           "[RFC1521, NSB]")
                 (news                        "[RFC 1036, Spencer]")
)(
 application     (octet-stream                            "[RFC1521, NSB]")
                 (postscript                              "[RFC1521, NSB]")
                 (oda                                     "[RFC1521, NSB]")
                 (atomicmail                           "[atomicmail, NSB]")
                 (andrew-inset                       "[andrew-inset, NSB]")
                 (slate                           "[slate, Crowley]")
                 (wita              "[Wang Info Transfer, Campbell]")
                 (dec-dx            "[Digital Doc Trans, Campbell]")
                 (dca-rft        "[IBM Doc Content Arch, Campbell]")
                 (activemessage                          "[Shapiro]")
                 (rtf                                    "[Lindner]")
                 (applefile                  "[MacMime, Faltstrom]")
                 (mac-binhex40               "[MacMime, Faltstrom]")
                 (news-message-id              "[RFC1036, Spencer]")
                 (news-transmission            "[RFC1036, Spencer]")
                 (wordperfect5.1                         "[Lindner]")
                 (pdf                                    "[Lindner]")
                 (zip                                    "[Lindner]")
                 (macwriteii                             "[Lindner]")
                 (msword                                 "[Lindner]")
                 (remote-printing                         "[RFC1486,MTR]")
)(
 image           (jpeg                                    "[RFC1521, NSB]")
                 (gif                                     "[RFC1521, NSB]")
                 (ief                                         "[RFC1314]")
                 (tiff                                            "[MTR]")
)(
 audio           (basic                                   "[RFC1521, NSB]")
)(
 video           (mpeg                                    "[RFC1521, NSB]")
                 (quicktime                              "[Lindner]")
)]

Standard values for the @racket[charset] field include
@racket['us-ascii], which is the default.

Standard values for the @racket[encoding] field are @racket['7bit],
@racket['8bit], @racket['binary], @racket['quoted-printable], and
@racket['base64]. The default is @racket['7bit].

The @racket[params] field contains a list of parameters from other
MIME headers.

The @racket[id] field is taken from the @racket["Content-Id"] header
field.

The @racket[description] field is taken from the
@racket["Content-description"] header field.

The @racket[other] field contains additional (non-standard) field
headers whose field names start with @racket["Content-"].

The @racket[fields] field contains additional field headers whose
field names @emph{do not} start with @racket["Content-"].

The @racket[parts] contains sub-parts from multipart MIME
messages. This list is non-empty only when @racket[type] is
@racket['multipart] or @racket['message].

The @racket[body] field represents the body as a function that
consumes an output out and writes the decoded message to the port. If
@racket[type] is @racket['multipart] or @racket['message]., then
@racket[body] is @racket['()]. All of the standard values of
@racket[encoding] are supported. The procedure only works once (since
the encoded body is pulled from a stream).}

@defstruct[disposition ([type symbol?]
                        [filename (or/c string? false/c)]
                        [creation (or/c string? false/c)]
                        [modification (or/c string? false/c)]
                        [read (or/c string? false/c)]
                        [size (or/c exact-nonnegative-integer? false/c)]
                        [params (listof (cons/c symbol? string?))])]{

Represents a @racket["Content-Disposition"] header as defined in RFC
2183.

Standard values for the @racket[type] field include @racket['inline]
and @racket['attachment].

The @racket[filename] field is drawn from the @racket["filename"]
parameter of the @racket["Content-Disposition"] header, if included in
the message.

The @racket[creation], @racket[modification], and @racket[read] fields
represent file timestamps as drawn from the @racket["creation-date"],
@racket["modification-date"], and @racket["read-date"] attributes of
the @racket["Content-Disposition"] header, if included in the message.

The @racket[size] field is drawn from the @racket["size"] parameter of
the @racket["Content-Disposition"] header, if included in the message.

The @racket[params] field stores any additional attribute bindings of
the @racket["Content-Disposition"] header, if included in the message.}

@; ----------------------------------------

@section[#:tag "mime-exns"]{Exceptions}

@defstruct[(mime-error exn:fail) ()]{

The supertype of all MIME exceptions. Only the subtype
@racket[missing-multipart-boundary-parameter] is ever actually
raised.}

@defstruct[(unexpected-termination mime-error) ([msg string?])]{

Originally raised when an end-of-file is reached while parsing the
headers of a MIME entity, but currently a mere warning is logged.}

@defstruct[(missing-multipart-boundary-parameter mime-error) ()]{

Raised when a multipart type is specified, but no @racket["Boundary"]
parameter is given.}

@defstruct[(malformed-multipart-entity mime-error) ([msg string?])]{

Never actually raised.}

@defstruct[(empty-mechanism mime-error) ()]{

Never actually raised.}

@defstruct[(empty-type mime-error) ()]{

Never actually raised.}


@defstruct[(empty-subtype mime-error) ()]{

Never actually raised.}


@defstruct[(empty-disposition-type mime-error) ()]{

Never actually raised.}

@; ----------------------------------------

@section{MIME Unit}

@margin-note{@racket[mime@] and @racket[mime^] are deprecated.
They exist for backward-compatibility and will likely be removed in
the future. New code should use the @racketmodname[net/mime] module.}

@defmodule[net/mime-unit]

@defthing[mime@ unit?]{

Imports nothing, exports @racket[mime^].}

@; ----------------------------------------

@section{MIME Signature}

@defmodule[net/mime-sig]

@defsignature[mime^ ()]{}

Includes everything exported by the @racketmodname[net/mime] module.
