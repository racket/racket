#lang scribble/doc
@(require "common.ss"
          scribble/struct
          (for-label net/mime
                     net/mime-unit
                     net/mime-sig))

@(define-syntax-rule (mime-table (type (sub-type0 ref0) (sub-type ref) ...) ...)
  (let ([spacer (hspace 1)]
        [to-flow (lambda (e)
                  (make-flow (list (make-paragraph (list e)))))])
   (make-table
    #f
    (append
     (list
      (list (to-flow (scheme 'type))
            (to-flow spacer)
            (to-flow (scheme 'sub-type0))
            (to-flow spacer)
            (to-flow ref0))
      (list (to-flow spacer)
            (to-flow spacer)
            (to-flow (scheme 'sub-type))
            (to-flow spacer)
            (to-flow ref))
      ...)
     ...))))

@title[#:tag "mime"]{MIME: Decoding Internet Data}

@defmodule[net/mime]{The @schememodname[net/mime] library provides
utilities for parsing and creating MIME encodings as described in RFC
2045 through RFC 2049.

The library was written by Francisco Solsona.}

@; ----------------------------------------

@section{Message Decoding}

@defproc[(mime-analyze [message-in (or/c bytes? input-port)]
                       [part? any/c])
         message?]{

Parses @scheme[message-in] and returns the parsed result as a
@scheme[message] instance.}

@defstruct[message ([version real?]
                    [entity entity]
                    [fields (listof string?)])]{

A decoded MIME message. The version is @scheme[1.0] by default. The
@scheme[entity] field represents the message data. The @scheme[fields]
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
                   [body (output-port? . -> . void?)])]{

Represents the content of a message or a sub-part.

Standard values for the @scheme[type] field include @scheme['text],
@scheme['image], @scheme['audio], @scheme['video],
@scheme['application], @scheme['message], and @scheme['multipart].

Standard values for the @scheme[subtype] field depend on the
@scheme[type] field, and include the following:

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

Standard values for the @scheme[charset] field include
@scheme['us-ascii], which is the default.

Standard values for the @scheme[encoding] field are @scheme['7bit],
@scheme['8bit], @scheme['binary], @scheme['quoted-printable], and
@scheme['base64]. The default is @scheme['7bit].

The @scheme[params] field contains a list of parameters from other
MIME headers.

The @scheme[id] field is taken from the @scheme["Content-Id"] header
field.

The @scheme[description] field is taken from the
@scheme["Content-description"] header field.

The @scheme[other] field contains additional (non-standard) field
headers whose field names start with @scheme["Content-"].

The @scheme[fields] field contains additional field headers whose
field names @emph{do not} start with @scheme["Content-"].

The @scheme[parts] contains sub-parts from multipart MIME
messages. This list is non-empty only when @scheme[type] is
@scheme['multipart] or @scheme['message].

The @scheme[body] field represents the body as a function that
consumes an output out and writes the decoded message to the port.  No
bytes are written if @scheme[type] is @scheme['multipart] or
@scheme['message].  All of the standard values of @scheme[encoding]
are supported. The procedure only works once (since the encoded body
is pulled from a stream).}

@defstruct[disposition ([type symbol?]
                        [filename (or/c string? false/c)]
                        [creation (or/c string? false/c)]
                        [modification (or/c string? false/c)]
                        [read (or/c string? false/c)]
                        [size (or/c exact-nonnegative-integer? false/c)]
                        [params (listof (cons/c symbol? string?))])]{

Represents a @scheme["Content-Disposition"] header as defined in RFC
2183.

Standard values for the @scheme[type] field include @scheme['inline]
and @scheme['attachment].

The @scheme[filename] field is drawn from the @scheme["filename"]
parameter of the @scheme["Content-Disposition"] header, if included in
the message.

The @scheme[creation], @scheme[modification], and @scheme[read] fields
represent file timestamps as drawn from the @scheme["creation-date"],
@scheme["modification-date"], and @scheme["read-date"] attributes of
the @scheme["Content-Disposition"] header, if included in the message.

The @scheme[size] field is drawn from the @scheme["size"] parameter of
the @scheme["Content-Disposition"] header, if included in the message.

The @scheme[params] field stores any additional attribute bindings of
the @scheme["Content-Disposition"] header, if included in the message.}

@; ----------------------------------------

@section[#:tag "mime-exns"]{Exceptions}

@defstruct[mime-error ()]{

The supertype of all MIME exceptions.}

@defstruct[(unexpected-termination mime-error) ([msg string?])]{

Raised when an end-of-file is reached while parsing the headers of a
MIME entity.  It usually means that the message does not conform
to RFC 2045 and friends.}

@defstruct[(missing-multipart-boundary-parameter mime-error) ()]{

Raised when a multipart type is specified, but no @scheme["Boundary"]
parameter is given or an end-of-file is encountered before the
boundary.}

@defstruct[(malformed-multipart-entity mime-error) ([msg string?])]{

Similar to @scheme[unexpected-termination], but used only while
scanning parts of a multipart message.}

@defstruct[(empty-mechanism mime-error) ()]{

Raised when no transport encoding mechanism was provided with the
@scheme["Content-Transfer-Encoding"] field.}

@defstruct[(empty-type mime-error) ()]{

Raised when no type is specified for @scheme["Content-Type"], or when
the specification is incorrectly formatted.}


@defstruct[(empty-subtype mime-error) ()]{

Raised when no sub-type is specified for @scheme["Content-Type"], or
when the specification is incorrectly formatted.}


@defstruct[(empty-disposition-type mime-error) ()]{

Raised when type specified for the @scheme["Content-Disposition"]
field, or when the specification is incorrectly formatted.}

@; ----------------------------------------

@section{MIME Unit}

@defmodule[net/mime-unit]

@defthing[mime@ unit?]{

Imports nothing, exports @scheme[mime^].}

@; ----------------------------------------

@section{MIME Signature}

@defmodule[net/mime-sig]

@defsignature[mime^ ()]{}

Includes everything exported by the @schememodname[net/mime] module.
