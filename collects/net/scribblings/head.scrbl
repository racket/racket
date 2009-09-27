#lang scribble/doc
@(require "common.ss"
          scribble/eval
          scribble/struct
          (for-label net/head
                     net/head-unit
                     net/head-sig))

@(define head-eval (make-base-eval))
@interaction-eval[#:eval head-eval (require net/head)]

@title[#:tag "head"]{Headers: Parsing and Constructing}

@defmodule[net/head]{The @schememodname[net/head] module provides
utilities for parsing and constructing RFC 822 headers
@cite["RFC822"], which are used in protocols such as HTTP, SMTP, and
NNTP.}

A @deftech{header} is represented as a string or byte string
containing CRLF-delimited lines. Each field within the header spans
one or more lines. In addition, the header ends with two CRLFs
(because the first one terminates the last field, and the second
terminates the header).

@; ----------------------------------------

@section{Functions}

@defthing[empty-header string?]{

The string @scheme["\r\n\r\n"], which corresponds to the empty header.
This value is useful for building up headers with
@scheme[insert-field] and @scheme[append-headers].}


@defproc[(validate-header [candidate (or string? bytes?)]) void?]{

Checks that @scheme[candidate] matches RFC 822. If it does not, an
exception is raised.}


@defproc[(extract-field [field (or/c string? bytes?)] [header (or/c string? bytes?)])
         (or/c string? bytes? false/c)]{

Returns the header content for the specified field, or @scheme[#f] if
the field is not in the header. The @scheme[field] string should not
end with @scheme[":"], and it is used case-insensitively. The returned
string will not contain the field name, color separator, or CRLF
terminator for the field; however, if the field spans multiple lines,
the CRLFs separating the lines will be intact.

The @scheme[field] and @scheme[header] arguments must be both strings
or both byte strings, and the result (if not @scheme[#f]) is of the
same type.

@examples[
#:eval head-eval
(extract-field "TO" (insert-field "to" "me@localhost" 
                                  empty-header))
]}

@defproc[(extract-all-fields [header (or/c string? bytes?)])
         (listof (cons/c (or/c string? bytes?)
                         (or/c string? bytes?)))]{

Returns an association-list version of the header; the case of the
field names is preserved, as well as the order and duplicate uses of a
field name.

The result provides strings if @scheme[header] is a string, byte
strings if @scheme[header] is a byte string.}


@defproc[(remove-field [field (or/c string? bytes?)]
                       [header (or/c string? bytes?)])
          (or/c string? bytes?)]{

Creates a new header by removing the specified field from
@scheme[header] (or the first instance of the field, if it occurs
multiple times). If the field is not in @scheme[header], then the
return value is @scheme[header].

The @scheme[field] and @scheme[header] arguments must be both strings
or both byte strings, and the result is of the same type.}


@defproc[(insert-field [field (or/c string? bytes?)]
                       [value (or/c string? bytes?)]
                       [header (or/c string? bytes?)])
          (or/c string? bytes?)]{

Creates a new header by prefixing the given @scheme[header] with the
given @scheme[field]-@scheme[value] pair. The @scheme[value] string
should not contain a terminating CRLF, but a multi-line value (perhaps
created with @scheme[data-lines->data]) may contain separator CRLFs.

The @scheme[field], @scheme[value], and @scheme[header] arguments must
be all strings or all byte strings, and the result is of the same
type.}


@defproc[(replaces-field [field (or/c string? bytes?)]
                         [value (or/c string? bytes? false/c)]
                         [header (or/c string? bytes?)])
          (or/c string? bytes?)]{

Composes @scheme[remove-field] and (if @scheme[value] is not
@scheme[#f]) @scheme[insert-field].}

@defproc[(append-headers [header1 (or/c string? bytes?)]
                         [header2 (or/c string? bytes?)])
          (or/c string? bytes?)]{

Appends two headers.

The @scheme[header1] and @scheme[header2] arguments must be both
strings or both byte strings, and the result is of the same type.}


@defproc[(standard-message-header [from string?]
                                  [to (listof string?)]
                                  [cc (listof string?)]
                                  [bcc (listof string?)]
                                  [subject string?])
          string?]{

Creates a standard mail header given the sender, various lists of
recipients, a subject. A @scheme["Date"] field is added to the header
automatically, using the current time.

The BCC recipients do not actually appear in the header, but they're
accepted anyway to complete the abstraction.}


@defproc[(data-lines->data (listof string?)) string?]{

Merges multiple lines for a single field value into one string,
adding CRLF-TAB separators.}


@defproc[(extract-addresses [line string?]
                            [kind (one-of/c 'name 'address
                                            'full 'all)])
          (or/c (listof string?)
                (listof (list/c string? string? string?)))]{

Parses @scheme[string] as a list of comma-delimited mail addresses,
raising an exception if the list is ill-formed. This procedure can be
used for single-address strings, in which case the returned list
contains only one address.

The @scheme[kind] argument specifies which portion of an address
should be returned:

@itemize[

 @item{@scheme['name] --- the free-form name in the address, or the
       address itself if no name is available.

       @examples[
       #:eval head-eval
       (extract-addresses "John Doe <doe@localhost>" 'name)
       (extract-addresses "doe@localhost (Johnny Doe)" 'name)
       (extract-addresses "doe@localhost" 'name)
       (extract-addresses " \"Doe, John\" <doe@localhost>, jane"
                          'name)
       ]}

 @item{@scheme['address] --- just the mailing address, without any free-form
               names.

       @examples[
       #:eval head-eval
       (extract-addresses "John Doe <doe@localhost>" 'address)
       (extract-addresses "doe@localhost (Johnny Doe)" 'address)
       (extract-addresses "doe@localhost" 'address)
       (extract-addresses " \"Doe, John\" <doe@localhost>, jane"
                          'address)
       ]}

 @item{@scheme['full] --- the full address, essentially as it appears in the
       input, but normalized.

       @examples[
       #:eval head-eval
       (extract-addresses "John Doe   < doe@localhost >" 'full)
       (extract-addresses "  doe@localhost  (Johnny Doe)" 'full)
       (extract-addresses "doe@localhost" 'full)
       (extract-addresses " \"Doe, John\" <doe@localhost>, jane"
                          'full)
       ]}

 @item{@scheme['all] --- a list containing each of the three possibilities:
              free-form name, address, and full address (in that
              order).

       @examples[
       #:eval head-eval
       (extract-addresses "John Doe <doe@localhost>" 'all)
       (extract-addresses "doe@localhost (Johnny Doe)" 'all)
       (extract-addresses "doe@localhost" 'all)
       (define r
         (extract-addresses " \"John\" <doe@localhost>, jane"
                            'all))
       (length r)
       (car r)
       (cadr r)
       ]}

]}


@defproc[(assemble-address-field (addrs (listof string?))) string?]{

Creates a header field value from a list of addresses. The addresses
are comma-separated, and possibly broken into multiple lines.

@examples[
#:eval head-eval
(assemble-address-field '("doe@localhost" 
                          "Jane <jane@elsewhere>"))
]}


@; ----------------------------------------

@section{Header Unit}

@defmodule[net/head-unit]

@defthing[head@ unit?]{

Imports nothing, exports @scheme[head^].}

@; ----------------------------------------

@section{Header Signature}

@defmodule[net/head-sig]

@defsignature[head^ ()]{}

Includes everything exported by the @schememodname[net/head] module.
