#lang scribble/doc
@(require "common.ss"
          scribble/bnf
          scribble/eval
          (for-label net/url
                     net/uri-codec
                     net/uri-codec-unit
                     net/uri-codec-sig))

@(define uri-codec-eval (make-base-eval))
@interaction-eval[#:eval uri-codec-eval (require net/uri-codec)]

@title[#:tag "uri-codec"]{URI Codec: Encoding and Decoding URIs}

@defmodule[net/uri-codec]{The @schememodname[net/uri-codec] module
provides utilities for encoding and decoding strings using the URI
encoding rules given in RFC 2396 @cite["RFC2396"], and to encode and
decode name/value pairs using the
@tt{application/x-www-form-urlencoded} mimetype given the in HTML 4.0
specification.  There are minor differences between the two encodings.}

The URI encoding uses allows a few characters to be represented as-is:
@litchar{a} through @litchar{z}, @litchar{A} through @litchar{Z},
@litchar{0}-@litchar{9}, @litchar{-}, @litchar{_}, @litchar{.},
@litchar{!}, @litchar{~}, @litchar{*}, @litchar{'}, @litchar{(} and
@litchar{)}.  The remaining characters are encoded as
@litchar{%}@nonterm{xx}, where @nonterm{xx} is the two-character hex
representation of the integer value of the character (where the
mapping character--integer is determined by US-ASCII if the integer is
less than 128).

The encoding, in line with RFC 2396's recommendation, represents a
character as-is, if possible.  The decoding allows any characters
to be represented by their hex values, and allows characters to be
incorrectly represented as-is.

The rules for the @tt{application/x-www-form-urlencoded} mimetype
given in the HTML 4.0 spec are:

@itemize[

  @item{Control names and values are escaped. Space characters are
  replaced by @litchar{+}, and then reserved characters are escaped as
  described in RFC 1738, section 2.2: Non-alphanumeric characters are
  replaced by @litchar{%}@nonterm{xx} representing the ASCII code of
  the character. Line breaks are represented as CRLF pairs:
  @litchar{%0D%0A}. Note that RFC 2396 supersedes RFC 1738
  @cite["RFC1738"].}

  @item{The control names/values are listed in the order they appear
  in the document. The name is separated from the value by @litchar{=}
  and name/value pairs are separated from each other by either
  @litchar{;} or @litchar{&}.  When encoding, @litchar{;} is used as
  the separator by default. When decoding, both @litchar{;} and
  @litchar{&} are parsed as separators by default.}

]

These rules differs slightly from the straight encoding in RFC 2396 in
that @litchar{+} is allowed, and it represents a space.  The
@schememodname[net/uri-codec] library follows this convention,
encoding a space as @litchar{+} and decoding @litchar{+} as a space.
In addtion, since there appear to be some brain-dead decoders on the
web, the library also encodes @litchar{!}, @litchar{~}, @litchar{'},
@litchar{(}, and @litchar{)} using their hex representation, which is
the same choice as made by the Java's @tt{URLEncoder}.

@; ----------------------------------------

@section[#:tag "uri-codec-proc"]{Functions}

@defproc[(uri-encode [str string?]) string?]{

Encode a string using the URI encoding rules.}


@defproc[(uri-decode [str string?]) string?]{

Decode a string using the URI decoding rules.}

@defproc[(uri-path-segment-encode [str string?]) string?]{
Encodes a string according to the rules in @cite["RFC3986"] for path segments.
}
@defproc[(uri-path-segment-decode [str string?]) string?]{
Decodes a string according to the rules in @cite["RFC3986"] for path segments.
}
@defproc[(uri-userinfo-encode [str string?]) string?]{
Encodes a string according to the rules in @cite["RFC3986"] for the userinfo field.
}
@defproc[(uri-userinfo-decode [str string?]) string?]{
Decodes a string according to the rules in @cite["RFC3986"] for the userinfo field.
}


@defproc[(form-urlencoded-encode [str string?]) string?]{

Encode a string using the @tt{application/x-www-form-urlencoded}
encoding rules. The result string contains no non-ASCII characters.}


@defproc[(form-urlencoded-decode [str string?]) string?]{

Decode a string encoded using the
@tt{application/x-www-form-urlencoded} encoding rules.}


@defproc[(alist->form-urlencoded [alist (listof (cons/c symbol? string?))])
         string?]{

Encode an association list using the
@tt{application/x-www-form-urlencoded} encoding rules.

The @scheme[current-alist-separator-mode] parameter determines the
separator used in the result.}


@defproc[(form-urlencoded->alist [str string])
         (listof (cons/c symbol? string?))]{

Decode a string encoded using the
@tt{application/x-www-form-urlencoded} encoding rules into an
association list. All keys are case-folded for conversion to symbols.

The @scheme[current-alist-separator-mode] parameter determines the way
that separators are parsed in the input.}


@defparam[current-alist-separator-mode mode 
          (one-of/c 'amp 'semi 'amp-or-semi 'semi-or-amp)]{

A parameter that determines the separator used/recognized between
associations in @scheme[form-urlencoded->alist],
@scheme[alist->form-urlencoded], @scheme[url->string], and
@scheme[string->url].

The default value is @scheme['amp-or-semi], which means that both
@litchar{&} and @litchar{;} are treated as separators when parsing,
and @litchar{&} is used as a separator when encoding. The other modes
use/recognize only of the separators.

@examples[
#:eval uri-codec-eval
(define ex '((x . "foo") (y . "bar") (z . "baz")))
(code:line (current-alist-separator-mode 'amp) (code:comment @#,t{try @scheme['amp]...}))
(form-urlencoded->alist "x=foo&y=bar&z=baz")
(form-urlencoded->alist "x=foo;y=bar;z=baz")
(alist->form-urlencoded ex)
(code:line (current-alist-separator-mode 'semi) (code:comment @#,t{try @scheme['semi]...}))
(form-urlencoded->alist "x=foo;y=bar;z=baz")
(form-urlencoded->alist "x=foo&y=bar&z=baz")
(alist->form-urlencoded ex)
(code:line (current-alist-separator-mode 'amp-or-semi) (code:comment @#,t{try @scheme['amp-or-semi]...}))
(form-urlencoded->alist "x=foo&y=bar&z=baz")
(form-urlencoded->alist "x=foo;y=bar;z=baz")
(alist->form-urlencoded ex)
(code:line (current-alist-separator-mode 'semi-or-amp) (code:comment @#,t{try @scheme['semi-or-amp]...}))
(form-urlencoded->alist "x=foo&y=bar&z=baz")
(form-urlencoded->alist "x=foo;y=bar;z=baz")
(alist->form-urlencoded ex)
]}
