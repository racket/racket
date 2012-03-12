#lang scribble/doc

@require[scribble/manual
         scribble/base
         scribble/eval
         scribble/bnf
         scheme/runtime-path]

@require[(for-syntax scheme/base)]

@require[(for-label scheme/base json)]

@define-runtime-path[home (build-path 'same)]

@define[the-eval
        (let ([the-eval (make-base-eval)])
          (parameterize ([current-directory home])
            (the-eval `(require (file ,(path->string (build-path home "main.ss"))))))
          the-eval)]

@title[#:tag "top"]{@bold{JSON}}

by Dave Herman (@tt{dherman at ccs dot neu dot edu})

This library provides utilities for marshalling and unmarshalling data in the JSON data exchange format.
See the @link["http://www.json.org"]{JSON web site} and the @link["http://www.ietf.org/rfc/rfc4627.txt?number=4627"]{JSON RFC}
for more information about JSON.

@defmodule[json]

@section[#:tag "jsexprs"]{JS-Expressions}

This library defines a subset of Scheme values that can be represented as JSON strings.
A @deftech{JS-Expression}, or @deftech{jsexpr}, is one of:

@itemlist[
@item{@schemevalfont{#\null}}
@item{@scheme[boolean?]}
@item{@scheme[string?]}
@item{@scheme[(or integer? inexact-real?)]}
@item{@scheme[(listof jsexpr?)]}
@item{@scheme[(hasheqof symbol? jsexpr?)]}
]

@defproc[(jsexpr? [x any]) boolean?]{
Performs a deep check to determine whether @scheme[x] is a @tech{jsexpr}.}

@defproc[(read-json [in input-port? (current-input-port)]) jsexpr?]{
Reads an immutable @tech{jsexpr} from a JSON-encoded input port @scheme[in].}

@defproc[(write-json [x jsexpr?] [out output-port? (current-output-port)]) any]{
Writes the @tech{jsexpr} @scheme[x], encoded as JSON, to output port @scheme[out].}

@defproc[(jsexpr->json [x jsexpr?]) string?]{
Generates a JSON source string for the @tech{jsexpr} @scheme[x].}

@defproc[(json->jsexpr [s string?]) jsexpr?]{
Parses the JSON string @scheme[s] as an immutable @tech{jsexpr}.}

@;examples[#:eval the-eval 42]

@section[#:tag "rationale"]{A word about design}

Because JSON distinguishes syntactically between @tt{null}, array literals, and object literals,
this library uses non-overlapping datatypes for the three corresponding variants of @tech{jsexpr}.

Since the Scheme null value @scheme['()] overlaps with lists, there is no natural choice for the
@tech{jsexpr} represented as @tt{null}. We prefer @schemevalfont{#\null} as the least objectionable
option from Scheme's host of singleton datatypes (note that the @void-const and @undefined-const
constants do not have @scheme[read]able and @scheme[write]able representations, which makes them
less convenient choices).

The @link["http://www.ietf.org/rfc/rfc4627.txt?number=4627"]{JSON RFC} only states that object
literal expressions "SHOULD" contain unique keys, but does not proscribe them entirely. Looking at
existing practice, it appears that popular JSON libraries parse object literals with duplicate keys
by simply picking one of the key-value pairs and discarding the others with the same key. This
behavior is naturally paralleled by PLT Scheme hash tables, making them a natural analog.

Finally, the @link["http://www.ietf.org/rfc/rfc4627.txt?number=4627"]{JSON RFC} is almost completely
silent about the order of key-value pairs. While the RFC only specifies the syntax of JSON, which of
course always must represent object literals as an ordered collection, the introduction states:

@nested[#:style 'inset]{An object is an unordered collection of zero or more name/value
pairs, where a name is a string and a value is a string, number,
boolean, null, object, or array.}

In practice, JSON libraries discard the order of object literals in parsed JSON text and make no
guarantees about the order of generated object literals. This again makes hash tables a good choice
for representing as JSON object literals.
