#lang scribble/manual

@(require (for-label racket/base json))

@(define website @link["http://json.org"]{JSON web site})
@(define rfc @link["http://www.ietf.org/rfc/rfc4627.txt"]{JSON RFC})

@; @(begin (require scribble/eval)
@;         (define ev (make-base-eval))
@;         (ev '(require json)))

@title{JSON}

@author["Eli Barzilay" "Dave Herman"]

@defmodule[json]

This library provides utilities for parsing and producing data in the
JSON data exchange format to/from Racket values.  See the @website and
the @rfc for more information about JSON.

@section{JS-Expressions}

@defproc[(jsexpr? [x any] [#:null jsnull any? (json-null)])
         boolean?]{
  Performs a deep check to determine whether @racket[x] is a @tech{jsexpr}.

  This library defines a subset of Racket values that can be represented
  as JSON strings, and this predicates checks for such values.  A
  @deftech{JS-Expression}, or @deftech{jsexpr}, is one of:

  @itemize[
    @item{the value of @racket[jsnull], @racket['null] by default}
    @item{@racket[boolean?]}
    @item{@racket[string?]}
    @item{@racket[(or exact-integer? inexact-real?)]}
    @item{@racket[(listof jsexpr?)]}
    @item{@racket[(hasheqof symbol? jsexpr?)]}]}

@defparam[json-null jsnull any?]{
  This parameter determines the default Racket value that corresponds to
  a JSON ``@tt{null}''.  By default, it is the @racket['null] symbol.
  In some cases a different value may better fit your needs, therefore
  all functions in this library accept a @racket[#:null] keyword
  argument for the value that is used to represent a JSON ``@tt{null}'',
  and this argument defaults to @racket[(json-null)].}

@section{Generating JSON Text from JS-Expressions}

@defproc[(write-json [x jsexpr?] [out output-port? (current-output-port)]
                     [#:null jsnull any? (json-null)]
                     [#:encode encode (or/c 'control 'all) 'control])
         any]{
  Writes the @racket[x] @tech{jsexpr}, encoded as JSON, to the
  @racket[outp] output port.

  By default, only ASCII control characters are encoded as
  ``@tt{\uHHHH}''.  If @racket[encode] is given as @racket['all], then
  in addition to ASCII control characters, non-ASCII characters are
  encoded as well.  This can be useful if you need to transport the text
  via channels that might not support UTF-8.  Note that characters in
  the range of @tt{U+10000} and above are encoded as two @tt{\uHHHH}
  escapes, see Section 2.5 of the @|rfc|.}

@defproc[(jsexpr->string [x jsexpr?]
                         [#:null jsnull any? (json-null)]
                         [#:encode encode (or/c 'control 'all) 'control])
         string?]{
  Generates a JSON source string for the @tech{jsexpr} @racket[x].}

@defproc[(jsexpr->bytes [x jsexpr?]
                        [#:null jsnull any? (json-null)]
                        [#:encode encode (or/c 'control 'all) 'control])
         bytes?]{
  Generates a JSON source byte string for the @tech{jsexpr} @racket[x].
  (The byte string is encoded in UTF-8.)}

@section{Parsing JSON Text into JS-Expressions}

@defproc[(read-json [in input-port? (current-input-port)]
                    [#:null jsnull any? (json-null)])
         (or/c jsexpr? eof-object?)]{
  Reads a @tech{jsexpr} from a JSON-encoded input port @racket[in] as a
  Racket (immutable) value, or produces @racket[eof] if only whitespace
  remains.}

@defproc[(string->jsexpr [str string?] [#:null jsnull any? (json-null)])
         jsexpr?]{
  Parses the JSON string @racket[str] as an immutable @tech{jsexpr}.}

@defproc[(bytes->jsexpr [str bytes?] [#:null jsnull any? (json-null)])
         jsexpr?]{
  Parses the JSON bytes string @racket[str] as an immutable @tech{jsexpr}.}

@section{A Word About Design}

@subsection{The JS-Expression Data Type}

JSON syntactically distinguishes ``@tt{null}'', array literals, and
object literals, and therefore there is a question of what Racket value
should represent a JSON ``@tt{null}''.  This library uses the Racket
@racket['null] symbol by default.  Note that this is unambiguous, since
Racket symbols are used only as object keys, which are required to be
strings in JSON.

Several other options have been used by various libaries.  For example,
Dave Herman's PLaneT library (which has been the basis for this library)
uses the @racket[#\nul] character, other libraries for Racket and other
Lisps use @racket[(void)], @tt{NIL} (some use it also for JSON
``@tt{false}''), and more.  The approach taken by this library is to use
a keyword argument for all functions, with a parameter that determines
its default, making it easy to use any value that fits your needs.

The @rfc only states that object literal expressions ``SHOULD'' contain
unique keys, but does not proscribe them entirely.  Looking at existing
practice, it appears that popular JSON libraries parse object literals
with duplicate keys by simply picking one of the key-value pairs and
discarding the others with the same key.  This behavior is naturally
paralleled by Racket hash tables, making them a natural analog.

Finally, the @rfc is almost completely silent about the order of
key-value pairs.  While the RFC only specifies the syntax of JSON, which
of course always must represent object literals as an ordered
collection, the introduction states:

@nested[#:style 'inset]{
  An object is an unordered collection of zero or more name/value pairs,
  where a name is a string and a value is a string, number, boolean,
  null, object, or array.}

In practice, JSON libraries discard the order of object literals in
parsed JSON text and make no guarantees about the order of generated
object literals, usually using a hash table of some flavor as a natural
choice.  We therefore use do so as well.

@subsection{Naming Conventions}

Some names in this library use ``jsexpr'' and some use ``json''.  The
rationale that the first is used for our representation, and the second
is used as information that is received from or sent to the outside
world.
