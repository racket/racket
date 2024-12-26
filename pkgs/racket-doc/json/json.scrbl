#lang scribble/manual

@(require (for-label racket/base racket/contract json racket/port racket/treelist))

@(define website @link["http://json.org"]{JSON web site})
@(define rfc @link["http://www.ietf.org/rfc/rfc8259.txt"]{JSON RFC})

@(begin (require scribble/eval)
        (define ev (make-base-eval))
        (ev '(require json racket/port)))

@title{JSON}

@author["Eli Barzilay" "Dave Herman"]

@defmodule[json]

This library provides utilities for parsing and producing data in the
JSON data exchange format to/from Racket values.  See the @website and
the @rfc for more information about JSON.

@section{JS-Expressions}

@defproc[(jsexpr? [x any/c] [#:null jsnull any/c (json-null)])
         boolean?]{
  Performs a deep check to determine whether @racket[x] is a @tech{jsexpr}.

  This library defines a subset of Racket values that can be represented
  as JSON strings, and this predicates checks for such values.  A
  @deftech{JS-Expression}, or @deftech{jsexpr}, is one of:

  @itemize[
    @item{the value of @racket[jsnull], @racket['null] by default,
          which is recognized using @racket[eq?]}
    @item{@racket[boolean?]}
    @item{@racket[string?]}
    @item{@racket[(or/c exact-integer? (and/c inexact-real? rational?))]}
    @item{@racket[(listof jsexpr?)]}
    @item{@racket[(hash/c symbol? jsexpr?)]}]

@examples[#:eval ev
  (jsexpr? 'null)
  (jsexpr? #t)
  (jsexpr? "cheesecake")
  (jsexpr? 3.5)
  (jsexpr? (list 18 'null #f))
  (jsexpr? #hasheq([turnip . 82]))
  (jsexpr? (vector 1 2 3 4))
  (jsexpr? #hasheq(["turnip" . 82]))
  (jsexpr? +inf.0)
]
}

@defparam[json-null jsnull any/c]{
  This parameter determines the default Racket value that corresponds to
  a JSON ``@tt{null}''. By default, it is the @racket['null] symbol.
  In some cases a different value may better fit your needs, therefore
  all functions in this library accept a @racket[#:null] keyword
  argument for the value that is used to represent a JSON ``@tt{null}'',
  and this argument defaults to @racket[(json-null)].

  Note that the value of @racket[(json-null)] (or an explicitly-provided
  @racket[#:null] argument) is recognized using @racket[eq?].
}

@section{Generating JSON Text from JS-Expressions}

@defproc[(write-json [x jsexpr?] [out output-port? (current-output-port)]
                     [#:null jsnull any/c (json-null)]
                     [#:encode encode (or/c 'control 'all) 'control]
                     [#:indent indent (or/c #f #\tab natural-number/c) #f])
         void?]{
  Writes the @racket[x] @tech{jsexpr}, encoded as JSON, to the
  @racket[out] output port.

  By default, only ASCII control characters are encoded as
  ``@tt{\uHHHH}''.  If @racket[encode] is given as @racket['all], then
  in addition to ASCII control characters, non-ASCII characters are
  encoded as well.  This can be useful if you need to transport the text
  via channels that might not support UTF-8.  Note that characters in
  the range of @tt{U+10000} and above are encoded as two @tt{\uHHHH}
  escapes, see Section 2.5 of the @|rfc|.

  If @racket[indent] is provided and is not @racket[#f], each array element or object key--value pair
  is written on a new line, and the value of @racket[indent] specifies the whitespace to be added
  for each level of nesting: either a @racket[#\tab] character or, if @racket[indent] is a number,
  the corresponding number of @racket[#\space] characters.

@examples[#:eval ev
  (with-output-to-string
    (λ () (write-json #hasheq([waffle . (1 2 3)]))))
  (with-output-to-string
    (λ () (write-json #hasheq([와플 . (1 2 3)])
                      #:encode 'all)))
  (for ([indent (in-list '(#f 0 4 #\tab))])
    (newline)
    (write-json #hasheq([waffle . (1 2 3)] [와플 . (1 2 3)])
                #:indent indent)
    (newline))
]
}

@defproc[(jsexpr->string [x jsexpr?]
                         [#:null jsnull any/c (json-null)]
                         [#:encode encode (or/c 'control 'all) 'control]
                         [#:indent indent (or/c #f #\tab natural-number/c) #f])
         string?]{
  Generates a JSON source string for the @tech{jsexpr} @racket[x].

@examples[#:eval ev
  (jsexpr->string #hasheq([waffle . (1 2 3)]))
]
}

@defproc[(jsexpr->bytes [x jsexpr?]
                        [#:null jsnull any/c (json-null)]
                        [#:encode encode (or/c 'control 'all) 'control]
                        [#:indent indent (or/c #f #\tab natural-number/c) #f])
         bytes?]{
  Generates a JSON source byte string for the @tech{jsexpr} @racket[x].
  (The byte string is encoded in UTF-8.)

@examples[#:eval ev
  (jsexpr->bytes #hasheq([waffle . (1 2 3)]))
]
}

@; -----------------------------------------------------------------------------
@section{Parsing JSON Text into JS-Expressions}

@defproc[(read-json [in input-port? (current-input-port)]
                    [#:null jsnull any/c (json-null)]
                    [#:replace-malformed-surrogate? replace-malformed-surrogate? any/c #f])
         (or/c jsexpr? eof-object?)]{
  Reads a @tech{jsexpr} from a single JSON-encoded input port @racket[in] as a
  Racket (immutable) value, or produces @racket[eof] if only whitespace
  remains. Like @racket[read], the function leaves all remaining
  characters in the port so that a second call can retrieve the
  remaining JSON input(s). If the JSON inputs aren't delimited per se
  (true, false, null), they must be separated by whitespace from the
  following JSON input. When @racket[replace-malformed-surrogate?] is
  not @racket[#f] an escaped malformed surrogate will be replaced with the
  unicode replacement character, otherwise @racket[exn:fail:read] will be
  raised. Raises @racket[exn:fail:read] if @racket[in] is not
  at EOF and starts with malformed JSON (that is, no initial sequence of bytes
  in @racket[in] can be parsed as JSON); see below for examples.

@examples[#:eval ev
  (with-input-from-string
    "{\"arr\" : [1, 2, 3, 4]}"
    (λ () (read-json)))

  (with-input-from-string
    "\"sandwich\""
    (λ () (read-json)))

  (with-input-from-string
    "true false"
    (λ () (list (read-json) (read-json))))

  (with-input-from-string
    "true[1,2,3]"
    (λ () (list (read-json) (read-json))))

  (with-input-from-string
    "true\"hello\""
    (λ () (list (read-json) (read-json))))

  (with-input-from-string
    "\"world\"41"
    (λ () (list (read-json) (read-json))))

  (eval:error
    (with-input-from-string
      "sandwich sandwich" (code:comment "invalid JSON")
      (λ () (read-json))))

  (with-input-from-string
    "false sandwich" (code:comment "valid JSON prefix, invalid remainder is not (immediately) problematic")
    (λ () (read-json)))

  (eval:error
    (with-input-from-string
      "false42" (code:comment "invalid JSON text sequence")
      (λ () (read-json))))

  (with-input-from-string
    "false 42" (code:comment "valid JSON text sequence (notice the space)")
    (λ () (list (read-json) (read-json))))

]

@history[#:changed "8.1.0.2" @list{Adjusted the whitespace handling to reject whitespace that isn't either
           @racket[#\space], @racket[#\tab], @racket[#\newline], or @racket[#\return].}]
@history[#:changed "8.15.0.12" @list{Added @racket[#:replace-malformed-surrogate?]}]
}

@defproc[(string->jsexpr [str string?] [#:null jsnull any/c (json-null)])
         jsexpr?]{
  Parses a recognizable prefix of the string @racket[str] as an immutable @tech{jsexpr}.
  If the prefix isn't delimited per se (true, false, null), it
  must be separated by whitespace from the remaining characters.
  Raises @racket[exn:fail:read] if the string is malformed JSON.


@examples[#:eval ev
  (string->jsexpr "{\"pancake\" : 5, \"waffle\" : 7}")
]
}

@defproc[(bytes->jsexpr [str bytes?] [#:null jsnull any/c (json-null)])
         jsexpr?]{
  Parses a recognizable prefix of the string @racket[str] as an immutable @tech{jsexpr}.
  If the prefix isn't delimited per se (true, false, null), it
  must be separated by whitespace from the remaining bytes. Raises
  @racket[exn:fail:read] if the byte string is malformed JSON.


@examples[#:eval ev
  (bytes->jsexpr #"{\"pancake\" : 5, \"waffle\" : 7}")
]
}

@section{Extension Procedures}

@defmodule[(submod json for-extension)]

The bindings documented in this section are provided by the
@racket[(submod json for-extension)] module, not @racketmodname[json].

It may be more convenient for some programs to use a different representation of
JSON than a @racket[jsexpr?].  These procedures allow for customization of
representation read or written. For example, in the Rhombus language it is more
natural to use strings (which are immutable) for object keys, Rhombus lists (a.k.a.
@racket[treelist?]) for JSON lists, and immutable strings as JSON string values.

@defproc[(write-json* [who symbol?]
                      [x any/c]
                      [out output-port?]
                      [#:null jsnull any/c]
                      [#:encode encode (or/c 'control 'all)]
                      [#:indent indent (or/c #f #\tab natural-number/c)]
                      [#:object-rep? object-rep? (-> any/c boolean?)]
                      [#:object-rep->hash object-rep->hash (-> any/c hash?)]
                      [#:list-rep? list-rep? (-> any/c boolean?)]
                      [#:list-rep->list list-rep->list (-> any/c list?)]
                      [#:key-rep? key-rep? (-> any/c boolean?)]
                      [#:key-rep->string key-rep->string (-> any/c string?)]
                      [#:string-rep? string-rep? (-> any/c boolean?)]
                      [#:string-rep->string string-rep->string (-> any/c string?)])
         void?]{
  Writes the value @racket[x], encoded as JSON, to the @racket[out] output port.

  The @racket[who] argument is used for error reporting.  The @racket[jsnull],
  @racket[encode], and @racket[indent] arguments behave the same as described
  for @racket[write-json].

  The @racket[object-rep?] function should recognize values that will be written
  as JSON objects, and @racket[object-rep->hash] must convert the value to a
  @racket[hash?].

  The @racket[list-rep?] function should recognize values that will be written
  as JSON arrays, and @racket[list-rep->list] must convert the value to a
  @racket[list?].

  The @racket[key-rep?] function should recognize values that will be written as
  JSON object keys, and @racket[key-rep->string] must convert the value to a
  @racket[string?].

  The @racket[string-rep?] function should recognize values that will be written
  as JSON strings, and @racket[string-rep->string] must convert the value to a
  @racket[string?].

@history[#:added "8.15.0.12"]
}

@defproc[(read-json* [who symbol?]
                     [in input-port?]
                     [#:replace-malformed-surrogate? replace-malformed-surrogate? any/c #f]
                     [#:null jsnull any/c]
                     [#:make-object make-object-rep (-> (listof pair?) any/c)]
                     [#:make-list make-list-rep (-> list? any/c)]
                     [#:make-key make-key-rep (-> string? any/c)]
                     [#:make-string make-string-rep (-> string? any/c)])
         any/c]{
  Reads a value from a single JSON-encoded input port @racket[in] as a
  Racket value, or produces @racket[eof] if only whitespace.

  The @racket[who] argument is used for error reporting.  The @racket[jsnull]
  and @racket[replace-malformed-surrogate?] arguments behave the same as
  described in @racket[read-json].

  The @racket[make-object-rep] receives a @racket[list?] of key-value pairs,
  and returns a custom representation of a JSON object.

  The @racket[make-list-rep] receives a @racket[list?] of values and returns a
  custom representation of a JSON array.

  The @racket[make-key-rep] receives a @racket[string?] values for an object
  key, and returns a custom representation of a JSON key.

  The @racket[make-string-rep] receives a @racket[string?] value,
  and returns a custom representation of a JSON string.

@history[#:added "8.15.0.12"]
}

@section{A Word About Design}

@subsection{The JS-Expression Data Type}

JSON syntactically distinguishes ``@tt{null}'', array literals, and
object literals, and therefore there is a question of what Racket value
should represent a JSON ``@tt{null}''.  This library uses the Racket
@racket['null] symbol by default.  Note that this is unambiguous, since
Racket symbols are used only as object keys, which are required to be
strings in JSON.

Several other options have been used by various libraries.  For example,
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
choice.  We therefore do so as well.

@subsection{Naming Conventions}

Some names in this library use ``jsexpr'' and some use ``json''.  The
rationale that the first is used for our representation, and the second
is used as information that is received from or sent to the outside
world.

@close-eval[ev]
