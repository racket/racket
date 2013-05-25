#lang scribble/doc
@(require "mz.rkt")

@title[#:style 'toc #:tag "data"]{Datatypes}

@guideintro["datatypes"]{Datatypes}

Each pre-defined datatype comes with a set of procedures for
manipulating instances of the datatype.

@local-table-of-contents[#:style 'immediate-only]

@; ------------------------------------------------------------
@include-section["booleans.scrbl"]

@; ------------------------------------------------------------
@include-section["numbers.scrbl"]

@; ------------------------------------------------------------
@include-section["strings.scrbl"]

@; ------------------------------------------------------------
@include-section["bytes.scrbl"]

@; ------------------------------------------------------------
@include-section["chars.scrbl"]

@; ------------------------------------------------------------
@include-section["symbols.scrbl"]

@; ------------------------------------------------------------
@include-section["regexps.scrbl"]

@; ------------------------------------------------------------
@section[#:tag "keywords"]{Keywords}

@guideintro["keywords"]{keywords}

A @deftech{keyword} is like an @tech{interned} symbol, but its printed
form starts with @litchar{#:}, and a keyword cannot be used as an
identifier. Furthermore, a keyword by itself is not a valid
expression, though a keyword can be @racket[quote]d to form an
expression that produces the symbol.

Two keywords are @racket[eq?] if and only if they print the same
(i.e., keywords are always @tech{interned}).

Like symbols, keywords are only weakly held by the internal keyword
table; see @secref["symbols"] for more information.

@see-read-print["keyword"]{keywords}

@defproc[(keyword? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a keyword, @racket[#f] otherwise.}

@defproc[(keyword->string [keyword keyword?]) string?]{

Returns a string for the @racket[display]ed form of @racket[keyword],
not including the leading @litchar{#:}.}

@defproc[(string->keyword [str string?]) keyword?]{

Returns a keyword whose @racket[display]ed form is the same as that of
@racket[str], but with a leading @litchar{#:}.}

@defproc[(keyword<? [a-keyword keyword?] [b-keyword keyword?] ...+) boolean?]{

Returns @racket[#t] if the arguments are sorted, where the comparison
for each pair of keywords is the same as using
@racket[keyword->string] and @racket[string<?].}

@; ----------------------------------------------------------------------
@include-section["pairs.scrbl"]

@; ----------------------------------------------------------------------
@include-section["mpairs.scrbl"]

@; ----------------------------------------------------------------------
@include-section["vectors.scrbl"]

@; ------------------------------------------------------------
@section[#:tag "boxes"]{Boxes}

@guideintro["boxes"]{boxes}

A @deftech{box} is like a single-element vector, normally used as
minimal mutable storage.

A literal or printed box starts with @litchar{#&}. @see-read-print["box"]{boxes}

@defproc[(box? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a box, @racket[#f] otherwise.}


@defproc[(box [v any/c]) box?]{

Returns a new mutable box that contains @racket[v].}


@defproc[(box-immutable [v any/c]) (and/c box? immutable?)]{

Returns a new immutable box that contains @racket[v].}


@defproc[(unbox [box box?]) any/c]{

Returns the content of @racket[box].}


For any @racket[v], @racket[(unbox (box v))] returns @racket[v].


@defproc[(set-box! [box (and/c box? (not/c immutable?))]
                   [v any/c]) void?]{

Sets the content of @racket[box] to @racket[v].}


@defproc[(box-cas! [box (and/c box? (not/c immutable?) (not/c impersonator?))]
                   [old any/c] 
                   [new any/c]) 
         boolean?]{
  Atomically updates the contents of @racket[box] to @racket[new], provided
  that @racket[box] currently contains a value that is @racket[eq?] to
  @racket[old], and returns @racket[#t] in that case.  If @racket[box] 
  does not contain @racket[old], then the result is @racket[#f].

  If no other @tech{threads} or @tech{futures} attempt to access
  @racket[box], the operation is equivalent to 

  @racketblock[
  (and (eq? old (unbox loc)) (set-box! loc new) #t)]

  When Racket is compiled with support for @tech{futures},
  @racket[box-cas!] uses a hardware @emph{compare and set} operation.
  Uses of @racket[box-cas!] be performed safely in a @tech{future} (i.e.,
  allowing the future thunk to continue in parallel). }

@; ----------------------------------------------------------------------
@include-section["hashes.scrbl"]

@; ----------------------------------------------------------------------
@include-section["sequences.scrbl"]

@; ----------------------------------------------------------------------
@include-section["dicts.scrbl"]

@; ----------------------------------------------------------------------
@include-section["sets.scrbl"]

@; ----------------------------------------------------------------------
@include-section["procedures.scrbl"]

@; ----------------------------------------------------------------------
@section[#:tag "void"]{Void and Undefined}

The constant @|void-const| is returned by most forms and procedures
that have a side-effect and no useful result. The constant
@|undefined-const| is used as the initial value for @racket[letrec]
bindings. 

The @|void-const| value is always @racket[eq?] to itself, and the
@|undefined-const| value is also @racket[eq?] to itself.

@defproc[(void? [v any/c]) void?]{Returns @racket[#t] if @racket[v] is the
 constant @|void-const|, @racket[#f] otherwise.}


@defproc[(void [v any/c] ...) void?]{Returns the constant @|void-const|. Each
 @racket[v] argument is ignored.}

