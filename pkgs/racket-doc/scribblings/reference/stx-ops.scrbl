#lang scribble/doc
@(require "mz.rkt" scribble/eval)

@(define stx-eval (make-base-eval))
@(stx-eval '(require (for-syntax racket/base)))

@title[#:tag "stxops"]{Syntax Object Content}


@defproc[(syntax? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a @tech{syntax object}, @racket[#f]
otherwise. See also @secref["stxobj-model"].

@examples[#:eval stx-eval
  (syntax? #'quinoa)
  (syntax? #'(spelt triticale buckwheat))
  (syntax? (datum->syntax #f 'millet))
  (syntax? "barley")
]}


@defproc[(identifier? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a @tech{syntax object} and
@racket[(syntax-e stx)] produces a symbol.

@examples[#:eval stx-eval
  (identifier? #'linguine)
  (identifier? #'(if wheat? udon soba))
  (identifier? 'ramen)
  (identifier? 15)
]}


@defproc[(syntax-source [stx syntax?]) any]{

Returns the source for the @tech{syntax object} @racket[stx], or @racket[#f]
if none is known. The source is represented by an arbitrary value
(e.g., one passed to @racket[read-syntax]), but it is typically a file
path string. Source-location information is dropped for a syntax
object that is marshaled as part of compiled code; see also
@racket[current-compile].}


@defproc[(syntax-line [stx syntax?]) 
         (or/c exact-positive-integer? #f)]{

Returns the line number (positive exact integer) for the start of the
@tech{syntax object} in its source, or @racket[#f] if the line number or
source is unknown. The result is @racket[#f] if and only if
@racket[(syntax-column stx)] produces @racket[#f]. See also
@secref["linecol"], and see @racket[syntax-source] for information
about marshaling compiled @tech{syntax object}s.}


@defproc[(syntax-column [stx syntax?])
         (or/c exact-nonnegative-integer? #f)]{

Returns the column number (non-negative exact integer) for the start
of the @tech{syntax object} in its source, or @racket[#f] if the source
column is unknown. The result is @racket[#f] if and only if
@racket[(syntax-line stx)] produces @racket[#f]. See also
@secref["linecol"], and see @racket[syntax-source] for information
about marshaling compiled @tech{syntax object}s.}


@defproc[(syntax-position [stx syntax?])
         (or/c exact-positive-integer? #f)]{

Returns the character position (positive exact integer) for the start
of the @tech{syntax object} in its source, or @racket[#f] if the source
position is unknown. See also @secref["linecol"], and see
@racket[syntax-source] for information about marshaling compiled
@tech{syntax object}s.}


@defproc[(syntax-span [stx syntax?])
         (or/c exact-nonnegative-integer? #f)]{

Returns the span (non-negative exact integer) in characters of the
@tech{syntax object} in its source, or @racket[#f] if the span is
unknown. See also @racket[syntax-source] for information about
marshaling compiled @tech{syntax object}s.}


@defproc[(syntax-original? [stx syntax?]) boolean?]{

Returns @racket[#t] if @racket[stx] has the property that
@racket[read-syntax] attaches to the
@tech{syntax object}s that they generate (see @secref["stxprops"]), and if
@racket[stx]'s @tech{lexical information} does not indicate that the
object was introduced by a syntax transformer (see
@secref["stxobj-model"]). The result is @racket[#f] otherwise. This
predicate can be used to distinguish @tech{syntax object}s in an expanded
expression that were directly present in the original expression, as
opposed to @tech{syntax object}s inserted by macros.}


@defproc[(syntax-source-module [stx syntax?] [source? any/c #f])
         (or/c module-path-index? symbol? path? resolved-module-path? #f)]{

Returns an indication of the module whose source contains
@racket[stx], or @racket[#f] if @racket[stx] has no source module.  If
@racket[source?] is @racket[#f], then result is a module path index or
symbol (see @secref["modpathidx"]) or a @tech{resolved module path}; if @racket[source?] is true, the
result is a path or symbol corresponding to the loaded module's
source in the sense of @racket[current-module-declare-source].}


@defproc[(syntax-e [stx syntax?]) any]{

Unwraps the immediate datum structure from a @tech{syntax object},
leaving nested syntax structure (if any) in place.  The result of
@racket[(syntax-e stx)] is one of the following:

    @itemize[

       @item{a symbol}

       @item{a @tech{syntax pair} (described below)}

       @item{the empty list}

       @item{an immutable vector containing @tech{syntax object}s}

       @item{an immutable box containing @tech{syntax object}s}

       @item{an immutable @tech{hash table} containing @tech{syntax
       object} values (but not necessarily @tech{syntax object} keys)}

       @item{an immutable @tech{prefab} structure containing @tech{syntax object}s}

       @item{some other kind of datum---usually a number, boolean, or
             string---that is @tech{interned} when
             @racket[datum-intern-literal] would convert the
             value}

    ]

@examples[#:eval stx-eval
  (syntax-e #'a)
  (syntax-e #'(x . y))
  (syntax-e #'#(1 2 (+ 3 4)))
  (syntax-e #'#&"hello world")
  (syntax-e #'#hash((imperial . "yellow") (festival . "green")))
  (syntax-e #'#(point 3 4))
  (syntax-e #'3)
  (syntax-e #'"three")
  (syntax-e #'#t)
]

A @deftech{syntax pair} is a pair containing a @tech{syntax object} as its
first element, and either the empty list, a syntax pair, or a syntax
object as its second element.

A @tech{syntax object} that is the result of @racket[read-syntax] reflects
the use of delimited @litchar{.} in the input by creating a syntax
object for every pair of parentheses in the source, and by creating a
pair-valued @tech{syntax object} @italic{only} for parentheses in the
source. See @secref["parse-pair"] for more information.

If @racket[stx] is @tech{tainted} or @tech{armed}, then any syntax
object in the result of @racket[(syntax-e stx)] is @tech{tainted}, and
multiple calls to @racket[syntax-e] may return values that are not
@racket[eq?]. For a @racket[stx] that is not @tech{armed}, the results from
multiple calls to @racket[syntax-e] of @racket[stx] are @racket[eq?].}


@defproc[(syntax->list [stx syntax?]) (or/c list? #f)]{

Returns a list of @tech{syntax object}s or @racket[#f]. The result is a list
of @tech{syntax object}s when @racket[(syntax->datum stx)] would produce a
list. In other words, @tech{syntax pairs} in @racket[(syntax-e stx)]
are flattened.

If @racket[stx] is @tech{tainted} or @tech{armed}, then any syntax
object in the result of @racket[(syntax->list stx)] is @tech{tainted}.

@examples[#:eval stx-eval
  (syntax->list #'())
  (syntax->list #'(1 (+ 3 4) 5 6))
  (syntax->list #'a)
]}


@defproc[(syntax->datum [stx syntax?]) any]{

Returns a datum by stripping the lexical information, source-location
information, properties, and tamper status from @racket[stx]. Inside of
pairs, (immutable) vectors, (immutable) boxes, immutable @tech{hash
table} values (not keys), and immutable @tech{prefab} structures,
@tech{syntax object}s are recursively stripped.

The stripping operation does not mutate @racket[stx]; it creates new
pairs, vectors, boxes, hash tables, and @tech{prefab} structures as
needed to strip lexical and source-location information recursively.

@examples[#:eval stx-eval
  (syntax->datum #'a)
  (syntax->datum #'(x . y))
  (syntax->datum #'#(1 2 (+ 3 4)))
  (syntax->datum #'#&"hello world")
  (syntax->datum #'#hash((imperial . "yellow") (festival . "green")))
  (syntax->datum #'#(point 3 4))
  (syntax->datum #'3)
  (syntax->datum #'"three")
  (syntax->datum #'#t)
]}

@defproc[(datum->syntax [ctxt (or/c syntax? #f)]
                        [v any/c]
                        [srcloc (or/c syntax? #f
                                      (list/c any/c
                                              (or/c exact-positive-integer? #f)
                                              (or/c exact-nonnegative-integer? #f)
                                              (or/c exact-positive-integer? #f)
                                              (or/c exact-nonnegative-integer? #f))
                                      (vector/c any/c
                                               (or/c exact-positive-integer? #f)
                                               (or/c exact-nonnegative-integer? #f)
                                               (or/c exact-positive-integer? #f)
                                               (or/c exact-nonnegative-integer? #f)))
                                #f]
                        [prop (or/c syntax? #f) #f]
                        [ignored (or/c syntax? #f) #f])
          syntax?]{

Converts the @tech{datum} @racket[v] to a @tech{syntax object}.
The contents of pairs, vectors, and boxes, the fields of @tech{prefab}
structures, and the values of immutable hash tables are recursively converted.
The keys of @tech{prefab} structures and the keys of immutable hash tables are
not converted. Mutable vectors and boxes are replaced by immutable vectors and
boxes. For any kind of value other than a
pair, vector, box, immutable @tech{hash table}, immutable
@tech{prefab} structure, or @tech{syntax object}, conversion means
wrapping the value with lexical information, source-location
information, and properties after the value is @tech{interned}
via @racket[datum-intern-literal].

Converted objects in @racket[v] are given the lexical context
information of @racket[ctxt] and the source-location information of
@racket[srcloc]. If @racket[v] is not already a @tech{syntax object},
then the resulting immediate @tech{syntax object} is given the
properties (see @secref["stxprops"]) of @racket[prop] (even the
hidden ones that would not be visible via @racket[syntax-property-symbol-keys]); if @racket[v]
is a pair, vector, box, immutable @tech{hash table}, or immutable
@tech{prefab} structure, recursively converted values are not given
properties. If @racket[ctxt] is @tech{tainted} or
@tech{armed}, then the resulting syntax object from
@racket[datum->syntax] is @tech{tainted}.

Any of @racket[ctxt], @racket[srcloc], or @racket[prop] can be
@racket[#f], in which case the resulting syntax has no lexical
context, source information, and/or new properties.

If @racket[srcloc] is not @racket[#f] or a @tech{syntax object}, it
must be a list or vector of five elements:

@racketblock[
  (list source-name line column position span)
  @#,elem{or} (vector source-name line column position span)
]

where @racket[source-name] is an arbitrary value for the source
name; @racket[line] is an integer for the source line, or @racket[#f];
@racket[column] is an integer for the source column, or @racket[#f];
@racket[position] is an integer for the source position, or
@racket[#f]; and @racket[span] is an integer for the source span, or
@racket[#f]. The @racket[line] and @racket[column] values must both be
numbers or both be @racket[#f], otherwise the
@exnraise[exn:fail:contract].

Graph structure is not preserved by the conversion of @racket[v] to a
@tech{syntax object}. Instead, @racket[v] is essentially unfolded into
a tree. If @racket[v] has a cycle through pairs, vectors, boxes,
immutable @tech{hash tables}, and immutable @tech{prefab} structures,
then the @exnraise[exn:fail:contract].

The @racket[ignored] argument is allowed for backward compatibility
and has no effect on the returned syntax object.}


@defproc[(datum-intern-literal [v any/c]) any/c]{

Converts some values to be consistent with an @tech{interned} result
produced by the default reader in @racket[read-syntax] mode.

If @racket[v] is a @tech{number}, @tech{character}, @tech{string},
@tech{byte string}, or @tech{regular expression}, then the result is a
value that is @racket[equal?] to @racket[v] and @racket[eq?] to a
potential result of the default reader. (Note that mutable strings and
byte strings are @tech{interned} as immutable strings and byte
strings.)

If @racket[v] is an @tech{uninterned} or an @tech{unreadable symbol},
the result is still @racket[v], since an @tech{interned} symbol would
not be @racket[equal?] to @racket[v].

The conversion process does not traverse compound values. For example,
if @racket[v] is a @tech{pair} containing strings, then the strings
within @racket[v] are not @tech{interned}.

If @racket[_v1] and @racket[_v2] are @racket[equal?] but not
@racket[eq?], then it is possible that @racket[(datum-intern-literal
_v1)] will return @racket[_v1] and---sometime after @racket[_v1]
becomes unreachable as determined by the garbage collector (see
@secref["gc-model"])---@racket[(datum-intern-literal _v2)] can still
return @racket[_v2]. In other words, @racket[datum-intern-literal]
may adopt a given value as an @tech{interned} representative, but
if a former representative becomes otherwise unreachable, then
@racket[datum-intern-literal] may adopt a new representative.}


@defproc[(syntax-shift-phase-level [stx syntax?]
                                   [shift (or/c exact-integer? #f)])
         syntax?]{

Returns a syntax object that is like @racket[stx], but with all of its
top-level and module bindings shifted by @racket[shift] @tech{phase
levels}. If @racket[shift] is @racket[#f], then only bindings
at @tech{phase level} 0 are shifted to the @tech{label phase level}.
If @racket[shift] is @racket[0], then the result is @racket[stx].}


@defproc[(generate-temporaries [stx-pair (or syntax? list?)]) 
         (listof identifier?)]{

Returns a list of identifiers that are distinct from all other
identifiers. The list contains as many identifiers as
@racket[stx-pair] contains elements. The @racket[stx-pair] argument
must be a syntax pair that can be flattened into a list. The elements
of @racket[stx-pair] can be anything, but string, symbol, keyword
(possibly wrapped as syntax), and identifier elements will be embedded
in the corresponding generated name, which is useful for debugging
purposes.

The generated identifiers are built with interned symbols (not
@racket[gensym]s); see also @secref["print-compiled"].

@examples[#:eval stx-eval
  (generate-temporaries '(a b c d))
  (generate-temporaries #'(1 2 3 4))
  (define-syntax (set!-values stx)
    (syntax-case stx ()
      [(_ (id ...) expr)
       (with-syntax ([(temp ...) (generate-temporaries #'(id ...))])
         #'(let-values ([(temp ...) expr])
             (set! id temp) ... (void)))]))
]}


@defproc[(identifier-prune-lexical-context [id-stx identifier?]
                                           [syms (listof symbol?) (list (syntax-e id-stx))])
         identifier?]{

Returns an identifier with the same binding as @racket[id-stx], but
without lexical information from @racket[id-stx] that does not apply
to the symbols in @racket[syms], where even further extension of the
lexical information drops information for other symbols. In
particular, transferring the lexical context via
@racket[datum->syntax] from the result of this function to a symbol
other than one in @racket[syms] produces an identifier with no binding.

See also @racket[quote-syntax/prune].}


@defproc[(identifier-prune-to-source-module [id-stx identifier?])
         identifier?]{

Returns an identifier with its lexical context minimized to that
needed for @racket[syntax-source-module]. The minimized lexical
context does not include any bindings.}


@defproc[(syntax-recertify [new-stx syntax?]
                           [old-stx syntax?]
                           [inspector inspector?]
                           [key any/c])
         syntax?]{

For backward compatibility only; returns @racket[new-stx].}

@close-eval[stx-eval]

