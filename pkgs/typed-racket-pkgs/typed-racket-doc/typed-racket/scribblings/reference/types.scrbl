#lang scribble/manual

@begin[(require "../utils.rkt"
                "numeric-tower-pict.rkt"
                scribble/eval
                racket/sandbox)
       (require (for-label (only-meta-in 0 [except-in typed/racket for])
                           racket/async-channel))]

@(define the-eval (make-base-eval))
@(the-eval '(require (except-in typed/racket #%top-interaction #%module-begin)))
@(define the-top-eval (make-base-eval))
@(the-top-eval '(require (except-in typed/racket #%module-begin)
                         racket/flonum racket/extflonum racket/fixnum))

@(define-syntax-rule (ex . args)
   (examples #:eval the-top-eval . args))


@title[#:tag "type-ref"]{Type Reference}

@defidform[Any]{Any Racket value. All other types are subtypes of @racket[Any].}

@defidform[Nothing]{The empty type.  No values inhabit this type, and
any expression of this type will not evaluate to a value.}

@section{Base Types}

@(define-syntax-rule
   (defnums (ids ...) . rest)
   (deftogether ((defidform ids) ...) . rest))

@subsection{Numeric Types}

These types represent the hierarchy of @rtech{numbers} of Racket. The
diagram below shows the relationships between the types in the hierarchy.

@centered[@numeric-tower-pict]

The regions with a solid border are @emph{layers} of the numeric hierarchy
corresponding to sets of numbers such as integers or rationals. Layers
contained within another are subtypes of the layer containing them. For
example, @racket[Exact-Rational] is a subtype of @racket[Exact-Number].

The @racket[Real] layer is also divided into positive and negative types
(shown with a dotted line). The @racket[Integer] layer is subdivided into
several fixed-width integers types, detailed later in this section.

@defnums[(Number Complex)]
@racket[Number] and @racket[Complex] are synonyms. This is the most general
numeric type, including all Racket numbers, both exact and inexact, including
complex numbers.

@defnums[(Integer)]
Includes Racket's exact integers and corresponds to the
@racket[exact-integer?] predicate. This is the most general type that is still
valid for indexing and other operations that require integral values.

@defnums[(Float Flonum)]
Includes Racket's double-precision (default) floating-point numbers and
corresponds to the @racket[flonum?] predicate. This type excludes
single-precision floating-point numbers.

@defnums[(Single-Flonum)]
Includes Racket's single-precision floating-point numbers and corresponds to
the @racket[single-flonum?] predicate. This type excludes double-precision
floating-point numbers.

@defnums[(Inexact-Real)]
Includes all of Racket's floating-point numbers, both single- and
double-precision.

@defnums[(Exact-Rational)]
Includes Racket's exact rationals, which include fractions and exact integers.

@defnums[(Real)]
Includes all of Racket's real numbers, which include both exact rationals and
all floating-point numbers. This is the most general type for which comparisons
(e.g. @racket[<]) are defined.

@defnums[(
Exact-Number
Float-Complex
Single-Flonum-Complex
Inexact-Complex)]
These types correspond to Racket's complex numbers.


The above types can be subdivided into more precise types if you want to
enforce tighter constraints. Typed Racket provides types for the positive,
negative, non-negative and non-positive subsets of the above types (where
applicable).

@defnums[(
Positive-Integer
Exact-Positive-Integer
Nonnegative-Integer
Exact-Nonnegative-Integer
Natural
Negative-Integer
Nonpositive-Integer
Zero
Positive-Float
Positive-Flonum
Nonnegative-Float
Nonnegative-Flonum
Negative-Float
Negative-Flonum
Nonpositive-Float
Nonpositive-Flonum
Float-Negative-Zero
Flonum-Negative-Zero
Float-Positive-Zero
Flonum-Positive-Zero
Float-Zero
Flonum-Zero
Float-Nan
Flonum-Nan
Positive-Single-Flonum
Nonnegative-Single-Flonum
Negative-Single-Flonum
Nonpositive-Single-Flonum
Single-Flonum-Negative-Zero
Single-Flonum-Positive-Zero
Single-Flonum-Zero
Single-Flonum-Nan
Positive-Inexact-Real
Nonnegative-Inexact-Real
Negative-Inexact-Real
Nonpositive-Inexact-Real
Inexact-Real-Negative-Zero
Inexact-Real-Positive-Zero
Inexact-Real-Zero
Inexact-Real-Nan
Positive-Exact-Rational
Nonnegative-Exact-Rational
Negative-Exact-Rational
Nonpositive-Exact-Rational
Positive-Real
Nonnegative-Real
Negative-Real
Nonpositive-Real
Real-Zero
)]
@racket[Natural] and @racket[Exact-Nonnegative-Integer] are synonyms. So are
the integer and exact-integer types, and the float and flonum
types. @racket[Zero] includes only the integer @racket[0]. @racket[Real-Zero]
includes exact @racket[0] and all the floating-point zeroes.

These types are useful when enforcing that values have a specific
sign. However, programs using them may require additional dynamic checks when
the type-checker cannot guarantee that the sign constraints will be respected.

In addition to being divided by sign, integers are further subdivided into
range-bounded types. The relationships between most of the range-bounded types
are shown in this diagram:

@centered[@integer-pict]

Like the previous diagram, types nested inside of another in the
diagram are subtypes of its containing types.

@defnums[(
One
Byte
Positive-Byte
Index
Positive-Index
Fixnum
Positive-Fixnum
Nonnegative-Fixnum
Negative-Fixnum
Nonpositive-Fixnum
)]
@racket[One] includes only the integer @racket[1]. @racket[Byte] includes
numbers from @racket[0] to @racket[255]. @racket[Index] is bounded by
@racket[0] and by the length of the longest possible Racket
vector. @racket[Fixnum] includes all numbers represented by Racket as machine
integers. For the latter two families, the sets of values included in the types
are architecture-dependent, but typechecking is architecture-independent.

These types are useful to enforce bounds on numeric values, but given the
limited amount of closure properties these types offer, dynamic checks may be
needed to check the desired bounds at runtime.

@ex[
7
8.3
(/ 8 3)
0
-12
3+4i]

@defnums[(
ExtFlonum
Positive-ExtFlonum
Nonnegative-ExtFlonum
Negative-ExtFlonum
Nonpositive-ExtFlonum
ExtFlonum-Negative-Zero
ExtFlonum-Positive-Zero
ExtFlonum-Zero
ExtFlonum-Nan
)]
80-bit @rtech{extflonum} types, for the values operated on by
@racketmodname[racket/extflonum] exports.
These are not part of the numeric tower.

@subsection{Other Base Types}

@deftogether[(
@defidform[Boolean]
@defidform[True]
@defidform[False]
@defidform[String]
@defidform[Keyword]
@defidform[Symbol]
@defidform[Char]
@defidform[Void]
@defidform[Input-Port]
@defidform[Output-Port]
@defidform[Port]
@defidform[Path]
@defidform[Path-For-Some-System]
@defidform[Regexp]
@defidform[PRegexp]
@defidform[Byte-Regexp]
@defidform[Byte-PRegexp]
@defidform[Bytes]
@defidform[Namespace]
@defidform[Namespace-Anchor]
@defidform[Variable-Reference]
@defidform[Null]
@defidform[EOF]
@defidform[Continuation-Mark-Set]
@defidform[Undefined]
@defidform[Module-Path]
@defidform[Module-Path-Index]
@defidform[Resolved-Module-Path]
@defidform[Compiled-Module-Expression]
@defidform[Compiled-Expression]
@defidform[Internal-Definition-Context]
@defidform[Pretty-Print-Style-Table]
@defidform[Special-Comment]
@defidform[Struct-Type-Property]
@defidform[Impersonator-Property]
@defidform[Read-Table]
@defidform[Bytes-Converter]
@defidform[Parameterization]
@defidform[Custodian]
@defidform[Inspector]
@defidform[Security-Guard]
@defidform[UDP-Socket]
@defidform[TCP-Listener]
@defidform[Logger]
@defidform[Log-Receiver]
@defidform[Log-Level]
@defidform[Thread]
@defidform[Thread-Group]
@defidform[Subprocess]
@defidform[Place]
@defidform[Place-Channel]
@defidform[Semaphore]
@defidform[Will-Executor]
@defidform[Pseudo-Random-Generator]
)]{
These types represent primitive Racket data.

@ex[
#t
#f
"hello"
(current-input-port)
(current-output-port)
(string->path "/")
#rx"a*b*"
#px"a*b*"
'#"bytes"
(current-namespace)
#\b
(thread (lambda () (add1 7)))
]
}

@defidform[Path-String]{
The union of the @racket[Path] and
@racket[String] types.  Note that this does not
match exactly what the predicate @racket[path-string?]
recognizes. For example, strings
that contain the character @racket[#\nul] have the type
@racket[Path-String] but @racket[path-string?] returns
@racket[#f] for those strings. For a complete specification
of which strings @racket[path-string?] accepts, see its
documentation.}


@section{Singleton Types}

Some kinds of data are given singleton types by default.  In
particular, @rtech{booleans}, @rtech{symbols}, and @rtech{keywords} have types which
consist only of the particular boolean, symbol, or keyword.  These types are
subtypes of @racket[Boolean], @racket[Symbol] and @racket[Keyword], respectively.

@ex[
#t
'#:foo
'bar
]

@section{Containers}


The following base types are parametric in their type arguments.

@defform[(Pairof s t)]{is the @rtech{pair} containing @racket[s] as the @racket[car]
  and @racket[t] as the @racket[cdr]}

@ex[
(cons 1 2)
(cons 1 "one")
]


@defform[(Listof t)]{Homogenous @rtech{lists} of @racket[t]}
@defform[(List t ...)]{is the type of the list with one element, in order,
  for each type provided to the @racket[List] type constructor.}
@defform/none[(#,(racket List) t ... trest #,(racket ...) bound)]{is the type of a list with
one element for each of the @racket[t]s, plus a sequence of elements
corresponding to @racket[trest], where @racket[bound]
  must be an identifier denoting a type variable bound with @racket[...].}
@defform[(List* t t1 ... s)]{is equivalent to @racket[(Pairof t (List* t1 ... s))].}

@ex[
(list 'a 'b 'c)
(plambda: (a ...) ([sym : Symbol] boxes : (Boxof a) ... a)
  (ann (cons sym boxes) (List Symbol (Boxof a) ... a)))
(map symbol->string (list 'a 'b 'c))
]

@defform[(MListof t)]{Homogenous @rtech{mutable lists} of @racket[t].}
@defform[(MPairof t u)]{@rtech{Mutable pairs} of @racket[t] and @racket[u].}

@defidform[MPairTop]{is the type of a @rtech{mutable pair} with unknown
  element types and is the supertype of all mutable pair types.
  This type typically appears in programs via the combination of
  occurrence typing and @racket[mpair?].
@ex[(lambda: ([x : Any]) (if (mpair? x) x (error "not an mpair!")))]
}

@defform[(Boxof t)]{A @rtech{box} of @racket[t]}

@ex[(box "hello world")]

@defidform[BoxTop]{is the type of a @rtech{box} with an unknown element
  type and is the supertype of all box types. Only read-only box operations
  (e.g. @racket[unbox]) are allowed on values of this type. This type
  typically appears in programs via the combination of occurrence
  typing and @racket[box?].
@ex[(lambda: ([x : Any]) (if (box? x) x (error "not a box!")))]
}

@defform[(Vectorof t)]{Homogenous @rtech{vectors} of @racket[t]}
@defform[(Vector t ...)]{is the type of the list with one element, in order,
  for each type provided to the @racket[Vector] type constructor.

  @ex[(vector 1 2 3)
  #(a b c)]}

@defidform[FlVector]{An @rtech{flvector}.
  @ex[(flvector 1.0 2.0 3.0)]}
@defidform[ExtFlVector]{An @rtech{extflvector}.
  @ex[(extflvector 1.0t0 2.0t0 3.0t0)]}
@defidform[FxVector]{An @rtech{fxvector}.
  @ex[(fxvector 1 2 3)]}

@defidform[VectorTop]{is the type of a @rtech{vector} with unknown length and
  element types and is the supertype of all vector types.
  Only read-only vector operations (e.g. @racket[vector-ref])
  are allowed on values of this type. This type typically appears in programs
  via the combination of occurrence typing and @racket[vector?].
@ex[(lambda: ([x : Any]) (if (vector? x) x (error "not a vector!")))]
}


@defform[(HashTable k v)]{is the type of a @rtech{hash table} with key type
   @racket[k] and value type @racket[v].

@ex[#hash((a . 1) (b . 2))]
}
@defidform[HashTableTop]{is the type of a @rtech{hash table} with unknown key
  and value types and is the supertype of all hash table types. Only read-only
  hash table operations (e.g.
  @racket[hash-ref]) are allowed on values of this type. This type typically
  appears in programs via the combination of occurrence typing and
  @racket[hash?].
@ex[(lambda: ([x : Any]) (if (hash? x) x (error "not a hash table!")))]
}

@defform[(Setof t)]{is the type of a @rtech{set} of @racket[t].
@ex[(set 0 1 2 3)]
}

@defform[(Channelof t)]{A @rtech{channel} on which only @racket[t]s can be sent.
@ex[
(ann (make-channel) (Channelof Symbol))
]
}

@defidform[ChannelTop]{is the type of a @rtech{channel} with unknown
  message type and is the supertype of all channel types. This type typically
  appears in programs via the combination of occurrence typing and
  @racket[channel?].
@ex[(lambda: ([x : Any]) (if (channel? x) x (error "not a channel!")))]
}

@defform[(Async-Channelof t)]{An @rtech{asynchronous channel} on which only @racket[t]s can be sent.
@ex[
(require typed/racket/async-channel)
(ann (make-async-channel) (Async-Channelof Symbol))
]
@history[#:added "1.1"]
}

@defidform[Async-ChannelTop]{is the type of an @rtech{asynchronous channel} with unknown
  message type and is the supertype of all asynchronous channel types. This type typically
  appears in programs via the combination of occurrence typing and
  @racket[async-channel?].
@ex[(require typed/racket/async-channel)
    (lambda: ([x : Any]) (if (async-channel? x) x (error "not an async-channel!")))]
@history[#:added "1.1"]
}

@defform*[[(Parameterof t)
           (Parameterof s t)]]{A @rtech{parameter} of @racket[t].  If two type arguments are supplied,
                                 the first is the type the parameter accepts, and the second is the type returned.
@ex[current-input-port
    current-directory]
}

@defform[(Promise t)]{A @rtech{promise} of @racket[t].
 @ex[(delay 3)]}

@defform[(Futureof t)]{A @rtech{future} which produce a value of type @racket[t] when touched.}

@defform[(Sequenceof t)]{A @rtech{sequence} that produces values of
type @racket[_t] on each iteration.}

@defform[(Custodian-Boxof t)]{A @rtech{custodian box} of @racket[t].}
@defform[(Thread-Cellof t)]{A @rtech{thread cell} of @racket[t].}
@defidform[Thread-CellTop]{is the type of a @rtech{thread cell} with unknown
  element type and is the supertype of all thread cell types. This type typically
  appears in programs via the combination of occurrence typing and
  @racket[thread-cell?].
@ex[(lambda: ([x : Any]) (if (thread-cell? x) x (error "not a thread cell!")))]
}

@defform[(Ephemeronof t)]{An @rtech{ephemeron} whose value is of type @racket[t].}

@defform[(Evtof t)]{A @rtech{synchronizable event} whose @rtech{synchronization result}
  is of type @racket[t].

  @ex[always-evt
      (system-idle-evt)
      (ann (thread (λ () (displayln "hello world"))) (Evtof Thread))]
}

@section{Syntax Objects}

The following types represent @rtech{syntax object}s and their content.

@defform[(Syntaxof t)]{A syntax object with content of type @racket[t].
Applying @racket[syntax-e] to a value of type @racket[(Syntaxof t)] produces a
value of type @racket[t].}

@defidform[Identifier]{A syntax object containing a @rtech{symbol}.  Equivalent
to @racket[(Syntaxof Symbol)].}

@defidform[Syntax]{A syntax object containing only @rtech{symbol}s,
@rtech{keyword}s, @rtech{string}s, @rtech{character}s, @rtech{boolean}s,
@rtech{number}s, @rtech{box}es containing @racket[Syntax], @rtech{vector}s of
@racket[Syntax], or (possibly improper) @rtech{list}s of @racket[Syntax].
Equivalent to @racket[(Syntaxof Syntax-E)].}

@defidform[Syntax-E]{The content of syntax objects of type @racket[Syntax].
Applying @racket[syntax-e] to a value of type @racket[Syntax] produces a value
of type @racket[Syntax-E].}

@defform[(Sexpof t)]{The recursive union of @racket[t] with @rtech{symbol}s,
@rtech{keyword}s, @rtech{string}s, @rtech{character}s, @rtech{boolean}s,
@rtech{number}s, @rtech{box}es, @rtech{vector}s, and (possibly improper)
@rtech{list}s.}

@defidform[Sexp]{Applying @racket[syntax->datum] to a value of type
@racket[Syntax] produces a value of type @racket[Sexp].  Equivalent to
@racket[(Sexpof Nothing)].}

@defidform[Datum]{Applying @racket[datum->syntax] to a value of type
@racket[Datum] produces a value of type @racket[Syntax].  Equivalent to
@racket[(Sexpof Syntax)].}


@section{Control}

The following types represent @rtech{prompt tag}s and
keys for @rtech{continuation mark}s for use with delimited continuation
functions and continuation mark functions.

@defform[(Prompt-Tagof s t)]{
  A prompt tag to be used in a continuation prompt whose body
  produces the type @racket[_s] and whose handler has the type
  @racket[_t]. The type @racket[_t] must be a function type.

  The domain of @racket[_t] determines the type of the values
  that can be aborted, using @racket[abort-current-continuation],
  to a prompt with this prompt tag.

  @ex[(make-continuation-prompt-tag 'prompt-tag)]
}

@defidform[Prompt-TagTop]{is the type of a @rtech{prompt tag} with unknown
  body and handler types and is the supertype of all prompt tag types. This type
  typically appears in programs via the combination of occurrence typing
  and @racket[continuation-prompt-tag?].
@ex[(lambda: ([x : Any]) (if (continuation-prompt-tag? x) x (error "not a prompt tag!")))]
}

@defform[(Continuation-Mark-Keyof t)]{
  A continuation mark key that is used for continuation mark
  operations such as @racket[with-continuation-mark] and
  @racket[continuation-mark-set->list]. The type @racket[_t]
  represents the type of the data that is stored in the
  continuation mark with this key.

  @ex[(make-continuation-mark-key 'mark-key)]
}

@defidform[Continuation-Mark-KeyTop]{is the type of a continuation mark
  key with unknown element type and is the supertype of all continuation mark key
  types. This type typically appears in programs
  via the combination of occurrence typing and @racket[continuation-mark-key?].
@ex[(lambda: ([x : Any]) (if (continuation-mark-key? x) x (error "not a mark key!")))]
}


@section{Other Type Constructors}

@defform*/subs[#:id -> #:literals (|@| * ... ! and or implies car cdr)
               [(-> dom ... rng optional-filter)
                (-> dom ... rest * rng)
                (-> dom ... rest ooo bound rng)

                (dom ... -> rng optional-filter)
                (dom ... rest * -> rng)
                (dom ... rest ooo bound -> rng)]
               ([ooo #,(racket ...)]
                [dom type
                     mandatory-kw
                     optional-kw]
                [mandatory-kw (code:line keyword type)]
                [optional-kw [keyword type]]
                [optional-filter (code:line)
                                 (code:line : type)
                                 (code:line : pos-filter neg-filter object)]
                [pos-filter (code:line)
                            (code:line #:+ proposition ...)]
                [neg-filter (code:line)
                            (code:line #:- proposition ...)]
                [object (code:line)
                        (code:line #:object index)]
                [proposition type
                             (! type)
                             (type |@| path-elem ... index)
                             (! type |@| path-elem ... index)
                             (and proposition ...)
                             (or proposition ...)
                             (implies proposition ...)]
                [path-elem car cdr]
                [index positive-integer
                       (positive-integer positive-integer)
                       identifier])]{
  The type of functions from the (possibly-empty)
  sequence @racket[dom ....] to the @racket[rng] type.

  @ex[(λ: ([x : Number]) x)
      (λ: () 'hello)]

  The second form specifies a uniform rest argument of type @racket[rest], and the
  third form specifies a non-uniform rest argument of type
  @racket[rest] with bound @racket[bound]. The bound refers to the type variable
  that is in scope within the rest argument type.

  @ex[(λ: ([x : Number] . [y : String *]) (length y))
      ormap]

  In the third form, the @racket[...] introduced by @racket[ooo] is literal,
  and @racket[bound] must be an identifier denoting a type variable.

  The @racket[dom]s can include both mandatory and optional keyword arguments.
  Mandatory keyword arguments are a pair of keyword and type, while optional
  arguments are surrounded by a pair of parentheses.

  @ex[(:print-type file->string)
      (: is-zero? : (-> Number #:equality (-> Number Number Any) [#:zero Number] Any))
      (define (is-zero? n #:equality equality #:zero [zero 0])
        (equality n zero))
      (is-zero? 2 #:equality =)
      (is-zero? 2 #:equality eq? #:zero 2.0)]

  When @racket[optional-filter] is provided, it specifies the @emph{filter} for the
  function type (for an introduction to filters, see @tr-guide-secref["filters-and-predicates"]).
  For almost all use cases, only the simplest form of filters, with a single type after a
  @racket[:], are necessary:

  @ex[string?]

  The filter specifies that when @racket[(string? x)] evaluates to a true value, the
  variable @racket[x] can be assumed to have type @racket[String]. Likewise, if the
  expression evaluates to @racket[#f], the variable has type @racket[String].

  In some cases, asymmetric type information is useful in filters. For example, the
  @racket[filter] function's first argument is specified with only a positive filter:

  @ex[filter]

  The use of @racket[#:+] indicates that when the function applied to a variable
  evaluates to a true value, the given type can be assumed for the variable. However,
  the type-checker gains no information in branches in which the result is @racket[#f].

  Conversely, @racket[#:-] specifies that a function provides information for the
  false branch of a conditional.

  The other filter proposition cases are rarely needed, but the grammar documents them
  for completeness. They correspond to logical operations on the propositions.

  The type of functions can also be specified with an @emph{infix} @racket[->]
  which comes immediately before the @racket[rng] type. The fourth through
  sixth forms match the first three cases, but with the infix style of arrow.

  @ex[(: add2 (Number -> Number))
      (define (add2 n) (+ n 2))]
}

@;; This is a trick to get a reference to ->* in another manual
@(module id-holder racket/base
   (require scribble/manual (for-label racket/contract))
   (provide ->*-element)
   (define ->*-element (racket ->*)))
@(require 'id-holder)

@defform[#:literals (* ...)
         (->* (mandatory-dom ...) optional-doms rest rng)
         #:grammar
         ([mandatory-dom type
                         (code:line keyword type)]
          [optional-doms (code:line)
                         (optional-dom ...)]
          [optional-dom type
                        (code:line keyword type)]
          [rest (code:line)
                (code:line #:rest type)])]{
  Constructs the type of functions with optional or rest arguments. The first
  list of @racket[mandatory-dom]s correspond to mandatory argument types. The list
  @racket[optional-doms], if provided, specifies the optional argument types.

  @ex[(: append-bar (->* (String) (Positive-Integer) String))
      (define (append-bar str [how-many 1])
        (apply string-append str (make-list how-many "bar")))]

  If provided, the @racket[rest] expression specifies the type of
  elements in the rest argument list.

  @ex[(: +all (->* (Integer) #:rest Integer (Listof Integer)))
      (define (+all inc . rst)
        (map (λ: ([x : Integer]) (+ x inc)) rst))
      (+all 20 1 2 3)]

  Both the mandatory and optional argument lists may contain keywords paired
  with types.

  @ex[(: kw-f (->* (#:x Integer) (#:y Integer) Integer))
      (define (kw-f #:x x #:y [y 0]) (+ x y))]

  The syntax for this type constructor matches the syntax of the @->*-element
  contract combinator, but with types instead of contracts.
  }

@deftogether[(
@defidform[Top]
@defidform[Bot])]{ These are filters that can be used with @racket[->].
  @racket[Top] is the filter with no information.
  @racket[Bot] is the filter which means the result cannot happen.
}


@defidform[Procedure]{is the supertype of all function types. The @racket[Procedure]
  type corresponds to values that satisfy the @racket[procedure?] predicate.
  Because this type encodes @emph{only} the fact that the value is a procedure, and
  @emph{not} its argument types or even arity, the type-checker cannot allow values
  of this type to be applied.

  For the types of functions with known arity and argument types, see the @racket[->]
  type constructor.

  @ex[
    (: my-list Procedure)
    (define my-list list)
    (my-list "zwiebelkuchen" "socca")
  ]
}


@defform[(U t ...)]{is the union of the types @racket[t ...].
 @ex[(λ: ([x : Real])(if (> 0 x) "yes" 'no))]}
@defform[(case-> fun-ty ...)]{is a function that behaves like all of
  the @racket[fun-ty]s, considered in order from first to last.  The @racket[fun-ty]s must all be function
  types constructed with @racket[->].
  @ex[(: add-map : (case->
                     [(Listof Integer) -> (Listof Integer)]
                     [(Listof Integer) (Listof Integer) -> (Listof Integer)]))]
  For the definition of @racket[add-map] look into @racket[case-lambda:].}

@defform/none[(t t1 t2 ...)]{is the instantiation of the parametric type
  @racket[t] at types @racket[t1 t2 ...]}
@defform*[[(All (a ...) t)
           (All (a ... a ooo) t)]]{
  is a parameterization of type @racket[t], with
  type variables @racket[v ...].  If @racket[t] is a function type
      constructed with infix @racket[->], the outer pair of parentheses
      around the function type may be omitted.
      @ex[(: list-length : (All (A) (Listof A) -> Natural))
          (define (list-length lst)
            (if (null? lst)
                0
                (add1 (list-length (cdr lst)))))
          (list-length (list 1 2 3))]}

@defform[(Values t ...)]{is the type of a sequence of multiple values, with
types @racket[t ...].  This can only appear as the return type of a
function.
@ex[(values 1 2 3)]}
@defform/none[v]{where @racket[v] is a number, boolean or string, is the singleton type containing only that value}
@defform/none[(quote val)]{where @racket[val] is a Racket value, is the singleton type containing only that value}
@defform/none[i]{where @racket[i] is an identifier can be a reference to a type
name or a type variable}
@defform[(Rec n t)]{is a recursive type where @racket[n] is bound to the
recursive type in the body @racket[t]
@ex[(define-type IntList (Rec List (Pair Integer (U List Null))))

    (define-type (List A) (Rec List (Pair A (U List Null))))]}

@defform[(Struct st)]{is a type which is a supertype of all instances of the
potentially-polymorphic structure type @racket[_st].  Note that structure
accessors for @racket[_st] will @emph{not} accept @racket[(Struct st)] as an
argument.}

@defform[(Struct-Type st)]{is a type for the structure type descriptor value
for the structure type @racket[st]. Values of this type are used with
reflective operations such as @racket[struct-type-info].

@ex[struct:arity-at-least
    (struct-type-info struct:arity-at-least)]
}

@defidform[Struct-TypeTop]{is the supertype of all types for structure type
descriptor values. The corresponding structure type is unknown for values of
this top type.

@ex[(struct-info (arity-at-least 0))]
}

@defalias[→ ->]
@defalias[case→ case->]
@defalias[∀ All]

@section{Other Types}

@defform[(Option t)]{Either @racket[t] or @racket[#f]}
@defform[(Opaque t)]{A type constructed using the @racket[#:opaque]
clause of @racket[require/typed].}

@(close-eval the-eval)
