#lang scribble/manual

@begin[(require "../utils.rkt" scribble/eval racket/sandbox)
       (require (for-label (only-meta-in 0 [except-in typed/racket for])))]

@(define the-eval (make-base-eval))
@(the-eval '(require (except-in typed/racket #%top-interaction #%module-begin)))
@(define the-top-eval (make-base-eval))
@(the-top-eval '(require (except-in typed/racket #%module-begin)))

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

These types represent the hierarchy of @rtech{numbers} of Racket.

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
range-bounded types.
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


The following base types are parameteric in their type arguments.

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
(map symbol->string (list 'a 'b 'c))
]

@defform[(MListof t)]{Homogenous @rtech{mutable lists} of @racket[t].}
@defform[(MPairof t u)]{@rtech{Mutable pairs} of @racket[t] and @racket[u].}

@defform[(Boxof t)]{A @rtech{box} of @racket[t]}

@ex[(box "hello world")]

@defform[(Vectorof t)]{Homogenous @rtech{vectors} of @racket[t]}
@defform[(Vector t ...)]{is the type of the list with one element, in order,
  for each type provided to the @racket[Vector] type constructor.}
@defidform[FlVector]{An @rtech{flvector}.}

@ex[(vector 1 2 3)
#(a b c)]

@defform[(HashTable k v)]{is the type of a @rtech{hash table} with key type
   @racket[k] and value type @racket[v].

@ex[#hash((a . 1) (b . 2))]
}

@defform[(Setof t)]{is the type of a @rtech{set} of @racket[t].
@ex[(set 0 1 2 3)]
}

@defform[(Channelof t)]{A @rtech{channel} on which only @racket[t]s can be sent.
@ex[
(ann (make-channel) (Channelof Symbol))
]
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

@defform[(Ephemeronof t)]{An @rtech{ephemeron} whose value is of type @racket[t].}


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

@defform[(Continuation-Mark-Keyof t)]{
  A continuation mark key that is used for continuation mark
  operations such as @racket[with-continuation-mark] and
  @racket[continuation-mark-set->list]. The type @racket[_t]
  represents the type of the data that is stored in the
  continuation mark with this key.

  @ex[(make-continuation-mark-key 'mark-key)]
}


@section{Other Type Constructors}

@defform*[#:id -> #:literals (* ...)
	       [(dom ... -> rng)
	        (dom ... rest * -> rng)
		(dom ... rest #,(racket ...) bound -> rng)
                (dom -> rng : pred)]]{is the type of functions from the (possibly-empty)
  sequence @racket[dom ...] to the @racket[rng] type.  The second form
  specifies a uniform rest argument of type @racket[rest], and the
  third form specifies a non-uniform rest argument of type
  @racket[rest] with bound @racket[bound].  In the third form, the
  second occurrence of @racket[...] is literal, and @racket[bound]
  must be an identifier denoting a type variable. In the fourth form,
  there must be only one @racket[dom] and @racket[pred] is the type
  checked by the predicate.

  @ex[(λ: ([x : Number]) x)
      (λ: ([x : Number] . [y : String *]) (length y))
      ormap
      string?]}

@defidform[Procedure]{is the supertype of all function types.}


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
@defform[(All (v ...) t)]{is a parameterization of type @racket[t], with
  type variables @racket[v ...].  If @racket[t] is a function type
      constructed with @racket[->], the outer pair of parentheses
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

@defalias[→ ->]
@defalias[case→ case->]
@defalias[∀ All]

@section{Other Types}

@defform[(Option t)]{Either @racket[t] or @racket[#f]}
@defform[(Opaque t)]{A type constructed using @racket[require-opaque-type].}

@(close-eval the-eval)
