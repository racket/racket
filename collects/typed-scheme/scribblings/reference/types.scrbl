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

@defnums[(
Number
Complex
Float-Complex
Real
Float
Nonnegative-Float
Inexact-Real
Exact-Rational
Integer
Natural
Exact-Nonnegative-Integer
Exact-Positive-Integer
Fixnum
Nonnegative-Fixnum
Positive-Fixnum
Zero
Byte
Exact-Number
Float-Negative-Zero
Float-Positive-Zero
Float-Zero
Flonum
Flonum-Negative-Zero
Flonum-Positive-Zero
Flonum-Zero
Index
Inexact-Complex
Inexact-Real-Negative-Zero
Inexact-Real-Positive-Zero
Inexact-Real-Zero
Negative-Exact-Rational
Negative-Float
Negative-Flonum
Negative-Inexact-Real
Negative-Integer
Negative-Real
Negative-Single-Flonum
Nonnegative-Exact-Rational
Nonnegative-Flonum
Nonnegative-Inexact-Real
Nonnegative-Real
Nonnegative-Single-Flonum
Nonpositive-Exact-Rational
Nonpositive-Fixnum
Nonpositive-Float
Nonpositive-Flonum
Nonpositive-Inexact-Real
Nonpositive-Integer
Nonpositive-Real
Nonpositive-Single-Flonum
One
Positive-Byte
Positive-Exact-Rational
Positive-Float
Positive-Flonum
Positive-Index
Positive-Inexact-Real
Positive-Integer
Positive-Real
Positive-Single-Flonum
Real-Zero
Single-Flonum
Single-Flonum-Complex
Single-Flonum-Negative-Zero
Single-Flonum-Positive-Zero
Single-Flonum-Zero
)]{These types represent the hierarchy of @rtech{numbers} of Racket.
@racket[Integer] includes only @rtech{integers} that are @rtech{exact
numbers}, corresponding to the predicate @racket[exact-integer?].
@racket[Real] includes both exact and inexact reals.
An @racket[Inexact-Real] can be either 32- or 64-bit floating-point
numbers. @racket[Float] is restricted to 64-bit floats, which are the
default in Racket.

@ex[
7
8.3
(/ 8 3)
0
-12
3+4i]
}

@subsection{Other Base Types}

@deftogether[(
@defidform[Boolean]
@defidform[True]
@defidform[False]
@defidform[String]
@defidform[Keyword]
@defidform[Symbol]
@defidform[Void]
@defidform[Input-Port]
@defidform[Output-Port]
@defidform[Port]
@defidform[Path]
@defidform[Path-String]
@defidform[Regexp]
@defidform[PRegexp]
@defidform[Byte-Regexp]
@defidform[Byte-PRegexp]
@defidform[Bytes]
@defidform[Namespace]
@defidform[Null]
@defidform[EOF]
@defidform[Continuation-Mark-Set]
@defidform[Char]
@defidform[Undefined]
@defidform[Module-Path]
@defidform[Module-Path-Index]
@defidform[Compiled-Module-Expression]
@defidform[Resolved-Module-Path]
@defidform[Thread])]{
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

@defform[(Sequenceof t ...)]{A @rtech{sequence} that produces values of the
types @racket[_t ...] on each iteration.}

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

@defform[(Ephemeronof t)]{An @rtech{ephemeron} whose value is of type @racket[t].}

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
      @ex[(: list-lenght : (All (A) (Listof A) -> Natural))
          (define (list-lenght lst)
            (if (null? lst)
                0
                (add1 (list-lenght (cdr lst)))))]}

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

@defalias[→ ->]
@defalias[∀ All]

@section{Other Types}

@defform[(Option t)]{Either @racket[t] or @racket[#f]}
@defform[(Opaque t)]{A type constructed using @racket[require-opaque-type].}

@(close-eval the-eval)
