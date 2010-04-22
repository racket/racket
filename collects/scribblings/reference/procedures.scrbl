#lang scribble/doc
@(require "mz.ss")

@title[#:tag "procedures"]{Procedures}

@defproc[(procedure? [v any/c]) boolean]{ Returns @scheme[#t] if
@scheme[v] is a procedure, @scheme[#f] otherwise.}


@defproc[(apply [proc procedure?] 
                [v any/c] ... [lst list?]
                [#:<kw> kw-arg any/c] ...) any]{

@guideintro["apply"]{@scheme[apply]}

Applies @scheme[proc] using the content of @scheme[(list* v ... lst)]
as the (by-position) arguments. The @scheme[#:<kw> kw-arg] sequence is
also supplied as keyword arguments to @scheme[proc], where
@scheme[#:<kw>] stands for any keyword.

The given @scheme[proc] must accept as many arguments as the number of
@scheme[v]s plus length of @scheme[lst], it must accept the supplied
keyword arguments, and it must not require any other keyword
arguments; otherwise, the @exnraise[exn:fail:contract]. The given
@scheme[proc] is called in tail position with respect to the
@scheme[apply] call.

@mz-examples[
(apply + '(1 2 3))
(apply + 1 2 '(3))
(apply + '())
(apply sort (list (list '(2) '(1)) <) #:key car)
]}

@defproc[(compose [proc procedure?] ...) procedure?]{

Returns a procedure that composes the given functions, applying the
last @scheme[proc] first and the first @scheme[proc] last. The
composed functions can consume and produce any number of values, as
long as each function produces as many values as the preceding
function consumes.  When no @scheme[proc] arguments are given, the
result is @scheme[values].

@mz-examples[
((compose - sqrt) 10)
((compose sqrt -) 10)
((compose list split-path) (bytes->path #"/a" 'unix))
]}

@defproc[(procedure-rename [proc procedure?]
                           [name symbol?])
         procedure?]{

Returns a procedure that is like @scheme[proc], except that its name
as returned by @scheme[object-name] (and as printed for debugging) is
@scheme[name].

The given @scheme[name] is used for printing an error message if the
resulting procedure is applied to the wrong number of arguments.  In
addition, if @scheme[proc] is an @tech{accessor} or @tech{mutator}
produced by @scheme[define-struct],
@scheme[make-struct-field-accessor], or
@scheme[make-struct-field-mutator], the resulting procedure also uses
@scheme[name] when its (first) argument has the wrong type. More
typically, however, @scheme[name] is not used for reporting errors,
since the procedure name is typically hard-wired into an internal
check.}
                    
@defproc[(procedure->method [proc procedure?])
         procedure?]{
                     
Returns a procedure that is like @scheme[proc] except that, when applied
to the wrong number of arguments, the resulting error hides the first
argument as if the procedure had been compiled with the
@indexed-scheme['method-arity-error] syntax property.}

@; ----------------------------------------
@section{Keywords and Arity}

@defproc[(keyword-apply [proc procedure?]
                        [kw-lst (listof keyword?)]
                        [kw-val-lst list?]
                        [v any/c] ...
                        [lst list?]
                        [#:<kw> kw-arg any/c] ...)
         any]{

@guideintro["apply"]{@scheme[keyword-apply]}

Like @scheme[apply], but @scheme[kw-lst] and @scheme[kw-val-lst]
supply by-keyword arguments in addition to the by-position arguments
of the @scheme[v]s and @scheme[lst], and in addition to the directly
supplied keyword arguments in the @scheme[#:<kw> kw-arg] sequence,
where @scheme[#:<kw>] stands for any keyword.

The given @scheme[kw-lst] must be sorted using @scheme[keyword<?].  No
keyword can appear twice in @scheme[kw-lst] or in both
@scheme[kw-list] and as a @scheme[#:<kw>], otherwise, the
@exnraise[exn:fail:contract]. The given @scheme[kw-val-lst] must have
the same length as @scheme[kw-lst], otherwise, the
@exnraise[exn:fail:contract]. The given @scheme[proc] must accept all
of the keywords in @scheme[kw-lst] plus the @scheme[#:<kw>]s, it must
not require any other keywords, and it must accept as many by-position
arguments as supplied via the @scheme[v]s and @scheme[lst]; otherwise,
the @exnraise[exn:fail:contract].

@defexamples[
(define (f x #:y y #:z [z 10])
  (list x y z))
(keyword-apply f '(#:y) '(2) '(1))
(keyword-apply f '(#:y #:z) '(2 3) '(1))
(keyword-apply f #:z 7 '(#:y) '(2) '(1))
]}

@defproc[(procedure-arity [proc procedure?])
         procedure-arity?]{

Returns information about the number of by-position arguments accepted
by @scheme[proc]. See also @scheme[procedure-arity?].}

@defproc[(procedure-arity? [v any/c]) boolean?]{

A valid arity @scheme[_a] is one of the following:

@itemize[

  @item{An exact non-negative integer, which means that the procedure
        accepts @scheme[_a] arguments, only.}

 @item{A @scheme[arity-at-least] instance, which means that the
       procedure accepts @scheme[(arity-at-least-value _a)] or more
       arguments.}

 @item{A list containing integers and @scheme[arity-at-least]
       instances, which means that the procedure accepts any number of
       arguments that can match one of the elements of @scheme[_a].}

]

Generally, @scheme[procedure-arity] always produces an arity that is normalized. 
Specifically, it is either the empty list (corresponding to the procedure 
@scheme[(case-lambda)]), one of the first two cases above, or a list
that contains at least two elements. If it is a list, there is at most one
@scheme[arity-at-least] instance that appears as the last element of the list,
all of the other elements are sorted in ascending order, and there are no duplicate
elements.

@mz-examples[
(procedure-arity cons)
(procedure-arity list)
(arity-at-least? (procedure-arity list))
(arity-at-least-value (procedure-arity list))
(arity-at-least-value (procedure-arity (lambda (x . y) x)))
(procedure-arity (case-lambda [(x) 0] [(x y) 1]))
]}

@defproc[(procedure-arity-includes? [proc procedure?] [k exact-nonnegative-integer?])
         boolean?]{

Returns @scheme[#t] if the procedure can accept @scheme[k] arguments
when no keyword arguments are supplied, @scheme[#f] otherwise.

@mz-examples[
(procedure-arity-includes? cons 2)
(procedure-arity-includes? display 3)
]}

@defproc[(procedure-reduce-arity [proc procedure?]
                                 [arity procedure-arity?])
         procedure?]{

Returns a procedure that is the same as @scheme[proc] (including
the same name returned by @scheme[object-name]), but that accepts
only arguments consistent with @scheme[arity]. In particular,
when @scheme[procedure-arity] is applied to the generated
procedure, it returns a value that is @scheme[equal?] to
@scheme[arity].

If the @scheme[arity] specification allows arguments that are not in
@scheme[(procedure-arity proc)], the @exnraise[exn:fail:contract].  If
@scheme[proc] accepts keyword argument, either the keyword arguments
must be all optional (and they are not accepted in by the
arity-reduced procedure) or @scheme[arity] must be the empty list
(which makes a procedure that cannot be called); otherwise, the
@exnraise[exn:fail:contract].

@examples[
(define my+ (procedure-reduce-arity + 2))
(my+ 1 2)
(my+ 1 2 3)
]}

@defproc[(procedure-keywords [proc procedure?])
         (values
          (listof keyword?)
          (or/c (listof keyword?) #f))]{

Returns information about the keyword arguments required and accepted
by a procedure. The first result is a list of keywords (sorted by
@scheme[keyword<?]) that are required when applying @scheme[proc]. The
second result is a list of accepted keywords (sorted by
@scheme[keyword<?]), or @scheme[#f] to mean that any keyword is
accepted. When the second result is a list, every element in the first
list is also in the second list.

@mz-examples[
(procedure-keywords +)
(procedure-keywords (lambda (#:tag t #:mode m) t))
(procedure-keywords (lambda (#:tag t #:mode [m #f]) t))
]}

@defproc[(make-keyword-procedure
          [proc (((listof keyword?) list?) () #:rest list? . ->* . any)]
          [plain-proc procedure? (lambda args (apply proc null null args))])
         procedure?]{

Returns a procedure that accepts all keyword arguments (without
requiring any keyword arguments). See also
@scheme[procedure-reduce-keyword-arity].

When the result is called with keyword arguments, then @scheme[proc]
is called; the first argument is a list of keywords sorted by
@scheme[keyword<?], the second argument is a parallel list containing a
value for each keyword, and the remaining arguments are the
by-position arguments.

When the result is called without keyword arguments, then
@scheme[plain-proc] is called. Furthermore, @scheme[procedure-arity]
obtains its result from @scheme[plain-proc].

@defexamples[
(define show
  (make-keyword-procedure (lambda (kws kw-args . rest)
                            (list kws kw-args rest))))

(show 1)
(show #:init 0 1 2 3 #:extra 4)
]}

@defproc[(procedure-reduce-keyword-arity [proc procedure?]
                                         [arity procedure-arity?]
                                         [required-kws (listof keyword?)]
                                         [allowed-kws (or/c (listof keyword?)
                                                            #f)])
         procedure?]{

Like @scheme[procedure-reduce-arity], but constrains the keyword
arguments according to @scheme[required-kws] and @scheme[allowed-kws],
which must be sorted using @scheme[keyword<?]. If @scheme[allowed-kws]
is @scheme[#f], then the resulting procedure still accepts any
keyword, otherwise the keywords in @scheme[required-kws] must be a
subset of those in @scheme[allowed-kws]. The original @scheme[proc]
must require no more keywords than the ones listed din
@scheme[required-kws], and it must allow at least the keywords in
@scheme[allowed-kws] (or it must allow all keywords if
@scheme[allowed-kws] is @scheme[#f]).

@defexamples[
(define orig-show
  (make-keyword-procedure (lambda (kws kw-args . rest)
                            (list kws kw-args rest))))
(define show (procedure-reduce-keyword-arity 
              orig-show 3 '(#:init) '(#:extra #:init)))
(show #:init 0 1 2 3 #:extra 4)
(show 1)
(show #:init 0 1 2 3 #:extra 4 #:more 7)
]}

@defstruct[arity-at-least ([value exact-nonnegative-integer?])]{

A structure type used for the result of @scheme[procedure-arity].
See also @scheme[procedure-arity?].}


@defthing[prop:procedure struct-type-property?]{

A @tech{structure type property} to identify structure types whose
instances can be applied as procedures. In particular, when
@scheme[procedure?] is applied to the instance, the result will be
@scheme[#t], and when an instance is used in the function position of
an application expression, a procedure is extracted from the instance
and used to complete the procedure call.

If the @scheme[prop:procedure] property value is an exact non-negative integer, it
designates a field within the structure that should contain a
procedure. The integer must be between @scheme[0] (inclusive) and the
number of non-automatic fields in the structure type (exclusive, not
counting supertype fields). The designated field must also be
specified as immutable, so that after an instance of the structure is
created, its procedure cannot be changed. (Otherwise, the arity and
name of the instance could change, and such mutations are generally
not allowed for procedures.) When the instance is used as the
procedure in an application expression, the value of the designated
field in the instance is used to complete the procedure call. (This
procedure can be another structure that acts as a procedure; the
immutability of procedure fields disallows cycles in the procedure
graph, so that the procedure call will eventually continue with a
non-structure procedure.) That procedure receives all of the arguments
from the application expression. The procedure's name (see
@scheme[object-name]), arity (see @scheme[procedure-arity]), and
keyword protocol (see @scheme[procedure-keywords]) are also used for
the name, arity, and keyword protocol of the structure. If the value
in the designated field is not a procedure, then the instance behaves
like @scheme[(case-lambda)] (i.e., a procedure which does not accept
any number of arguments). See also @scheme[procedure-extract-target].

Providing an integer @scheme[proc-spec] argument to
@scheme[make-struct-type] is the same as both supplying the value with
the @scheme[prop:procedure] property and designating the field as
immutable (so that a property binding or immutable designation is
redundant and disallowed).

@examples[
(define-struct annotated-proc (base note)
               #:property prop:procedure 
                          (struct-field-index base))
(define plus1 (make-annotated-proc
                (lambda (x) (+ x 1))
                "adds 1 to its argument"))
(procedure? plus1)
(annotated-proc? plus1)
(plus1 10)
(annotated-proc-note plus1)
]

When the @scheme[prop:procedure] value is a procedure, it should
accept at least one non-keyword argument. When an instance of the
structure is used in an application expression, the property-value
procedure is called with the instance as the first argument. The
remaining arguments to the property-value procedure are the arguments
from the application expression (including keyword arguments). Thus,
if the application expression provides five non-keyword arguments, the
property-value procedure is called with six non-keyword arguments. The
name of the instance (see @scheme[object-name]) and its keyword
protocol (see @scheme[procedure-keywords]) are unaffected by the
property-value procedure, but the instance's arity is determined by
subtracting one from every possible non-keyword argument count of the
property-value procedure. If the property-value procedure cannot
accept at least one argument, then the instance behaves like
@scheme[(case-lambda)].

Providing a procedure @scheme[proc-spec] argument to
@scheme[make-struct-type] is the same as supplying the value with the
@scheme[prop:procedure] property (so that a specific property binding
is disallowed).

@mz-examples[
(define-struct fish (weight color)
               #:mutable
               #:property 
               prop:procedure  
               (lambda (f n) 
                 (let ([w (fish-weight f)])
                  (set-fish-weight! f (+ n w)))))
(define wanda (make-fish 12 'red))
(fish? wanda)
(procedure? wanda)
(fish-weight wanda)
(for-each wanda '(1 2 3))
(fish-weight wanda)
]

If the value supplied for the @scheme[prop:procedure] property is not
an exact non-negative integer or a procedure, the
@exnraise[exn:fail:contract].}

@defproc[(procedure-struct-type? [type struct-type?]) boolean?]{

Returns @scheme[#t] if instances of the structure type represented by
@scheme[type] are procedures (according to @scheme[procedure?]),
@scheme[#f] otherwise.}

@defproc[(procedure-extract-target [proc procedure?]) (or/c #f procedure?)]{

If @scheme[proc] is an instance of a structure type with property
@scheme[prop:procedure], and if the property value indicates a field
of the structure, and if the field value is a procedure, then
@scheme[procedure-extract-target] returns the field value. Otherwise,
the result if @scheme[#f].

When a @scheme[prop:procedure] property value is a procedure, the
procedure is @emph{not} returned by
@scheme[procedure-extract-target]. Such a procedure is different from
one accessed through a structure field, because it consumes an extra
argument, which is always the structure that was applied as a
procedure. Keeping the procedure private ensures that is it always
called with a suitable first argument.}

@defthing[prop:arity-string struct-type-property?]{

A @tech{structure type property} that is used for reporting arity-mismatch errors when a
structure type with the @scheme[prop:procedure] property is applied to
the wrong number of arguments. The value of the
@scheme[prop:arity-string] property must be a procedure that takes a
single argument, which is the misapplied structure, and returns a
string. The result string is used after the word ``expects,'' and it
is followed in the error message by the number of actual arguments.

Arity-mismatch reporting automatically uses
@scheme[procedure-extract-target] when the @scheme[prop:arity-string]
property is not associated with a procedure structure type.

@examples[
(define-struct evens (proc)
  #:property prop:procedure (struct-field-index proc)
  #:property prop:arity-string
  (lambda (p)
    "an even number of arguments"))

(define pairs
  (make-evens
   (case-lambda
    [() null]
    [(a b . more)
     (cons (cons a b)
           (apply pairs more))])))

(pairs 1 2 3 4)
(pairs 5)]}


@defthing[prop:checked-procedure struct-type-property?]{

A @tech{structure type property} that is used with
@scheme[checked-procedure-check-and-extract], which is a hook to allow
the compiler to improve the performance of keyword arguments. The
property can only be attached to a @tech{structure type} without a
supertype and with at least two fields.}


@defproc[(checked-procedure-check-and-extract [type struct-type?]
                                              [v any/c]
                                              [proc (any/c any/c any/c . -> . any/c)]
                                              [v1 any/c]
                                              [v2 any/c]) any/c]{

Extracts a value from @scheme[v] if it is an instance of
@scheme[type], which must have the property
@scheme[prop:checked-procedure]. If @scheme[v] is such an instance,
then the first field of @scheme[v] is extracted and applied to
@scheme[v1] and @scheme[v2]; if the result is a true value, the result
is the value of the second field of @scheme[v].

If @scheme[v] is not an instance of @scheme[type], or if the first
field of @scheme[v] applied to @scheme[v1] and @scheme[v2] produces
@scheme[#f], then @scheme[proc] is applied to @scheme[v], @scheme[v1],
and @scheme[v2], and its result is returned by
@scheme[checked-procedure-check-and-extract].}

@; ----------------------------------------------------------------------

@section{Reflecting on Primitives}

A @idefterm{primitive procedure} is a built-in procedure that is
implemented in low-level language. Not all procedures of
@schememodname[racket/base] are primitives, but many are. The
distinction is mainly useful to other low-level code.

@defproc[(primitive? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a primitive procedure,
@scheme[#f] otherwise.}

@defproc[(primitive-closure? [v any/c]) boolean]{

Returns @scheme[#t] if @scheme[v] is internally implemented as a
primitive closure rather than a simple primitive procedure,
@scheme[#f] otherwise.}


@defproc[(primitive-result-arity [prim primitive?]) procedure-arity?]{

Returns the arity of the result of the primitive procedure
@scheme[prim] (as opposed to the procedure's input arity as returned
by @scheme[procedure-arity]). For most primitives, this procedure
returns @scheme[1], since most primitives return a single value when
applied.}

@; ----------------------------------------
@section{Additional Procedure Functions}

@note-lib[racket/function]
@(define fun-eval (make-base-eval))
@(interaction-eval #:eval fun-eval (require racket/function))

@defproc[(const [v any]) procedure?]{

Returns a procedure that accepts any arguments and returns @scheme[v].

@mz-examples[#:eval fun-eval
((const 'foo) 1 2 3)
((const 'foo))
]}

@defproc[(negate [proc procedure?]) procedure?]{

Returns a procedure that is just like @scheme[proc], except that it
returns the @scheme[not] of @scheme[proc]'s result.

@mz-examples[#:eval fun-eval
(filter (negate symbol?) '(1 a 2 b 3 c))
(map (negate =) '(1 2 3) '(1 1 1))
]}

@defproc*[([(curry [proc procedure?]) procedure?]
           [(curry [proc procedure?] [v any/c] ...+) any/c])]{

Returns a procedure that is a curried version of @scheme[proc]. When
the resulting procedure is first applied, unless it is given the
maximum number of arguments that it can accept, the result is a
procedure to accept additional arguments.

@mz-examples[#:eval fun-eval
((curry list) 1 2)
((curry cons) 1)
((curry cons) 1 2)
]

After the first application of the result of @scheme[curry], each
further application accumulates arguments until an acceptable number
of arguments have been accumulated, at which point the original
@scheme[proc] is called.

@mz-examples[#:eval fun-eval
(((curry list) 1 2) 3)
(((curry list) 1) 3)
((((curry foldl) +) 0) '(1 2 3))
]

A function call @scheme[(curry proc v ...)] is equivalent to
@scheme[((curry proc) v ...)]. In other words, @scheme[curry] itself
is curried.

The @scheme[curry] function provides limited support for keyworded
functions: only the @scheme[curry] call itself can receive keyworded
arguments to be propagated eventually to @scheme[proc].

@mz-examples[#:eval fun-eval
  (map ((curry +) 10) '(1 2 3))
  (map (curry + 10) '(1 2 3))
  (map (compose (curry * 2) (curry + 10)) '(1 2 3))
  (define foo (curry (lambda (x y z) (list x y z))))
  (foo 1 2 3)
  (((((foo) 1) 2)) 3)
]}


@defproc*[([(curryr [proc procedure?]) procedure?]
           [(curryr [proc procedure?] [v any/c] ...+) any/c])]{

Like @scheme[curry], except that the arguments are collected in the
opposite direction: the first step collects the rightmost group of
arguments, and following steps add arguments to the left of these.

@mz-examples[#:eval fun-eval
  (map (curryr list 'foo) '(1 2 3))
]}


@close-eval[fun-eval]
