#lang scribble/doc
@(require "mz.ss")

@(define-syntax op
  (syntax-rules ()
    [(_ (x ...)) (x ...)]
    [(_ id) @scheme[id]]))
@(define-syntax-rule (operations i ...)
   (itemlist #:style 'compact @item{@op[i]} ...))

@title[#:tag "chaperones"]{Proxies and Chaperones}

A @deftech{proxy} is a wrapper for a value where the wrapper
redirects certain of the value's operations. Proxies apply only to procedures,
@tech{structures} for which an accessor or mutator is available,
@tech{structure types}, @tech{hash tables}, @tech{vectors},
and @tech{box}es. A proxied value is @scheme[equal?] to the original
value, but not @scheme[eq?] to the original value.

A @deftech{chaperone} is a kind of proxy whose refinement of a value's
operation is restricted to side effects (including, in particular,
raising an exception) or chaperoning values supplied to or produced by
the operation. For example, a vector chaperone can redirect
@scheme[vector-ref] to raise an exception if the accessed vector slot
contains a string, or it can cause the result of @scheme[vector-ref]
to be a chaperoned variant of the value that is in the accessed vector
slot, but it cannot redirect @scheme[vector-ref] to produce a value
that is arbitrarily different from the value in the vector slot.

A non-@tech{chaperone} @tech{proxy}, in contrast, can refine an operation to swap one
value for any another. A proxy cannot be applied to an immutable value
or refine the access to an immutable field in an instance of a @tech{structure
type}, since arbitrary replacement of an operation's value amounts to
mutation of the proxied value.

Beware that each of the following operations can be redirected to
arbitrary procedure through proxies on the operation's
argument---assuming that the operation is available to the creator of
the proxy:

@operations[@t{a structure-field accesor}
            @t{a structure-field mutator}
            @t{a structure type property accessor}
            @t{application of a procedure}
            unbox set-box!
            vector-ref vector-set!
            hash-ref hash-set hash-set! hash-remove hash-remove!]

Derived operations, such as printing a value, can be redirected
through proxies due to their use of accessor functions. The
@scheme[equal?], @scheme[equal-hash-code], and
@scheme[equal-secondary-hash-code] operations, in contrast, may bypass
proxies (but they are not obliged to).

In addition to redirecting operations that work on a value, a
proxy can include @deftech{proxy properties} for a proxied
value. A @tech{proxy property} is similar to a @tech{structure
type property}, but it applies to chaperones instead of structure
types and their instances.


@defproc[(proxy? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a proxy, @scheme[#f] otherwise.

Programs and libraries generally should avoid @scheme[proxy?] and
treat proxies the same as unproxied values. In rare cases,
@scheme[proxy?] may be needed to guard against redirection by a
proxy of an operation to an arbitrary procedure.}


@defproc[(chaperone? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a chaperone, @scheme[#f] otherwise.

Programs and libraries generally should avoid @scheme[chaperone?] for
the same reason that they should avoid @racket[proxy?].}


@defproc[(proxy-of? [v1 any/c] [v2 any/c]) boolean?]{

Indicates whether @scheme[v1] can be considered equivalent modulo
proxies to @scheme[v2].

For values that include no proxies, @scheme[v1] and @scheme[v2] can
be considered proxies of each other if they are @scheme[equal?].

Otherwise, all proxies of @scheme[v2] must be intact in @scheme[v1],
in the sense that parts of @scheme[v2] must be derived from
@scheme[v1] through one of the proxy constructors (e.g.,
@scheme[proxy-procedure] or @racket[chaperone-procedure]).

See also @racket[prop:proxy-of].}


@defproc[(chaperone-of? [v1 any/c] [v2 any/c]) boolean?]{

Indicates whether @scheme[v1] can be considered equivalent modulo
chaperones to @scheme[v2].

For values that include no chaperones, @scheme[v1] and @scheme[v2] can
be considered chaperones of each other if they are @scheme[equal?],
except that the mutability of vectors and boxes with @scheme[v1] and
@scheme[v2] must be the same.

Otherwise, all chaperones of @scheme[v2] must be intact in
@scheme[v1], in the sense that parts of @scheme[v2] must be derived
from @scheme[v1] through one of the chaperone constructors (e.g.,
@scheme[chaperone-procedure]).}

@; ------------------------------------------------------------
@section{Proxy Constructors}

@defproc[(proxy-procedure [proc procedure?]
                          [wrapper-proc procedure?]
                          [prop proxy-property?]
                          [prop-val any] ... ...)
         (and/c procedure? proxy?)]{

Returns a proxied procedure that has the same arity, name, and
other attributes as @scheme[proc]. When the proxied procedure is
applied, the arguments are first passed to @scheme[wrapper-proc], and
then the results from @scheme[wrapper-proc] are passed to
@scheme[proc]. The @scheme[wrapper-proc] can also supply a procedure
that processes the results of @scheme[proc].

The arity of @scheme[wrapper-proc] must include the arity of
@scheme[proc]. The allowed keyword arguments of @scheme[wrapper-proc]
must be a superset of the allowed keywords of @scheme[proc]. The
required keyword arguments of @scheme[wrapper-proc] must be a subset
of the required keywords of @scheme[proc].

For applications without keywords, the result of @scheme[wrapper-proc]
must be either the same number of values as supplied to it or one more
than the number of supplied values, where an extra result is supplied
before the others. The additional result, if any, must be a procedure
that accepts as many results as produced by @scheme[proc]; it must
return the same number of results.  If @scheme[wrapper-proc] returns
the same number of values as it is given (i.e., it does not return a
procedure to proxy @scheme[proc]'s result), then @scheme[proc] is
called in @tech{tail position} with respect to the call to the proxy.

For applications that include keyword arguments, @scheme[wrapper-proc]
must return an additional value before any other values but after the
result-proxying procedure (if any). The additional value must be a
list of proxys of the keyword arguments that were supplied to the
proxied procedure (i.e., not counting optional arguments that were
not supplied). The arguments must be ordered according to the sorted
order of the supplied arguments' keywords.

Pairs of @scheme[prop] and @scheme[prop-val] (the number of arguments
to @scheme[procedure-proxy] must be even) add proxy properties
or override proxy-property values of @scheme[proc].

If any @scheme[prop] is @racket[proxy-prop:application-mark] and if the
associated @racket[prop-val] is a pair, then the call to @racket[proc]
is wrapped with @racket[with-continuation-mark] using @racket[(car
prop-val)] as the mark key and @racket[(cdr prop-val)] as the mark
value. In addition, if @racket[continuation-mark-set-first] with
@racket[(car prop-val)] produces a value for the immediate
continuation frame of the call to the proxied procedure, the value is
also installed as an immediate value for @racket[(car prop-val)] as a
mark during the call to @racket[wrapper-proc] (which allows tail-calls
of proxies with respect to wrapping proxies to be detected within
@racket[wrapper-proc]).}


@defproc[(proxy-struct [v any/c]
                       [orig-proc (or/c struct-accessor-procedure?
                                        struct-mutator-procedure?)]
                       [redirect-proc procedure?] ... ...
                       [prop proxy-property?]
                       [prop-val any] ... ...)
          any/c]{

Returns a proxied value like @scheme[v], but with certain
operations on the proxied redirected. The @scheme[orig-proc]s
indicate the operations to redirect, and the corresponding
@scheme[redirect-proc]s supply the redirections.

The protocol for a @scheme[redirect-proc] depends on the corresponding
@scheme[orig-proc]:

@itemlist[

 @item{A structure-field: @scheme[redirect-proc]
      must accept two arguments, @scheme[v] and the value
      @scheme[_field-v] that @scheme[orig-proc] produces for
      @scheme[v]; it must return a replacement for
      @scheme[_field-v]. The corresponding field must not be
      immutable.}

 @item{A structure field mutator: @scheme[redirect-proc] must accept
      two arguments, @scheme[v] and the value @scheme[_field-v]
      supplied to the mutator; it must return a replacement for
      @scheme[_field-v] to be propagated to @scheme[orig-proc] and
      @scheme[v].}

]

Pairs of @scheme[prop] and @scheme[prop-val] (the number of arguments
to @scheme[proxy-procedure] must be odd) add proxy properties
or override proxy-property values of @scheme[v].}

@defproc[(proxy-vector [vec (and/c vector? (not/c immutable?))]
                       [ref-proc (vector? exact-nonnegative-integer? any/c . -> . any/c)]
                       [set-proc (vector? exact-nonnegative-integer? any/c . -> . any/c)]
                       [prop proxy-property?]
                       [prop-val any] ... ...)
          (and/c vector? proxy?)]{

Returns a proxied value like @scheme[vec], but with
@scheme[vector-ref] and @scheme[vector-set!] operations on the
proxied vector redirected.

The @scheme[ref-proc] must accept @scheme[vec], an index passed to
@scheme[vector-ref], and the value that @scheme[vector-ref] on
@scheme[vec] produces for the given index; it must produce a
replacement for the value, which is the result of @scheme[vector-ref]
on the proxy.

The @scheme[set-proc] must accept @scheme[vec], an index passed to
@scheme[vector-set!], and the value passed to @scheme[vector-set!]; it
must produce a replacement for the value, which is used
with @scheme[vector-set!] on the original @scheme[vec] to install the
value.

Pairs of @scheme[prop] and @scheme[prop-val] (the number of arguments
to @scheme[proxy-vector] must be odd) add proxy properties
or override proxy-property values of @scheme[vec].}

@defproc[(proxy-box [box (and/c box? (not/c immutable?))]
                    [unbox-proc (box? any/c . -> . any/c)]
                    [set-proc (box? any/c . -> . any/c)]
                    [prop proxy-property?]
                    [prop-val any] ... ...)
          (and/c box? proxy?)]{

Returns a proxied value like @scheme[bx], but with
@scheme[unbox] and @scheme[set-box!] operations on the
proxied box redirected.

The @scheme[unbox-proc] must accept @scheme[bx] and the value that
@scheme[unbox] on @scheme[bx] produces index; it must produce a replacement
value, which is the result of
@scheme[unbox] on the proxy.

The @scheme[set-proc] must accept @scheme[bx] and the value passed to
@scheme[set-box!]; it must produce a replacement
value, which is used with @scheme[set-box!] on the original
@scheme[bx] to install the value.

Pairs of @scheme[prop] and @scheme[prop-val] (the number of arguments
to @scheme[proxy-box] must be odd) add proxy properties
or override proxy-property values of @scheme[bx].}


@defproc[(proxy-hash [hash (and/c hash? (not/c immutable?))]
                     [ref-proc (hash? any/c . -> . (values 
                                                    any/c 
                                                    (hash? any/c any/c . -> . any/c)))]
                     [set-proc (hash? any/c any/c . -> . (values any/c any/c))]
                     [remove-proc (hash? any/c . -> . any/c)]
                     [key-proc (hash? any/c . -> . any/c)]
                     [prop proxy-property?]
                     [prop-val any] ... ...)
          (and/c hash? proxy?)]{

Returns a proxied value like @scheme[hash], but with
@scheme[hash-ref], @scheme[hash-set!] or @scheme[hash-set] (as
applicable) and @scheme[hash-remove] or @scheme[hash-remove!] (as
application) operations on the proxied hash table redirected. When
@scheme[hash-set] or @scheme[hash-remove] is used on a proxied hash
table, the resulting hash table is given all of the proxys of the
given hash table. In addition, operations like
@scheme[hash-iterate-key] or @scheme[hash-map], which extract
keys from the table, use @scheme[key-proc] to filter keys extracted
from the table. Operations like @scheme[hash-iterate-value] or
@scheme[hash-iterate-map] implicitly use @scheme[hash-ref] and
therefore redirect through @scheme[ref-proc].

The @scheme[ref-proc] must accept @scheme[hash] and a key passed
@scheme[hash-ref]. It must return a replacement key
as well as a procedure. The returned procedure is called only if the
returned key is found in @scheme[hash] via @scheme[hash-ref], in which
case the procedure is called with @scheme[hash], the previously
returned key, and the found value. The returned procedure must itself
return a replecement for the found value.

The @scheme[set-proc] must accept @scheme[hash], a key passed to
@scheme[hash-set!] or @scheme[hash-set], and the value passed to
@scheme[hash-set!] or @scheme[hash-set]; it must produce two values: a
replacement for the key and a replacement for the value. The returned
key and value are used with @scheme[hash-set!] or @scheme[hash-set] on
the original @scheme[hash] to install the value.

The @scheme[remove-proc] must accept @scheme[hash] and a key passed to
@scheme[hash-remove!] or @scheme[hash-remove]; it must produce the a
replacement for the key, which is used with @scheme[hash-remove!] or
@scheme[hash-remove] on the original @scheme[hash] to remove any
mapping using the (proxy-replaced) key.

The @scheme[key-proc] must accept @scheme[hash] and a key that has
been extracted from @scheme[hash] (by @scheme[hash-iterate-key] or
other operations that use @scheme[hash-iterate-key] internally); it
must produce a replacement for the key, which is then reported as a
key extracted from the table.

The @racket[hash-iterate-value], @racket[hash-map], or
@racket[hash-for-each] functions use a combination of
@racket[hash-iterate-key] and @racket[hash-ref]. If a key
produced by @scheme[key-proc] does not yield a value through
@racket[hash-ref], then the @exnraise[exn:fail:contract].

Pairs of @scheme[prop] and @scheme[prop-val] (the number of arguments
to @scheme[proxy-hash] must be odd) add proxy properties
or override proxy-property values of @scheme[hash].}


@defthing[prop:proxy-of struct-type-property?]{

A @tech{structure type property} (see @secref["structprops"]) that
supplies a procedure for extracting a proxied value from a structure
that represents a proxy. The property is used for @racket[proxy-of]
as well as @racket[equal?].

The property value must be a procedure of one argument, which is a
structure whose structure type has the property. The result can be
@scheme[#f] to indicate the structure does not represent a proxy,
otherwise the result is a value for which the original structure is a
proxy (so the original structure is a @racket[proxy-of?] and it is
@racket[equal?] to the result value). The result value must have the
same @racket[prop:proxy-of] and @racket[prop:equal+hash] property
values as the original structure, and the property values must be
inherited from the same structure type (which ensures some consistency
between @racket[proxy-of?] and @racket[equal?]).}

@; ------------------------------------------------------------
@section{Chaperone Constructors}

@defproc[(chaperone-procedure [proc procedure?]
                              [wrapper-proc procedure?]
                              [prop proxy-property?]
                              [prop-val any] ... ...)
         (and/c procedure? chaperone?)]{

Like @racket[proxy-procedure], but for each value supplied to
@scheme[wrapper-proc], the corresponding result must be the same or a
chaperone of (in the sense of @scheme[chaperone-of?])  the supplied
value. The additional result, if any, that precedes the chaperoned
values must be a procedure that accepts as many results as produced by
@scheme[proc]; it must return the same number of results, each of
which is the same or a chaperone of the corresponding original result.

For applications that include keyword arguments, @scheme[wrapper-proc]
must return an additional value before any other values but after the
result-chaperoning procedure (if any). The additional value must be a
list of chaperones of the keyword arguments that were supplied to the
chaperoned procedure (i.e., not counting optional arguments that were
not supplied). The arguments must be ordered according to the sorted
order of the supplied arguments' keywords.}

@defproc[(chaperone-struct [v any/c]
                           [orig-proc (or/c struct-accessor-procedure?
                                            struct-mutator-procedure?
                                            struct-type-property-accessor-procedure?
                                            (one-of/c struct-info))]
                           [redirect-proc procedure?] ... ...
                           [prop proxy-property?]
                           [prop-val any] ... ...)
          any/c]{

Like @racket[proxy-struct], but with the following refinements:

@itemlist[

 @item{With a structure-field accessor as @racket[orig-proc],
      @scheme[redirect-proc] must accept two arguments, @scheme[v] and
      the value @scheme[_field-v] that @scheme[orig-proc] produces for
      @scheme[v]; it must return chaperone of @scheme[_field-v]. The
      corresponding field may be immutable.}

 @item{A property accessor can be supplied as @racket[orig-proc].  The
       corresponding @racket[redirect-proc] uses the same protocol as
       for a structure-field selector.}

 @item{With structure-field mutator as @racket[orig-proc],
      @scheme[redirect-proc] must accept two arguments, @scheme[v] and
      the value @scheme[_field-v] supplied to the mutator; it must
      return chaperone of @scheme[_field-v] to be propagated to
      @scheme[orig-proc] and @scheme[v].}

 @item{With @scheme[struct-info] as @racket[orig-proc], the
       corresponding @scheme[redirect-proc] must accept two values,
       which are the results of @scheme[struct-info] on @scheme[v]; it
       must return each values or a chaperone of each value. The
       @scheme[redirect-proc] is not called if @scheme[struct-info] would
       return @scheme[#f] as its first argument.}

]

An @scheme[orig-proc] can be @scheme[struct-info] only if some other
@scheme[orig-proc] is supplied, and each @scheme[orig-proc] must
indicate a distinct operation. If no @scheme[orig-proc]s are supplied,
then no @scheme[prop]s must be supplied, and @scheme[v] is returned
unchaperoned.}

@defproc[(chaperone-vector [vec vector?]
                           [ref-proc (vector? exact-nonnegative-integer? any/c . -> . any/c)]
                           [set-proc (vector? exact-nonnegative-integer? any/c . -> . any/c)]
                           [prop proxy-property?]
                           [prop-val any] ... ...)
          (and/c vector? chaperone?)]{

Like @racket[proxy-vector], but with support for mutable vectors. The
@scheme[ref-proc] procedure must produce the same value or a chaperone
of the original value, and @scheme[set-proc] must produce the value
that is given or a chaperone of the value. The @scheme[set-proc] will
not be used if @scheme[vec] is immutable.}

@defproc[(chaperone-box [bx box?]
                        [unbox-proc (box? any/c . -> . any/c)]
                        [set-proc (box? any/c . -> . any/c)]
                        [prop proxy-property?]
                        [prop-val any] ... ...)
          (and/c box? chaperone?)]{

Like @racket[prox-box], but with support for immutable boxes. The
@scheme[unbox-proc] procedure must produce the same value or a
chaperone of the original value, and @scheme[set-proc] must produce
the same value or a chaperone of the value that it is given.  The
@scheme[set-proc] will not be used if @scheme[bx] is immutable.}


@defproc[(chaperone-hash [hash hash?]
                         [ref-proc (hash? any/c . -> . (values 
                                                        any/c 
                                                        (hash? any/c any/c . -> . any/c)))]
                         [set-proc (hash? any/c any/c . -> . (values any/c any/c))]
                         [remove-proc (hash? any/c . -> . any/c)]
                         [key-proc (hash? any/c . -> . any/c)]
                         [prop proxy-property?]
                         [prop-val any] ... ...)
          (and/c hash? chaperone?)]{

Like @racket[proxy-hash], but with constraints on the given functions
and support for immutable hashes. The @scheme[ref-proc] procedure must
return a found value or a chaperone of the value. The
@scheme[set-proc] procedure must produce two values: the key that it
is given or a chaperone of the key and the value that it is given or a
chaperone of the value. The @scheme[remove-proc] and @scheme[key-proc]
procedures must produce the given key or a chaperone of the key.}

@defproc[(chaperone-struct-type [struct-type struct-type?]
                                [struct-info-proc procedure?]
                                [make-constructor-proc (procedure? . -> . procedure?)]
                                [guard-proc procedure?]
                                [prop proxy-property?]
                                [prop-val any] ... ...)
          (and/c struct-type? chaperone?)]{

Returns a chaperoned value like @scheme[struct-type], but with
@scheme[struct-type-info] and @scheme[struct-type-make-constructor]
operations on the chaperoned structure type redirected. In addition,
when a new structure type is created as a subtype of the chaperoned
structure type, @scheme[guard-proc] is interposed as an extra guard on
creation of instances of the subtype.

The @scheme[struct-info-proc] must accept 8 arguments---the result of
@scheme[struct-type-info] on @scheme[struct-type]. It must return 8
values, where each is the same or a chaperone of the corresponding
argument. The 8 values are used as the results of
@scheme[struct-type-info] for the chaperoned structure type.

The @scheme[make-constructor-proc] must accept a single procedure
argument, which is a constructor produced by
@scheme[struct-type-make-constructor] on @scheme[struct-type]. It must
return the same or a chaperone of the procedure, which is used as the
result of @scheme[struct-type-make-constructor] on the chaperoned
structure type.

The @scheme[guard-proc] must accept as many argument as a constructor
for @scheme[struct-type]; it must return the same number of arguments,
each the same or a chaperone of the corresponding argument. The
@scheme[guard-proc] is added as a constructor guard when a subtype is
created of the chaperoned structure type.

Pairs of @scheme[prop] and @scheme[prop-val] (the number of arguments
to @scheme[chaperone-struct-type] must be even) add proxy properties
or override proxy-property values of @scheme[struct-type].}

@defproc[(chaperone-evt [evt evt?]
                        [proc (evt? . -> . (values evt? (any/c . -> . any/c)))]
                        [prop proxy-property?]
                        [prop-val any] ... ...)
          (and/c evt? chaperone?)]{

Returns a chaperoned value like @scheme[evt], but with @scheme[proc]
as an event generator when the result is synchronized with functions
like @racket[sync].

The @racket[proc] generator is called on synchronization, much like
the procedure passed to @racket[guard-evt], except that @racket[proc]
is given @scheme[evt]. The @racket[proc] must return two values: a
@tech{synchronizable event} that is a chaperone of @racket[evt], and a
procedure that is used to check the event's result if it is chosen in
a selection. The latter procedure accepts the result of @racket[evt],
and it must return a chaperone of that value.

Pairs of @scheme[prop] and @scheme[prop-val] (the number of arguments
to @scheme[chaperone-struct-type] must be even) add proxy properties
or override proxy-property values of @scheme[evt].}

@; ------------------------------------------------------------
@section{Proxy Properties}

@defproc[(make-proxy-property [name symbol?])
         (values proxy-property?
                 (-> any/c boolean?)
                 (-> chaperone? any))]{

Creates a new @tech{proxy property} and returns three values:

@itemize[

 @item{a @deftech{proxy property descriptor}, for use with
       @scheme[chaperone-procedure], @scheme[chaperone-struct], and
       other chaperone constructors;}

 @item{a @deftech{proxy property predicate} procedure, which takes
       an arbitrary value and returns @scheme[#t] if the value is a
       chaperone with a value for the property, @scheme[#f]
       otherwise;}

 @item{an @deftech{proxy property accessor} procedure, which
       returns the value associated with a chaperone for the property;
       if a value given to the accessor is not a chaperone or does not
       have a value for the property (ie if the corresponding chaperone
       property predicate returns @racket[#f]), the accessor raises
       @exnraise[exn:fail:contract].}

]}

@defproc[(proxy-property? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a @tech{proxy property
descriptor} value, @scheme[#f] otherwise.}

@defproc[(proxy-property-accessor-procedure? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is an accessor procedure produced
by @scheme[make-proxy-property], @scheme[#f] otherwise.}


@defthing[proxy-prop:application-mark proxy-property?]{

A @tech{proxy property} that is recognized by @racket[proxy-procedure]
and @racket[chaperone-procedure].}

