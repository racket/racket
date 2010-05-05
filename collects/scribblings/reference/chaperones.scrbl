#lang scribble/doc
@(require "mz.ss")

@(define-syntax op
  (syntax-rules ()
    [(_ (x ...)) (x ...)]
    [(_ id) @scheme[id]]))
@(define-syntax-rule (operations i ...)
   (itemlist #:style 'compact @item{@op[i]} ...))

@title[#:tag "chaperones"]{Chaperones}

A @deftech{chaperone} is a wrapper for a value where the wrapper
implements primitive support for @tech{contract}-like checks on the
value's operations. Chaperones apply only to procedures,
@tech{structures} for which an accessor or mutator is available,
@tech{structure types}, @tech{hash tables}, @tech{vectors},
@tech{box}es. A chaperoned value is @scheme[equal?] to the original
value, but not @scheme[eq?] to the original value.

A chaperone's refinement of a value's operation is restricted to side
effects (including, in particular, raising and exception) or
chaperoning values supplied to or produced by the operation. For
example, a vector chaperone can redirect @scheme[vector-ref] to raise
an exception if the accessed vector slot contains a string, or it can
cause the result of @scheme[vector-ref] to be a chaperoned variant of
the value that is in the accessed vector slot, but it cannot redirect
@scheme[vector-ref] to produce a value that is arbitrarily different
from the value in the vector slot.

Beware that each of the following operations can be redirected to
arbitrary procedure through chaperones on the operation's
argument---assuming that the operation is available to the creator of
the chaperone:

@operations[@t{a structure-field accesor}
            @t{a structure-field mutator}
            @t{a structure type property accessor}
            @t{application of a procedure}
            unbox set-box!
            vector-ref vector-set!
            hash-ref hash-set hash-set! hash-remove hash-remove!]

Derived operations, such as printing a value, can be redirected
through chaperones due to their use of accessor functions. The
@scheme[equal?], @scheme[equal-hash-code], and
@scheme[equal-secondary-hash-code] operations, in contrast, may bypass
chaperones (but they are not obliged to).

In addition to redirecting operations that work on a value, a
chaperone can include @deftech{chaperone properties} for a chaperoned
value. A @tech{chaperone property} is similar to a @tech{structure
type property}, but it applies to chaperones instead of structure
types and their instances.


@defproc[(chaperone? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a chaperone, @scheme[#f] otherwise.

Programs and libraries generally should avoid @scheme[chaperone?] and
treat chaperones the same as unchaperoned values. In rare cases,
@scheme[chaperone?] may be needed to guard against redirection by a
chaperone of an operation to an arbitrary procedure.}


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
@section{Chaperone Constructors}

@defproc[(chaperone-procedure [proc procedure?]
                              [wrapper-proc procedure?]
                              [prop chaperone-property?]
                              [prop-val any] ... ...)
         (and/c procedure? chaperone?)]{

Returns a chaperoned procedure that has the same arity, name, and
other attributes as @scheme[proc]. When the chaperoned procedure is
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
before the others. For each supplied value, the corresponding result
must be the same or a chaperone of (in the sense of
@scheme[chaperone-of?])  the supplied value. The additional result, if
any, that precedes the chaperoned values must be a procedure that
accepts as many results as produced by @scheme[proc]; it must return
the same number of results, each of which is the same or a chaperone
of the corresponding original result.  If @scheme[wrapper-proc]
returns the same number of values as it is given (i.e., it does not
return a procedure to chaperone @scheme[proc]'s result), then
@scheme[proc] is called in @tech{tail position} with respect to the
call to the chaperone.

For applications that include keyword arguments, @scheme[wrapper-proc]
must return an additional value before any other values but after the
result-chaperoning procedure (if any). The additional value must be a
list of chaperones of the keyword arguments that were supplied to the
chaperoned procedure (i.e., not counting optional arguments that were
not supplied). The arguments must be ordered according to the sorted
order of the supplied arguments' keywords.

Pairs of @scheme[prop] and @scheme[prop-val] (the number of arguments
to @scheme[procedure-chaperone] must be even) add chaperone properties
or override chaperone-property values of @scheme[proc].}

@defproc[(chaperone-struct [v any/c]
                           [orig-proc (or/c struct-accessor-procedure?
                                            struct-mutator-procedure?
                                            struct-type-property-accessor-procedure?
                                            (one-of/c struct-info))]
                           [redirect-proc procedure?] ... ...
                           [prop chaperone-property?]
                           [val any] ... ...)
          any/c]{

Returns a chaperoned value like @scheme[v], but with certain
operations on the chaperoned redirected. The @scheme[orig-proc]s
indicate the operations to redirect, and the corresponding
@scheme[redirect-proc]s supply the redirections.

The protocol for a @scheme[redirect-proc] depends on the corresponding
@scheme[orig-proc]:

@itemlist[

 @item{A structure-field or property accessor: @scheme[orig-proc] must
      accept two arguments, @scheme[v] and the value @scheme[_field-v]
      that @scheme[orig-proc] produces for @scheme[v]; it must return
      chaperone of @scheme[_field-v].}

 @item{A structure field mutator: @scheme[orig-proc] must accept two
      arguments, @scheme[v] and the value @scheme[_field-v] supplied
      to the mutator; it must return chaperone of @scheme[_field-v]
      to be propagated to @scheme[orig-proc] and @scheme[v].}

 @item{@scheme[struct-info]: @scheme[orig-proc] must accept two
       values, which are the results of @scheme[struct-info] on
       @scheme[v]; it must return two values that are chaperones of
       its arguments. The @scheme[orig-proc] is not called if
       @scheme[struct-info] would return @scheme[#f] as its first
       argument.}

]

An @scheme[orig-proc] can be @scheme[struct-info] only if some other
@scheme[orig-proc] is supplied, and each @scheme[orig-proc] must
indicate a distinct operation. If no @scheme[orig-proc]s are supplied,
then no @scheme[prop]s must be supplied, and @scheme[v] is returned
unchaperoned.

Pairs of @scheme[prop-val] and @scheme[val] (the number of arguments
to @scheme[chaperone-procedure] must be even) add chaperone properties
or override chaperone-property values of @scheme[v].}

@defproc[(chaperone-vector [vec vector?]
                           [ref-proc (vector? exact-nonnegative-integer? any/c . -> . any/c)]
                           [set-proc (vector? exact-nonnegative-integer? any/c . -> . any/c)]
                           [prop chaperone-property?]
                           [val any] ... ...)
          (and/c vector? chaperone?)]{

Returns a chaperoned value like @scheme[vec], but with
@scheme[vector-ref] and @scheme[vector-set!] operations on the
chaperoned vector redirected.

The @scheme[ref-proc] must accept @scheme[vec], an index passed to
@scheme[vector-ref], and the value that @scheme[vector-ref] on
@scheme[vec] produces for the given index; it must produce the same
value or a chaperone of the value, which is the result of
@scheme[vector-ref] on the chaperone.

The @scheme[set-proc] must accept @scheme[vec], an index passed to
@scheme[vector-set!], and the value passed to @scheme[vector-set!]; it
must produce the same value or a chaperone of the value, which is used
with @scheme[vector-set!] on the original @scheme[vec] to install the
value. The @scheme[set-proc] will not be used if @scheme[vec] is
immutable.

Pairs of @scheme[prop-val] and @scheme[val] (the number of arguments
to @scheme[chaperone-vector] must be odd) add chaperone properties
or override chaperone-property values of @scheme[vec].}

@defproc[(chaperone-box [bx box?]
                        [unbox-proc (box? any/c . -> . any/c)]
                        [set-proc (box? any/c . -> . any/c)]
                        [prop chaperone-property?]
                        [val any] ... ...)
          (and/c box? chaperone?)]{

Returns a chaperoned value like @scheme[bx], but with
@scheme[unbox] and @scheme[set-box!] operations on the
chaperoned box redirected.

The @scheme[unbox-proc] must accept @scheme[bx] and the value that
@scheme[unbox] on @scheme[bx] produces index; it must produce the same
value or a chaperone of the value, which is the result of
@scheme[unbox] on the chaperone.

The @scheme[set-proc] must accept @scheme[bx] and the value passed to
@scheme[set-box!]; it must produce the same value or a chaperone of
the value, which is used with @scheme[set-box!] on the original
@scheme[bx] to install the value.  The @scheme[set-proc] will not be
used if @scheme[bx] is immutable.

Pairs of @scheme[prop-val] and @scheme[val] (the number of arguments
to @scheme[chaperone-box] must be odd) add chaperone properties
or override chaperone-property values of @scheme[bx].}


@defproc[(chaperone-hash [hash hash?]
                         [ref-proc (hash? any/c . -> . (values 
                                                        any/c 
                                                        (hash? any/c any/c . -> . any/c)))]
                         [set-proc (hash? any/c any/c . -> . (values any/c any/c))]
                         [remove-proc (hash? any/c . -> . any/c)]
                         [key-proc (hash? any/c . -> . any/c)]
                         [prop chaperone-property?]
                         [val any] ... ...)
          (and/c hash? chaperone?)]{

Returns a chaperoned value like @scheme[hash], but with
@scheme[hash-ref], @scheme[hash-set!] or @scheme[hash-set] (as
applicable) and @scheme[hash-remove] or @scheme[hash-remove!] (as
application) operations on the chaperoned hash table redirected. When
@scheme[hash-set] or @scheme[hash-remove] is used on a chaperoned hash
table, the resulting hash table is given all of the chaperones of the
given hash table. In addition, operations like
@scheme[hash-iterate-key] or @scheme[hash-map], which extract
keys from the table, use @scheme[key-proc] to filter keys extracted
from the table. Operations like @scheme[hash-iterate-value] or
@scheme[hash-iterate-map] implicitly use @scheme[hash-ref] and
therefore redirect through @scheme[ref-proc].

The @scheme[ref-proc] must accept @scheme[hash] and a key passed
@scheme[hash-ref]. It must returned the key or a chaperone of the key
as well as a procedure. The returned procedure is called only if the
returned key is found in @scheme[hash] via @scheme[hash-ref], in which
case the procedure is called with @scheme[hash], the previously
returned key, and the found value. The returned procedure must itself
return the found value or a chaperone of the value.

The @scheme[set-proc] must accept @scheme[hash], a key passed to
@scheme[hash-set!] or @scheme[hash-set], and the value passed to
@scheme[hash-set!] or @scheme[hash-set]; it must produce two values:
the same key or a chaperone of the key and the same value or a
chaperone of the value. The returned key and value are used with
@scheme[hash-set!] or @scheme[hash-set] on the original @scheme[hash]
to install the value.

The @scheme[remove-proc] must accept @scheme[hash] and a key passed to
@scheme[hash-remove!] or @scheme[hash-remove]; it must produce the
same key or a chaperone of the key, which is used with
@scheme[hash-remove!] or @scheme[hash-remove] on the original
@scheme[hash] to remove any mapping using the (chaperoned) key.

The @scheme[key-proc] must accept @scheme[hash] and a key that has
been extracted from @scheme[hash] (by @scheme[hash-iterate-key] or
other operations that use @scheme[hash-iterate-key] internally); it
must produce the same key or a chaperone of the key, which is then
reported as a key extracted from the table.

Pairs of @scheme[prop-val] and @scheme[val] (the number of arguments
to @scheme[chaperone-hash] must be odd) add chaperone properties
or override chaperone-property values of @scheme[hash].}

@defproc[(chaperone-struct-type [struct-type struct-type?]
                                [struct-info-proc procedure?]
                                [make-constructor-proc (procedure? . -> . procedure?)]
                                [guard-proc procedure?]
                                [prop chaperone-property?]
                                [val any] ... ...)
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

Pairs of @scheme[prop-val] and @scheme[val] (the number of arguments
to @scheme[chaperone-struct-type] must be even) add chaperone properties
or override chaperone-property values of @scheme[struct-type].}

@; ------------------------------------------------------------
@section{Chaperone Properties}

@defproc[(make-chaperone-property [name symbol?])
         (values chaperone-property?
                 procedure?
                 procedure?)]{

Creates a new structure type property and returns three values:

@itemize[

 @item{a @deftech{chaperone property descriptor}, for use with
       @scheme[chaperone-procedure], @scheme[chaperone-struct], and
       other chaperone constructors;}

 @item{a @deftech{chaperone property predicate} procedure, which takes
       an arbitrary value and returns @scheme[#t] if the value is a
       chaperone with a value for the property, @scheme[#f]
       otherwise;}

 @item{an @deftech{chaperone property accessor} procedure, which
       returns the value associated with a chaperone for the property;
       if a value given to the accessor is not a chaperone or does not
       have a value for the property, the
       @exnraise[exn:fail:contract].}

]}

@defproc[(chaperone-property? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a @tech{chaperone property
descriptor} value, @scheme[#f] otherwise.}

@defproc[(chaperone-property-accessor-procedure? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is an accessor procedure produced
by @scheme[make-chaperone-property], @scheme[#f] otherwise.}
