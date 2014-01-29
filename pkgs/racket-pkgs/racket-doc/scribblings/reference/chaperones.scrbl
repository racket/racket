#lang scribble/doc
@(require "mz.rkt")

@(define-syntax op
  (syntax-rules ()
    [(_ (x ...)) (x ...)]
    [(_ id) @racket[id]]))
@(define-syntax-rule (operations i ...)
   (itemlist #:style 'compact @item{@op[i]} ...))

@title[#:tag "chaperones"]{Impersonators and Chaperones}

An @deftech{impersonator} is a wrapper for a value where the wrapper
redirects some of the value's operations. Impersonators apply only to procedures,
@tech{structures} for which an accessor or mutator is available,
@tech{structure types}, @tech{hash tables}, @tech{vectors},
@tech{box}es, @tech{channels}, and @tech{prompt tag}s.
An impersonator is @racket[equal?] to the original
value, but not @racket[eq?] to the original value.

A @deftech{chaperone} is a kind of impersonator whose refinement of a value's
operation is restricted to side effects (including, in particular,
raising an exception) or chaperoning values supplied to or produced by
the operation. For example, a vector chaperone can redirect
@racket[vector-ref] to raise an exception if the accessed vector slot
contains a string, or it can cause the result of @racket[vector-ref]
to be a chaperoned variant of the value that is in the accessed vector
slot, but it cannot redirect @racket[vector-ref] to produce a value
that is arbitrarily different from the value in the vector slot.

A non-@tech{chaperone} @tech{impersonator}, in contrast, can refine an operation to swap one
value for any other. An impersonator cannot be applied to an immutable value
or refine the access to an immutable field in an instance of a @tech{structure
type}, since arbitrary redirection of an operation amounts to
mutation of the impersonated value.

Beware that each of the following operations can be redirected to an
arbitrary procedure through an impersonator on the operation's
argument---assuming that the operation is available to the creator of
the impersonator:

@operations[@t{a structure-field accessor}
            @t{a structure-field mutator}
            @t{a structure type property accessor}
            @t{application of a procedure}
            unbox set-box!
            vector-ref vector-set!
            hash-ref hash-set hash-set! hash-remove hash-remove!
            channel-get channel-put
            call-with-continuation-prompt
            abort-current-continuation]

Derived operations, such as printing a value, can be redirected
through impersonators due to their use of accessor functions. The
@racket[equal?], @racket[equal-hash-code], and
@racket[equal-secondary-hash-code] operations, in contrast, may bypass
impersonators (but they are not obliged to).

In addition to redirecting operations that work on a value, a
impersonator can include @deftech{impersonator properties} for an impersonated
value. An @tech{impersonator property} is similar to a @tech{structure
type property}, but it applies to impersonators instead of structure
types and their instances.


@defproc[(impersonator? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is an @tech{impersonator}, @racket[#f] otherwise.

Programs and libraries generally should avoid @racket[impersonator?] and
treat impersonators the same as non-impersonator values. In rare cases,
@racket[impersonator?] may be needed to guard against redirection by an
impersonator of an operation to an arbitrary procedure.}


@defproc[(chaperone? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a @tech{chaperone}, @racket[#f] otherwise.

Programs and libraries generally should avoid @racket[chaperone?] for
the same reason that they should avoid @racket[impersonator?].}


@defproc[(impersonator-of? [v1 any/c] [v2 any/c]) boolean?]{

Indicates whether @racket[v1] can be considered equivalent modulo
impersonators to @racket[v2].

For values that include no impersonators, @racket[v1] and @racket[v2] can
be considered impersonators of each other if they are @racket[equal?].

Otherwise, all impersonators of @racket[v2] must be intact in @racket[v1],
in the sense that parts of @racket[v2] must be derived from
@racket[v1] through one of the impersonator constructors (e.g.,
@racket[impersonate-procedure] or @racket[chaperone-procedure]).

See also @racket[prop:impersonator-of].}


@defproc[(chaperone-of? [v1 any/c] [v2 any/c]) boolean?]{

Indicates whether @racket[v1] can be considered equivalent modulo
chaperones to @racket[v2].

For values that include no chaperones, @racket[v1] and @racket[v2] can
be considered chaperones of each other if they are @racket[equal?],
except that the mutability of vectors and boxes with @racket[v1] and
@racket[v2] must be the same.

Otherwise, all chaperones of @racket[v2] must be intact in
@racket[v1], in the sense that parts of @racket[v2] must be derived
from @racket[v1] through one of the chaperone constructors (e.g.,
@racket[chaperone-procedure]).}

@defproc[(impersonator-ephemeron [v any/c]) ephemeron?]{

Produces an @tech{ephemeron} that can be used to connect the
reachability of @racket[v] (in the sense of garbage collection; see
@secref["gc-model"]) with the reachability of any value for which
@racket[v] is an @tech{impersonator}. That is, the value @racket[v]
will be considered reachable as long as the result ephemeron is
reachable in addition to any value that @racket[v] impersonates
(including itself).}

@; ------------------------------------------------------------
@section{Impersonator Constructors}

@defproc[(impersonate-procedure [proc procedure?]
                                [wrapper-proc procedure?]
                                [prop impersonator-property?]
                                [prop-val any] ... ...)
         (and/c procedure? impersonator?)]{

Returns an impersonator procedure that has the same arity, name, and
other attributes as @racket[proc]. When the impersonator procedure is
applied, the arguments are first passed to @racket[wrapper-proc], and
then the results from @racket[wrapper-proc] are passed to
@racket[proc]. The @racket[wrapper-proc] can also supply a procedure
that processes the results of @racket[proc].

The arity of @racket[wrapper-proc] must include the arity of
@racket[proc]. The allowed keyword arguments of @racket[wrapper-proc]
must be a superset of the allowed keywords of @racket[proc]. The
required keyword arguments of @racket[wrapper-proc] must be a subset
of the required keywords of @racket[proc].

For applications without keywords, the result of @racket[wrapper-proc]
must be either the same number of values as supplied to it or one more
than the number of supplied values, where an extra result is supplied
before the others. The additional result, if any, must be a procedure
that accepts as many results as produced by @racket[proc]; it must
return the same number of results.  If @racket[wrapper-proc] returns
the same number of values as it is given (i.e., it does not return a
procedure to impersonator @racket[proc]'s result), then @racket[proc] is
called in @tech{tail position} with respect to the call to the impersonator.

For applications that include keyword arguments, @racket[wrapper-proc]
must return an additional value before any other values but after the
result-impersonating procedure (if any). The additional value must be a
list of replacements for the keyword arguments that were supplied to the
impersonator (i.e., not counting optional arguments that were
not supplied). The arguments must be ordered according to the sorted
order of the supplied arguments' keywords.

Pairs of @racket[prop] and @racket[prop-val] (the number of arguments
to @racket[procedure-impersonator] must be even) add impersonator properties
or override impersonator-property values of @racket[proc].

If any @racket[prop] is @racket[impersonator-prop:application-mark] and if the
associated @racket[prop-val] is a pair, then the call to @racket[proc]
is wrapped with @racket[with-continuation-mark] using @racket[(car
prop-val)] as the mark key and @racket[(cdr prop-val)] as the mark
value. In addition, if @racket[continuation-mark-set-first] with
@racket[(car prop-val)] produces a value for the immediate
continuation frame of the call to the impersonated procedure, the value is
also installed as an immediate value for @racket[(car prop-val)] as a
mark during the call to @racket[wrapper-proc] (which allows tail-calls
of impersonators with respect to wrapping impersonators to be detected within
@racket[wrapper-proc]).}


@defproc[(impersonate-struct [v any/c]
                             [orig-proc (or/c struct-accessor-procedure?
                                              struct-mutator-procedure?
                                              struct-type-property-accessor-procedure?)]
                             [redirect-proc procedure?] ... ...
                             [prop impersonator-property?]
                             [prop-val any] ... ...)
          any/c]{

Returns an impersonator of @racket[v], which redirects certain
operations on the impersonated value. The @racket[orig-proc]s
indicate the operations to redirect, and the corresponding
@racket[redirect-proc]s supply the redirections.

The protocol for a @racket[redirect-proc] depends on the corresponding
@racket[orig-proc]:

@itemlist[

 @item{A structure-field accessor: @racket[redirect-proc]
      must accept two arguments, @racket[v] and the value
      @racket[_field-v] that @racket[orig-proc] produces for
      @racket[v]; it must return a replacement for
      @racket[_field-v]. The corresponding field must not be
      immutable, and either the field's structure type must be
      accessible via the current @tech{inspector} or one of the other
      @racket[orig-proc]s must be a structure-field mutator for the
      same field.}

 @item{A structure-field mutator: @racket[redirect-proc] must accept
      two arguments, @racket[v] and the value @racket[_field-v]
      supplied to the mutator; it must return a replacement for
      @racket[_field-v] to be propagated to @racket[orig-proc] and
      @racket[v].}

 @item{A property accessor: @racket[redirect-proc] uses the same
       protocol as for a structure-field accessor. The accessor's
       property must have been created with @racket['can-impersonate]
       as the second argument to @racket[make-struct-type-property].}

]

Pairs of @racket[prop] and @racket[prop-val] (the number of arguments
to @racket[impersonate-struct] must be odd) add impersonator properties
or override impersonator-property values of @racket[v].

Each @racket[orig-proc] must indicate a distinct operation. If no
@racket[orig-proc]s are supplied, then no @racket[prop]s must be
supplied, and @racket[v] is returned unimpersonated.}


@defproc[(impersonate-vector [vec (and/c vector? (not/c immutable?))]
                             [ref-proc (vector? exact-nonnegative-integer? any/c . -> . any/c)]
                             [set-proc (vector? exact-nonnegative-integer? any/c . -> . any/c)]
                             [prop impersonator-property?]
                             [prop-val any] ... ...)
          (and/c vector? impersonator?)]{

Returns an impersonator of @racket[vec], which redirects the
@racket[vector-ref] and @racket[vector-set!] operations.

The @racket[ref-proc] must accept @racket[vec], an index passed to
@racket[vector-ref], and the value that @racket[vector-ref] on
@racket[vec] produces for the given index; it must produce a
replacement for the value, which is the result of @racket[vector-ref]
on the impersonator.

The @racket[set-proc] must accept @racket[vec], an index passed to
@racket[vector-set!], and the value passed to @racket[vector-set!]; it
must produce a replacement for the value, which is used
with @racket[vector-set!] on the original @racket[vec] to install the
value.

Pairs of @racket[prop] and @racket[prop-val] (the number of arguments
to @racket[impersonate-vector] must be odd) add impersonator properties
or override impersonator-property values of @racket[vec].}

@defproc[(impersonate-box [box (and/c box? (not/c immutable?))]
                          [unbox-proc (box? any/c . -> . any/c)]
                          [set-proc (box? any/c . -> . any/c)]
                          [prop impersonator-property?]
                          [prop-val any] ... ...)
          (and/c box? impersonator?)]{

Returns an impersonator of @racket[box], which redirects the
@racket[unbox] and @racket[set-box!] operations.

The @racket[unbox-proc] must accept @racket[box] and the value that
@racket[unbox] produces on @racket[box]; it must produce a replacement
value, which is the result of @racket[unbox] on the impersonator.

The @racket[set-proc] must accept @racket[box] and the value passed to
@racket[set-box!]; it must produce a replacement
value, which is used with @racket[set-box!] on the original
@racket[box] to install the value.

Pairs of @racket[prop] and @racket[prop-val] (the number of arguments
to @racket[impersonate-box] must be odd) add impersonator properties
or override impersonator-property values of @racket[box].}


@defproc[(impersonate-hash [hash (and/c hash? (not/c immutable?))]
                           [ref-proc (hash? any/c . -> . (values 
                                                          any/c 
                                                          (hash? any/c any/c . -> . any/c)))]
                           [set-proc (hash? any/c any/c . -> . (values any/c any/c))]
                           [remove-proc (hash? any/c . -> . any/c)]
                           [key-proc (hash? any/c . -> . any/c)]
                           [clear-proc (or/c #f (hash? . -> . any)) #f]
                           [prop impersonator-property?]
                           [prop-val any] ... ...)
          (and/c hash? impersonator?)]{

Returns an impersonator of @racket[hash], which redirects the
@racket[hash-ref], @racket[hash-set!] or @racket[hash-set] (as
applicable), @racket[hash-remove] or @racket[hash-remove!] (as
applicable), @racket[hash-clear] or @racket[hash-clear!] (as
applicable and if @racket[clear-proc] is not @racket[#f]) operations. When
@racket[hash-set], @racket[hash-remove] or @racket[hash-clear] is used on an impersonator of a hash
table, the result is an impersonator with the same redirecting procedures. 
In addition, operations like
@racket[hash-iterate-key] or @racket[hash-map], which extract
keys from the table, use @racket[key-proc] to filter keys extracted
from the table. Operations like @racket[hash-iterate-value] or
@racket[hash-iterate-map] implicitly use @racket[hash-ref] and
therefore redirect through @racket[ref-proc].

The @racket[ref-proc] must accept @racket[hash] and a key passed
to @racket[hash-ref]. It must return a replacement key
as well as a procedure. The returned procedure is called only if the
returned key is found in @racket[hash] via @racket[hash-ref], in which
case the procedure is called with @racket[hash], the previously
returned key, and the found value. The returned procedure must itself
return a replacement for the found value.

The @racket[set-proc] must accept @racket[hash], a key passed to
@racket[hash-set!] or @racket[hash-set], and the value passed to
@racket[hash-set!] or @racket[hash-set]; it must produce two values: a
replacement for the key and a replacement for the value. The returned
key and value are used with @racket[hash-set!] or @racket[hash-set] on
the original @racket[hash] to install the value.

The @racket[remove-proc] must accept @racket[hash] and a key passed to
@racket[hash-remove!] or @racket[hash-remove]; it must produce the a
replacement for the key, which is used with @racket[hash-remove!] or
@racket[hash-remove] on the original @racket[hash] to remove any
mapping using the (impersonator-replaced) key.

The @racket[key-proc] must accept @racket[hash] and a key that has
been extracted from @racket[hash] (by @racket[hash-iterate-key] or
other operations that use @racket[hash-iterate-key] internally); it
must produce a replacement for the key, which is then reported as a
key extracted from the table.

If @racket[clear-proc] is not @racket[#f], it must accept
@racket[hash] as an argument, and its result is ignored. The fact that
@racket[clear-proc] returns (as opposed to raising an exception or
otherwise escaping) grants the capability to remove all keys from @racket[hash].
If @racket[clear-proc] is @racket[#f], then @racket[hash-clear] or
@racket[hash-clear!] on the impersonator is implemented using
@racket[hash-iterate-key] and @racket[hash-remove] or @racket[hash-remove!].

The @racket[hash-iterate-value], @racket[hash-map], or
@racket[hash-for-each] functions use a combination of
@racket[hash-iterate-key] and @racket[hash-ref]. If a key
produced by @racket[key-proc] does not yield a value through
@racket[hash-ref], then the @exnraise[exn:fail:contract].

Pairs of @racket[prop] and @racket[prop-val] (the number of arguments
to @racket[impersonate-hash] must be odd) add impersonator properties
or override impersonator-property values of @racket[hash].}


@defproc[(impersonate-channel [channel channel?]
                              [get-proc (channel? . -> . (values channel? (any/c . -> . any/c)))]
                              [put-proc (channel? any/c . -> . any/c)]
                              [prop impersonator-property?]
                              [prop-val any] ... ...)
          (and/c channel? impersonator?)]{

Returns an impersonator of @racket[channel], which redirects the
@racket[channel-get] and @racket[channel-put] operations.

The @racket[get-proc] generator is called on @racket[channel-get]
or any other operation that fetches results from the channel (such
as a @racket[sync] on the channel). The @racket[get-proc] must return
two values: a @tech{channel} that is an impersonator of @racket[channel], and a
procedure that is used to check the channel's contents.

The @racket[put-proc] must accept @racket[channel] and the value passed to
@racket[channel-put]; it must produce a replacement
value, which is used with @racket[channel-put] on the original
@racket[channel] to send the value over the channel.

Pairs of @racket[prop] and @racket[prop-val] (the number of arguments
to @racket[impersonate-channel] must be odd) add impersonator properties
or override impersonator-property values of @racket[channel].}


@defproc[(impersonate-prompt-tag [prompt-tag continuation-prompt-tag?]
                                 [handle-proc procedure?]
                                 [abort-proc procedure?]
                                 [cc-guard-proc procedure? values]
                                 [callcc-impersonate-proc (procedure? . -> . procedure?) (lambda (p) p)]
                                 [prop impersonator-property?]
                                 [prop-val any] ... ...)
          (and/c continuation-prompt-tag? impersonator?)]{

Returns an impersonator of @racket[prompt-tag], which redirects
the @racket[call-with-continuation-prompt] and
@racket[abort-current-continuation] operations.

The @racket[handle-proc] must accept the values that the handler
of a continuation prompt would take and it must produce replacement
values, which will be passed to the handler.

The @racket[abort-proc] must accept the values passed to
@racket[abort-current-continuation]; it must produce replacement
values, which are aborted to the appropriate prompt.

The @racket[cc-guard-proc] must accept the values produced by
@racket[call-with-continuation-prompt] in the case that a
non-composable continuation is applied to replace the continuation
that is delimited by the prompt, but only if
@racket[abort-current-continuation] is not later used to abort the
continuation delimited by the prompt (in which case
@racket[abort-proc] is used).

The @racket[callcc-impersonate-proc] must accept a procedure that
guards the result of a continuation captured by
@racket[call-with-current-continuation] with the impersonated prompt
tag. The @racket[callcc-impersonate-proc] is applied (under a
@tech{continuation barrier}) when the captured continuation is applied
to refine a guard function (initially @racket[values]) that is
specific to the delimiting prompt; this prompt-specific guard is
ultimately composed with any @racket[cc-guard-proc] that is in effect
at the delimiting prompt, and it is not used in the same case that a
@racket[cc-guard-proc] is not used (i.e., when
@racket[abort-current-continuation] is used to abort to the
prompt). In the special case where the delimiting prompt at
application time is a thread's built-in initial prompt,
@racket[callcc-impersonate-proc] is ignored (partly on the grounds
that the initial prompt's result is ignored).

Pairs of @racket[prop] and @racket[prop-val] (the number of arguments
to @racket[impersonate-prompt-tag] must be odd) add impersonator properties
or override impersonator-property values of @racket[prompt-tag].

@examples[
  (define tag
    (impersonate-prompt-tag
     (make-continuation-prompt-tag)
     (lambda (n) (* n 2))
     (lambda (n) (+ n 1))))

  (call-with-continuation-prompt
    (lambda ()
      (abort-current-continuation tag 5))
    tag
    (lambda (n) n))
]
}


@defproc[(impersonate-continuation-mark-key
          [key continuation-mark-key?]
          [get-proc procedure?]
          [set-proc procedure?]
          [prop impersonator-property?]
          [prop-val any] ... ...)
         (and/c continuation-mark? impersonator?)]{

Returns an impersonator of @racket[key], which redirects
@racket[with-continuation-mark] and continuation mark accessors such
as @racket[continuation-mark-set->list].

The @racket[get-proc] must accept the value attached to a
continuation mark and it must produce a replacement
value, which will be returned by the continuation mark accessor.

The @racket[set-proc] must accept a value passed to
@racket[with-continuation-mark]; it must produce a replacement
value, which is attached to the continuation frame.

Pairs of @racket[prop] and @racket[prop-val] (the number of arguments
to @racket[impersonate-prompt-tag] must be odd) add impersonator properties
or override impersonator-property values of @racket[key].

@examples[
  (define mark-key
    (impersonate-continuation-mark-key
     (make-continuation-mark-key)
     (lambda (l) (map char-upcase l))
     (lambda (s) (string->list s))))

  (with-continuation-mark mark-key "quiche"
    (continuation-mark-set-first
     (current-continuation-marks)
     mark-key))
]
}


@defthing[prop:impersonator-of struct-type-property?]{

A @tech{structure type property} (see @secref["structprops"]) that
supplies a procedure for extracting an impersonated value from a structure
that represents an impersonator. The property is used for @racket[impersonator-of?]
as well as @racket[equal?].

The property value must be a procedure of one argument, which is a
structure whose structure type has the property. The result can be
@racket[#f] to indicate the structure does not represent an impersonator,
otherwise the result is a value for which the original structure is an
impersonator (so the original structure is an @racket[impersonator-of?] and
@racket[equal?] to the result value). The result value must have the
same @racket[prop:impersonator-of] and @racket[prop:equal+hash] property
values as the original structure, and the property values must be
inherited from the same structure type (which ensures some consistency
between @racket[impersonator-of?] and @racket[equal?]).}

@; ------------------------------------------------------------
@section{Chaperone Constructors}

@defproc[(chaperone-procedure [proc procedure?]
                              [wrapper-proc procedure?]
                              [prop impersonator-property?]
                              [prop-val any] ... ...)
         (and/c procedure? chaperone?)]{

Like @racket[impersonate-procedure], but for each value supplied to
@racket[wrapper-proc], the corresponding result must be the same or a
chaperone of (in the sense of @racket[chaperone-of?])  the supplied
value. The additional result, if any, that precedes the chaperoned
values must be a procedure that accepts as many results as produced by
@racket[proc]; it must return the same number of results, each of
which is the same or a chaperone of the corresponding original result.

For applications that include keyword arguments, @racket[wrapper-proc]
must return an additional value before any other values but after the
result-chaperoning procedure (if any). The additional value must be a
list of chaperones of the keyword arguments that were supplied to the
chaperone procedure (i.e., not counting optional arguments that were
not supplied). The arguments must be ordered according to the sorted
order of the supplied arguments' keywords.}

@defproc[(chaperone-struct [v any/c]
                           [orig-proc (or/c struct-accessor-procedure?
                                            struct-mutator-procedure?
                                            struct-type-property-accessor-procedure?
                                            (one-of/c struct-info))]
                           [redirect-proc procedure?] ... ...
                           [prop impersonator-property?]
                           [prop-val any] ... ...)
          any/c]{

Like @racket[impersonate-struct], but with the following refinements:

@itemlist[

 @item{With a structure-field accessor as @racket[orig-proc],
      @racket[redirect-proc] must accept two arguments, @racket[v] and
      the value @racket[_field-v] that @racket[orig-proc] produces for
      @racket[v]; it must return a chaperone of @racket[_field-v]. The
      corresponding field may be immutable.}

 @item{With structure-field mutator as @racket[orig-proc],
      @racket[redirect-proc] must accept two arguments, @racket[v] and
      the value @racket[_field-v] supplied to the mutator; it must
      return a chaperone of @racket[_field-v] to be propagated to
      @racket[orig-proc] and @racket[v].}

 @item{A property accessor can be supplied as @racket[orig-proc], and
       the property need not have been created with
       @racket['can-impersonate].  The corresponding
       @racket[redirect-proc] uses the same protocol as for a
       structure-field accessor.}

 @item{With @racket[struct-info] as @racket[orig-proc], the
       corresponding @racket[redirect-proc] must accept two values,
       which are the results of @racket[struct-info] on @racket[v]; it
       must return each values or a chaperone of each value. The
       @racket[redirect-proc] is not called if @racket[struct-info]
       would return @racket[#f] as its first argument. An
       @racket[orig-proc] can be @racket[struct-info] only if some
       other @racket[orig-proc] is supplied.}

]}


@defproc[(chaperone-vector [vec vector?]
                           [ref-proc (vector? exact-nonnegative-integer? any/c . -> . any/c)]
                           [set-proc (vector? exact-nonnegative-integer? any/c . -> . any/c)]
                           [prop impersonator-property?]
                           [prop-val any] ... ...)
          (and/c vector? chaperone?)]{

Like @racket[impersonate-vector], but with support for immutable vectors. The
@racket[ref-proc] procedure must produce the same value or a chaperone
of the original value, and @racket[set-proc] must produce the value
that is given or a chaperone of the value. The @racket[set-proc] will
not be used if @racket[vec] is immutable.}

@defproc[(chaperone-box [box box?]
                        [unbox-proc (box? any/c . -> . any/c)]
                        [set-proc (box? any/c . -> . any/c)]
                        [prop impersonator-property?]
                        [prop-val any] ... ...)
          (and/c box? chaperone?)]{

Like @racket[impersonate-box], but with support for immutable boxes. The
@racket[unbox-proc] procedure must produce the same value or a
chaperone of the original value, and @racket[set-proc] must produce
the same value or a chaperone of the value that it is given.  The
@racket[set-proc] will not be used if @racket[box] is immutable.}


@defproc[(chaperone-hash [hash hash?]
                         [ref-proc (hash? any/c . -> . (values 
                                                        any/c 
                                                        (hash? any/c any/c . -> . any/c)))]
                         [set-proc (hash? any/c any/c . -> . (values any/c any/c))]
                         [remove-proc (hash? any/c . -> . any/c)]
                         [key-proc (hash? any/c . -> . any/c)]
                         [clear-proc (or/c #f (hash? . -> . any)) #f]
                         [prop impersonator-property?]
                         [prop-val any] ... ...)
          (and/c hash? chaperone?)]{

Like @racket[impersonate-hash], but with constraints on the given functions
and support for immutable hashes. The @racket[ref-proc] procedure must
return a found value or a chaperone of the value. The
@racket[set-proc] procedure must produce two values: the key that it
is given or a chaperone of the key and the value that it is given or a
chaperone of the value. The @racket[remove-proc] and @racket[key-proc]
procedures must produce the given key or a chaperone of the key.}

@defproc[(chaperone-struct-type [struct-type struct-type?]
                                [struct-info-proc procedure?]
                                [make-constructor-proc (procedure? . -> . procedure?)]
                                [guard-proc procedure?]
                                [prop impersonator-property?]
                                [prop-val any] ... ...)
          (and/c struct-type? chaperone?)]{

Returns a chaperoned value like @racket[struct-type], but with
@racket[struct-type-info] and @racket[struct-type-make-constructor]
operations on the chaperoned structure type redirected. In addition,
when a new structure type is created as a subtype of the chaperoned
structure type, @racket[guard-proc] is interposed as an extra guard on
creation of instances of the subtype.

The @racket[struct-info-proc] must accept 8 arguments---the result of
@racket[struct-type-info] on @racket[struct-type]. It must return 8
values, where each is the same or a chaperone of the corresponding
argument. The 8 values are used as the results of
@racket[struct-type-info] for the chaperoned structure type.

The @racket[make-constructor-proc] must accept a single procedure
argument, which is a constructor produced by
@racket[struct-type-make-constructor] on @racket[struct-type]. It must
return the same or a chaperone of the procedure, which is used as the
result of @racket[struct-type-make-constructor] on the chaperoned
structure type.

The @racket[guard-proc] must accept as many argument as a constructor
for @racket[struct-type]; it must return the same number of arguments,
each the same or a chaperone of the corresponding argument. The
@racket[guard-proc] is added as a constructor guard when a subtype is
created of the chaperoned structure type.

Pairs of @racket[prop] and @racket[prop-val] (the number of arguments
to @racket[chaperone-struct-type] must be even) add impersonator properties
or override impersonator-property values of @racket[struct-type].}

@defproc[(chaperone-evt [evt evt?]
                        [proc (evt? . -> . (values evt? (any/c . -> . any/c)))]
                        [prop impersonator-property?]
                        [prop-val any] ... ...)
          (and/c evt? chaperone?)]{

Returns a chaperoned value like @racket[evt], but with @racket[proc]
as an event generator when the result is synchronized with functions
like @racket[sync].

The @racket[proc] generator is called on synchronization, much like
the procedure passed to @racket[guard-evt], except that @racket[proc]
is given @racket[evt]. The @racket[proc] must return two values: a
@tech{synchronizable event} that is a chaperone of @racket[evt], and a
procedure that is used to check the event's result if it is chosen in
a selection. The latter procedure accepts the result of @racket[evt],
and it must return a chaperone of that value.

Pairs of @racket[prop] and @racket[prop-val] (the number of arguments
to @racket[chaperone-evt] must be even) add impersonator properties
or override impersonator-property values of @racket[evt].}


@defproc[(chaperone-channel [channel channel?]
                            [get-proc (channel? . -> . (values channel? (any/c . -> . any/c)))]
                            [put-proc (channel? any/c . -> . any/c)]
                            [prop impersonator-property?]
                            [prop-val any] ... ...)
          (and/c channel? chaperone?)]{

Like @racket[impersonate-channel], but with restrictions on the
@racket[get-proc] and @racket[put-proc] procedures.

The @racket[get-proc] must return two values: a @tech{channel}
that is a chaperone of @racket[channel], and a procedure that
is used to check the channel's contents. The latter procedure
must return the original value or a chaperone of that value.

The @racket[put-proc] must produce a replacement value that is
either the original value communicated on the channel or a
chaperone of that value.

Pairs of @racket[prop] and @racket[prop-val] (the number of arguments
to @racket[chaperone-channel] must be odd) add impersonator properties
or override impersonator-property values of @racket[channel].}


@defproc[(chaperone-prompt-tag [prompt-tag continuation-prompt-tag?]
                               [handle-proc procedure?]
                               [abort-proc procedure?]
                               [cc-guard-proc procedure? values]
                               [callcc-chaperone-proc (procedure? . -> . procedure?) (lambda (p) p)]
                               [prop impersonator-property?]
                               [prop-val any] ... ...)
          (and/c continuation-prompt-tag? chaperone?)]{

Like @racket[impersonate-prompt-tag], but produces a chaperoned value.
The @racket[handle-proc] procedure must produce the same values or
chaperones of the original values, @racket[abort-proc] must produce
the same values or chaperones of the values that it is given, and
@racket[cc-guard-proc] must produce the same values or chaperones of
the original result values, and @racket[callcc-chaperone-proc] must
produce a procedure that is a chaperone or the same as the given
procedure.

@examples[
  (define bad-chaperone
    (chaperone-prompt-tag
     (make-continuation-prompt-tag)
     (lambda (n) (* n 2))
     (lambda (n) (+ n 1))))

  (call-with-continuation-prompt
    (lambda ()
      (abort-current-continuation bad-chaperone 5))
    bad-chaperone
    (lambda (n) n))

  (define good-chaperone
    (chaperone-prompt-tag
     (make-continuation-prompt-tag)
     (lambda (n) (if (even? n) n (error "not even")))
     (lambda (n) (if (even? n) n (error "not even")))))

  (call-with-continuation-prompt
    (lambda ()
      (abort-current-continuation good-chaperone 2))
    good-chaperone
    (lambda (n) n))
]
}


@defproc[(chaperone-continuation-mark-key
          [key continuation-mark-key?]
          [get-proc procedure?]
          [set-proc procedure?]
          [prop impersonator-property?]
          [prop-val any] ... ...)
         (and/c continuation-mark-key? chaperone?)]{

Like @racket[impersonate-continuation-mark-key], but produces a
chaperoned value.  The @racket[get-proc] procedure must produce the
same value or a chaperone of the original value, and @racket[set-proc]
must produce the same value or a chaperone of the value that it is
given.

@examples[
  (define bad-chaperone
    (chaperone-continuation-mark-key
     (make-continuation-mark-key)
     (lambda (l) (map char-upcase l))
     string->list))

  (with-continuation-mark bad-chaperone "timballo"
    (continuation-mark-set-first
     (current-continuation-marks)
     bad-chaperone))

  (define (checker s)
    (if (> (string-length s) 5)
        s
        (error "expected string of length at least 5")))

  (define good-chaperone
    (chaperone-continuation-mark-key
     (make-continuation-mark-key)
     checker
     checker))

  (with-continuation-mark good-chaperone "zabaione"
    (continuation-mark-set-first
     (current-continuation-marks)
     good-chaperone))
]
}

@; ------------------------------------------------------------
@section{Impersonator Properties}

@defproc[(make-impersonator-property [name symbol?])
         (values impersonator-property?
                 (-> any/c boolean?)
                 (-> impersonator? any))]{

Creates a new @tech{impersonator property} and returns three values:

@itemize[

 @item{an @deftech{impersonator property descriptor}, for use with
       @racket[impersonate-procedure], @racket[chaperone-procedure], 
       and other impersonator constructors;}

 @item{an @deftech{impersonator property predicate} procedure, which takes
       an arbitrary value and returns @racket[#t] if the value is an
       impersonator with a value for the property, @racket[#f]
       otherwise;}

 @item{an @deftech{impersonator property accessor} procedure, which
       returns the value associated with an impersonator for the property;
       if a value given to the accessor is not an impersonator or does not
       have a value for the property (i.e. if the corresponding impersonator
       property predicate returns @racket[#f]), the @exnraise[exn:fail:contract].}

]}

@defproc[(impersonator-property? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a @tech{impersonator property
descriptor} value, @racket[#f] otherwise.}

@defproc[(impersonator-property-accessor-procedure? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is an accessor procedure produced
by @racket[make-impersonator-property], @racket[#f] otherwise.}


@defthing[impersonator-prop:application-mark impersonator-property?]{

An @tech{impersonator property} that is recognized by @racket[impersonate-procedure]
and @racket[chaperone-procedure].}

