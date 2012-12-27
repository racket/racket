#lang scribble/doc
@(require "mz.rkt")

@title[#:tag "inspectors"]{Structure Inspectors}

An @deftech{inspector} provides access to structure fields and
structure type information without the normal field accessors and
mutators. (Inspectors are also used to control access to module
bindings; see @secref["modprotect"].) Inspectors are primarily
intended for use by debuggers.

When a structure type is created, an inspector can be supplied. The
given inspector is not the one that will control the new structure
type; instead, the given inspector's parent will control the type. By
using the parent of the given inspector, the structure type remains
opaque to ``peer'' code that cannot access the parent inspector.

The @racket[current-inspector] @tech{parameter} determines a default
inspector argument for new structure types. An alternate inspector can
be provided though the @racket[#:inspector] option of the
@racket[define-struct] form (see @secref["define-struct"]), or
through an optional @racket[inspector] argument to
@racket[make-struct-type].


@defproc[(inspector? [v any/c]) boolean?]{Returns @racket[#t] if
@racket[v] is an inspector, @racket[#f] otherwise.}


@defproc[(make-inspector [inspector inspector? (current-inspector)])
         inspector?]{

Returns a new inspector that is a subinspector of
@racket[inspector]. Any structure type controlled by the new inspector
is also controlled by its ancestor inspectors, but no other
inspectors.}


@defproc[(make-sibling-inspector [inspector inspector? (current-inspector)])
         inspector?]{

Returns a new inspector that is a subinspector of the same inspector
as @racket[inspector]. That is, @racket[inspector] and the result
inspector control mutually disjoint sets of structure types.}


@defparam[current-inspector insp inspector?]{

A @tech{parameter} that determines the default inspector for newly created
structure types.}


@defproc[(struct-info [v any/c])
         (values (or/c struct-type? #f)
                 boolean?)]{

Returns two values:

@itemize[

  @item{@racket[_struct-type]: a structure type descriptor or @racket[#f];
  the result is a structure type descriptor of the most specific type
  for which @racket[v] is an instance, and for which the current
  inspector has control, or the result is @racket[#f] if the current
  inspector does not control any structure type for which the
  @racket[struct] is an instance.}

  @item{@racket[_skipped?]: @racket[#f] if the first result corresponds to
  the most specific structure type of @racket[v], @racket[#t] otherwise.}

]}

@defproc[(struct-type-info [struct-type struct-type?])
         (values symbol?
                 exact-nonnegative-integer?
                 exact-nonnegative-integer?
                 struct-accessor-procedure?
                 struct-mutator-procedure?
                 (listof exact-nonnegative-integer?)
                 (or/c struct-type? #f)
                 boolean?)]{

Returns eight values that provide information about the structure type
 descriptor @racket[struct-type], assuming that the type is controlled
 by the current inspector:

 @itemize[

  @item{@racket[_name]: the structure type's name as a symbol;}

  @item{@racket[_init-field-cnt]: the number of fields defined by the
   structure type provided to the constructor procedure (not counting
   fields created by its ancestor types);}

  @item{@racket[_auto-field-cnt]: the number of fields defined by the
   structure type without a counterpart in the constructor procedure
   (not counting fields created by its ancestor types);}

  @item{@racket[_accessor-proc]: an accessor procedure for the structure
   type, like the one returned by @racket[make-struct-type];}

  @item{@racket[_mutator-proc]: a mutator procedure for the structure
   type, like the one returned by @racket[make-struct-type];}

  @item{@racket[_immutable-k-list]: an immutable list of exact
   non-negative integers that correspond to immutable fields for the
   structure type;}

  @item{@racket[_super-type]: a structure type descriptor for the
   most specific ancestor of the type that is controlled by the
   current inspector, or @racket[#f] if no ancestor is controlled by
   the current inspector;}

  @item{@racket[_skipped?]: @racket[#f] if the seventh result is the
   most specific ancestor type or if the type has no supertype,
   @racket[#t] otherwise.}

]

If the type for @racket[struct-type] is not controlled by the current inspector,
the @exnraise[exn:fail:contract].}

@defproc[(struct-type-make-constructor [struct-type struct-type?])
         struct-constructor-procedure?]{

Returns a @tech{constructor} procedure to create instances of the type
for @racket[struct-type].  If the type for @racket[struct-type] is not
controlled by the current inspector, the
@exnraise[exn:fail:contract].}

@defproc[(struct-type-make-predicate [struct-type any/c]) any]{

Returns a @tech{predicate} procedure to recognize instances of the
type for @racket[struct-type].  If the type for @racket[struct-type]
is not controlled by the current inspector, the
@exnraise[exn:fail:contract].}



@defproc[(object-name [v any/c]) any]{

Returns a value for the name of @racket[v] if @racket[v] has a name,
@racket[#f] otherwise. The argument @racket[v] can be any value, but
only (some) procedures, @tech{structures}, @tech{structure types},
@tech{structure type properties}, @tech{regexp values},
@tech{ports}, and @tech{loggers} have names. See also @secref["infernames"].

The name (if any) of a procedure is always a symbol. The
@racket[procedure-rename] function creates a procedure with a specific
name.

The name of a @tech{structure}, @tech{structure type}, @tech{structure
type property} is always a symbol. If a @tech{structure} is a
procedure as implemented by one of its fields (i.e., the
@racket[prop:procedure] property value for the structure's type is an
integer), then its name is the implementing procedure's name;
otherwise, its name matches the name of the @tech{structure type} that
it instantiates.

The name of a @tech{regexp value} is a string or byte string. Passing
the string or byte string to @racket[regexp], @racket[byte-regexp],
@racket[pregexp], or @racket[byte-pregexp] (depending on the kind of
regexp whose name was extracted) produces a value that matches the
same inputs.

The name of a port can be any value, but many tools use a path or
string name as the port's for (to report source locations, for
example).

The name of a @tech{logger} is either a symbol or @racket[#f].}

