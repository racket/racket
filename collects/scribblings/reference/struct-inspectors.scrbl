#lang scribble/doc
@(require "mz.ss")

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

The @scheme[current-inspector] @tech{parameter} determines a default
inspector argument for new structure types. An alternate inspector can
be provided though the @scheme[#:inspector] option of the
@scheme[define-struct] form (see @secref["define-struct"]), or
through an optional @scheme[inspector] argument to
@scheme[make-struct-type].


@defproc[(inspector? [v any/c]) boolean?]{Returns @scheme[#t] if
@scheme[v] is an inspector, @scheme[#f] otherwise.}


@defproc[(make-inspector [inspector inspector? (current-inspector)])
         inspector?]{

Returns a new inspector that is a subinspector of
@scheme[inspector]. Any structure type controlled by the new inspector
is also controlled by its ancestor inspectors, but no other
inspectors.}


@defproc[(make-sibling-inspector [inspector inspector? (current-inspector)])
         inspector?]{

Returns a new inspector that is a subinspector of the same inspector
as @scheme[inspector]. That is, @scheme[inspector] and the result
inspector control mutually disjoint sets of structure types.}


@defparam[current-inspector insp inspector?]{

A parameter that determines the default inspector for newly created
structure types.}


@defproc[(struct-info [v any/c])
         (values (or/c struct-type? #f)
                 boolean?)]{

Returns two values:

@itemize{

  @item{@scheme[struct-type]: a structure type descriptor or @scheme[#f];
  the result is a structure type descriptor of the most specific type
  for which @scheme[v] is an instance, and for which the current
  inspector has control, or the result is @scheme[#f] if the current
  inspector does not control any structure type for which the
  @scheme[struct] is an instance.}

  @item{@scheme[skipped?]: @scheme[#f] if the first result corresponds to
  the most specific structure type of @scheme[v], @scheme[#t] otherwise.}

}}

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
 descriptor @scheme[struct-type], assuming that the type is controlled
 by the current inspector:

 @itemize{

  @item{@scheme[name]: the structure type's name as a symbol;}

  @item{@scheme[init-field-cnt]: the number of fields defined by the
   structure type provided to the constructor procedure (not counting
   fields created by its ancestor types);}

  @item{@scheme[auto-field-cnt]: the number of fields defined by the
   structure type without a counterpart in the constructor procedure
   (not counting fields created by its ancestor types);}

  @item{@scheme[accessor-proc]: an accessor procedure for the structure
   type, like the one returned by @scheme[make-struct-type];}

  @item{@scheme[mutator-proc]: a mutator procedure for the structure
   type, like the one returned by @scheme[make-struct-type];}

  @item{@scheme[immutable-k-list]: an immutable list of exact
   non-negative integers that correspond to immutable fields for the
   structure type;}

  @item{@scheme[super-type]: a structure type descriptor for the
   most specific ancestor of the type that is controlled by the
   current inspector, or @scheme[#f] if no ancestor is controlled by
   the current inspector;}

  @item{@scheme[skipped?]: @scheme[#f] if the seventh result is the
   most specific ancestor type or if the type has no supertype,
   @scheme[#t] otherwise.}

}

If the type for @scheme[struct-type] is not controlled by the current inspector,
the @exnraise[exn:fail:contract].}

@defproc[(struct-type-make-constructor [struct-type struct-type?])
         struct-constructor-procedure?]{

Returns a @tech{constructor} procedure to create instances of the type
for @scheme[struct-type].  If the type for @scheme[struct-type] is not
controlled by the current inspector, the
@exnraise[exn:fail:contract].}

@defproc[(struct-type-make-predicate [struct-type any/c]) any]{

Returns a @tech{predicate} procedure to recognize instances of the
type for @scheme[struct-type].  If the type for @scheme[struct-type]
is not controlled by the current inspector, the
@exnraise[exn:fail:contract].}



@defproc[(object-name [v any/c]) any]{

Returns a value for the name of @scheme[v] if @scheme[v] has a name,
@scheme[#f] otherwise. The argument @scheme[v] can be any value, but
only (some) procedures, structs, struct types, struct type properties,
regexp values, and ports have names. The name of a procedure, struct,
struct type, or struct type property is always a symbol. The name of a
regexp value is a string, and a byte-regexp value's name is a byte
string. The name of a port is typically a path or a string, but it can
be arbitrary. See also @secref["infernames"].}

