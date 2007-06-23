#reader(lib "docreader.ss" "scribble")
@require["mz.ss"]

@title[#:tag "mz:structures"]{Structures}

A @pidefterm{structure type} is a record datatype composing a number
of @idefterm{fields}. A @pidefterm{structure}, an instance of a
structure type, is a first-class value that contains a value for each
field of the structure type. A structure instance is created with a
type-specific @tech{constructor} procedure, and its field values are
accessed and changed with type-specific @tech{accessor} and
@tech{mutator} procedures. In addition, each structure type has a
@tech{predicate} procedure that answers @scheme[#t] for instances of
the structure type and @scheme[#f] for any other value.

A structure type's fields are essentially unnamed, though names are
supported for error-reporting purposes. The constructor procedure
takes one value for each field of the structure type, except that some
of the fields of a structure type can be @deftech{automatic fields};
the @tech{automatic fields} are initialized to a constant that is
associated with the structure type, and the corresponding arguments
are omitted for the constructor procedure. All automatic fields in a
structure type follow the non-automatic fields.

A structure type can be created as a @pidefterm{structure subtype} of
an existing base structure type. An instance of a structure subtype
can always be used as an instance of the base structure type, but the
subtype gets its own predicate procedure, and it may have its own
fields in addition to the fields of the base type.

A structure subtype ``inherits'' the fields of its base type. If the
base type has @math{m} fields, and if @math{n} fields are specified
for the new structure subtype, then the resulting structure type has
@math{m+n} fields. The value for automatic fields can be different in
a subtype than in its base type.

If @math{m'} of the original @math{m} fields are non-automatic (where
@math{m'<m}), and @math{n'} of the new fields are automatic (where
@math{n'<n}), then @math{m'+n'} field values must be provided to the
subtype's constructor procedure. Values for the first @math{m} fields
of a subtype instance are accessed with selector procedures for the
original base type (or its supertypes), and the last @math{n} are
accessed with subtype-specific selectors. Subtype-specific
@tech{accessors} and @tech{mutators} for the first @math{m} fields do
not exist.

@index['("structures" "equality")]{Two} structure values are
@scheme[eqv?] if and only if they are @scheme[eq?]. Two structure
values are @scheme[equal?] if they are @scheme[eq?], or if they are
instances of the same structure type, no fields are opaque, and the
results of applying @scheme[struct->vector] to the structs are
@scheme[equal?]. (Consequently, @scheme[equal?] testing for structures
depends on the current inspector.)

@;------------------------------------------------------------------------
@section[#:tag "mz:creatingmorestructs"]{Creating Structure Types}

@defproc[(make-struct-type [name symbol?]
                           [super-type (or/c struct-type? false/c)]
                           [init-field-cnt non-negative-exact-integer?]
                           [auto-field-cnt non-negative-exact-integer?]
                           [auto-v any/c #f]
                           [props (listof (cons/c struct-type-property?
                                                  any/c))
                                  null]
                           [inspector (or/c inspector? false/c)
                                      (current-inspector)]
                           [proc-spec (or/c procedure? 
                                            non-negative-exact-integer?
                                            false/c)
                                      #f]
                           [immutables (listof non-negative-exact-integer?)
                                       null]
                           [guard (or/c procedure? false/c) #f])
          (values 
           struct-type?
           struct-constructor-procedure?
           struct-predicate-procedure?
           struct-accessor-procedure?
           struct-mutator-procedure?)]{

Creates a new structure type.  The @scheme[name] argument is used as
the type name. If @scheme[super-type] is not @scheme[#f], the new type
is a subtype of the corresponding structure type.

The new structure type has @scheme[(+ init-field-cnt auto-field-cnt)]
fields (in addition to any fields from @scheme[super-type]), but only
@scheme[init-field-cnt] constructor arguments (in addition to any
constructor arguments from @scheme[super-type]). The remaining
fields are initialized with @scheme[auto-v].
 
The @scheme{props} argument is a list of pairs, where the @scheme[car]
of each pair is a structure type property descriptor, and the
@scheme[cdr] is an arbitrary value. See @secref["mz:structprops"] for
more information about properties.

The @scheme[inspector] argument controls access to reflective
information about the structure type and its instances; see
@secref["mz:inspectors"] for more information.

If @scheme[proc-spec] is an integer or procedure, instances of the
structure type act as procedures. See @secref["mz:procstructs"] for
further information.  Providing a non-@scheme[#f] value for
@scheme[proc-spec] is the same as pairing the value with
@scheme[prop:procedure] in @scheme[props], plus including
@scheme[proc-spec] in @scheme[immutables] when
@scheme[proc-spec] is an integer.

The @scheme[immutables] argument provides a list of field
positions. Each element in the list must be unique, otherwise
@exnraise[exn:fail:contract]. Each element must also fall in the range
@scheme[0] (inclusive) to @scheme[init-field-cnt] (exclusive), otherwise
@exnraise[exn:fail:contract].

The @scheme[guard] argument is either a procedure of @math{n}
arguments or @scheme[#f], where @math{n} is the number of arguments
for the new structure type's constructor (i.e., @scheme[init-field-cnt]
plus constructor arguments implied by @scheme[super-type], if any). If
@scheme[guard] is a procedure, then the procedure is called
whenever an instance of the type is constructed, or whenever an
instance of a subtype is created.  The arguments to
@scheme[guard] are the values provided for the structure's first
@math{n} fields, followed by the name of the instantiated structure
type (which is @scheme[name], unless a subtype is instantiated). The
@scheme[guard] result must be @math{n} values, which become the
actual values for the structure's fields. The @scheme[guard] can
raise an exception to prevent creation of a structure with the given
field values. If a structure subtype has its own guard, the subtype
guard is applied first, and the first @math{n} values produced by the
subtype's guard procedure become the first @math{n} arguments to
@scheme[guard].

The result of @scheme[make-struct-type] is five values:
%
@itemize{

 @item{a @tech{structure type descriptor},}

 @item{a @tech{constructor} procedure,}

 @item{a @tech{predicate} procedure,}

 @item{an @tech{accessor} procedure, which consumes a structure and a field
 index between @math{0} (inclusive) and
 @math{@scheme[init-field-cnt]+@scheme[auto-field-cnt]} (exclusive),
 and}

 @item{a @tech{mutator} procedure, which consumes a structure, a field
 index, and a field value.}

}

@examples[
(define-values (struct:a make-a a? a-ref a-set!) 
  (make-struct-type 'a #f 2 1 'uninitialized)) 
(define an-a (make-a 'x 'y)) 
(a-ref an-a 1)
(a-ref an-a 2)
(define a-first (make-struct-field-accessor a-ref 0)) 
(a-first an-a)

(define-values (struct:b make-b b? b-ref b-set!) 
  (make-struct-type 'b struct:a 1 2 'b-uninitialized))
(define a-b (make-b 'x 'y 'z)) 
(a-ref a-b 1)
(a-ref a-b 2)
(b-ref a-b 0)
(b-ref a-b 1)
(b-ref a-b 2)

(define-values (struct:c make-c c? c-ref c-set!) 
  (make-struct-type 
   'c struct:b 0 0 #f null (make-inspector) #f null
   (code:comment #,(t "guard checks for a number, and makes it inexact"))
   (lambda (a1 a2 b1 name)
     (unless (number? a2)
       (error (string->symbol (format "make-~a" name))
              "second field must be a number"))
     (values a1 (exact->inexact a2) b1))))
(make-c 'x 'y 'z)
(define a-c (make-c 'x 2 'z))
(a-ref a-c 1)
]}

@defproc[(make-struct-field-accessor [accessor-proc struct-accessot-procedure?]
                                     [field-pos-k exact-nonnegative-integer?]
                                     [field-name symbol?])
         procedure?]{

Returns a field accessor that is equivalent to @scheme[(lambda (s)
(accessor-proc s field-pos-k))].  The @scheme[accessor-proc] must be
an @tech{accessor} returned by @scheme[make-struct-type]. The name of the
resulting procedure for debugging purposes is derived from
@scheme[field-name] and the name of @scheme[accessor-proc]'s
structure type.

For examples, see @scheme[make-struct-type].}

@defproc[(make-struct-field-mutator [mutator-proc struct-mutator-procedure?]
                                    [field-pos-k exact-nonnegative-integer?]
                                    [field-name symbol?])
         procedure?]{

Returns a field mutator that is equivalent to @scheme[(lambda (s v)
(mutator-proc s field-pos-k v))].  The @scheme[mutator-proc] must be
a @tech{mutator} returned by @scheme[make-struct-type]. The name of the
resulting procedure for debugging purposes is derived from
@scheme[field-name] and the name of @scheme[mutator-proc]'s
structure type.

For examples, see @scheme[make-struct-type].}


@;------------------------------------------------------------------------
@section[#:tag "mz:structprops"]{Structure Type Properties}

A @index['("structure type properties")]{@defterm{structure type
 property}} allows per-type information to be associated with a
 structure type (as opposed to per-instance information associated
 with a structure value). A property value is associated with a
 structure type through the @scheme[make-struct-type] procedure (see
 @secref["mz:creatingmorestructs"]) or through the @scheme[#:property]
 option of @scheme[define-struct].  Subtypes inherit the property
 values of their parent types, and subtypes can override an inherited
 property value with a new value.

@defproc[(make-struct-type-property [name symbol?]
                                    [guard (or/c procedure? false/c) #f]) 
         (values 
          struct-type-property?
          procedure?
          procedure?)]{

 Creates a new structure type property and returns three values:

@itemize{

 @item{a @deftech{structure property type descriptor}, for use with
       @scheme[make-struct-type] and @scheme[define-struct];}

 @item{a @deftech{property predicate} procedure, which takes an
       arbitrary value and returns @scheme[#t] if the value is a
       descriptor or instance of a structure type that has a value for
       the property, @scheme[#f] otherwise;}

 @item{an @deftech{property accessor} procedure, which returns the
       value associated with structure type given its descriptor or
       one of its instances; if the structure type does not have a
       value for the property, or if any other kind of value is
       provided, the @exnraise[exn:fail:contract].}

}

If the optional @scheme[guard] is supplied as a procedure, it is
called by @scheme[make-struct-type] before attaching the property to a
new structure type. The @scheme[guard-proc] must accept two arguments:
a value for the property supplied to @scheme[make-struct-type], and a
list containing information about the new structure type. The list
contains the values that @scheme[struct-type-info] would return for
the new structure type if it skipped the immediate current-inspector
control check (but not the check for exposing an ancestor structure
type, if any; see @secref["mz:inspectors"]).

The result of calling @scheme[guard] is associated with the property
in the target structure type, instead of the value supplied to
@scheme[make-struct-type]. To reject a property association (e.g.,
because the value supplied to @scheme[make-struct-type] is
inappropriate for the property), the @scheme[guard] can raise an
exception. Such an exception prevents @scheme[make-struct-type] from
returning a structure type descriptor.

@examples[
(define-values (prop:p p? p-ref) (make-struct-type-property 'p))

(define-values (struct:a make-a a? a-ref a-set!) 
  (make-struct-type 'a #f 2 1 'uninitialized 
                    (list (cons prop:p 8))))
(p? struct:a)
(p? 13)
(define an-a (make-a 'x 'y)) 
(p? an-a)
(p-ref an-a)

(define-values (struct:b make-b b? b-ref b-set!) 
  (make-struct-type 'b #f 0 0 #f)) 
(p? struct:b)
]}

@defproc[(struct-type-property? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a @tech{structure type property
descriptor} value, @scheme[#f] otherwise.

}

@;------------------------------------------------------------------------
@section[#:tag "mz:inspectors"]{Structure Inspectors}

An @pidefterm{inspector} provides access to structure fields and
structure type information without the normal field accessors and
mutators. (Inspectors are also used to control access to module
bindings; see @secref["mz:modprotect"].) Inspectors are primarily
intended for use by debuggers.

When a structure type is created, an inspector can be supplied. The
given inspector is not the one that will control the new structure
type; instead, the given inspector's parent will control the type. By
using the parent of the given inspector, the structure type remains
opaque to ``peer'' code that cannot access the parent inspector.

The @scheme[current-inspector] @tech{parameter} determines a default
inspector argument for new structure types. An alternate inspector can
be provided though the @scheme[#:inspector] option of the
@scheme[define-struct] form (see @secref["mz:define-struct"]), or
through an optional @scheme[inspector] argument to
@scheme[make-struct-type] (see @secref["mz:creatingmorestructs"]).

@defproc[(make-inspector [inspector inspector? (current-inspector)])
         inspector?]{

Returns a new inspector that is a subinspector of
@scheme[inspector]. Any structure type controlled by the new inspector
is also controlled by its ancestor inspectors, but no other
inspectors.}

@defproc[(inspector? [v any/c]) boolean?]{Returns @scheme[#t] if
@scheme[v] is an inspector, @scheme[#f] otherwise.}

@defproc[(struct-info [v any/c])
         (values (or/c struct-type? false/c)
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
         (values 
          symbol?
          nonnegative-exact-integer?
          nonnegative-exact-integer?
          struct-accessor-procedure?
          struct-mutator-procedure?
          (listof nonnegative-exact-integer?)
          (or/c struct-type? false/c)
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

@;------------------------------------------------------------------------
@section[#:tag "mz:structutils"]{Structure Utilities}

@defproc[(struct->vector [v any/c] [opaque-v any/c '...]) vector?]{

Creates a vector representing @scheme[v].  The first slot of the
result vector contains a symbol whose printed name has the form
@schemeidfont{struct:}@scheme[_id]. Each remaining slot contains
either the value of a field in @scheme[v], if it is accessible via the
current inspector, or @scheme[opaque-v] for a field that is not
accessible. A single @scheme[opaque-v] value is used in the vector for
contiguous inaccessible fields. (Consequently, the size of the vector
does not match the size of the @scheme[struct] if more than one field
is inaccessible.)}

@defproc[(struct? [v any/c]) any]{ Returns @scheme[#t] if
 @scheme[struct->vector] exposes any fields of @scheme[v] with the
 current inspector, @scheme[#f] otherwise.}

@defproc[(struct-type? [v any/c]) boolean?]{Returns @scheme[#t] if
 @scheme[v] is a structure type descriptor value, @scheme[#f]
 otherwise.}

@defproc[(struct-constructor-procedure? [v any/c]) boolean?]{Returns
 @scheme[#t] if @scheme[v] is a constructor procedure generated by
 @scheme[define-struct] or @scheme[make-struct-type], @scheme[#f]
 otherwise.}

@defproc[(struct-predicate-procedure? [v any/c]) boolean?]{Returns
 @scheme[#t] if @scheme[v] is a predicate procedure generated by
 @scheme[define-struct] or @scheme[make-struct-type], @scheme[#f]
 otherwise.}

@defproc[(struct-accessor-procedure? [v any/c]) boolean?]{Returns
 @scheme[#t] if @scheme[v] is an accessor procedure generated by
 @scheme[define-struct], @scheme[make-struct-type], or
 @scheme[make-struct-field-accessor], @scheme[#f] otherwise.}

@defproc[(struct-mutator-procedure? [v any/c]) boolean?] {Returns
 @scheme[#t] if @scheme[v] is a mutator procedure generated by
 @scheme[define-struct], @scheme[make-struct-type], or
 @scheme[make-struct-field-mutator], @scheme[#f] otherwise.}
