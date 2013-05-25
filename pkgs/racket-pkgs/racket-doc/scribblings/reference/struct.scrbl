#lang scribble/doc
@(require "mz.rkt" (for-label racket/struct-info))

@(define struct-eval (make-base-eval))
@(define struct-copy-eval (make-base-eval))

@title[#:tag "structures" #:style 'toc]{Structures}

@guideintro["define-struct"]{structure types via @racket[struct]}

A @deftech{structure type} is a record datatype composing a number of
@idefterm{fields}. A @deftech{structure}, an instance of a structure
type, is a first-class value that contains a value for each field of
the structure type. A structure instance is created with a
type-specific @tech{constructor} procedure, and its field values are
accessed and changed with type-specific @tech{accessor} and
@tech{mutator} procedures. In addition, each structure type has a
@tech{predicate} procedure that answers @racket[#t] for instances of
the structure type and @racket[#f] for any other value.

A structure type's fields are essentially unnamed, though names are
supported for error-reporting purposes. The constructor procedure
takes one value for each field of the structure type, except that some
of the fields of a structure type can be @deftech{automatic fields};
the @tech{automatic fields} are initialized to a constant that is
associated with the structure type, and the corresponding arguments
are omitted from the constructor procedure. All automatic fields in a
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

The @racket[struct] form and @racket[make-struct-type]
procedure typically create a new structure type, but they can also
access @deftech{prefab} (i.e., previously fabricated) structure types
that are globally shared, and whose instances can be parsed and
written by the default reader (see @secref["reader"]) and printer (see
@secref["printing"]). Prefab structure types can inherit only from
other prefab structure types, and they cannot have guards (see
@secref["creatingmorestructs"]) or properties (see
@secref["structprops"]). Exactly one prefab structure type exists for
each combination of name, supertype, field count, automatic field
count, automatic field value (when there is at least one automatic
field), and field mutability.

@refalso["serialization"]{reading and writing structures}

@index['("structures" "equality")]{Two} structure values are
@racket[eqv?] if and only if they are @racket[eq?]. Two structure
values are @racket[equal?] if they are @racket[eq?]. By default, two
structure values are also @racket[equal?] if they are instances of the
same structure type, no fields are opaque, and the results of applying
@racket[struct->vector] to the structs are
@racket[equal?]. (Consequently, @racket[equal?]  testing for
structures may depend on the current inspector.) A structure type can
override the default @racket[equal?] definition through the
@racket[gen:equal+hash] @tech{generic interface}.

@local-table-of-contents[]

@;------------------------------------------------------------------------
@include-section["define-struct.scrbl"]

@;------------------------------------------------------------------------
@section[#:tag "creatingmorestructs"]{Creating Structure Types}

@defproc[(make-struct-type [name symbol?]
                           [super-type (or/c struct-type? #f)]
                           [init-field-cnt exact-nonnegative-integer?]
                           [auto-field-cnt exact-nonnegative-integer?]
                           [auto-v any/c #f]
                           [props (listof (cons/c struct-type-property?
                                                  any/c))
                                  null]
                           [inspector (or/c inspector? #f 'prefab)
                                      (current-inspector)]
                           [proc-spec (or/c procedure?
                                            exact-nonnegative-integer?
                                            #f)
                                      #f]
                           [immutables (listof exact-nonnegative-integer?)
                                       null]
                           [guard (or/c procedure? #f) #f]
                           [constructor-name (or/c symbol? #f) #f])
          (values struct-type?
                  struct-constructor-procedure?
                  struct-predicate-procedure?
                  struct-accessor-procedure?
                  struct-mutator-procedure?)]{

Creates a new structure type, unless @racket[inspector] is
@racket['prefab], in which case @racket[make-struct-type] accesses a
@techlink{prefab} structure type.  The @racket[name] argument is used
as the type name. If @racket[super-type] is not @racket[#f], the
resulting type is a subtype of the corresponding structure type.

The resulting structure type has
@math{@racket[init-field-cnt]+@racket[auto-field-cnt]} fields (in
addition to any fields from @racket[super-type]), but only
@racket[init-field-cnt] constructor arguments (in addition to any
constructor arguments from @racket[super-type]). The remaining fields
are initialized with @racket[auto-v]. The total field count (including
@racket[super-type] fields) must be no more than 32768.

The @racket[props] argument is a list of pairs, where the @racket[car]
of each pair is a structure type property descriptor, and the
@racket[cdr] is an arbitrary value. A property can be specified
multiple times in @racket[props] (including properties that are
automatically added by properties that are directly included in
@racket[props]) only if the associated values are @racket[eq?],
otherwise the @exnraise[exn:fail:contract]. See @secref["structprops"]
for more information about properties. When @racket[inspector] is
@racket['prefab], then @racket[props] must be @racket[null].

The @racket[inspector] argument normally controls access to reflective
information about the structure type and its instances; see
@secref["inspectors"] for more information. If @racket[inspector] is
@racket['prefab], then the resulting @tech{prefab} structure type and
its instances are always transparent.

If @racket[proc-spec] is an integer or procedure, instances of the
structure type act as procedures. See @racket[prop:procedure] for
further information.  Providing a non-@racket[#f] value for
@racket[proc-spec] is the same as pairing the value with
@racket[prop:procedure] at the end of @racket[props], plus including
@racket[proc-spec] in @racket[immutables] when @racket[proc-spec] is
an integer.

The @racket[immutables] argument provides a list of field
positions. Each element in the list must be unique, otherwise
@exnraise[exn:fail:contract]. Each element must also fall in the range
@racket[0] (inclusive) to @racket[init-field-cnt] (exclusive), otherwise
@exnraise[exn:fail:contract].

The @racket[guard] argument is either a procedure of @math{n+1}
arguments or @racket[#f], where @math{n} is the number of arguments
for the new structure type's constructor (i.e.,
@racket[init-field-cnt] plus constructor arguments implied by
@racket[super-type], if any). If @racket[guard] is a procedure, then
the procedure is called whenever an instance of the type is
constructed, or whenever an instance of a subtype is created.  The
arguments to @racket[guard] are the values provided for the
structure's first @math{n} fields, followed by the name of the
instantiated structure type (which is @racket[name], unless a subtype
is instantiated). The @racket[guard] result must be @math{n} values,
which become the actual values for the structure's fields. The
@racket[guard] can raise an exception to prevent creation of a
structure with the given field values. If a structure subtype has its
own guard, the subtype guard is applied first, and the first @math{n}
values produced by the subtype's guard procedure become the first
@math{n} arguments to @racket[guard]. When @racket[inspector] is
@racket['prefab], then @racket[guard] must be @racket[#f].

If @racket[constructor-name] is not @racket[#f], it is used as the
name of the generated @tech{constructor} procedure as returned by
@racket[object-name] or in the printed form of the constructor value.

The result of @racket[make-struct-type] is five values:

@itemize[

 @item{a @tech{structure type descriptor},}

 @item{a @tech{constructor} procedure,}

 @item{a @tech{predicate} procedure,}

 @item{an @tech{accessor} procedure, which consumes a structure and a field
 index between @math{0} (inclusive) and
 @math{@racket[init-field-cnt]+@racket[auto-field-cnt]} (exclusive),
 and}

 @item{a @tech{mutator} procedure, which consumes a structure, a field
 index, and a field value.}

]

@examples[
#:eval struct-eval
(define-values (struct:a make-a a? a-ref a-set!)
  (make-struct-type 'a #f 2 1 'uninitialized))
(define an-a (make-a 'x 'y))
(a-ref an-a 1)
(a-ref an-a 2)
(define a-first (make-struct-field-accessor a-ref 0))
(a-first an-a)
]

@interaction[
#:eval struct-eval
(define-values (struct:b make-b b? b-ref b-set!)
  (make-struct-type 'b struct:a 1 2 'b-uninitialized))
(define a-b (make-b 'x 'y 'z))
(a-ref a-b 1)
(a-ref a-b 2)
(b-ref a-b 0)
(b-ref a-b 1)
(b-ref a-b 2)
]

@interaction[
#:eval struct-eval
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

@interaction[
#:eval struct-eval
(define p1 #s(p a b c))
(define-values (struct:p make-p p? p-ref p-set!)
  (make-struct-type 'p #f 3 0 #f null 'prefab #f '(0 1 2)))
(p? p1)
(p-ref p1 0)
(make-p 'x 'y 'z)
]

@defproc[(make-struct-field-accessor [accessor-proc struct-accessor-procedure?]
                                     [field-pos exact-nonnegative-integer?]
                                     [field-name (or/c symbol? #f) 
                                                 (symbol->string (format "field~a" field-pos))])
         procedure?]{

Returns a field accessor that is equivalent to @racket[(lambda (s)
(accessor-proc s field-pos))].  The @racket[accessor-proc] must be
an @tech{accessor} returned by @racket[make-struct-type]. The name of the
resulting procedure for debugging purposes is derived from
@racket[field-name] and the name of @racket[accessor-proc]'s
structure type if @racket[field-name] is a symbol.

For examples, see @racket[make-struct-type].}

@defproc[(make-struct-field-mutator [mutator-proc struct-mutator-procedure?]
                                    [field-pos exact-nonnegative-integer?]
                                    [field-name (or/c symbol? #f)
                                                (symbol->string (format "field~a" field-pos))])
         procedure?]{

Returns a field mutator that is equivalent to @racket[(lambda (s v)
(mutator-proc s field-pos v))].  The @racket[mutator-proc] must be
a @tech{mutator} returned by @racket[make-struct-type]. The name of the
resulting procedure for debugging purposes is derived from
@racket[field-name] and the name of @racket[mutator-proc]'s
structure type if @racket[field-name] is a symbol.

For examples, see @racket[make-struct-type].}


@;------------------------------------------------------------------------
@section[#:tag "structprops"]{Structure Type Properties}

@margin-note{@secref{struct-generics} provide a high-level API on top of
structure type properties.}

A @deftech{structure type property} allows per-type information to be
 associated with a structure type (as opposed to per-instance
 information associated with a structure value). A property value is
 associated with a structure type through the
 @racket[make-struct-type] procedure (see
 @secref["creatingmorestructs"]) or through the @racket[#:property]
 option of @racket[struct].  Subtypes inherit the property
 values of their parent types, and subtypes can override an inherited
 property value with a new value.

@defproc[(make-struct-type-property [name symbol?]
                                    [guard (or/c procedure? #f 'can-impersonate) #f]
                                    [supers (listof (cons/c struct-type-property?
                                                            (any/c . -> . any/c)))
                                            null]
                                    [can-impersonate? any/c #f])
         (values struct-type-property?
                 procedure?
                 procedure?)]{

Creates a new structure type property and returns three values:

@itemize[

 @item{a @deftech{structure type property descriptor}, for use with
       @racket[make-struct-type] and @racket[struct];}

 @item{a @deftech{property predicate} procedure, which takes an
       arbitrary value and returns @racket[#t] if the value is a
       descriptor or instance of a structure type that has a value for
       the property, @racket[#f] otherwise;}

 @item{a @deftech{property accessor} procedure, which returns the
       value associated with the structure type given its descriptor or
       one of its instances; if the structure type does not have a
       value for the property, or if any other kind of value is
       provided, the @exnraise[exn:fail:contract] unless a second
       argument, @racket[_failure-result], is supplied to the
       procedure. In that case, if @racket[_failure-result] is a
       procedure, it is called (through a tail call) with no arguments
       to produce the result of the property accessor procedure;
       otherwise, @racket[_failure-result] is itself returned as the
       result.}

]

If the optional @racket[guard] is supplied as a procedure, it is
called by @racket[make-struct-type] before attaching the property to a
new structure type. The @racket[guard] must accept two arguments:
a value for the property supplied to @racket[make-struct-type], and a
list containing information about the new structure type. The list
contains the values that @racket[struct-type-info] would return for
the new structure type if it skipped the immediate current-inspector
control check (but not the check for exposing an ancestor structure
type, if any; see @secref["inspectors"]).

The result of calling @racket[guard] is associated with the property
in the target structure type, instead of the value supplied to
@racket[make-struct-type]. To reject a property association (e.g.,
because the value supplied to @racket[make-struct-type] is
inappropriate for the property), the @racket[guard] can raise an
exception. Such an exception prevents @racket[make-struct-type] from
returning a structure type descriptor.

If @racket[guard] is @racket['can-impersonate], then the property's
accessor can be redirected through
@racket[impersonate-struct]. This option is identical to supplying
@racket[#t] as the @racket[can-impersonate?] argument and is provided
for backwards compatibility.

The optional @racket[supers] argument is a list of properties that are
automatically associated with some structure type when the newly
created property is associated to the structure type. Each property in
@racket[supers] is paired with a procedure that receives the value
supplied for the new property (after it is processed by
@racket[guard]) and returns a value for the associated property (which
is then sent to that property's guard, of any).

The optional @racket[can-impersonate?] argument determines if the
structure type property can be redirected through @racket[impersonate-struct].
If the argument is @racket[#f], then redirection is not allowed.
Otherwise, the property accessor may be redirected by a struct
impersonator.

@examples[
#:eval struct-eval
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

(define-values (prop:q q? q-ref) (make-struct-type-property 
                                  'q (lambda (v si) (add1 v))
                                  (list (cons prop:p sqrt))))
(define-values (struct:c make-c c? c-ref c-set!)
  (make-struct-type 'c #f 0 0 'uninit
                    (list (cons prop:q 8))))
(q-ref struct:c)
(p-ref struct:c)
]}

@defproc[(struct-type-property? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a @tech{structure type property
descriptor} value, @racket[#f] otherwise.}

@defproc[(struct-type-property-accessor-procedure? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is an accessor procedure produced
by @racket[make-struct-type-property], @racket[#f] otherwise.}

@;------------------------------------------------------------------------
@include-section["generic.scrbl"]

@;------------------------------------------------------------------------
@section[#:tag "struct-copy"]{Copying and Updating Structures}

@defform/subs[(struct-copy id struct-expr fld-id ...)
              ((fld-id [field-id expr]
                       [field-id #:parent parent-id expr]))]{

Creates a new instance of the structure type @racket[id] with the same
field values as the structure produced by @racket[struct-expr], except
that the value of each supplied @racket[field-id] is instead
determined by the corresponding @racket[expr]. If @racket[#:parent]
is specified, the @racket[parent-id] must be bound to a parent
structure type of @racket[id].

The @racket[id] must have a @tech{transformer binding} that
encapsulates information about a structure type (i.e., like the
initial identifier bound by @racket[struct]), and the binding
must supply a constructor, a predicate, and all field accessors.

Each @racket[field-id] is combined with @racket[id] 
(or @racket[parent-id], if present) to form
@racket[id]@racketidfont{-}@racket[field-id] (using the lexical
context of @racket[field-id]), which must be one of the accessor
bindings in @racket[id]. The accessor bindings determined by different
@racket[field-id]s must be distinct. The order of the
@racket[field-id]s need not match the order of the corresponding
fields in the structure type.

The @racket[struct-expr] is evaluated first. The result must be an
instance of the @racket[id] structure type, otherwise the
@exnraise[exn:fail:contract]. Next, the field @racket[expr]s are
evaluated in order (even if the fields that correspond to the
@racket[field-id]s are in a different order). Finally, the new
structure instance is created.

The result of @racket[struct-expr] can be an instance of a sub-type of
@racket[id], but the resulting copy is an immediate instance of
@racket[id] (not the sub-type).

@examples[
#:eval struct-copy-eval
(struct fish (color weight) #:transparent)
(define marlin (fish 'orange-and-white 11))
(define dory (struct-copy fish marlin
                          [color 'blue]))
dory
             
(struct shark fish (weeks-since-eating-fish) #:transparent)
(define bruce (shark 'grey 110 3))
(define chum (struct-copy shark bruce
                          [weight #:parent fish 90]
                          [weeks-since-eating-fish 0]))
chum

(code:comment "subtypes can be copied as if they were supertypes,")
(code:comment "but the result is an instance of the supertype")
(define not-really-chum
  (struct-copy fish bruce
               [weight 90]))
not-really-chum
]

}

@;------------------------------------------------------------------------
@section[#:tag "structutils"]{Structure Utilities}

@defproc[(struct->vector [v any/c] [opaque-v any/c '...]) vector?]{

Creates a vector representing @racket[v].  The first slot of the
result vector contains a symbol whose printed name has the form
@racketidfont{struct:}@racket[_id]. Each remaining slot contains
either the value of a field in @racket[v], if it is accessible via the
current inspector, or @racket[opaque-v] for a field that is not
accessible. A single @racket[opaque-v] value is used in the vector for
contiguous inaccessible fields. (Consequently, the size of the vector
does not match the size of the @racket[struct] if more than one field
is inaccessible.)}

@defproc[(struct? [v any/c]) any]{ Returns @racket[#t] if
 @racket[struct-info] exposes any structure types of @racket[v] with
 the current inspector, @racket[#f] otherwise.

 Typically, when @racket[(struct? v)] is true, then
 @racket[(struct->vector v)] exposes at least one field value. It is
 possible, however, for the only visible types of @racket[v] to
 contribute zero fields.}

@defproc[(struct-type? [v any/c]) boolean?]{Returns @racket[#t] if
 @racket[v] is a structure type descriptor value, @racket[#f]
 otherwise.}

@defproc[(struct-constructor-procedure? [v any/c]) boolean?]{Returns
 @racket[#t] if @racket[v] is a constructor procedure generated by
 @racket[struct] or @racket[make-struct-type], @racket[#f]
 otherwise.}

@defproc[(struct-predicate-procedure? [v any/c]) boolean?]{Returns
 @racket[#t] if @racket[v] is a predicate procedure generated by
 @racket[struct] or @racket[make-struct-type], @racket[#f]
 otherwise.}

@defproc[(struct-accessor-procedure? [v any/c]) boolean?]{Returns
 @racket[#t] if @racket[v] is an accessor procedure generated by
 @racket[struct], @racket[make-struct-type], or
 @racket[make-struct-field-accessor], @racket[#f] otherwise.}

@defproc[(struct-mutator-procedure? [v any/c]) boolean?]{Returns
 @racket[#t] if @racket[v] is a mutator procedure generated by
 @racket[struct], @racket[make-struct-type], or
 @racket[make-struct-field-mutator], @racket[#f] otherwise.}

@defproc[(prefab-struct-key [v any/c]) (or/c #f symbol? list?)]{

Returns @racket[#f] if @racket[v] is not an instance of a
@tech{prefab} structure type. Otherwise, the result is the shorted key
that could be used with @racket[make-prefab-struct] to create an instance
of the structure type.

@examples[
(prefab-struct-key #s(cat "Garfield"))
(struct cat (name) #:prefab)
(struct cute-cat cat (shipping-dest) #:prefab)
(cute-cat "Nermel" "Abu Dhabi")
(prefab-struct-key (cute-cat "Nermel" "Abu Dhabi"))
]}


@defproc[(make-prefab-struct [key prefab-key?] [v any/c] ...) struct?]{

Creates an instance of a @tech{prefab} structure type, using the
@racket[v]s as field values. The @racket[key] and the number of
@racket[v]s determine the @tech{prefab} structure type.

A @racket[key] identifies a structure type based on a list with the
following items:

@itemize[

 @item{A symbol for the structure type's name.}

 @item{An exact, nonnegative integer representing the number of
       non-automatic fields in the structure type, not counting fields
       from the supertype (if any).}

 @item{A list of two items, where the first is an exact, nonnegative
       integer for the number of automatic fields in the structure
       type that are not from the supertype (if any), and the second
       element is an arbitrary value that is the value for the
       automatic fields.}

 @item{A vector of exact, nonnegative integers that indicate mutable
       non-automatic fields in the structure type, counting from
       @racket[0] and not including fields from the supertype (if
       any).}

 @item{Nothing else, if the structure type has no
       supertype. Otherwise, the rest of the list is the key
       for the supertype.}

]

An empty vector and an auto-field list that starts with @racket[0] can
be omitted. Furthermore, the first integer (which indicates the number
of non-automatic fields) can be omitted, since it can be inferred from
the number of supplied @racket[v]s. Finally, a single symbol can be
used instead of a list that contains only a symbol (in the case that
the structure type has no supertype, no automatic fields, and no
mutable fields).

The total field count must be no more than 32768. If the number of
fields indicated by @racket[key] is inconsistent with the number of
supplied @racket[v]s, the @exnraise[exn:fail:contract].

@examples[
(make-prefab-struct 'clown "Binky" "pie")
(make-prefab-struct '(clown 2) "Binky" "pie")
(make-prefab-struct '(clown 2 (0 #f) #()) "Binky" "pie")
(make-prefab-struct '(clown 1 (1 #f) #()) "Binky" "pie")
(make-prefab-struct '(clown 1 (1 #f) #(0)) "Binky" "pie")
]}


@defproc[(prefab-key->struct-type [key prefab-key?]
                                  [field-count (integer-in 0 32768)])
         struct-type?]{

Returns a @tech{structure type descriptor} for the @tech{prefab}
structure type specified by the combination of @racket[key] and
@racket[field-count].

If the number of fields indicated by @racket[key] is inconsistent with
@racket[field-count], the @exnraise[exn:fail:contract].}


@defproc[(prefab-key? [v any/c]) boolean?]{

Return @racket[#t] if @racket[v] can be a @tech{prefab} structure type
key, @racket[#f] otherwise.

See @racket[make-prefab-struct] for a description of valid key shapes.}

@;------------------------------------------------------------------------
@section[#:tag "structinfo"]{Structure Type Transformer Binding}

The @racket[struct] form binds the name of a structure type as
a @tech{transformer binding} that records the other identifiers bound
to the structure type, the constructor procedure, the predicate
procedure, and the field accessor and mutator procedures. This
information can be used during the expansion of other expressions via
@racket[syntax-local-value].

For example, the @racket[struct] variant for subtypes uses the
base type name @racket[_t] to find the variable
@racketidfont{struct:}@racket[_t] containing the base type's descriptor; it
also folds the field accessor and mutator information for the base
type into the information for the subtype. As another example, the
@racket[match] form uses a type name to find the predicates and field
accessors for the structure type. The @racket[struct] form in an
imported signature for @racket[unit] causes the @racket[unit]
transformer to generate information about imported structure types, so
that @racket[match] and subtyping @racket[struct] forms work
within the unit.

The expansion-time information for a structure type can be represented
directly as a list of six elements (of the same sort that the
encapsulated procedure must return):

@itemize[

 @item{an identifier that is bound to the structure type's descriptor,
 or @racket[#f] if none is known;}

 @item{an identifier that is bound to the structure type's constructor,
 or @racket[#f] if none is known;}

 @item{an identifier that is bound to the structure type's predicate,
 or @racket[#f] if none is known;}

 @item{a list of identifiers bound to the field accessors of the
 structure type, optionally with @racket[#f] as the list's last
 element. A @racket[#f] as the last element indicates that the
 structure type may have additional fields, otherwise the list is a
 reliable indicator of the number of fields in the structure
 type. Furthermore, the accessors are listed in reverse order for the
 corresponding constructor arguments. (The reverse order enables
 sharing in the lists for a subtype and its base type.)}

 @item{a list of identifiers bound to the field mutators of
 the structure type, or @racket[#f] for each field that has no known
 mutator, and optionally with an extra @racket[#f] as the list's last
 element (if the accessor list has such a @racket[#f]). The list's
 order and the meaning of a final @racket[#f] are the same as for the
 accessor identifiers, and the length of the mutator list is the same
 as the accessor list's length.}

 @item{an identifier that determines a super-type for the structure
 type, @racket[#f] if the super-type (if any) is unknown, or
 @racket[#t] if there is no super-type. If a super-type is specified,
 the identifier is also bound to structure-type expansion-time
 information.}

]

Instead of this direct representation, the representation can be a
structure created by @racket[make-struct-info] (or an instance of a
subtype of @racket[struct:struct-info]), which encapsulates a
procedure that takes no arguments and returns a list of six
elements. Alternately, the representation can be a structure whose
type has the @racket[prop:struct-info] @tech{structure type property}.
Finally, the representation can be an instance of a structure type
derived from @racket[struct:struct-info] or with the
@racket[prop:struct-info] property that also implements
@racket[prop:procedure], and where the instance is further is wrapped
by @racket[make-set!-transformer]. In addition, the representation may
implement the @racket[prop:struct-auto-info] property.

Use @racket[struct-info?] to recognize all allowed forms of the
information, and use @racket[extract-struct-info] to obtain a list
from any representation.

The implementor of a syntactic form can expect users of the form to
know what kind of information is available about a structure type. For
example, the @racket[match] implementation works with structure
information containing an incomplete set of accessor bindings, because
the user is assumed to know what information is available in the
context of the @racket[match] expression. In particular, the
@racket[match] expression can appear in a @racket[unit] form with an
imported structure type, in which case the user is expected to know
the set of fields that are listed in the signature for the structure
type.

@note-lib-only[racket/struct-info]

@defproc[(struct-info? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is either a six-element list with
the correct shape for representing structure-type information, a
procedure encapsulated by @racket[make-struct-info], a structure with
the @racket[prop:struct-info] property, or a structure type derived
from @racket[struct:struct-info] or with @racket[prop:struct-info] and
wrapped with @racket[make-set!-transformer].}

@defproc[(checked-struct-info? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a procedure encapsulated by
@racket[make-struct-info] and produced by @racket[struct], but
only when no parent type is specified or the parent type is also
specified through a transformer binding to such a value.}

@defproc[(make-struct-info [thunk (-> (and/c struct-info? list?))])
         struct-info?]{

Encapsulates a thunk that returns structure-type information in list
form. Note that accessors are listed in reverse order, as mentioned in @secref{structinfo}.}
      
@(struct-eval '(require (for-syntax racket/base)))
@(struct-eval '(require racket/match))
@(struct-eval '(require (for-syntax racket/struct-info)))
@examples[
#:eval struct-eval
(define (new-pair? x) (displayln "new pair?") (pair? x))
(define (new-car x) (displayln "new car") (car x))
(define (new-cdr x) (displayln "new cdr") (cdr x))
(define-syntax new-list 
  (make-struct-info 
   (λ () (list #f 
               #'cons 
               #'new-pair? 
               (list #'new-cdr #'new-car) 
               (list #f #f)
               #t))))
(match (list 1 2 3)
  [(new-list hd tl) (append tl (list hd))])
]

@examples[
#:eval struct-eval
(struct A (x y))
(define (new-A-x a) (displayln "A-x") (A-x a))
(define (new-A-y a) (displayln "A-y") (A-y a))
(define (new-A? a) (displayln "A?") (A? a))
(define-syntax A-info
  (make-struct-info
   (λ () (list #'A
               #'A
               #'new-A?
               (list #'new-A-y #'new-A-x)
               (list #f #f)
               #t))))
(define-match-expander B
  (syntax-rules () [(_ x ...) (A-info x ...)]))
(match (A 10 20)
  [(B x y) (list y x)])
]

@defproc[(extract-struct-info [v struct-info?])
         (and/c struct-info? list?)]{

Extracts the list form of the structure type information represented
by @racket[v].}

@defthing[struct:struct-info struct-type?]{

The @tech{structure type descriptor} for the structure type returned
by @racket[make-struct-info]. This @tech{structure type descriptor} is
mostly useful for creating structure subtypes. The structure type
includes a guard that checks an instance's first field in the same way
as @racket[make-struct-info].}

@defthing[prop:struct-info struct-type-property?]{

The @tech{structure type property} for creating new structure types
like @racket[struct:struct-info]. The property value must be a procedure
of one argument that takes an instance structure and returns
structure-type information in list form.}

@deftogether[(
@defthing[prop:struct-auto-info struct-type-property?]
@defproc[(struct-auto-info? [v any/c]) boolean?]
@defproc[(struct-auto-info-lists [sai struct-auto-info?]) 
         (list/c (listof identifier?) (listof identifier?))]
)]{

The @racket[prop:struct-auto-info] property is implemented to provide
static information about which of the accessor and mutator identifiers
for a structure type correspond to @racket[#:auto] fields (so that
they have no corresponding argument in the constructor). The property
value must be a procedure that accepts an instance structure to which
the property is given, and the result must be two lists of identifiers
suitable as a result from @racket[struct-auto-info-lists].

The @racket[struct-auto-info?] predicate recognizes values that
implement the @racket[prop:struct-auto-info] property.

The @racket[struct-auto-info-lists] function extracts two lists of
identifiers from a value that implements the
@racket[prop:struct-auto-info] property. The first list should be a
subset of the accessor identifiers for the structure type described by
@racket[sai], and the second list should be a subset of the mutator
identifiers. The two subsets correspond to @racket[#:auto] fields.}

@; ----------------------------------------------------------------------

@close-eval[struct-eval]
@close-eval[struct-copy-eval]
