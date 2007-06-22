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

@section{Creating Structure Types}

@defproc[(make-struct-type [name symbol?]
                           [super-type (or/c struct-type? false/c)]
                           [init-field-k non-negative-exact-integer?]
                           [auto-field-k non-negative-exact-integer?]
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
          struct-type?]{

Creates a new structure type.  The @scheme[name] argument is used as
the type name. If @scheme[super-type] is not @scheme[#f], the new type
is a subtype of the corresponding structure type.

The new structure type has @scheme[(+ init-field-k auto-field-k)]
fields (in addition to any fields from @scheme[super-type]), but only
@scheme[init-field-k] constructor arguments (in addition to any
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
@scheme[0] (inclusive) to @scheme[init-field-k] (exclusive), otherwise
@exnraise[exn:fail:contract].

The @scheme[guard-proc] argument is either a procedure of @math{n}
arguments or @scheme[#f], where @math{n} is the number of arguments
for the new structure type's constructor (i.e., @scheme[init-field-k]
plus constructor arguments implied by @scheme[super-type], if any). If
@scheme[guard-proc] is a procedure, then the procedure is called
whenever an instance of the type is constructed, or whenever an
instance of a subtype is created.  The arguments to
@scheme[guard-proc] are the values provided for the structure's first
@math{n} fields, followed by the name of the instantiated structure
type (which is @scheme[name], unless a subtype is instantiated). The
@scheme[guard-proc] result must be @math{n} values, which become the
actual values for the structure's fields. The @scheme[guard-proc] can
raise an exception to prevent creation of a structure with the given
field values. If a structure subtype has its own guard, the subtype
guard is applied first, and the first @math{n} values produced by the
subtype's guard procedure become the first @math{n} arguments to
@scheme[guard-proc].

The result of @scheme[make-struct-type] is five values:
%
@itemize{

 @item{a @tech{structure type descriptor},}

 @item{a @tech{constructor} procedure,}

 @item{a @tech{predicate} procedure,}

 @item{an @tech{accessor} procedure, which consumes a structure and a field
 index between @math{0} (inclusive) and
 @math{@scheme[init-field-k]+@scheme[auto-field-k]} (exclusive),
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
  (make-struct-type 'c struct:b 0 0 #f null (make-inspector) #f null
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

@defproc[(make-struct-field-accessor [accessor-proc procedure?]
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

@defproc[(make-struct-field-mutator [mutator-proc procedure?]
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
