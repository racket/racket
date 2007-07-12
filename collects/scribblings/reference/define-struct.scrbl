#reader(lib "docreader.ss" "scribble")
@require["mz.ss"]

@title[#:tag "mz:define-struct"]{Defining Structure Types: @scheme[define-struct]}

@guideintro["guide:define-struct"]{@scheme[define-struct]}

@defform/subs[(define-struct id-maybe-super (field ...)
                             struct-option ...)
              ([id-maybe-super id
                               (id super-id)]
               [field field-id
                      [field-id field-option ...]]
               [struct-option #:immutable
                              (code:line #:super super-expr)
                              (code:line #:inspector inspector-expr)
                              (code:line #:auto-value auto-expr)
                              (code:line #:guard guard-expr)
                              (code:line #:property prop-expr val-exr)
                              #:omit-define-syntaxes
                              #:omit-define-values]
               [field-option #:immutable
                             #:auto])]{

Creates a new @techlink{structure type}, and binds transformers and
variables related to the new @tech{structure type}. A
@scheme[define-struct] form with @math{n} @scheme[field]s defines
up to @math{4+2n} names:

@itemize{

 @item{@schemeidfont{struct:}@scheme[id], a @deftech{structure type
       descriptor} value that represents the new @tech{structure
       type}.}

 @item{@schemeidfont{make-}@scheme[id], a @deftech{constructor}
       procedure that takes @math{m} arguments and returns a new
       instance of the @tech{structure type}, where @math{m} is the
       number of @scheme[field]s that do not include an
       @scheme[#:auto] option.}

 @item{@scheme[id]@schemeidfont{?}, a @deftech{predicate} procedure
       that returns @scheme[#t] for instances of the @tech{structure
       type} (constructed by @schemeidfont{make-}@scheme[id] or the
       @tech{constructor} for a subtype) and @scheme[#f] for any other
       value.}

 @item{@scheme[id]@schemeidfont{-}@scheme[field-id], for each
       @scheme[field]; an @deftech{accessor} procedure that takes an
       instance of the @tech{structure type} and extracts the value
       for the corresponding field.}

 @item{@schemeidfont{set-}@scheme[id]@schemeidfont{-}@scheme[field-id]@schemeidfont{!},
       for each @scheme[field] that does not include a
       @scheme[#:immutable] option, and only when the
       @scheme[#:immutable] option is not specified as a
       @scheme[struct-option]; a @deftech{mutator} procedure that
       takes an instance of the @tech{structure type} and a new field
       value. The structure is destructively updated with the new
       value, and @|void-const| is returned.}

 @item{@scheme[id], a @tech{transformer binding} that encapsulates
       information about the structure type declaration. This binding
       is used to define subtypes, and it also works with the
       @scheme[shared] and @scheme[match] forms. For detailed
       information about the binding of @scheme[id], see
       @secref["mz:structinfo"].}

}

If @scheme[super-id] is provided, it must have a transformer binding
of the same sort bound to @scheme[id] (see @secref["mz:structinfo"]),
and it specifies a supertype for the new structure type. Alternately,
the @scheme[#:super] option can be used to specify an expression that
must produce a @tech{structure type descriptor}. See
@secref["mz:structures"] for more information on structure subtypes
and supertypes. If both @scheme[super-id] and @scheme[#:super] are
provided, a syntax error is reported.

If the @scheme[#:immutable] option is specified for an individual
field, then the field cannot be mutated in instances of the structure
type, and no @tech{mutator} procedure is bound. Supplying
@scheme[#:immutable] as a @scheme[struct-option] is the same as
supplying it for all @scheme[field]s. If @scheme[#:immutable] is
specified as both a @scheme[field-option] and @scheme[struct-option],
a syntax error is reported.

The @scheme[#:inspector], @scheme[#:auto-value], and @scheme[#:guard]
options specify an inspector, value for automatic fields, and guard
procedure, respectively. See @scheme[make-struct-type] (in
@secref["mz:creatingmorestructs"]) for more information on these
properties of a structure type. The @scheme[#:property] option, which
is the only one that can be specified multiple times, attaches a
property value to the structure type; see @secref["mz:structprops"]
for more information on properties.

If the @scheme[#:omit-define-syntaxes] option is supplied, then
@scheme[id] is not bound as a transformer. If the
@scheme[#:omit-define-values] option is supplied, then none of the
usual variables are bound. If both are supplied, then the
@scheme[define-struct] form is equivalent to @scheme[(begin)].

If @scheme[#:auto] is supplied as a @scheme[field-option], then the
@tech{constructor} procedure for the structure type does not accept an
argument corresponding to the field. Instead, the structure type's
automatic value is used for the field, as specified by the
@scheme[#:auto-value] option, or as defaults to @scheme[#f] when
@scheme[#:auto-value] is not supplied.

If a @scheme[field] includes the @scheme[#:auto] option, then all
fields after it must also include @scheme[#:auto], otherwise a syntax
error is reported. If any @scheme[field-option] or
@scheme[struct-option] keyword is repeated, other than
@scheme[#:property], a syntax error is reported.

@defexamples[
(define-struct posn (x y [z #:auto])
               #:auto-value 0
               #:inspector #f)
(make-posn 1 2)
(posn? (make-posn 1 2))
(posn-y (make-posn 1 2))
]

@defs+int[
[(define-struct (color-posn posn) (hue))
 (define cp (make-color-posn 1 2 "blue"))]
(color-posn-hue cp)
cp
(set-posn-z! cp 3)
]}

@defform[(struct-field-index field-id)]{

This form can only appear as an expression within a
@scheme[define-struct] form; normally, it is used with
@scheme[#:property], especially for a property like
@scheme[prop:procedure]. The result of

@defexamples[
(define-struct mood-procedure ([base #:immutable] rating)
               #:property prop:procedure (struct-field-index base))
(define happy+ (make-mood-procedure add1 10))
(happy+ 2)
(mood-procedure-rating happy+)
]}
