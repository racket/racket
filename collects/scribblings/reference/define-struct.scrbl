#lang scribble/doc
@(require "mz.ss"
          (for-syntax scheme/base)
          (for-label scheme/serialize))

@(define posn-eval (make-base-eval))
@(interaction-eval #:eval posn-eval (require (for-syntax scheme/base)))

@title[#:tag "define-struct"]{Defining Structure Types: @scheme[define-struct]}

@guideintro["define-struct"]{@scheme[define-struct]}

@defform/subs[(define-struct id-maybe-super (field ...)
                             struct-option ...)
              ([id-maybe-super id
                               (id super-id)]
               [field field-id
                      [field-id field-option ...]]
               [struct-option #:mutable
                              (code:line #:super super-expr)
                              (code:line #:inspector inspector-expr)
                              (code:line #:auto-value auto-expr)
                              (code:line #:guard guard-expr)
                              (code:line #:property prop-expr val-exr)
                              (code:line #:transparent)
                              (code:line #:prefab)
                              (code:line #:constructor-name constructor-id)
                              #:omit-define-syntaxes
                              #:omit-define-values]
               [field-option #:mutable
                             #:auto])]{

Creates a new @techlink{structure type} (or uses a pre-existing
structure type if @scheme[#:prefab] is specified), and binds
transformers and variables related to the @tech{structure type}.

A @scheme[define-struct] form with @math{n} @scheme[field]s defines up
to @math{4+2n} names:

@itemize[

 @item{@schemeidfont{struct:}@scheme[id], a @deftech{structure type
       descriptor} value that represents the @tech{structure type}.}

 @item{@scheme[constructor-id] (which defaults to
       @schemeidfont{make-}@scheme[id]), a @deftech{constructor}
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
       for each @scheme[field] that includes a
       @scheme[#:mutable] option, or when the
       @scheme[#:mutable] option is specified as a
       @scheme[struct-option]; a @deftech{mutator} procedure that
       takes an instance of the @tech{structure type} and a new field
       value. The structure is destructively updated with the new
       value, and @|void-const| is returned.}

 @item{@scheme[id], a @tech{transformer binding} that encapsulates
       information about the structure type declaration. This binding
       is used to define subtypes, and it also works with the
       @scheme[shared] and @scheme[match] forms. For detailed
       information about the binding of @scheme[id], see
       @secref["structinfo"].
       
       The @scheme[constructor-id] and @scheme[id] can be the same, in
       which case @scheme[id] performs both roles.}

]

If @scheme[super-id] is provided, it must have a transformer binding
of the same sort bound to @scheme[id] (see @secref["structinfo"]),
and it specifies a supertype for the structure type. Alternately,
the @scheme[#:super] option can be used to specify an expression that
must produce a @tech{structure type descriptor}. See
@secref["structures"] for more information on structure subtypes
and supertypes. If both @scheme[super-id] and @scheme[#:super] are
provided, a syntax error is reported.

If the @scheme[#:mutable] option is specified for an individual
field, then the field can be mutated in instances of the structure
type, and a @tech{mutator} procedure is bound. Supplying
@scheme[#:mutable] as a @scheme[struct-option] is the same as
supplying it for all @scheme[field]s. If @scheme[#:mutable] is
specified as both a @scheme[field-option] and @scheme[struct-option],
a syntax error is reported.

The @scheme[#:inspector], @scheme[#:auto-value], and @scheme[#:guard]
options specify an inspector, value for automatic fields, and guard
procedure, respectively. See @scheme[make-struct-type] for more
information on these attributes of a structure type.  The
@scheme[#:property] option, which is the only one that can be supplied
multiple times, attaches a property value to the structure type; see
@secref["structprops"] for more information on properties. The
@scheme[#:transparent] option is a shorthand for @scheme[#:inspector
#f].

@margin-note{Use the @scheme[prop:procedure] to property implement an
@as-index{applicable structure}, use @scheme[prop:evt] to create a
structure type whose instances are @tech{synchronizable events}, and
so on. By convention, property names start with @schemeidfont{prop:}.}

The @scheme[#:prefab] option obtains a @techlink{prefab} (pre-defined,
globally shared) structure type, as opposed to creating a new
structure type. Such a structure type is inherently transparent and
cannot have a guard or properties, so using @scheme[#:prefab] with
@scheme[#:transparent], @scheme[#:inspector], @scheme[#:guard], or
@scheme[#:property] is a syntax error. If a supertype is specified, it
must also be a @tech{prefab} structure type.

If the @scheme[#:omit-define-syntaxes] option is supplied, then
@scheme[id] is not bound as a transformer. If the
@scheme[#:omit-define-values] option is supplied, then none of the
usual variables are bound, but @scheme[id] is bound. If both are
supplied, then the @scheme[define-struct] form is equivalent to
@scheme[(begin)].

If @scheme[#:auto] is supplied as a @scheme[field-option], then the
@tech{constructor} procedure for the structure type does not accept an
argument corresponding to the field. Instead, the structure type's
automatic value is used for the field, as specified by the
@scheme[#:auto-value] option, or as defaults to @scheme[#f] when
@scheme[#:auto-value] is not supplied. The field is mutable (e.g.,
through reflective operations), but a mutator procedure is bound only
if @scheme[#:mutable] is specified.

If a @scheme[field] includes the @scheme[#:auto] option, then all
fields after it must also include @scheme[#:auto], otherwise a syntax
error is reported. If any @scheme[field-option] or
@scheme[struct-option] keyword is repeated, other than
@scheme[#:property], a syntax error is reported.

For serialization, see @scheme[define-serializable-struct].

@defexamples[
#:eval posn-eval
(define-struct posn (x y [z #:auto])
               #:auto-value 0
               #:transparent)
(make-posn 1 2)
(posn? (make-posn 1 2))
(posn-y (make-posn 1 2))
]

@defs+int[
#:eval posn-eval
[(define-struct (color-posn posn) (hue) #:mutable)
 (define cp (make-color-posn 1 2 "blue"))]
(color-posn-hue cp)
cp
(set-posn-z! cp 3)
]}

@defform[(struct-field-index field-id)]{

This form can only appear as an expression within a
@scheme[define-struct] form; normally, it is used with
@scheme[#:property], especially for a property like
@scheme[prop:procedure]. The result of a @scheme[struct-field-index]
expression is an exact, non-negative integer that corresponds to the
position within the structure declaration of the field named by
@scheme[field-id].

@defexamples[
#:eval posn-eval
(define-struct mood-procedure (base rating)
               #:property prop:procedure (struct-field-index base))
(define happy+ (make-mood-procedure add1 10))
(happy+ 2)
(mood-procedure-rating happy+)
]}

@defform[(define-struct/derived (id . rest-form) 
           id-maybe-super (field ...) struct-option ...)]{

Like @scheme[define-struct], but intended for use by macros that
expand to @scheme[define-struct]. The form immediately after
@scheme[define-struct/derived] is used for all syntax-error reporting,
and the only constraint on the form is that it starts with some
@scheme[id].

@defexamples[
#:eval posn-eval
(define-syntax (define-xy-struct stx)
  (syntax-case stx ()
   [(ds name . rest) 
    (with-syntax ([orig stx])
      #'(define-struct/derived orig name (x y) . rest))]))

(define-xy-struct posn)
(posn-x (make-posn 1 2))
(define-xy-struct posn #:mutable)
(set-posn-x! (make-posn 1 2) 0)
(define-xy-struct posn #:bad-option)
]}

@; ----------------------------------------

@close-eval[posn-eval]
