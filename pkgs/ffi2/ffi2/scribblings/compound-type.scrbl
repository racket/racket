#lang scribble/manual
@(require scribble/example
          "common.rkt"
          "racket-id.rkt")

@(define-syntax-rule (deftypeform id)
   @defidform[#:kind "ffi2 type" id])

@(define ffi2-eval (make-base-eval))
@examples[#:eval ffi2-eval #:hidden (require ffi2)]

@title[#:tag "compound-ffi2-type"]{Compound Foreign Types}

@defform[#:kind "ffi2 type"
         (struct maybe-tag
           [field-id field-type]
           ...)
         #:grammar ([maybe-tag id
                               ϵ])]{

@margin-note{The @racket[struct] FFI2 type constructor conflicts with
or shadows the @racket-struct form that is provided by
@racketmodname[racket/base]. Consider using @racket[define-struct]
instead of @racket-struct, or rename one or the other @racket[struct]
bindings on import.}

Describes a type that is represented by a @tt{struct} declaration on
the C side and a @tech{pointer} object in the Racket side. If
@racket[maybe-tag] is an identifier, the symbol form of the identifier
with a @litchar{*} suffix added is used as a @tech{tag} for pointers that
represent instances of the @racket[struct] type, otherwise a generic
@racket[ptr_t] pointer is used.

Each @racket[field-id] must be distinct, and the corresponding
@racket[field-type] describes the field's representation on the C side
and the representation used on the Racket side if the field's value is
extracted from a representation of the @racket[struct] type.

When @racket[struct] is used as the @racket[_parent-type] in a
@racket[define-ffi2-type] definition of @racket[_name] without any
options (such as @racket[#:tag] or @racket[#:racket->c]), then
@racket[struct] and @racket[define-ffi2-type] influence each other:

@itemlist[

 @item{@racket[_name] is used as a @racket[maybe-tag]
       identifier if @racket[maybe-tag] is not present.}

 @item{@racketidfont{@racket[_name]*} is also defined as a
       pointer type using @racketidfont{@racket[maybe-tag]*} or
       @racketidfont{@racket[_name]*} as its tag.}

 @item{@racketidfont{@racket[_name]*/gcable} is also defined
       like @racketidfont{@racket[_name]*}, but it treats a
       C-to-Scheme conversion like @racket[ptr_t/gcable] by treating
       the C-side pointer as (potentialy) referencing memory that is
       managed by Racket's garbage collector.}

 @item{@racketidfont{@racket[_name]*?} is defined to recognize
       suitably tagged Racket @tech{pointer} representations.}

 @item{@racketidfont{@racket[_name]-@racket[field-id]} is defined for
      each @racket[field-id] as an accessor: it takes a pointer for a
      @racket[struct] instance and extracts a representation of the
      corresponding field value based on the conversion implied by the
      associated @racket[field-type].}

 @item{@racketidfont{set-@racket[_name]-@racket[field-id]!} is defined
      for each @racket[field-id] as a mutator: it takes a pointer for
      a @racket[struct] instance and a representation of the
      corresponding field value, and it installs a converted value
      (based the associated @racket[field-type]) into the
      @racket[struct] instance.}

 @item{@racket[_name] is defined both as a type and as an expression
       form. As an expression, it accepts as may subexpressions as
       @racket[field-id]s, it allocates an instance of the
       @racket[struct] type via @racket[ffi2-malloc], and it installs
       each subexpression's result into the allocated memory in the
       same way as
       @racketidfont{set-@racket[_name]-@racket[field-id]!}. An
       optional allocation mode can be provided before the field
       subexpressions, and the default allocation mode is
       @racket[#:gcable].}

]

Note that the Racket-side representation is the same for
@racket[_name] and @racketidfont{@racket[_name]*}, even though the
C-side representation differs.

@examples[
#:eval ffi2-eval
(define-ffi2-type point_t (struct
                            [x double_t]
                            [y double_t]))
(ffi2-sizeof point_t)
(ffi2-sizeof point_t*)
(define p1 (point_t 1.0 2.0))
p1
(point_t*? p1)
(point_t-x p1)
(point_t-y p1)
(eval:error
 (point_t-x (ffi2-malloc 16)))
]

With this example's definition of @racket[point_t], a field in another
@racket[struct] type would take up 16 bytes, while a @racket[point_t*]
field would take up 8 bytes. Accessing the field in either case would
produce a Racket representation that is a @tech{pointer} tagged as
@racket[point_t*]. In the case of a @racket[point_t] field, the
returned pointer would refer to memory within the accessed
@racket[struct] instance.

Along similar lines, a pointer tagged with @racket[point_t*] is
suitable as an argument to a C function that has either a
@racket[point_t] or @racket[point_t*] argument. In the case of a
@racket[point_t] argument, the C function receives a copy of the
content of the pointer. In the case of a @racket[point_t*] argument,
the C function receives the same address as encapsulated by the
pointer.

}

@defform[#:kind "ffi2 type"
         (union maybe-tag
           [field-id field-type]
           ...)
         #:grammar ([maybe-tag id
                               ϵ])]{

Similar to @racket[struct], but for a type that uses @tt{union} on the
C side.

The interaction of @racket[define-ffi2-type] and @racket[union] is
like the interaction of @racket[define-ffi2-type] and @racket[struct],
except for the way the defined @racket[_name] is bound as an
expression form:

@itemlist[

 @item{@racket[_name] an expression expects a single field name
       followed by a single field subexpression, and it installs that
       field's value after allocating the @racket[union]
       representation. An optional allocation mode can be provided
       before the field name.}

]

@examples[
#:eval ffi2-eval
(define-ffi2-type grade_t (union
                            [score double_t]
                            [pass-fail bool_t]))
(ffi2-sizeof grade_t)
(define g1 (grade_t score 93.0))
(grade_t-score g1)
(define g2 (grade_t pass-fail #t))
(grade_t-pass-fail g2)
(define g3 (grade_t score 0.0))
(grade_t-pass-fail g3)
]

}

@defform[#:kind "ffi2 type"
         (array elem_type count)
         #:grammar ([count exact-nonnegative-integer
                           *])]{

Describes a type that is represented by an array or pointer
declaration on the C side and a @tech{pointer} object in the Racket
side. The array's @racket[count] must be a literal nonnegative exact
integer for a C array declaration, or it can be literally @racket[*]
to indicate a C pointer.

The Racket-side pointer representation uses a @tech{tag} formed by adding a
@litchar{*} suffix on the name of @racket[elem_type], as long as it
has a name. If 2rhombus[elem-type] is an immediate @racket[struct],
@racket[union], @racket[array], or @racket[->] for, then it has no
name, and the Racket-side representation is a generic pointer.

When @racket[array] is used as the @racket[_parent-type] in a
@racket[define-ffi2-type] definition of @racket[_name] without any
options (such as @racket[#:racket->c]) other than @racket[#:tag], then
@racket[array] and @racket[define-ffi2-type] influence each other:

@itemlist[

 @item{The @racket[#:tag] option of @racket[define-ffi2-type] can
       replace the tag used for pointer representations of the array,
       which is normally @litchar{*} added as suffix on the name of
       @racket[elem_type]. If the @racket[#:tag] option is not present,
       then @racket[_name] is added to the end of the Racket pointer
       representation's tag to create a @tech{pointer subtype}, where
       the array type is a subtype of an @racket[elem_type]-pointer type.}

 @item{@racketidfont{@racket[_name]/gcable} is also defined like
       @racketidfont{@racket[_name]*} if @racket[count] is @racket[*].
       It treats a C-to-Scheme conversion like @racket[ptr_t/gcable]
       by treating the C-side pointer as (potentialy) referencing
       memory that is managed by Racket's garbage collector.}

 @item{@racketidfont{@racket[_name]?} is defined to recognize
       suitably tagged Racket @tech{pointer} representations.}

 @item{@racketidfont{@racket[_name]-ref} is defined as an accessor: it
      takes a pointer for a @racket[array] instance and an exact
      integer, and it extracts a representation of the corresponding
      element value based on the conversion implied by
      @racket[elem-type]. If @racket[count] is not @racket[*], the
      integer passed to @racketidfont{@racket[_name]-ref} must be in the range
      @racket[0] (inclusive) to @racket[count] (exclusive).}

 @item{@racketidfont{@racket[_name]-ref} is defined as a mutator: it
      takes a pointer for a @racket[array] instance, an exact integer,
      and a field value; it installs a converted value
      (based @racket[elem-type]) into the @racket[array] instance.
      If @racket[count] is not @racket[*], the
      integer passed to @racketidfont{@racket[_name]-set!} is
      constrained in the same way as for @racketidfont{@racket[_name]-ref}.}

]

@examples[
#:eval ffi2-eval
(define-ffi2-type triple_t (array double_t 3))
(ffi2-sizeof triple_t)
(define p (ffi2-malloc triple_t))
p
(triple_t-set! p 0 0.0)
(triple_t-set! p 1 10.0)
(triple_t-set! p 2 20.0)
(eval:error (triple_t-set! p 3 30.0))
(triple_t-ref p 1)
]


}

@close-eval[ffi2-eval]
