#lang scribble/doc
@(require "mz.rkt" (for-syntax racket/base) (for-label racket/serialize
                                                       racket/generic))

@(define posn-eval (make-base-eval))
@examples[#:hidden #:eval posn-eval
  (require racket/match racket/stream (for-syntax racket/base))]

@title[#:tag "define-struct"]{Defining Structure Types: @racket[struct]}

@guideintro["define-struct"]{@racket[struct]}

@defform/subs[(struct id maybe-super (field ...)
                      struct-option ...)
              ([maybe-super code:blank
                            super-id]
               [field field-id
                      [field-id field-option ...]]
               [struct-option #:mutable
                              (code:line #:super super-expr)
                              (code:line #:inspector inspector-expr)
                              (code:line #:auto-value auto-expr)
                              (code:line #:guard guard-expr)
                              (code:line #:property prop-expr val-expr)
                              (code:line #:transparent)
                              (code:line #:prefab)
                              (code:line #:constructor-name constructor-id)
                              (code:line #:extra-constructor-name constructor-id)
                              (code:line #:reflection-name symbol-expr)
                              (code:line #:methods gen:name method-defs)
                              #:omit-define-syntaxes
                              #:omit-define-values]
               [field-option #:mutable
                             #:auto]
               [method-defs (definition ...)])
               #:contracts
               ([gen:name identifier?])]{

Creates a new @techlink{structure type} (or uses a pre-existing
structure type if @racket[#:prefab] is specified), and binds
transformers and variables related to the @tech{structure type}.

A @racket[struct] form with @math{n} @racket[field]s defines up
to @math{4+2n} names:

@itemize[

 @item{@racketidfont{struct:}@racket[id], a @deftech{structure type
       descriptor} value that represents the @tech{structure type}.}

 @item{@racket[constructor-id] (which defaults to @racket[id]), a
       @deftech{constructor} procedure that takes @math{m} arguments
       and returns a new instance of the @tech{structure type}, where
       @math{m} is the number of @racket[field]s that do not include
       an @racket[#:auto] option.}

 @item{@racket[id], a @tech{transformer} binding that encapsulates
       information about the structure type declaration. This binding
       is used to define subtypes, and it also works with the
       @racket[shared] and @racket[match] forms. For detailed
       information about the binding of @racket[id], see
       @secref["structinfo"].
       
       The @racket[constructor-id] and @racket[id] can be the same, in
       which case @racket[id] performs both roles. In that case, the
       expansion of @racket[id] as an expression produces an otherwise
       inaccessible identifier that is bound to the constructor
       procedure; the expanded identifier has a
       @racket['constructor-for] property whose value is an identifier
       that is @racket[free-identifier=?] to @racket[id] as well as 
       a syntax property accessible via 
       @racket[syntax-procedure-alias-property] with an identifier
       that is @racket[free-identifier=?] to @racket[id].}

 @item{@racket[id]@racketidfont{?}, a @deftech{predicate} procedure
       that returns @racket[#t] for instances of the @tech{structure
       type} (constructed by @racket[constructor-id] or the
       @tech{constructor} for a subtype) and @racket[#f] for any other
       value.}

 @item{@racket[id]@racketidfont{-}@racket[field-id], for each
       @racket[field]; an @deftech{accessor} procedure that takes an
       instance of the @tech{structure type} and extracts the value
       for the corresponding field.}

 @item{@racketidfont{set-}@racket[id]@racketidfont{-}@racket[field-id]@racketidfont{!},
       for each @racket[field] that includes a
       @racket[#:mutable] option, or when the
       @racket[#:mutable] option is specified as a
       @racket[struct-option]; a @deftech{mutator} procedure that
       takes an instance of the @tech{structure type} and a new field
       value. The structure is destructively updated with the new
       value, and @|void-const| is returned.}

]

If @racket[super-id] is provided, it must have a transformer binding
of the same sort bound to @racket[id] (see @secref["structinfo"]),
and it specifies a supertype for the structure type. Alternately,
the @racket[#:super] option can be used to specify an expression that
must produce a @tech{structure type descriptor}. See
@secref["structures"] for more information on structure subtypes
and supertypes. If both @racket[super-id] and @racket[#:super] are
provided, a syntax error is reported.

@examples[#:eval posn-eval
  (struct document (author title content))
  (struct book document (publisher))
  (struct paper (journal) #:super struct:document)
]

If the @racket[#:mutable] option is specified for an individual
field, then the field can be mutated in instances of the structure
type, and a @tech{mutator} procedure is bound. Supplying
@racket[#:mutable] as a @racket[struct-option] is the same as
supplying it for all @racket[field]s. If @racket[#:mutable] is
specified as both a @racket[field-option] and @racket[struct-option],
a syntax error is reported.

@examples[#:eval posn-eval
  (struct cell ([content #:mutable]) #:transparent)
  (define a-cell (cell 0))
  (set-cell-content! a-cell 1)
]

The @racket[#:inspector], @racket[#:auto-value], and @racket[#:guard]
options specify an inspector, value for automatic fields, and guard
procedure, respectively. See @racket[make-struct-type] for more
information on these attributes of a structure type.  The
@racket[#:property] option, which is the only one that can be supplied
multiple times, attaches a property value to the structure type; see
@secref["structprops"] for more information on properties. The
@racket[#:transparent] option is a shorthand for @racket[#:inspector
#f].

@examples[#:eval posn-eval
  (struct point (x y) #:inspector #f)
  (point 3 5)
  (struct celsius (temp)
    #:guard (λ (temp name)
              (unless (and (real? temp) (>= temp -273.15))
                (error "not a valid temperature"))
              temp))
  (eval:error (celsius -275))
]

@margin-note{Use the @racket[prop:procedure] property to implement an
@as-index{applicable structure}, use @racket[prop:evt] to create a
structure type whose instances are @tech{synchronizable events}, and
so on. By convention, property names start with @racketidfont{prop:}.}

The @racket[#:prefab] option obtains a @techlink{prefab} (pre-defined,
globally shared) structure type, as opposed to creating a new
structure type. Such a structure type is inherently transparent and
cannot have a guard or properties, so using @racket[#:prefab] with
@racket[#:transparent], @racket[#:inspector], @racket[#:guard], or
@racket[#:property] is a syntax error. If a supertype is specified, it
must also be a @tech{prefab} structure type.

@examples[#:eval posn-eval
  (struct prefab-point (x y) #:prefab)
  (prefab-point 1 2)
  (prefab-point? #s(prefab-point 1 2))
]

If @racket[constructor-id] is supplied, then the @tech{transformer}
binding of @racket[id] records @racket[constructor-id] as the
constructor binding; as a result, for example, @racket[struct-out]
includes @racket[constructor-id] as an export. If
@racket[constructor-id] is supplied via
@racket[#:extra-constructor-name] and it is not @racket[id], applying
@racket[object-name] on the constructor produces the symbolic form of
@racket[id] rather than @racket[constructor-id]. If
@racket[constructor-id] is supplied via @racket[#:constructor-name]
and it is not the same as @racket[id], then @racket[id] does not serve
as a constructor, and @racket[object-name] on the constructor produces
the symbolic form of @racket[constructor-id]. Only one of 
@racket[#:extra-constructor-name] and @racket[#:constructor-name]
can be provided within a @racket[struct] form.

@examples[#:eval posn-eval
  (struct color (r g b) #:constructor-name -color)
  (struct rectangle (w h color) #:extra-constructor-name rect)
  (rectangle 13 50 (-color 192 157 235))
  (rect 50 37 (-color 35 183 252))
]

If @racket[#:reflection-name symbol-expr] is provided, then
@racket[symbol-expr] must produce a symbol that is used to identify
the structure type in reflective operations such as
@racket[struct-type-info]. It corresponds to the first argument of
@racket[make-struct-type]. Structure printing uses the reflective
name, as do the various procedures that are bound by @racket[struct].

@examples[#:eval posn-eval
  (struct circle (radius) #:reflection-name '<circle>)
  (circle 15)
  (eval:error (circle-radius "bad"))
]

If @racket[#:methods gen:name method-defs] is provided, then
@racket[gen:name] must be a transformer binding for the static
information about a generic interface produced by @racket[define-generics].
The @racket[method-defs] define the methods of the @racket[gen:name]
interface. A @racket[define/generic] form or auxiliary definitions
and expressions may also appear in @racket[method-defs].

@examples[#:eval posn-eval
  (struct constant-stream (val)
    #:methods gen:stream
    [(define (stream-empty? stream) #f)
     (define (stream-first stream)
       (constant-stream-val stream))
     (define (stream-rest stream) stream)])
  (stream-ref (constant-stream 'forever) 0)
  (stream-ref (constant-stream 'forever) 50)
]

If the @racket[#:omit-define-syntaxes] option is supplied, then
@racket[id] is not bound as a transformer. If the
@racket[#:omit-define-values] option is supplied, then none of the
usual variables are bound, but @racket[id] is bound. If both are
supplied, then the @racket[struct] form is equivalent to
@racket[(begin)].

@examples[#:eval posn-eval
  (struct square (side) #:omit-define-syntaxes)
  (eval:error
   (match (square 5)
     (code:comment "fails to match because syntax is omitted")
     [(struct square x) x]))
  (struct ellipse (width height) #:omit-define-values)
  (eval:error ellipse-width)
]

If @racket[#:auto] is supplied as a @racket[field-option], then the
@tech{constructor} procedure for the structure type does not accept an
argument corresponding to the field. Instead, the structure type's
automatic value is used for the field, as specified by the
@racket[#:auto-value] option, or as defaults to @racket[#f] when
@racket[#:auto-value] is not supplied. The field is mutable (e.g.,
through reflective operations), but a mutator procedure is bound only
if @racket[#:mutable] is specified.

If a @racket[field] includes the @racket[#:auto] option, then all
fields after it must also include @racket[#:auto], otherwise a syntax
error is reported. If any @racket[field-option] or
@racket[struct-option] keyword is repeated, other than
@racket[#:property], a syntax error is reported.

@examples[
#:eval posn-eval
(eval:no-prompt
 (struct posn (x y [z #:auto #:mutable])
   #:auto-value 0
   #:transparent))
(posn 1 2)
(posn? (posn 1 2))
(posn-y (posn 1 2))
(posn-z (posn 1 2))

(eval:no-prompt
 (struct color-posn posn (hue) #:mutable)
 (define cp (color-posn 1 2 "blue")))
(color-posn-hue cp)
cp
(set-posn-z! cp 3)
]

For serialization, see @racket[define-serializable-struct].
}


@defform[(struct-field-index field-id)]{

This form can only appear as an expression within a
@racket[struct] form; normally, it is used with
@racket[#:property], especially for a property like
@racket[prop:procedure]. The result of a @racket[struct-field-index]
expression is an exact, non-negative integer that corresponds to the
position within the structure declaration of the field named by
@racket[field-id].

@examples[
#:eval posn-eval
(eval:no-prompt
 (struct mood-procedure (base rating)
   #:property prop:procedure (struct-field-index base))
 (define happy+ (mood-procedure add1 10)))
(happy+ 2)
(mood-procedure-rating happy+)
]}


@defform/subs[(define-struct id-maybe-super (field ...)
                             struct-option ...)
              ([id-maybe-super id
                               (id super-id)])]{

Like @racket[struct], except that the syntax for supplying a
@racket[super-id] is different, and a @racket[_constructor-id] that
has a @racketidfont{make-} prefix on @racket[id] is implicitly
supplied via @racket[#:extra-constructor-name] if neither
@racket[#:extra-constructor-name] nor @racket[#:constructor-name] is
provided.

This form is provided for backwards compatibility; @racket[struct] is
preferred.

@examples[
#:eval posn-eval
(eval:no-prompt
 (define-struct posn (x y [z #:auto])
    #:auto-value 0
    #:transparent))
(make-posn 1 2)
(posn? (make-posn 1 2))
(posn-y (make-posn 1 2))
]}


@defform[(define-struct/derived (id . rest-form) 
           id-maybe-super (field ...) struct-option ...)]{

The same as @racket[define-struct], but with an extra @racket[(id
. rest-form)] sub-form that is treated as the overall form for
syntax-error reporting and otherwise ignored.  The only constraint on
the sub-form for error reporting is that it starts with @racket[id].
The @racket[define-struct/derived] form is intended for use by macros
that expand to @racket[define-struct].

@examples[
#:eval posn-eval
(eval:no-prompt
 (define-syntax (define-xy-struct stx)
   (syntax-case stx ()
    [(ds name . rest) 
     (with-syntax ([orig stx])
       #'(define-struct/derived orig name (x y) . rest))])))

(define-xy-struct posn)
(posn-x (make-posn 1 2))
(define-xy-struct posn #:mutable)
(set-posn-x! (make-posn 1 2) 0)
(code:comment "this next line will cause an error due to a bad keyword")
(eval:error (define-xy-struct posn #:bad-option))
]}

@; ----------------------------------------

@close-eval[posn-eval]
