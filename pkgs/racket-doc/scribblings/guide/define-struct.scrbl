#lang scribble/doc
@(require scribble/manual scribble/eval scribble/bnf "guide-utils.rkt"
          (for-label racket/dict racket/serialize))

@(define posn-eval (make-base-eval))

@title[#:tag "define-struct"]{Programmer-Defined Datatypes}

@refalso["structures"]{structure types}

New datatypes are normally created with the @racket[struct]
form, which is the topic of this chapter. The class-based object
system, which we defer to @secref["classes"], offers an alternate
mechanism for creating new datatypes, but even classes and objects are
implemented in terms of structure types.

@; ------------------------------------------------------------
@section{Simple Structure Types: @racket[struct]}

@refalso["define-struct"]{@racket[struct]}

To a first approximation, the syntax of @racket[struct] is

@specform[
(struct struct-id (field-id ...))
]{}

@as-examples[@racketblock+eval[
#:eval posn-eval
(struct posn (x y))
]]

The @racket[struct] form binds @racket[_struct-id] and a number of
identifiers that are built from @racket[_struct-id] and the
@racket[_field-id]s:

@itemize[

 @item{@racket[_struct-id] : a @deftech{constructor} function that
       takes as many arguments as the number of @racket[_field-id]s,
       and returns an instance of the structure type.

       @examples[#:eval posn-eval (posn 1 2)]}

 @item{@racket[_struct-id]@racketidfont{?} : a @deftech{predicate}
       function that takes a single argument and returns @racket[#t]
       if it is an instance of the structure type, @racket[#f]
       otherwise.

       @examples[#:eval posn-eval (posn? 3) (posn? (posn 1 2))]}

 @item{@racket[_struct-id]@racketidfont{-}@racket[_field-id] : for
       each @racket[_field-id], an @deftech{accessor} that extracts
       the value of the corresponding field from an instance of the
       structure type.

       @examples[#:eval posn-eval 
                 (posn-x (posn 1 2)) (posn-y (posn 1 2))]}

 @item{@racketidfont{struct:}@racket[_struct-id] : a
       @deftech{structure type descriptor}, which is a value that
       represents the structure type as a first-class value (with
       @racket[#:super], as discussed later in
       @secref["struct-options"]).}

]

A @racket[struct] form places no constraints on the kinds of
values that can appear for fields in an instance of the structure
type. For example, @racket[(posn "apple" #f)] produces an
instance of @racket[posn], even though @racket["apple"] and
@racket[#f] are not valid coordinates for the obvious uses of
@racket[posn] instances. Enforcing constraints on field values, such
as requiring them to be numbers, is normally the job of a contract, as
discussed later in @secref["contracts"].

@; ------------------------------------------------------------
@section[#:tag "struct-copy"]{Copying and Update}

The @racket[struct-copy] form clones a structure and optionally
updates specified fields in the clone. This process is sometimes
called a @deftech{functional update}, because the result is a
structure with updated field values. but the original structure is not
modified.

@specform[
(struct-copy struct-id struct-expr [field-id expr] ...)
]

The @racket[_struct-id] that appears after @racket[struct-copy] must
be a structure type name bound by @racket[struct] (i.e., the
name that cannot be used directly as an expression). The
@racket[_struct-expr] must produce an instance of the structure type.
The result is a new instance of the structure type that is like the old
one, except that the field indicated by each @racket[_field-id] gets
the value of the corresponding @racket[_expr].

@examples[
#:eval posn-eval 
(define p1 (posn 1 2))
(define p2 (struct-copy posn p1 [x 3]))
(list (posn-x p2) (posn-y p2))
(list (posn-x p1) (posn-x p2))
]


@; ------------------------------------------------------------
@section[#:tag "struct-subtypes"]{Structure Subtypes}

An extended form of @racket[struct] can be used to define a
@defterm{structure subtype}, which is a structure type that extends an
existing structure type:

@specform[
(struct struct-id super-id (field-id ...))
]

The @racket[_super-id] must be a structure type name bound by
@racket[struct] (i.e., the name that cannot be used directly as
an expression).

@as-examples[@racketblock+eval[
#:eval posn-eval 
(struct posn (x y))
(struct 3d-posn posn (z))
]]

A structure subtype inherits the fields of its supertype, and the
subtype constructor accepts the values for the subtype fields after
values for the supertype fields. An instance of a structure subtype
can be used with the predicate and accessors of the
supertype.

@examples[
#:eval posn-eval 
(define p (3d-posn 1 2 3))
p
(posn? p)
(3d-posn-z p)
(code:comment "a 3d-posn has an x field, but there is no 3d-posn-x selector:")
(3d-posn-x p)
(code:comment "use the supertype's posn-x selector to access the x field:")
(posn-x p)
]

@; ------------------------------------------------------------
@section[#:tag "trans-struct"]{Opaque versus Transparent Structure Types}

With a structure type definition like

@racketblock[
(struct posn (x y))
]

an instance of the structure type prints in a way that does not show
any information about the fields' values. That is, structure types by
default are @deftech{opaque}. If the accessors and mutators of a
structure type are kept private to a module, then no other module can
rely on the representation of the type's instances.

To make a structure type @deftech{transparent}, use the
@racket[#:transparent] keyword after the field-name sequence:

@def+int[
#:eval posn-eval
(struct posn (x y)
        #:transparent)
(posn 1 2)
]

An instance of a transparent structure type prints like a call to the
constructor, so that it shows the structures field values. A
transparent structure type also allows reflective operations, such as
@racket[struct?] and @racket[struct-info], to be used on its instances
(see @secref["reflection"]).

Structure types are opaque by default, because opaque structure
instances provide more encapsulation guarantees. That is, a library
can use an opaque structure to encapsulate data, and clients of the
library cannot manipulate the data in the structure except as allowed
by the library.

@; ------------------------------------------------------------
@section[#:tag "struct-equal"]{Structure Comparisons}

A generic @racket[equal?] comparison automatically recurs on the
fields of a transparent structure type, but @racket[equal?] defaults
to mere instance identity for opaque structure types:

@def+int[
#:eval posn-eval
(struct glass (width height) #:transparent)
(equal? (glass 1 2) (glass 1 2))
]
@def+int[
#:eval posn-eval
(struct lead (width height))
(define slab (lead 1 2))
(equal? slab slab)
(equal? slab (lead 1 2))
]

To support instances comparisons via @racket[equal?] without making
the structure type transparent, you can use the @racket[#:methods]
keyword, @racket[gen:equal+hash], and implement three methods:

@def+int[
#:eval posn-eval
(struct lead (width height)
  #:methods
  gen:equal+hash
  [(define (equal-proc a b equal?-recur)
     (code:comment @#,t{compare @racket[a] and @racket[b]})
     (and (equal?-recur (lead-width a) (lead-width b))
          (equal?-recur (lead-height a) (lead-height b))))
   (define (hash-proc a hash-recur)
     (code:comment @#,t{compute primary hash code of @racket[a]})
     (+ (hash-recur (lead-width a))
        (* 3 (hash-recur (lead-height a)))))
   (define (hash2-proc a hash2-recur)
     (code:comment @#,t{compute secondary hash code of @racket[a]})
     (+ (hash2-recur (lead-width a))
             (hash2-recur (lead-height a))))])
(equal? (lead 1 2) (lead 1 2))
]

The first function in the list implements the @racket[equal?] test on
two @racket[lead]s; the third argument to the function is used instead
of @racket[equal?] for recursive equality testing, so that data cycles
can be handled correctly. The other two functions compute primary and
secondary hash codes for use with @tech{hash tables}:

@interaction[
#:eval posn-eval
(define h (make-hash))
(hash-set! h (lead 1 2) 3)
(hash-ref h (lead 1 2))
(hash-ref h (lead 2 1))
]

The first function provided with @racket[gen:equal+hash] is not
required to recursively compare the fields of the structure. For
example, a structure type representing a set might implement equality
by checking that the members of the set are the same, independent of
the order of elements in the internal representation. Just take care
that the hash functions produce the same value for any two structure
types that are supposed to be equivalent.

@; ------------------------------------------------------------
@section{Structure Type Generativity}

Each time that a @racket[struct] form is evaluated, it
generates a structure type that is distinct from all existing
structure types, even if some other structure type has the same name
and fields.

This generativity is useful for enforcing abstractions and
implementing programs such as interpreters, but beware of placing a
@racket[struct] form in positions that are evaluated multiple
times.

@defexamples[
(define (add-bigger-fish lst)
  (struct fish (size) #:transparent) (code:comment #,(t "new every time"))
  (cond
   [(null? lst) (list (fish 1))]
   [else (cons (fish (* 2 (fish-size (car lst))))
               lst)]))

(add-bigger-fish null)
(add-bigger-fish (add-bigger-fish null))
]
@defs+int[
[(struct fish (size) #:transparent)
 (define (add-bigger-fish lst)
   (cond
    [(null? lst) (list (fish 1))]
    [else (cons (fish (* 2 (fish-size (car lst))))
                lst)]))]
(add-bigger-fish (add-bigger-fish null))
]

@; ------------------------------------------------------------
@section[#:tag "prefab-struct"]{Prefab Structure Types}

Although a @tech{transparent} structure type prints in a way that
shows its content, the printed form of the structure cannot be used in
an expression to get the structure back, unlike the printed form of a
number, string, symbol, or list.

A @deftech{prefab} (``previously fabricated'') structure type is a
built-in type that is known to the Racket printer and expression
reader. Infinitely many such types exist, and they are indexed by
name, field count, supertype, and other such details. The printed form
of a prefab structure is similar to a vector, but it starts
@litchar{#s} instead of just @litchar{#}, and the first element in the
printed form is the prefab structure type's name.

The following examples show instances of the @racketidfont{sprout}
prefab structure type that has one field. The first instance has a
field value @racket['bean], and the second has field value
@racket['alfalfa]:

@interaction[
'#s(sprout bean)
'#s(sprout alfalfa)
]

Like numbers and strings, prefab structures are ``self-quoting,'' so
the quotes above are optional:

@interaction[
#s(sprout bean)
]

When you use the @racket[#:prefab] keyword with
@racket[struct], instead of generating a new structure type,
you obtain bindings that work with the existing prefab structure type:

@interaction[
#:eval posn-eval
(define lunch '#s(sprout bean))
(struct sprout (kind) #:prefab)
(sprout? lunch)
(sprout-kind lunch)
(sprout 'garlic)
]

The field name @racketidfont{kind} above does not matter for finding
the prefab structure type; only the name @racketidfont{sprout} and the
number of fields matters. At the same time, the prefab structure type
@racketidfont{sprout} with three fields is a different structure type
than the one with a single field:

@interaction[
#:eval posn-eval
(sprout? #s(sprout bean #f 17))
(code:line (struct sprout (kind yummy? count) #:prefab) (code:comment @#,t{redefine}))
(sprout? #s(sprout bean #f 17))
(sprout? lunch)
]

A prefab structure type can have another prefab structure type as its
supertype, it can have mutable fields, and it can have auto
fields. Variations in any of these dimensions correspond to different
prefab structure types, and the printed form of the structure type's
name encodes all of the relevant details.

@interaction[
(struct building (rooms [location #:mutable]) #:prefab)
(struct house building ([occupied #:auto]) #:prefab
  #:auto-value 'no)
(house 5 'factory)
]

Every @tech{prefab} structure type is @tech{transparent}---but even
less abstract than a @tech{transparent} type, because instances can be
created without any access to a particular structure-type declaration
or existing examples. Overall, the different options for structure
types offer a spectrum of possibilities from more abstract to more
convenient:

@itemize[

 @item{@tech{Opaque} (the default) : Instances cannot be inspected or
       forged without access to the structure-type declaration. As
       discussed in the next section, @tech{constructor guards} and
       @tech{properties} can be attached to the structure type to
       further protect or to specialize the behavior of its
       instances.}

 @item{@tech{Transparent} : Anyone can inspect or create an instance
       without access to the structure-type declaration, which means
       that the value printer can show the content of an instance. All
       instance creation passes through a @tech{constructor guard},
       however, so that the content of an instance can be controlled,
       and the behavior of instances can be specialized through
       @tech{properties}. Since the structure type is generated by its
       definition, instances cannot be manufactured simply through the
       name of the structure type, and therefore cannot be generated
       automatically by the expression reader. }

 @item{@tech{Prefab} : Anyone can inspect or create an instance at any
       time, without prior access to a structure-type declaration or
       an example instance. Consequently, the expression reader can
       manufacture instances directly. The instance cannot have a
       @tech{constructor guard} or @tech{properties}.}

]

Since the expression reader can generate @tech{prefab} instances, they
are useful when convenient @tech{serialization} is more important than
abstraction. @tech{Opaque} and @tech{transparent} structures also can
be serialized, however, if they are defined with
@racket[serializable-struct] as described in
@secref["serialization"].

@; ------------------------------------------------------------
@section[#:tag "struct-options"]{More Structure Type Options}

The full syntax of @racket[struct] supports many options, both
at the structure-type level and at the level of individual fields:

@specform/subs[(struct struct-id maybe-super (field ...)
                       struct-option ...)
               ([maybe-super code:blank
                             super-id]
                [field field-id
                       [field-id field-option ...]])]

A @racket[_struct-option] always starts with a keyword:

 @specspecsubform[#:mutable]{

    Causes all fields of the structure to be mutable, and introduces
    for each @racket[_field-id] a @deftech{mutator}
     @racketidfont{set-}@racket[_struct-id]@racketidfont{-}@racket[_field-id]@racketidfont{!}
    that sets the value of the corresponding field in an instance of
    the structure type.

     @defexamples[(struct dot (x y) #:mutable)
                  (define d (dot 1 2))
                  (dot-x d)
                  (set-dot-x! d 10)
                  (dot-x d)]

   The @racket[#:mutable] option can also be used as a
   @racket[_field-option], in which case it makes an individual field
   mutable.
       
   @defexamples[
   (struct person (name [age #:mutable]))
   (define friend (person "Barney" 5))
   (set-person-age! friend 6)
   (set-person-name! friend "Mary")]}

 @specspecsubform[(code:line #:transparent)]{
  Controls reflective access to structure instances, as discussed
  in a previous section, @secref["trans-struct"].}

 @specspecsubform[(code:line #:inspector inspector-expr)]{
  Generalizes @racket[#:transparent] to support more controlled access
  to reflective operations.}

 @specspecsubform[(code:line #:prefab)]{
  Accesses a built-in structure type, as discussed
  in a previous section, @secref["prefab-struct"].}

 @specspecsubform[(code:line #:auto-value auto-expr)]{

  Specifies a value to be used for all automatic fields in the
  structure type, where an automatic field is indicated by the
  @racket[#:auto] field option. The constructor procedure does not
  accept arguments for automatic fields. Automatic fields are
  implicitly mutable (via reflective operations), but mutator
  functions are bound only if @racket[#:mutable] is also specified.

  @defexamples[
    (struct posn (x y [z #:auto])
                 #:transparent
                 #:auto-value 0)
    (posn 1 2)
  ]}

@;-- FIXME:
@;-- Explain when to use guards instead of contracts, and vice versa

 @specspecsubform[(code:line #:guard guard-expr)]{ Specifies a
  @deftech{constructor guard} procedure to be called whenever an
  instance of the structure type is created. The guard takes as many
  arguments as non-automatic fields in the structure type, plus one
  more for the name of the instantiated type (in case a sub-type is
  instantiated, in which case it's best to report an error using the
  sub-type's name). The guard should return the same number of values
  as given, minus the name argument. The guard can raise an exception
  if one of the given arguments is unacceptable, or it can convert an
  argument.

 @defexamples[
   #:eval posn-eval
   (struct thing (name)
           #:transparent
           #:guard (lambda (name type-name)
                     (cond
                       [(string? name) name]
                       [(symbol? name) (symbol->string name)]
                       [else (error type-name 
                                    "bad name: ~e" 
                                    name)])))
   (thing "apple")
   (thing 'apple)
   (thing 1/2)
  ]

  The guard is called even when subtype instances are created. In that
  case, only the fields accepted by the constructor are provided to
  the guard (but the subtype's guard gets both the original fields and
  fields added by the subtype).

 @defexamples[
  #:eval posn-eval
  (struct person thing (age)
          #:transparent
          #:guard (lambda (name age type-name)
                    (if (negative? age)
                        (error type-name "bad age: ~e" age)
                        (values name age))))
  (person "John" 10)
  (person "Mary" -1)
  (person 10 10)]}

 @specspecsubform[(code:line #:methods interface-expr [body ...])]{
  Associates method definitions for the structure type that correspond
  to a @defterm{generic interface}.  For example, implementing the
  methods for @racket[gen:dict] allows instances of a structure
  type to be used as dictionaries. Implementing
  the methods for @racket[gen:custom-write] allows the customization
  of how an instance of a structure type is @racket[display]ed.

  @defexamples[
    (struct cake (candles)
            #:methods gen:custom-write
            [(define (write-proc cake port mode)
               (define n (cake-candles cake))
               (show "   ~a   ~n" n #\. port)
               (show " .-~a-. ~n" n #\| port)
               (show " | ~a | ~n" n #\space port)
               (show "---~a---~n" n #\- port))
             (define (show fmt n ch port)
               (fprintf port fmt (make-string n ch)))])
    (display (cake 5))]}

 @specspecsubform[(code:line #:property prop-expr val-expr)]{
   Associates a @deftech{property} and value with the structure type.
   For example, the @racket[prop:procedure] property allows a
   structure instance to be used as a function; the property value
   determines how a call is implemented when using the structure as a
   function.

 @defexamples[
   (struct greeter (name)
           #:property prop:procedure
                      (lambda (self other)
                        (string-append
                         "Hi " other
                         ", I'm " (greeter-name self))))
   (define joe-greet (greeter "Joe"))
   (greeter-name joe-greet)
   (joe-greet "Mary")
   (joe-greet "John")]}

 @specspecsubform[(code:line #:super super-expr)]{

  An alternative to supplying a @racket[super-id] next to
  @racket[struct-id]. Instead of the name of a structure type (which is
  not an expression), @racket[super-expr] should produce a
  @tech{structure type descriptor} value. An advantage of
  @racket[#:super] is that structure type descriptors are values, so
  they can be passed to procedures.

  @defexamples[
    #:eval posn-eval
    (define (raven-constructor super-type)
      (struct raven ()
              #:super super-type
              #:transparent
              #:property prop:procedure (lambda (self)
                                          'nevermore))
      raven)
    (let ([r ((raven-constructor struct:posn) 1 2)])
      (list r (r)))
    (let ([r ((raven-constructor struct:thing) "apple")])
      (list r (r)))]}

@; ----------------------------------------

@refdetails["structures"]{structure types}

@close-eval[posn-eval]
