#lang scribble/doc
@(require scribble/manual scribble/eval racket/class "guide-utils.rkt"
          (for-label racket/class racket/trait racket/contract))

@(define class-eval
   (let ([e (make-base-eval)])
     (e '(require racket/class))
     e))

@; FIXME: at some point, discuss classes vs. units vs. modules


@title[#:tag "classes"]{Classes and Objects}

@margin-note{This chapter is based on a paper @cite["Flatt06"].}

A @racket[class] expression denotes a first-class value,
just like a @racket[lambda] expression:

@specform[(class superclass-expr decl-or-expr ...)]

The @racket[_superclass-expr] determines the superclass for the new
class. Each @racket[_decl-or-expr] is either a declaration related to
methods, fields, and initialization arguments, or it is an expression
that is evaluated each time that the class is instantiated. In other
words, instead of a method-like constructor, a class has
initialization expressions interleaved with field and method
declarations.

By convention, class names end with @racketidfont{%}. The built-in root class is
@racket[object%]. The following expression creates a class with
public methods @racket[get-size], @racket[grow], and @racket[eat]:

@racketblock[
(class object%
  (init size)                (code:comment #,(t "initialization argument"))

  (define current-size size) (code:comment #,(t "field"))

  (super-new)                (code:comment #,(t "superclass initialization"))

  (define/public (get-size)
    current-size)

  (define/public (grow amt)
    (set! current-size (+ amt current-size)))

  (define/public (eat other-fish)
    (grow (send other-fish get-size))))
]

@(interaction-eval
#:eval class-eval
(define fish%
  (class object%
    (init size)
    (define current-size size)
    (super-new)
    (define/public (get-size)
      current-size)
    (define/public (grow amt)
      (set! current-size (+ amt current-size)))
    (define/public (eat other-fish)
      (grow (send other-fish get-size))))))

The @racket[size] initialization argument must be supplied via a named
 argument when instantiating the class through the @racket[new] form:

@racketblock[
(new (class object% (init size) ....) [size 10])
]

Of course, we can also name the class and its instance:

@racketblock[
(define fish% (class object% (init size) ....))
(define charlie (new fish% [size 10]))
]

@(interaction-eval
#:eval class-eval
(define charlie (new fish% [size 10])))

In the definition of @racket[fish%], @racket[current-size] is a
private field that starts out with the value of the @racket[size]
initialization argument. Initialization arguments like @racket[size]
are available only during class instantiation, so they cannot be
referenced directly from a method. The @racket[current-size] field, in
contrast, is available to methods.

The @racket[(super-new)] expression in @racket[fish%] invokes the
initialization of the superclass. In this case, the superclass is
@racket[object%], which takes no initialization arguments and performs
no work; @racket[super-new] must be used, anyway, because a class must
always invoke its superclass's initialization.

Initialization arguments, field declarations, and expressions such as
@racket[(super-new)] can appear in any order within a @racket[class],
and they can be interleaved with method declarations. The relative
order of expressions in the class determines the order of evaluation
during instantiation. For example, if a field's initial value requires
calling a method that works only after superclass initialization, then
the field declaration must be placed after the @racket[super-new]
call. Ordering field and initialization declarations in this way helps
avoid imperative assignment. The relative order of method declarations
makes no difference for evaluation, because methods are fully defined
before a class is instantiated.

@section[#:tag "methods"]{Methods}

Each of the three @racket[define/public] declarations in
@racket[fish%] introduces a new method. The declaration uses the same
syntax as a Racket function, but a method is not accessible as an
independent function.  A call to the @racket[grow] method of a
@racket[fish%] object requires the @racket[send] form:

@interaction[
#:eval class-eval
(send charlie grow 6)
(send charlie get-size)
]

Within @racket[fish%], self methods can be called like functions,
because the method names are in scope.  For example, the @racket[eat]
method within @racket[fish%] directly invokes the @racket[grow]
method.  Within a class, attempting to use a method name in any way
other than a method call results in a syntax error.

In some cases, a class must call methods that are supplied by the superclass
but not overridden. In that case, the class can use @racket[send]
with @racket[this] to access the method:

@def+int[
#:eval class-eval
(define hungry-fish% (class fish% (super-new)
                       (define/public (eat-more fish1 fish2)
                         (send this eat fish1)
                         (send this eat fish2))))
]

Alternately, the class can declare the existence of a method using @racket[inherit],
which brings the method name into scope for a direct call:

@def+int[
#:eval class-eval
(define hungry-fish% (class fish% (super-new)
                       (inherit eat)
                       (define/public (eat-more fish1 fish2)
                         (eat fish1) (eat fish2))))
]

With the @racket[inherit] declaration, if @racket[fish%] had not
provided an @racket[eat] method, an error would be signaled in the
evaluation of the @racket[class] form for @racket[hungry-fish%]. In
contrast, with @racket[(send this ....)], an error would not be
signaled until the @racket[eat-more] method is called and the
@racket[send] form is evaluated. For this reason, @racket[inherit] is
preferred.

Another drawback of @racket[send] is that it is less efficient than
@racket[inherit]. Invocation of a method via @racket[send] involves
finding a method in the target object's class at run time, making
@racket[send] comparable to an interface-based method call in Java. In
contrast, @racket[inherit]-based method invocations use an offset
within the class's method table that is computed when the class is
created.

To achieve performance similar to @racket[inherit]-based method calls when
invoking a method from outside the method's class, the programmer must use the
@racket[generic] form, which produces a class- and method-specific
@defterm{generic method} to be invoked with @racket[send-generic]:

@def+int[
#:eval class-eval
(define get-fish-size (generic fish% get-size))
(send-generic charlie get-fish-size)
(send-generic (new hungry-fish% [size 32]) get-fish-size)
(send-generic (new object%) get-fish-size)
]

Roughly speaking, the form translates the class and the external
method name to a location in the class's method table. As illustrated
by the last example, sending through a generic method checks that its
argument is an instance of the generic's class.

Whether a method is called directly within a @racket[class],
through a generic method,
or through @racket[send], method overriding works in the usual way:

@defs+int[
#:eval class-eval
[
(define picky-fish% (class fish% (super-new)
                      (define/override (grow amt)
                        ;; Doesn't eat all of its food
                        (super grow (* 3/4 amt)))))
(define daisy (new picky-fish% [size 20]))
]
(send daisy eat charlie)
(send daisy get-size)
]

The @racket[grow] method in @racket[picky-fish%] is declared with
@racket[define/override] instead of @racket[define/public], because
@racket[grow] is meant as an overriding declaration. If @racket[grow]
had been declared with @racket[define/public], an error would have
been signaled when evaluating the @racket[class] expression, because
@racket[fish%] already supplies @racket[grow].

Using @racket[define/override] also allows the invocation of the
overridden method via a @racket[super] call. For example, the
@racket[grow] implementation in @racket[picky-fish%] uses
@racket[super] to delegate to the superclass implementation.

@section[#:tag "initargs"]{Initialization Arguments}

Since @racket[picky-fish%] declares no initialization arguments, any
initialization values supplied in @racket[(new picky-fish% ....)]  are
propagated to the superclass initialization, i.e., to @racket[fish%].
A subclass can supply additional initialization arguments for its
superclass in a @racket[super-new] call, and such initialization
arguments take precedence over arguments supplied to @racket[new]. For
example, the following @racket[size-10-fish%] class always generates
fish of size 10:

@def+int[
#:eval class-eval
(define size-10-fish% (class fish% (super-new [size 10])))
(send (new size-10-fish%) get-size)
]

In the case of @racket[size-10-fish%], supplying a @racket[size]
initialization argument with @racket[new] would result in an
initialization error; because the @racket[size] in @racket[super-new]
takes precedence, a @racket[size] supplied to @racket[new] would have
no target declaration.

An initialization argument is optional if the @racket[class] form
declares a default value. For example, the following @racket[default-10-fish%]
class accepts a @racket[size] initialization argument, but its value defaults to
10 if no value is supplied on instantiation:

@def+int[
#:eval class-eval
(define default-10-fish% (class fish%
                           (init [size 10])
                           (super-new [size size])))
(new default-10-fish%)
(new default-10-fish% [size 20])
]

In this example, the @racket[super-new] call propagates its own
@racket[size] value as the @racket[size] initialization argument to
the superclass.

@section[#:tag "intnames"]{Internal and External Names}

The two uses of @racket[size] in @racket[default-10-fish%] expose the
double life of class-member identifiers. When @racket[size] is the
first identifier of a bracketed pair in @racket[new] or
@racket[super-new], @racket[size] is an @defterm{external name} that
is symbolically matched to an initialization argument in a class. When
@racket[size] appears as an expression within
@racket[default-10-fish%], @racket[size] is an @defterm{internal name}
that is lexically scoped. Similarly, a call to an inherited
@racket[eat] method uses @racket[eat] as an internal name, whereas a
@racket[send] of @racket[eat] uses @racket[eat] as an external name.

The full syntax of the @racket[class] form allows a programmer to
specify distinct internal and external names for a class member. Since
internal names are local, they can be renamed to avoid shadowing or
conflicts. Such renaming is not frequently necessary, but workarounds
in the absence of renaming can be especially cumbersome.

@section{Interfaces}

Interfaces are useful for checking that an object or a class
implements a set of methods with a particular (implied) behavior.
This use of interfaces is helpful even without a static type system
(which is the main reason that Java has interfaces).

An interface in Racket is created using the @racket[interface]
form, which merely declares the method names required to implement the
interface. An interface can extend other interfaces, which means that
implementations of the interface automatically implement the extended
interfaces.

@specform[(interface (superinterface-expr ...) id ...)]

To declare that a class implements an interface, the
@racket[class*] form must be used instead of @racket[class]:

@specform[(class* superclass-expr (interface-expr ...) decl-or-expr ...)]

For example, instead of forcing all fish classes to be derived from
@racket[fish%], we can define @racket[fish-interface] and change the
@racket[fish%] class to declare that it implements
@racket[fish-interface]:

@racketblock[
(define fish-interface (interface () get-size grow eat))
(define fish% (class* object% (fish-interface) ....))
]

If the definition of @racket[fish%] does not include
@racket[get-size], @racket[grow], and @racket[eat] methods, then an
error is signaled in the evaluation of the @racket[class*] form,
because implementing the @racket[fish-interface] interface requires
those methods.

The @racket[is-a?] predicate accepts an object as its first argument
and either a class or interface as its second argument. When given a
class, @racket[is-a?] checks whether the object is an instance of that
class or a derived class.  When given an interface, @racket[is-a?]
checks whether the object's class implements the interface. In
addition, the @racket[implementation?]  predicate checks whether a
given class implements a given interface.

@section[#:tag "inner"]{Final, Augment, and Inner}

As in Java, a method in a @racket[class] form can be specified as
@defterm{final}, which means that a subclass cannot override the
method.  A final method is declared using @racket[public-final] or
@racket[override-final], depending on whether the declaration is for a
new method or an overriding implementation.

Between the extremes of allowing arbitrary overriding and disallowing
overriding entirely, the class system also supports Beta-style
@defterm{augmentable} methods @cite["Goldberg04"]. A method
declared with @racket[pubment] is like @racket[public], but the method
cannot be overridden in subclasses; it can be augmented only. A
@racket[pubment] method must explicitly invoke an augmentation (if any)
using @racket[inner]; a subclass augments the method using
@racket[augment], instead of @racket[override].

In general, a method can switch between augment and override modes in
a class derivation. The @racket[augride] method specification
indicates an augmentation to a method where the augmentation is itself
overrideable in subclasses (though the superclass's implementation
cannot be overridden). Similarly, @racket[overment] overrides a method
and makes the overriding implementation augmentable.

@section[#:tag "extnames"]{Controlling the Scope of External Names}

@margin-note{
  Java's access modifiers (like @index["protected method"]{@tt{protected}})
  play a role similar to @racket[define-member-name], but
  unlike in Java, Racket's mechanism for controlling access
  is based on lexical scope, not the inheritance hierarchy.
}

As noted in @secref["intnames"], class members have both
internal and external names. A member definition binds an internal
name locally, and this binding can be locally renamed.  External
names, in contrast, have global scope by default, and a member
definition does not bind an external name. Instead, a member
definition refers to an existing binding for an external name, where
the member name is bound to a @defterm{member key}; a class ultimately
maps member keys to methods, fields, and initialization arguments.

Recall the @racket[hungry-fish%] @racket[class] expression:

@racketblock[
(define hungry-fish% (class fish% ....
                       (inherit eat)
                       (define/public (eat-more fish1 fish2)
                         (eat fish1) (eat fish2))))
]

During its evaluation, the @racket[hungry-fish%] and @racket[fish%]
classes refer to the same global binding of @racket[eat].  At run
time, calls to @racket[eat] in @racket[hungry-fish%] are matched with
the @racket[eat] method in @racket[fish%] through the shared method
key that is bound to @racket[eat].

The default binding for an external name is global, but a
programmer can introduce an external-name binding with the
@racket[define-member-name] form.

@specform[(define-member-name id member-key-expr)]

In particular, by using @racket[(generate-member-key)] as the
@racket[member-key-expr], an external name can be localized for a
particular scope, because the generated member key is inaccessible
outside the scope. In other words, @racket[define-member-name] gives
an external name a kind of package-private scope, but generalized from
packages to arbitrary binding scopes in Racket.

For example, the following @racket[fish%] and @racket[pond%] classes cooperate
via a @racket[get-depth] method that is only accessible to the
cooperating classes:

@racketblock[
(define-values (fish% pond%) (code:comment #,(t "two mutually recursive classes"))
  (let () ; create a local definition scope
    (define-member-name get-depth (generate-member-key))
    (define fish%
      (class ....
        (define my-depth ....)
	(define my-pond ....)
	(define/public (dive amt)
        (set! my-depth
              (min (+ my-depth amt)
                   (send my-pond get-depth))))))
    (define pond%
      (class ....
        (define current-depth ....)
        (define/public (get-depth) current-depth)))
    (values fish% pond%)))
]

External names are in a namespace that separates them from other Racket
names. This separate namespace is implicitly used for the method name in
@racket[send], for initialization-argument names in @racket[new], or for
the external name in a member definition.  The special form
@racket[member-name-key] provides access to the binding of an external name
in an arbitrary expression position: @racket[(member-name-key id)]
produces the member-key binding of @racket[id] in the current scope.

A member-key value is primarily used with a
@racket[define-member-name] form. Normally, then,
@racket[(member-name-key id)] captures the method key of @racket[id]
so that it can be communicated to a use of @racket[define-member-name]
in a different scope. This capability turns out to be useful for
generalizing mixins, as discussed next.

@; ----------------------------------------------------------------------

@section{Mixins}

Since @racket[class] is an expression form instead of a top-level
declaration as in Smalltalk and Java, a @racket[class] form can be
nested inside any lexical scope, including @racket[lambda]. The result
is a @deftech{mixin}, i.e., a class extension that is parameterized
with respect to its superclass.

For example, we can parameterize the @racket[picky-fish%] class over
its superclass to define @racket[picky-mixin]:

@racketblock[
(define (picky-mixin %)
  (class % (super-new)
    (define/override (grow amt) (super grow (* 3/4 amt)))))
(define picky-fish% (picky-mixin fish%))
]

Many small differences between Smalltalk-style classes and Racket
classes contribute to the effective use of mixins. In particular, the
use of @racket[define/override] makes explicit that
@racket[picky-mixin] expects a class with a @racket[grow] method. If
@racket[picky-mixin] is applied to a class without a @racket[grow]
method, an error is signaled as soon as @racket[picky-mixin] is
applied.

Similarly, a use of @racket[inherit] enforces a ``method existence''
requirement when the mixin is applied:

@racketblock[
(define (hungry-mixin %)
  (class % (super-new)
    (inherit eat)
    (define/public (eat-more fish1 fish2) 
      (eat fish1) 
      (eat fish2))))
]

The advantage of mixins is that we can easily combine them to create
new classes whose implementation sharing does not fit into a
single-inheritance hierarchy---without the ambiguities associated with
multiple inheritance. Equipped with @racket[picky-mixin] and
@racket[hungry-mixin], creating a class for a hungry, yet picky fish
is straightforward:

@racketblock[
(define picky-hungry-fish% 
  (hungry-mixin (picky-mixin fish%)))
]

The use of keyword initialization arguments is critical for the easy
use of mixins. For example, @racket[picky-mixin] and
@racket[hungry-mixin] can augment any class with suitable @racket[eat]
and @racket[grow] methods, because they do not specify initialization
arguments and add none in their @racket[super-new] expressions:

@racketblock[
(define person% 
  (class object%
    (init name age)
    ....
    (define/public (eat food) ....)
    (define/public (grow amt) ....)))
(define child% (hungry-mixin (picky-mixin person%)))
(define oliver (new child% [name "Oliver"] [age 6]))
]

Finally, the use of external names for class members (instead of
lexically scoped identifiers) makes mixin use convenient. Applying
@racket[picky-mixin] to @racket[person%] works because the names
@racket[eat] and @racket[grow] match, without any a priori declaration
that @racket[eat] and @racket[grow] should be the same method in
@racket[fish%] and @racket[person%]. This feature is a potential
drawback when member names collide accidentally; some accidental
collisions can be corrected by limiting the scope external names, as
discussed in @secref["extnames"].

@subsection{Mixins and Interfaces}

Using @racket[implementation?], @racket[picky-mixin] could require
that its base class implements @racket[grower-interface], which could
be implemented by both @racket[fish%] and @racket[person%]:

@racketblock[
(define grower-interface (interface () grow))
(define (picky-mixin %)
  (unless (implementation? % grower-interface)
    (error "picky-mixin: not a grower-interface class"))
  (class % ....))
]

Another use of interfaces with a mixin is to tag classes generated by
the mixin, so that instances of the mixin can be recognized. In other
words, @racket[is-a?] cannot work on a mixin represented as a
function, but it can recognize an interface (somewhat like a
@defterm{specialization interface}) that is consistently implemented
by the mixin.  For example, classes generated by @racket[picky-mixin]
could be tagged with @racket[picky-interface], enabling the
@racket[is-picky?] predicate:

@racketblock[
(define picky-interface (interface ()))
(define (picky-mixin %)
  (unless (implementation? % grower-interface)
    (error "picky-mixin: not a grower-interface class"))
  (class* % (picky-interface) ....))
(define (is-picky? o)
  (is-a? o picky-interface))
]

@subsection{The @racket[mixin] Form}

To codify the @racket[lambda]-plus-@racket[class] pattern for
implementing mixins, including the use of interfaces for the domain
and range of the mixin, the class system provides a @racket[mixin]
macro:

@specform[
(mixin (interface-expr ...) (interface-expr ...)
  decl-or-expr ...)
]

The first set of @racket[interface-expr]s determines the domain of the
mixin, and the second set determines the range. That is, the expansion
is a function that tests whether a given base class implements the
first sequence of @racket[interface-expr]s and produces a class that
implements the second sequence of @racket[interface-expr]s. Other
requirements, such as the presence of @racket[inherit]ed methods in
the superclass, are then checked for the @racket[class] expansion of
the @racket[mixin] form.

Mixins not only override methods and introduce public methods, they
can also augment methods, introduce augment-only methods, add an
overrideable augmentation, and add an augmentable override --- all of
the things that a class can do (see @secref["inner"]).


@subsection[#:tag "parammixins"]{Parameterized Mixins}

As noted in @secref["extnames"], external names can be bound with
@racket[define-member-name]. This facility allows a mixin to be
generalized with respect to the methods that it defines and uses.  For
example, we can parameterize @racket[hungry-mixin] with respect to the
external member key for @racket[eat]:

@racketblock[
(define (make-hungry-mixin eat-method-key)
  (define-member-name eat eat-method-key)
  (mixin () () (super-new)
    (inherit eat)
    (define/public (eat-more x y) (eat x) (eat y))))
]

To obtain a particular hungry-mixin, we must apply this function to a
member key that refers to a suitable
@racket[eat] method, which we can obtain using @racket[member-name-key]: 

@racketblock[
((make-hungry-mixin (member-name-key eat))
 (class object% .... (define/public (eat x) 'yum)))
]

Above, we apply @racket[hungry-mixin] to an anonymous class that provides
@racket[eat], but we can also combine it with a class that provides 
@racket[chomp], instead:

@racketblock[
((make-hungry-mixin (member-name-key chomp))
 (class object% .... (define/public (chomp x) 'yum)))
]

@; ----------------------------------------------------------------------

@section{Traits}

A @defterm{trait} is similar to a mixin, in that it encapsulates a set
of methods to be added to a class. A trait is different from a mixin
in that its individual methods can be manipulated with trait operators
such as @racket[trait-sum] (merge the methods of two traits), @racket[trait-exclude]
(remove a method from a trait), and @racket[trait-alias] (add a copy of a
method with a new name; do not redirect any calls to the old name).

The practical difference between mixins and traits is that two traits
can be combined, even if they include a common method and even if
neither method can sensibly override the other. In that case, the
programmer must explicitly resolve the collision, usually by aliasing
methods, excluding methods, and merging a new trait that uses the
aliases.

Suppose our @racket[fish%] programmer wants to define two class
extensions, @racket[spots] and @racket[stripes], each of which
includes a @racket[get-color] method. The fish's spot color should not
override the stripe color nor vice versa; instead, a
@racket[spots+stripes-fish%] should combine the two colors, which is
not possible if @racket[spots] and @racket[stripes] are implemented as
plain mixins. If, however, @racket[spots] and @racket[stripes] are
implemented as traits, they can be combined. First, we alias
@racket[get-color] in each trait to a non-conflicting name. Second,
the @racket[get-color] methods are removed from both and the traits
with only aliases are merged. Finally, the new trait is used to create
a class that introduces its own @racket[get-color] method based on the
two aliases, producing the desired @racket[spots+stripes] extension.

@subsection{Traits as Sets of Mixins}

One natural approach to implementing traits in Racket is as a set
of mixins, with one mixin per trait method.  For example, we might
attempt to define the spots and stripes traits as follows, using
association lists to represent sets:

@racketblock[
(define spots-trait
  (list (cons 'get-color 
               (lambda (%) (class % (super-new)
                             (define/public (get-color) 
                               'black))))))
(define stripes-trait
  (list (cons 'get-color 
              (lambda (%) (class % (super-new)
                            (define/public (get-color) 
                              'red))))))
]

A set representation, such as the above, allows @racket[trait-sum] and
@racket[trait-exclude] as simple manipulations; unfortunately, it does
not support the @racket[trait-alias] operator. Although a mixin can be
duplicated in the association list, the mixin has a fixed method name,
e.g., @racket[get-color], and mixins do not support a method-rename
operation. To support @racket[trait-alias], we must parameterize the
mixins over the external method name in the same way that @racket[eat]
was parameterized in @secref["parammixins"].

To support the @racket[trait-alias] operation, @racket[spots-trait]
should be represented as:

@racketblock[
(define spots-trait
  (list (cons (member-name-key get-color)
              (lambda (get-color-key %) 
                (define-member-name get-color get-color-key)
                (class % (super-new)
                  (define/public (get-color) 'black))))))
]

When the @racket[get-color] method in @racket[spots-trait] is aliased
to @racket[get-trait-color] and the @racket[get-color] method is
removed, the resulting trait is the same as

@racketblock[
(list (cons (member-name-key get-trait-color)
            (lambda (get-color-key %)
              (define-member-name get-color get-color-key)
              (class % (super-new)
                (define/public (get-color) 'black)))))
]

To apply a trait @racket[_T] to a class @racket[_C] and obtain a derived
class, we use @racket[((trait->mixin _T) _C)]. The @racket[trait->mixin]
function supplies each mixin of @racket[_T] with the key for the mixin's
method and a partial extension of @racket[_C]:

@racketblock[
(define ((trait->mixin T) C)
  (foldr (lambda (m %) ((cdr m) (car m) %)) C T))
]

Thus, when the trait above is combined with other traits and then
applied to a class, the use of @racket[get-color] becomes a reference
to the external name @racket[get-trait-color].

@subsection{Inherit and Super in Traits}

This first implementation of traits supports @racket[trait-alias], and it
 supports a trait method that calls itself, but it does not support
 trait methods that call each other. In particular, suppose that a spot-fish's
 market value depends on the color of its spots:

@racketblock[
(define spots-trait
  (list (cons (member-name-key get-color) ....)
        (cons (member-name-key get-price)
              (lambda (get-price %) ....
                (class % ....
                  (define/public (get-price) 
                    .... (get-color) ....))))))
]

In this case, the definition of @racket[spots-trait] fails, because
@racket[get-color] is not in scope for the @racket[get-price]
mixin. Indeed, depending on the order of mixin application when the
trait is applied to a class, the @racket[get-color] method may not be
available when @racket[get-price] mixin is applied to the class.
Therefore adding an @racket[(inherit get-color)] declaration to the
@racket[get-price] mixin does not solve the problem.

One solution is to require the use of @racket[(send this get-color)] in
methods such as @racket[get-price]. This change works because
@racket[send] always delays the method lookup until the method call is
evaluated. The delayed lookup is more expensive than a direct call,
however. Worse, it also delays checking whether a @racket[get-color] method
even exists.

A second, effective, and efficient solution is to change the encoding
of traits. Specifically, we represent each method as a pair of mixins:
one that introduces the method and one that implements it. When a
trait is applied to a class, all of the method-introducing mixins are
applied first. Then the method-implementing mixins can use
@racket[inherit] to directly access any introduced method.

@racketblock[
(define spots-trait
  (list (list (local-member-name-key get-color)
              (lambda (get-color get-price %) ....
                (class % ....
                  (define/public (get-color) (void))))
              (lambda (get-color get-price %) ....
                (class % ....
                  (define/override (get-color) 'black))))
        (list (local-member-name-key get-price)
              (lambda (get-price get-color %) ....
                (class % ....
                  (define/public (get-price) (void))))
              (lambda (get-color get-price %) ....
                (class % ....
                  (inherit get-color)
                  (define/override (get-price)
                    .... (get-color) ....))))))
]

With this trait encoding, @racket[trait-alias] adds a new method with
a new name, but it does not change any references to the old method.

@subsection{The @racket[trait] Form}

The general-purpose trait pattern is clearly too complex for a
programmer to use directly, but it is easily codified in a
@racket[trait] macro:

@specform[
(trait trait-clause ...)
]

The @racket[id]s in the optional @racket[inherit] clause are available for direct
reference in the method @racket[expr]s, and they must be supplied
either by other traits or the base class to which
the trait is ultimately applied.

Using this form in conjunction with trait operators such as
@racket[trait-sum], @racket[trait-exclude], @racket[trait-alias], and
@racket[trait->mixin], we can implement @racket[spots-trait] and
@racket[stripes-trait] as desired.

@racketblock[
(define spots-trait
  (trait
    (define/public (get-color) 'black)
    (define/public (get-price) ... (get-color) ...)))

(define stripes-trait
  (trait 
    (define/public (get-color) 'red)))

(define spots+stripes-trait
  (trait-sum
   (trait-exclude (trait-alias spots-trait
                               get-color get-spots-color)
                  get-color)
   (trait-exclude (trait-alias stripes-trait
                               get-color get-stripes-color)
                  get-color)
   (trait
     (inherit get-spots-color get-stripes-color)
     (define/public (get-color)
       .... (get-spots-color) .... (get-stripes-color) ....))))
]

@; ----------------------------------------------------------------------

@; Set up uses of contract forms below
@(class-eval '(require racket/contract))

@section{Class Contracts}

As classes are values, they can flow across contract boundaries, and we
may wish to protect parts of a given class with contracts.  For this,
the @racket[class/c] form is used.  The @racket[class/c] form has many
subforms, which describe two types of contracts on fields and methods:
those that affect uses via instantiated objects and those that affect
subclasses.

@subsection{External Class Contracts}

In its simplest form, @racket[class/c] protects the public fields and methods
of objects instantiated from the contracted class.  There is also an
@racket[object/c] form that can be used to similarly protect the public fields
and methods of a particular object. Take the following definition of
@racket[animal%], which uses a public field for its @racket[size] attribute:

@racketblock[
(define animal%
  (class object% 
    (super-new)
    (field [size 10])
    (define/public (eat food)
      (set! size (+ size (get-field size food))))))]

For any instantiated @racket[animal%], accessing the @racket[size] field
should return a positive number.  Also, if the @racket[size] field is set,
it should be assigned a positive number.  Finally, the @racket[eat] method
should receive an argument which is an object with a @racket[size] field
that contains a positive number. To ensure these conditions, we will define
the @racket[animal%] class with an appropriate contract:

@racketblock[
(define positive/c (and/c number? positive?))
(define edible/c (object/c (field [size positive/c])))
(define/contract animal%
  (class/c (field [size positive/c])
           [eat (->m edible/c void?)])
  (class object% 
    (super-new)
    (field [size 10])
    (define/public (eat food)
      (set! size (+ size (get-field size food))))))]

@interaction-eval[
#:eval class-eval
(begin
  (define positive/c
    (flat-named-contract 'positive/c (and/c number? positive?)))
  (define edible/c (object/c (field [size positive/c])))
  (define/contract animal%
    (class/c (field [size positive/c])
             [eat (->m edible/c void?)])
    (class object% 
      (super-new)
      (field [size 10])
      (define/public (eat food)
        (set! size (+ size (get-field size food)))))))]

Here we use @racket[->m] to describe the behavior of @racket[eat] since we
do not need to describe any requirements for the @racket[this] parameter.
Now that we have our contracted class, we can see that the contracts
on both @racket[size] and @racket[eat] are enforced:

@interaction[
#:eval class-eval
(define bob (new animal%))
(set-field! size bob 3)
(get-field size bob)
(set-field! size bob 'large)
(define richie (new animal%))
(send bob eat richie)
(get-field size bob)
(define rock (new object%))
(send bob eat rock)
(define giant (new (class object% (super-new) (field [size 'large]))))
(send bob eat giant)]

There are two important caveats for external class contracts. First,
external method contracts are only enforced when the target of dynamic
dispatch is the method implementation of the contracted class, which
lies within the contract boundary.  Overriding that implementation, and
thus changing the target of dynamic dispatch, will mean that the contract
is no longer enforced for clients, since accessing the method no longer
crosses the contract boundary.  Unlike external method contracts, external
field contracts are always enforced for clients of subclasses, since fields
cannot be overridden or shadowed.

Second, these contracts do not restrict subclasses of @racket[animal%]
in any way.  Fields and methods that are inherited and used by subclasses
are not checked by these contracts, and uses of the superclass's methods
via @racket[super] are also unchecked.  The following example illustrates
both caveats:

@def+int[
#:eval class-eval
(define large-animal%
  (class animal%
    (super-new)
    (inherit-field size)
    (set! size 'large)
    (define/override (eat food)
      (display "Nom nom nom") (newline))))
(define elephant (new large-animal%))
(send elephant eat (new object%))
(get-field size elephant)]

@subsection{Internal Class Contracts}

Notice that retrieving the @racket[size] field from the object
@racket[elephant] blames @racket[animal%] for the contract violation.
This blame is correct, but unfair to the @racket[animal%] class,
as we have not yet provided it with a method for protecting itself from
subclasses.  To this end we add internal class contracts, which
provide directives to subclasses for how they may access and override
features of the superclass.  This distinction between external and internal
class contracts allows for weaker contracts within the class hierarchy, where
invariants may be broken internally by subclasses but should be enforced
for external uses via instantiated objects.

As a simple example of what kinds of protection are available, we provide
an example aimed at the @racket[animal%] class that uses all the applicable
forms:

@racketblock[
(class/c (field [size positive/c])
         (inherit-field [size positive/c])
         [eat (->m edible/c void?)]
         (inherit [eat (->m edible/c void?)])
         (super [eat (->m edible/c void?)])
         (override [eat (->m edible/c void?)]))]

This class contract not only ensures that objects of class @racket[animal%]
are protected as before, but also ensure that subclasses of @racket[animal%]
only store appropriate values within the @racket[size] field and use
the implementation of @racket[size] from @racket[animal%] appropriately.
These contract forms only affect uses within the class hierarchy, and only
for method calls that cross the contract boundary.

That means that @racket[inherit] will only affect subclass uses of a method
until a subclass overrides that method, and that @racket[override] only
affects calls from the superclass into a subclass's overriding implementation
of that method.  Since these only affect internal uses, the @racket[override]
form does not automatically enter subclasses into obligations when objects of
those classes are used.  Also, use of @racket[override] only makes sense, and
thus can only be used, for methods where no Beta-style augmentation has taken
place. The following example shows this difference:

@racketblock[
(define/contract sloppy-eater%
  (class/c [eat (->m edible/c edible/c)])
  (begin
    (define/contract glutton%
      (class/c (override [eat (->m edible/c void?)]))
      (class animal%
        (super-new)
        (inherit eat)
        (define/public (gulp food-list)
          (for ([f food-list])
            (eat f)))))
    (class glutton%
      (super-new)
      (inherit-field size)
      (define/override (eat f)
        (let ([food-size (get-field size f)])
          (set! size (/ food-size 2))
          (set-field! size f (/ food-size 2))
          f)))))]

@interaction-eval[
#:eval class-eval
(define/contract sloppy-eater%
  (class/c [eat (->m edible/c edible/c)])
  (begin
    (define/contract glutton%
      (class/c (override [eat (->m edible/c void?)]))
      (class animal%
        (super-new)
        (inherit eat)
        (define/public (gulp food-list)
          (for ([f food-list])
            (eat f)))))
    (class glutton%
      (super-new)
      (inherit-field size)
      (define/override (eat f)
        (let ([food-size (get-field size f)])
          (set! size (/ food-size 2))
          (set-field! size f (/ food-size 2))
          f)))))]

@interaction[
#:eval class-eval
(define pig (new sloppy-eater%))
(define slop1 (new animal%))
(define slop2 (new animal%))
(define slop3 (new animal%))
(send pig eat slop1)
(get-field size slop1)
(send pig gulp (list slop1 slop2 slop3))]

In addition to the internal class contract forms shown here, there are
similar forms for Beta-style augmentable methods.  The @racket[inner]
form describes to the subclass what is expected from augmentations of
a given method.  Both @racket[augment] and @racket[augride] tell the
subclass that the given method is a method which has been augmented and
that any calls to the method in the subclass will dynamically
dispatch to the appropriate implementation in the superclass.  Such
calls will be checked according to the given contract.  The two forms
differ in that  use of @racket[augment] signifies that subclasses can
augment the given method, whereas use of @racket[augride] signifies that
subclasses must override the current augmentation instead.

This means that not all forms can be used at the same time.  Only one of the
@racket[override], @racket[augment], and @racket[augride] forms can be used
for a given method, and none of these forms can be used if the given method
has been finalized.  In addition, @racket[super] can be specified for a given
method only if @racket[augride] or @racket[override] can be specified.
Similarly, @racket[inner] can be specified only if @racket[augment] or
@racket[augride] can be specified.

@; ----------------------------------------------------------------------

@close-eval[class-eval]
