#reader(lib "docreader.ss" "scribble")
@require[(lib "manual.ss" "scribble")]
@require[(lib "eval.ss" "scribble")]
@require[(lib "class.ss")]
@require["guide-utils.ss"]

@title[#:tag "classes"]{Classes and Objects}

A @scheme[class] expression denotes a first-class value,
just like a @scheme[lambda] expression:

@specform[(class superclass-expr decl-or-expr ...)]

The @scheme[_superclass-expr] determines the superclass for the new
class. Each @scheme[_decl-or-expr] is either a declaration related to
methods, fields, and intialization arguments, or it is an expression
that is evaluated each time that the class is instantiated. In other
words, instead of a method-like constructor, a class has
initialization expressions interleaved with field and method
declarations.

By convention, class names end with @schemeidfont{%}. The built-in root class is
@scheme[object%]. The following expression creates a class with
public methods @scheme[get-size], @scheme[grow], and @scheme[eat]:

@schemeblock[
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

@interaction-eval[
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
    (grow (send other-fish get-size)))))]

The @scheme[size] initialization argument must be supplied via a named
 argument when instantiating the class through the @scheme[new] form:

@schemeblock[
(new (class object% (init size) ...) [size 10])
]

Of course, we can also name the class and its instance:

@schemeblock[
(define fish% (class object% (init size) ...))
(define charlie (new fish% [size 10]))
]

@interaction-eval[(define charlie (new fish% [size 10]))]

In the definition of @scheme[fish%], @scheme[current-size] is a
private field that starts out with the value of the @scheme[size]
initialization argument. Initialization arguments like @scheme[size]
are available only during class instantiation, so they cannot be
referenced directly from a method. The @scheme[current-size] field, in
contrast, is available to methods.

The @scheme[(super-new)] expression in @scheme[fish%] invokes the
initialization of the superclass. In this case, the superclass is
@scheme[object%], which takes no initialization arguments and performs
no work; @scheme[super-new] must be used, anyway, because a class must
always invoke its superclass's initialization.

Initialization arguments, field declarations, and expressions such as
@scheme[(super-new)] can appear in any order within a @scheme[class],
and they can be interleaved with method declarations. The relative
order of expressions in the class determines the order of evaluation
during instantiation. For example, if a field's initial value requires
calling a method that works only after superclass initialization, then
the field declaration is placed after the @scheme[super-new]
call. Ordering field and initialization declarations in this way helps
avoid imperative assignment. The relative order of method declarations
makes no difference for evaluation, because methods are fully defined
before a class is instantiated.

@section[#:tag "guide:methods"]{Methods}

Each of the three @scheme[define/public] declarations in
@scheme[fish%] introduces a new method. The declaration uses the same
syntax as a Scheme function, but a method is not accessible as an
independent function.  A call to the @scheme[grow] method of a
@scheme[fish%] object requires the @scheme[send] form:

@interaction[
(send charlie grow 6)
(send charlie get-size)
]

Within @scheme[fish%], self methods can be called like functions,
because the method names are in scope.  For example, the @scheme[eat]
method within @scheme[fish%] directly invokes the @scheme[grow]
method.  Within a class, attempting to use a method name in any way
other than a method call results in a syntax error.

In some cases, a class must call methods that are supplied by the superclass
but not overridden. In that case, the class can use @scheme[send]
with @scheme[this] to access the method:

@def+int[
(define hungry-fish% (class fish% (super-new)
                       (define/public (eat-more fish1 fish2)
                         (send this eat fish1)
                         (send this eat fish2))))
]

Alternately, the class can declare the existence of a method using @scheme[inherit],
which brings the method name into scope for a direct call:

@def+int[
(define hungry-fish% (class fish% (super-new)
                       (inherit eat)
                       (define/public (eat-more fish1 fish2)
                         (eat fish1) (eat fish2))))
]

With the @scheme[inherit] declaration, if @scheme[fish%] had not
provided an @scheme[eat] method, an error would be signaled in the
evaluation of the @scheme[class] form for @scheme[hungry-fish%]. In
contrast, with @scheme[(send this ...)], an error would not be
signaled until the @scheme[eat-more] method is called and the
@scheme[send] form is evaluated. For this reason, @scheme[inherit] is
preferred.

Another drawback of @scheme[send] is that it is less efficient than
@scheme[inherit]. Invocation of a method via @scheme[send] involves
finding a method in the target object's class at run time, making
@scheme[send] comparable to an interface-based method call in Java. In
contrast, @scheme[inherit]-based method invocations use an offset
within the class's method table that is computed when the class is
created.

To achieve performance similar to @scheme[inherit]-based method calls when
invoking a method from outside the method's class, the programmer must use the
@scheme[generic] form, which produces a class- and method-specific
@defterm{generic method} to be invoked with @scheme[send-generic]:

@def+int[
(define get-fish-size (generic fish% get-size))
(send-generic charlie get-fish-size)
(send-generic (new hungry-fish% [size 32]) get-fish-size)
(send-generic (new object%) get-fish-size)
]

Roughly speaking, the form translates the class and the external
method name to a location in the class's method table. As illustrated
by the last example, sending through a generic method checks that its
argument is an instance of the generic's class.

Whether a method is called directly within a @scheme[class],
through a generic method,
or through @scheme[send], method overriding works in the usual way:

@defs+int[
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

The @scheme[grow] method in @scheme[picky-fish%] is declared with
@scheme[define/override] instead of @scheme[define/public], because
@scheme[grow] is meant as an overriding declaration. If @scheme[grow]
had been declared with @scheme[define/public], an error would have
been signaled when evaluating the @scheme[class] expression, because
@scheme[fish%] already supplies @scheme[grow].

Using @scheme[define/override] also allows the invocation of the
overridden method via a @scheme[super] call. For example, the
@scheme[grow] implementation in @scheme[picky-fish%] uses
@scheme[super] to delegate to the superclass implementation.

@section[#:tag "guide:initargs"]{Initialization Arguments}

Since @scheme[picky-fish%] declares no initialization arguments, any
initialization values supplied in @scheme[(new picky-fish% ...)]  are
propagated to the superclass initialization, i.e., to @scheme[fish%].
A subclass can supply additional initialization arguments for its
superclass in a @scheme[super-new] call, and such initialization
arguments take precedence over arguments supplied to @scheme[new]. For
example, the following @scheme[size-10-fish%] class always generates
fish of size 10:

@def+int[
(define size-10-fish% (class fish% (super-new [size 10])))
(send (new size-10-fish%) get-size)
]

In the case of @scheme[size-10-fish%], supplying a @scheme[size]
initialization argument with @scheme[new] would result in an
initialization error; because the @scheme[size] in @scheme[super-new]
takes precedence, a @scheme[size] supplied to @scheme[new] would have
no target declaration.

An initialization argument is optional if the @scheme[class] form
declares a default value. For example, the following @scheme[default-10-fish%]
class accepts a @scheme[size] initialization argument, but its value defaults to
10 if no value is supplied on instantiation:

@def+int[
(define default-10-fish% (class fish%
                           (init [size 10])
                           (super-new [size size])))
(new default-10-fish%)
(new default-10-fish% [size 20])
]

In this example, the @scheme[super-new] call propagates its own
@scheme[size] value as the @scheme[size] initialization argument to
the superclass.

@section[#:tag "guide:intnames"]{Internal and External Names}

The two uses of @scheme[size] in @scheme[default-10-fish%] expose the
double life of class-member identifiers. When @scheme[size] is the
first identifier of a bracketed pair in @scheme[new] or
@scheme[super-new], @scheme[size] is an @defterm{external name} that
is symbolically matched to an initialization argument in a class. When
@scheme[size] appears as an expression within
@scheme[default-10-fish%], @scheme[size] is an \defterm{internal name}
that is lexically scoped. Similarly, a call to an inherited
@scheme[eat] method uses @scheme[eat] as an internal name, whereas a
@scheme[send] of @scheme[eat] uses @scheme[eat] as an external name.

The full syntax of the @scheme[class] form allows a programmer to
specify distinct internal and external names for a class member. Since
internal names are local, they can be renamed to avoid shadowing or
conflicts. Such renaming is not frequently necessary, but workarounds
in the absence of renaming can be especially cumbersome.

@section{Interfaces}

Interfaces are useful for checking that an object or a class
implements a set of methods with a particular (implied) behavior.
This use of interfaces is helpful even without a static type system
(which is the main reason that Java has interfaces).

An interface in PLT Scheme is created using the @scheme[interface]
form, which merely declares the method names required to implement the
interface. An interface can extend other interfaces, which means that
implementations of the interface automatically implement the extended
interfaces.

@specform[(interface (superinterface-expr ...) id ...)]

To declare that a class implements an interface, the
@scheme[class*] form must be used instead of @scheme[class]:

@specform[(class* superclass-expr (interface-expr ...) decl-or-expr ...)]

For example, instead of forcing all fish classes to be derived from
@scheme[fish%], we can define @scheme[fish-interface] and change the
@scheme[fish%] class to declare that it implements
@scheme[fish-interface]:

@schemeblock[
(define fish-interface (interface () get-size grow eat))
(define fish% (class* object% (fish-interface) ...))
]

If the definition of @scheme[fish%] does not include
@scheme[get-size], @scheme[grow], and @scheme[eat] methods, then an
error is signaled in the evaluation of the @scheme[class*] form,
because implementing the @scheme[fish-interface] interface requires
those methods.

The @scheme[is-a?] predicate accepts either a class or interface as
its first argument and an object as its second argument. When given a
class, @scheme[is-a?] checks whether the object is an instance of that
class or a derived class.  When given an interface, @scheme[is-a?]
checks whether the object's class implements the interface. In
addition, the @scheme[implementation?]  predicate checks whether a
given class implements a given interface.

@section[#:tag "guide:inner"]{Final, Augment, and Inner}

As in Java, a method in a @scheme[class] form can be specified as
@defterm{final}, which means that a subclass cannot override the
method.  A final method is declared using @scheme[public-final] or
@scheme[override-final], depending on whether the declaration is for a
new method or an overriding implementation.

Between the extremes of allowing arbitrary overriding and disallowing
overriding entirely, the {class} system also supports Beta-style
@defterm{augmentable} methods~\cite{beta}. A method
declared with @scheme[pubment] is like @scheme[public], but the method
cannot be overridden in subclasses; it can be augmented only. A
@scheme[pubment] method must explicitly invoke an augmentation (if any)
using @scheme[inner]; a subclass augments the method using
@scheme[augment], instead of @scheme[override].

In general, a method can switch between augment and override modes in
a class derivation. The @scheme[augride] method specification
indicates an augmentation to a method where the augmentation is itself
overrideable in subclasses (though the superclass's implementation
cannot be overridden). Similarly, @scheme[overment] overrides a method
and makes the overriding implementation augmentable. Our earlier
work~\cite{Super+Inner} motivates and explains these extensions and
their interleaving.

@section[#:tag "guide:extnames"]{Controlling the Scope of External Names}

As noted in @secref["guide:intnames"], class members have both
internal and external names. A member definition binds an internal
name locally, and this binding can be locally renamed.  External
names, in contrast, have global scope by default, and a member
definition does not bind an external name. Instead, a member
definition refers to an existing binding for an external name, where
the member name is bound to a @defterm{member key}; a class ultimately
maps member keys to methods, fields, and initialization arguments.

Recall the @scheme[hungry-fish%] @scheme[class] expression:

@schemeblock[
(define hungry-fish% (class fish% ...
                       (inherit eat)
                       (define/public (eat-more fish1 fish2)
                         (eat fish1) (eat fish2))))
]

During its evaluation, the @scheme[hungry-fish%] and @scheme[fish%]
classes refer to the same global binding of @scheme[eat].  At run
time, calls to @scheme[eat] in @scheme[hungry-fish%] are matched with
the @scheme[eat] method in @scheme[fish%] through the shared method
key that is bound to @scheme[eat].

The default binding for an external name is global, but a
programmer can introduce an external-name binding with the
@scheme[define-member-name] form.

@specform[(define-member-name id member-key-expr)]

In particular, by using @scheme[(generate-member-key)] as the
@scheme[member-key-expr], an external name can be localized for a
particular scope, because the generated member key is inaccessible
outside the scope. In other words, @scheme[define-member-name] gives
an external name a kind of package-private scope, but generalized from
packages to arbitrary binding scopes in Scheme.

For example, the following @scheme[fish%] and @scheme[pond%] classes cooperate
via a @scheme[get-depth] method that is only accessible to the
cooperating classes:

@schemeblock[
(define-values (fish% pond%) (code:comment #,(t "two mutually recursive classes"))
  (let () ; create a local definition scope
    (define-member-name get-depth (generate-member-key))
    (define fish%
      (class ... (define my-depth ...)
	         (define my-pond ...)
		 (define/public (dive amt)
		   (set! my-depth
		     (min (+ my-depth amt)
		          (send my-pond get-depth))))))
    (define pond%
      (class ... (define current-depth ...)
	         (define/public (get-depth) current-depth)))
    (values fish% pond%)))
]

External names are in a namespace that separates them from other Scheme
names. This separate namespace is implicitly used for the method name in
@scheme[send], for initialization-argument names in @scheme[new], or for
the external name in a member definition.  The special
@scheme[member-name-key] provides access to the binding of an external name
in an arbitrary expression position: @scheme[(member-name-key id)] form
produces the member-key binding of @scheme[id] in the current scope.

A member-key value is primarily used on with a
@scheme[define-member-name] form. Normally, then,
@scheme[(member-name-key id)] captures the method key of @scheme[id]
so that it can be communicated to a use of @scheme[define-member-name]
in a different scope. This capability turns out to be useful for
generalizing mixins (see \SecRef{sec:parammixins}).
