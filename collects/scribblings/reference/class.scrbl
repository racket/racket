#lang scribble/doc
@(require "mz.ss"
          scheme/class
          (for-syntax scheme/base)
          (for-label scheme/trait))

@(begin

(define-syntax sees
  (syntax-rules ()
    [(_) ""]
    [(_ s) (elem " and " (secref s))]
    [(_ s ... s0) (elem (elem ", " (secref s)) ... ", and " (secref s0))]))

(define-syntax (defclassforms stx)
 (syntax-case stx (*)
  [(_ [* (form ...) (also ...)])
   #'(defform* (form  ...)
       "See " (scheme class*) (sees also ...) "; use"
       " outside the body of a " (scheme class*) " form is a syntax error.")]
  [(_ [form (also ...)])
   #'(defclassforms [* (form) (also ...)])]
  [(_ form ...)
   #'(begin (defclassforms form) ...)]))

(define-syntax (defstarshorthands stx)
  (syntax-case stx ()
    [(_ form)
     (with-syntax ([name (string->symbol
                           (let ([s (symbol->string (syntax-e #'form))])
                             (substring s 0 (sub1 (string-length s)))))]
                   [tmpl (let ([s #'(... (thing (id expr) ...))])
                           (datum->syntax s
                                          (cons (datum->syntax
                                                 #'form
                                                 (syntax-e #'form)
                                                 (car (syntax-e s)))
                                                (cdr (syntax-e s)))
                                          s))])
       #'(...
          (defform tmpl
            "Shorthand for " (scheme (begin (#,(scheme name) id) ... (define id _expr) ...)) ".")))]
     [(_ form ...)
      #'(begin (defstarshorthands form) ...)]))

(define-syntax (defdefshorthands stx)
  (syntax-case stx ()
    [(_ form)
     (with-syntax ([name (string->symbol
                          (let ([s (symbol->string (syntax-e #'form))])
                            (string-append "define/" s)))])
       (with-syntax ([tmpl1 (let ([s #'(define id expr)])
                              (datum->syntax s
                                             (cons (datum->syntax
                                                    #'form
                                                    (syntax-e #'name)
                                                    (car (syntax-e s)))
                                                   (cdr (syntax-e s)))
                                             s))])
         #'(...
            (defform* [tmpl1 (#,(scheme name) (id . formals) body ...+)]
              "Shorthand for "
              (scheme (begin (#,(scheme form) id) (define id expr)))
              " or "
              (scheme (begin (#,(scheme form) id) (define (id . formals) body ...+)))))))]
     [(_ form ...)
      #'(begin (defdefshorthands form) ...)]))

(define class-eval (make-base-eval))

)

@(interaction-eval #:eval class-eval (require scheme/class))


@title[#:tag "mzlib:class" #:style 'toc]{Classes and Objects}

@guideintro["classes"]{classes and objects}

@note-lib[scheme/class #:use-sources (scheme/private/class-internal)]

A @deftech{class} specifies

@itemize[
 
 @item{a collection of fields;}

 @item{a collection of methods;}

 @item{initial value expressions for the fields;  and}

 @item{initialization variables that are bound to initialization
 arguments.}

]

In the context of the class system, an @defterm{object} is a
collection of bindings for fields that are instantiated according to a
class description.

The class system allows a program to define a new class (a
@deftech{derived class}) in terms of an existing class (the
@deftech{superclass}) using inheritance, overriding, and augmenting:

@itemize[

 @item{@deftech{inheritance}: An object of a derived class supports
 methods and instantiates fields declared by the derived class's
 superclass, as well as methods and fields declared in the derived
 class expression.}

 @item{@deftech{overriding}: Some methods declared in a superclass can
 be replaced in the derived class. References to the overridden method
 in the superclass use the implementation in the derived class.}

 @item{@deftech{augmenting}: Some methods declared in a superclass can
 be merely extended in the derived class. The superclass method
 specifically delegates to the augmenting method in the derived class.}

]

An @deftech{interface} is a collection of method names to be
implemented by a class, combined with a derivation requirement. A
class @deftech{implements} an interface when it

@itemize[

 @item{declares (or inherits) a public method for each variable in the
 interface;}

 @item{is derived from the class required by the interface, if any; and}

 @item{specifically declares its intention to implement the interface.}

]

A class can implement any number of interfaces. A derived class
automatically implements any interface that its superclass
implements. Each class also implements an implicitly-defined interface
that is associated with the class. The implicitly-defined interface
contains all of the class's public method names, and it requires that
all other implementations of the interface are derived from the class.

A new interface can @deftech{extend} one or more interfaces with
additional method names; each class that implements the extended
interface also implements the original interfaces. The derivation
requirements of the original interface must be consistent, and the
extended interface inherits the most specific derivation requirement
from the original interfaces.

Classes, objects, and interfaces are all values. However, a class or
interface is not an object (i.e., there are no ``meta-classes'' or
``meta-interfaces'').

@local-table-of-contents[]

@; ------------------------------------------------------------------------

@section[#:tag "createinterface"]{Creating Interfaces}

@guideintro["classes"]{classes, objects, and interfaces}

@defform[(interface (super-interface-expr ...) id ...)]{

Produces an interface. The @scheme[id]s must be mutually distinct.

Each @scheme[super-interface-expr] is evaluated (in order) when the
@scheme[interface] expression is evaluated. The result of each
@scheme[super-interface-expr] must be an interface value, otherwise
the @exnraise[exn:fail:object].  The interfaces returned by the
@scheme[super-interface-expr]s are the new interface's
superinterfaces, which are all extended by the new interface. Any
class that implements the new interface also implements all of the
superinterfaces.

The result of an @scheme[interface] expression is an interface that
includes all of the specified @scheme[id]s, plus all identifiers from
the superinterfaces. Duplicate identifier names among the
superinterfaces are ignored, but if a superinterface contains one of
the @scheme[id]s in the @scheme[interface] expression, the
@exnraise[exn:fail:object].

If no @scheme[super-interface-expr]s are provided, then the derivation
requirement of the resulting interface is trivial: any class that
implements the interface must be derived from @scheme[object%].
Otherwise, the implementation requirement of the resulting interface
is the most specific requirement from its superinterfaces. If the
superinterfaces specify inconsistent derivation requirements, the
@exnraise[exn:fail:object].

@defexamples[
#:eval class-eval
(define file-interface
  (interface () open close read-byte write-byte))
(define directory-interface
  (interface (file-interface) file-list parent-directory))
]}

@defform[(interface* (super-interface-expr ...) 
                     ([property-expr val-expr] ...)
           id ...)]{

Like @scheme[interface], but also associates to the interface the
structure-type properties produced by the @scheme[property-expr]s with
the corresponding @scheme[val-expr]s.

Whenever the resulting interface (or a sub-interface derived from it)
is explicitly implemented by a class through the @scheme[class*] form,
each property is attached with its value to a structure type that
instantiated by instances of the class. Specifically, the property is
attached to a structure type with zero immediate fields, which is
extended to produce the internal structure type for instances of the
class (so that no information about fields is accessible to the
structure type property's guard, if any).

@defexamples[
#:eval class-eval
(define i (interface* () ([prop:custom-write (lambda (obj port write?) (void))])
                      method1 method2 method3))
]}

@; ------------------------------------------------------------------------

@section[#:tag "createclass"]{Creating Classes}

@guideintro["classes"]{classes and objects}

@defthing[object% class?]{

A built-in class that has no methods fields, implements only its own
interface @scheme[(class->interface object%)], and is transparent
(i.e,. its inspector is @scheme[#f], so all immediate instances are
@scheme[equal?]). All other classes are derived from @scheme[object%].}


@defform/subs[
#:literals (inspect init init-field field inherit-field init-rest init-rest
            public pubment public-final override override-final overment augment augride
            augment-final private inherit inherit/super inherit/inner rename-super
            rename-inner begin lambda case-lambda let-values letrec-values
            define-values #%plain-lambda)
(class* superclass-expr (interface-expr ...)
  class-clause
  ...)
([class-clause
  (inspect inspector-expr)
  (init init-decl ...)
  (init-field init-decl ...)
  (field field-decl ...)
  (inherit-field maybe-renamed ...)
  (init-rest id)
  (init-rest)
  (public maybe-renamed ...)
  (pubment maybe-renamed ...)
  (public-final maybe-renamed ...)
  (override maybe-renamed ...)
  (overment maybe-renamed ...)
  (override-final maybe-renamed ...)
  (augment maybe-renamed ...)
  (augride maybe-renamed ...)
  (augment-final maybe-renamed ...)
  (private id ...)
  (inherit maybe-renamed ...)
  (inherit/super maybe-renamed ...)
  (inherit/inner maybe-renamed ...)
  (rename-super renamed ...)
  (rename-inner renamed ...)
  method-definition
  definition
  expr
  (begin class-clause ...)]

[init-decl
  id
  (maybe-renamed)
  (maybe-renamed default-value-expr)]

[field-decl
  (maybe-renamed default-value-expr)]

[maybe-renamed
  id
  renamed]

[renamed
  (internal-id external-id)]

[method-definition
  (define-values (id) method-procedure)]

[method-procedure
  (lambda kw-formals expr ...+)
  (case-lambda (formals expr ...+) ...)
  (#%plain-lambda formals expr ...+)
  (let-values (((id) method-procedure) ...)
    method-procedure)
  (letrec-values (((id) method-procedure) ...)
    method-procedure)
  (let-values (((id) method-procedure) ...+) 
    id)
  (letrec-values (((id) method-procedure) ...+) 
    id)])]{

Produces a class value.

The @scheme[superclass-expr] expression is evaluated when the
@scheme[class*] expression is evaluated. The result must be a class
value (possibly @scheme[object%]), otherwise the
@exnraise[exn:fail:object].  The result of the
@scheme[superclass-expr] expression is the new class's superclass.

The @scheme[interface-expr] expressions are also evaluated when the
@scheme[class*] expression is evaluated, after
@scheme[superclass-expr] is evaluated. The result of each
@scheme[interface-expr] must be an interface value, otherwise the
@exnraise[exn:fail:object].  The interfaces returned by the
@scheme[interface-expr]s are all implemented by the class. For each
identifier in each interface, the class (or one of its ancestors) must
declare a public method with the same name, otherwise the
@exnraise[exn:fail:object]. The class's superclass must satisfy the
implementation requirement of each interface, otherwise the
@exnraise[exn:fail:object].

An @scheme[inspect] @scheme[class-clause] selects an inspector (see
@secref["inspectors"]) for the class extension. The
@scheme[inspector-expr] must evaluate to an inspector or @scheme[#f]
when the @scheme[class*] form is evaluated. Just as for structure
types, an inspector controls access to the class's fields, including
private fields, and also affects comparisons using @scheme[equal?]. If
no @scheme[inspect] clause is provided, access to the class is
controlled by the parent of the current inspector (see
@secref["inspectors"]). A syntax error is reported if more than one
@scheme[inspect] clause is specified.

The other @scheme[class-clause]s define initialization arguments,
public and private fields, and public and private methods. For each
@scheme[id] or @scheme[maybe-renamed] in a @scheme[public],
@scheme[override], @scheme[augment], @scheme[pubment],
@scheme[overment], @scheme[augride], @scheme[public-final],
@scheme[override-final], @scheme[augment-final], or @scheme[private]
clause, there must be one @scheme[method-definition]. All other
definition @scheme[class-clause]s create private fields. All remaining
@scheme[expr]s are initialization expressions to be evaluated when the
class is instantiated (see @secref["objcreation"]).

The result of a @scheme[class*] expression is a new class, derived
from the specified superclass and implementing the specified
interfaces. Instances of the class are created with the
@scheme[instantiate] form or @scheme[make-object] procedure, as
described in @secref["objcreation"].

Each @scheme[class-clause] is (partially) macro-expanded to reveal its
shapes. If a @scheme[class-clause] is a @scheme[begin] expression, its
sub-expressions are lifted out of the @scheme[begin] and treated as
@scheme[class-clause]s, in the same way that @scheme[begin] is
flattened for top-level and embedded definitions.

Within a @scheme[class*] form for instances of the new class,
@scheme[this] is bound to the object itself;
@scheme[this%] is bound to the class of the object;
@scheme[super-instantiate], @scheme[super-make-object], and
@scheme[super-new] are bound to forms to initialize fields in the
superclass (see @secref["objcreation"]); @scheme[super] is
available for calling superclass methods (see
@secref["clmethoddefs"]); and @scheme[inner] is available for
calling subclass augmentations of methods (see
@secref["clmethoddefs"]).}

@defform[(class superclass-expr class-clause ...)]{

Like @scheme[class*], but omits the @scheme[interface-expr]s, for the case that none are needed.

@defexamples[
#:eval class-eval
(define book-class
  (class object%
    (field (pages 5))
    (define/public (letters)
      (* pages 500))
    (super-new)))
]}

@defidform[this]{

@index['("self")]{Within} a @scheme[class*] form, @scheme[this] refers
to the current object (i.e., the object being initialized or whose
method was called). Use outside the body of a @scheme[class*] form is
a syntax error.

@defexamples[
#:eval class-eval
(define (describe obj)
  (printf "Hello ~a\n" obj))
(define table
  (class object%
    (define/public (describe-self)
      (describe this))
    (super-new)))
(send (new table) describe-self)
]}

@defidform[this%]{
                  
Within a @scheme[class*] form, @scheme[this%] refers to the class
of the current object (i.e., the object being initialized or whose
method was called).  Use outside the body of a @scheme[class*] form is
a syntax error.

@defexamples[
#:eval class-eval
(define account%
  (class object% 
    (super-new)
    (init-field balance)
    (define/public (add n)
      (new this% [balance (+ n balance)]))))
(define savings%
  (class account%
    (super-new)
    (inherit-field balance)
    (define interest 0.04)
    (define/public (add-interest)
      (send this add (* interest balance)))))
(let* ([acct (new savings% [balance 500])]
       [acct (send acct add 500)]
       [acct (send acct add-interest)])
  (printf "Current balance: ~a\n" (get-field balance acct)))
]}

@defclassforms[
  [(inspect inspector-expr) ()]
  [(init init-decl ...) ("clinitvars")]
  [(init-field init-decl ...) ("clinitvars" "clfields")]
  [(field field-decl ...) ("clfields")]
  [(inherit-field maybe-renamed ...) ("clfields")]
  [* ((init-rest id) (init-rest)) ("clinitvars")]
  [(public maybe-renamed ...) ("clmethoddefs")]
  [(pubment maybe-renamed ...) ("clmethoddefs")]
  [(public-final maybe-renamed ...) ("clmethoddefs")]
  [(override maybe-renamed ...) ("clmethoddefs")]
  [(overment maybe-renamed ...) ("clmethoddefs")]
  [(override-final maybe-renamed ...) ("clmethoddefs")]
  [(augment maybe-renamed ...) ("clmethoddefs")]
  [(augride maybe-renamed ...) ("clmethoddefs")]
  [(augment-final maybe-renamed ...) ("clmethoddefs")]
  [(private id ...) ("clmethoddefs")]
  [(inherit maybe-renamed ...) ("classinherit")]
  [(inherit/super maybe-renamed ...)  ("classinherit")]
  [(inherit/inner maybe-renamed ...) ("classinherit")]
  [(rename-super renamed ...) ("classinherit")]
  [(rename-inner renamed ...) ("classinherit")]
]

@defstarshorthands[
 public* 
 pubment*
 public-final*
 override*
 overment*
 override-final*
 augment*
 augride*
 augment-final*
 private*
]

@defdefshorthands[
 public pubment public-final override
 overment override-final augment augride
 augment-final private
]


@defform[
(class/derived original-datum
  (name-id super-expr (interface-expr ...) deserialize-id-expr)
  class-clause
  ...)
]{

Like @scheme[class*], but includes a sub-expression to use used as the
source for all syntax errors within the class definition. For example,
@scheme[define-serializable-class] expands to @scheme[class/derived]
so that error in the body of the class are reported in terms of
@scheme[define-serializable-class] instead of @scheme[class].

The @scheme[original-datum] is the original expression to use for
reporting errors.

The @scheme[name-id] is used to name the resulting class; if it
is @scheme[#f], the class name is inferred.

The @scheme[super-expr], @scheme[interface-expr]s, and
@scheme[class-clause]s are as for @scheme[class*].

If the @scheme[deserialize-id-expr] is not literally @scheme[#f], then
a serializable class is generated, and the result is two values
instead of one: the class and a deserialize-info structure produced by
@scheme[make-deserialize-info]. The @scheme[deserialize-id-expr]
should produce a value suitable as the second argument to
@scheme[make-serialize-info], and it should refer to an export whose
value is the deserialize-info structure.

Future optional forms may be added to the sequence that currently ends
with @scheme[deserialize-id-expr].}

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

@subsection[#:tag "clinitvars"]{Initialization Variables}

A class's initialization variables, declared with @scheme[init],
@scheme[init-field], and @scheme[init-rest], are instantiated
for each object of a class. Initialization variables can be used in
the initial value expressions of fields, default value expressions
for initialization arguments, and in initialization expressions.  Only
initialization variables declared with @scheme[init-field] can be
accessed from methods; accessing any other initialization variable
from a method is a syntax error.

The values bound to initialization variables are

@itemize[

 @item{the arguments provided with @scheme[instantiate] or passed to
 @scheme[make-object], if the object is created as a direct instance
 of the class; or,}

 @item{the arguments passed to the superclass initialization form or
 procedure, if the object is created as an instance of a derived
 class.}

]

If an initialization argument is not provided for an initialization
variable that has an associated @scheme[_default-value-expr], then the
@scheme[_default-value-expr] expression is evaluated to obtain a value
for the variable. A @scheme[_default-value-expr] is only evaluated when
an argument is not provided for its variable. The environment of
@scheme[_default-value-expr] includes all of the initialization
variables, all of the fields, and all of the methods of the class. If
multiple @scheme[_default-value-expr]s are evaluated, they are
evaluated from left to right. Object creation and field initialization
are described in detail in @secref["objcreation"].

If an initialization variable has no @scheme[_default-value-expr], then
the object creation or superclass initialization call must supply an
argument for the variable, otherwise the @exnraise[exn:fail:object].

Initialization arguments can be provided by name or by position.  The
external name of an initialization variable can be used with
@scheme[instantiate] or with the superclass initialization form. Those
forms also accept by-position arguments. The @scheme[make-object]
procedure and the superclass initialization procedure accept only
by-position arguments.

Arguments provided by position are converted into by-name arguments
using the order of @scheme[init] and @scheme[init-field] clauses and
the order of variables within each clause. When a @scheme[instantiate]
form provides both by-position and by-name arguments, the converted
arguments are placed before by-name arguments. (The order can be
significant; see also @secref["objcreation"].)

Unless a class contains an @scheme[init-rest] clause, when the number
of by-position arguments exceeds the number of declared initialization
variables, the order of variables in the superclass (and so on, up the
superclass chain) determines the by-name conversion.

If a class expression contains an @scheme[init-rest] clause, there
must be only one, and it must be last. If it declares a variable, then
the variable receives extra by-position initialization arguments as a
list (similar to a dotted ``rest argument'' in a procedure).  An
@scheme[init-rest] variable can receive by-position initialization
arguments that are left over from a by-name conversion for a derived
class. When a derived class's superclass initialization provides even
more by-position arguments, they are prefixed onto the by-position
arguments accumulated so far.

If too few or too many by-position initialization arguments are
provided to an object creation or superclass initialization, then the
@exnraise[exn:fail:object]. Similarly, if extra by-position arguments
are provided to a class with an @scheme[init-rest] clause, the
@exnraise[exn:fail:object].

Unused (by-name) arguments are to be propagated to the superclass, as
described in @secref["objcreation"].  Multiple initialization
arguments can use the same name if the class derivation contains
multiple declarations (in different classes) of initialization
variables with the name. See @secref["objcreation"] for further
details.

See also @secref["extnames"] for information about internal and
external names.

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

@subsection[#:tag "clfields"]{Fields}

Each @scheme[field], @scheme[init-field], and non-method
@scheme[define-values] clause in a class declares one or more new
fields for the class. Fields declared with @scheme[field] or
@scheme[init-field] are public. Public fields can be accessed and
mutated by subclasses using @scheme[inherit-field]. Public fields are
also accessible outside the class via @scheme[class-field-accessor]
and mutable via @scheme[class-field-mutator] (see
@secref["ivaraccess"]). Fields declared with @scheme[define-values]
are accessible only within the class.

A field declared with @scheme[init-field] is both a public field and
an initialization variable. See @secref["clinitvars"] for
information about initialization variables.

An @scheme[inherit-field] declaration makes a public field defined by
a superclass directly accessible in the class expression. If the
indicated field is not defined in the superclass, the
@exnraise[exn:fail:object] when the class expression is evaluated.
Every field in a superclass is present in a derived class, even if it
is not declared with @scheme[inherit-field] in the derived class. The
@scheme[inherit-field] clause does not control inheritance, but merely
controls lexical scope within a class expression.

When an object is first created, all of its fields have the
@|undefined-const| value (see @secref["void"]). The fields of a
class are initialized at the same time that the class's initialization
expressions are evaluated; see @secref["objcreation"] for more
information.

See also @secref["extnames"] for information about internal and
external names.

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

@subsection[#:tag "clmethods"]{Methods}

@subsubsection[#:tag "clmethoddefs"]{Method Definitions}

Each @scheme[public], @scheme[override], @scheme[augment],
@scheme[pubment], @scheme[overment], @scheme[augride],
@scheme[public-final], @scheme[override-final],
@scheme[augment-final], and @scheme[private] clause in a class
declares one or more method names. Each method name must have a
corresponding @scheme[method-definition]. The order of
@scheme[public], @|etc| clauses and their corresponding definitions
(among themselves, and with respect to other clauses in the class)
does not matter.

As shown in the grammar for @scheme[class*], a method definition is
syntactically restricted to certain procedure forms, as defined by the
grammar for @scheme[method-procedure]; in the last two forms of
@scheme[method-procedure], the body @scheme[id] must be one of the
@scheme[id]s bound by @scheme[let-values] or @scheme[letrec-values]. A
@scheme[method-procedure] expression is not evaluated
directly. Instead, for each method, a class-specific method procedure
is created; it takes an initial object argument, in addition to the
arguments the procedure would accept if the @scheme[method-procedure]
expression were evaluated directly. The body of the procedure is
transformed to access methods and fields through the object argument.

A method declared with @scheme[public], @scheme[pubment], or
@scheme[public-final] introduces a new method into a class. The method
must not be present already in the superclass, otherwise the
@exnraise[exn:fail:object] when the class expression is evaluated. A
method declared with @scheme[public] can be overridden in a subclass
that uses @scheme[override], @scheme[overment], or
@scheme[override-final].  A method declared with @scheme[pubment] can
be augmented in a subclass that uses @scheme[augment],
@scheme[augride], or @scheme[augment-final]. A method declared with
@scheme[public-final] cannot be overridden or augmented in a subclass.

A method declared with @scheme[override], @scheme[overment], or
@scheme[override-final] overrides a definition already present in the
superclass. If the method is not already present, the
@exnraise[exn:fail:object] when the class expression is evaluated.  A
method declared with @scheme[override] can be overridden again in a
subclass that uses @scheme[override], @scheme[overment], or
@scheme[override-final].  A method declared with @scheme[overment] can
be augmented in a subclass that uses @scheme[augment],
@scheme[augride], or @scheme[augment-final]. A method declared with
@scheme[override-final] cannot be overridden further or augmented in a
subclass.

A method declared with @scheme[augment], @scheme[augride], or
@scheme[augment-final] augments a definition already present in the
superclass. If the method is not already present, the
@exnraise[exn:fail:object] when the class expression is evaluated.  A
method declared with @scheme[augment] can be augmented further in a
subclass that uses @scheme[augment], @scheme[augride], or
@scheme[augment-final]. A method declared with @scheme[augride] can be
overridden in a subclass that uses @scheme[override],
@scheme[overment], or @scheme[override-final]. (Such an override
merely replaces the augmentation, not the method that is augmented.)
A method declared with @scheme[augment-final] cannot be overridden or
augmented further in a subclass.

A method declared with @scheme[private] is not accessible outside the
class expression, cannot be overridden, and never overrides a method
in the superclass.

When a method is declared with @scheme[override], @scheme[overment],
or @scheme[override-final], then the superclass implementation of the
method can be called using @scheme[super] form.

When a method is declared with @scheme[pubment], @scheme[augment], or
@scheme[overment], then a subclass augmenting method can be called
using the @scheme[inner] form. The only difference between
@scheme[public-final] and @scheme[pubment] without a corresponding
@scheme[inner] is that @scheme[public-final] prevents the declaration
of augmenting methods that would be ignored.

@defform*[[(super id arg ...)
           (super id arg ... . arg-list-expr)]]{

Always accesses the superclass method, independent of whether the
method is overridden again in subclasses. Using the @scheme[super]
form outside of @scheme[class*] is an syntax error. Each @scheme[arg]
is as for @scheme[#%app]: either @scheme[_arg-expr] or
@scheme[_keyword _arg-expr].

The second form is analogous to using @scheme[apply] with a procedure;
the @scheme[arg-list-expr] must not be a parenthesized expression.}

@defform*[[(inner default-expr id arg ...)
           (inner default-expr id arg ... . arg-list-expr)]]{

If the object's class does not supply an augmenting method, then
@scheme[default-expr] is evaluated, and the @scheme[arg] expressions
are not evaluated. Otherwise, the augmenting method is called with the
@scheme[arg] results as arguments, and @scheme[default-expr] is not
evaluated. If no @scheme[inner] call is evaluated for a particular
method, then augmenting methods supplied by subclasses are never
used. Using the @scheme[inner] form outside of @scheme[class*] is an
syntax error.

The second form is analogous to using @scheme[apply] with a procedure;
the @scheme[arg-list-expr] must not be a parenthesized expression.}

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

@subsubsection[#:tag "classinherit"]{Inherited and Superclass Methods}

Each @scheme[inherit], @scheme[inherit/super], @scheme[inherit/inner],
@scheme[rename-super], and @scheme[rename-inner] clause declares one
or more methods that are defined in the class, but must be present in
the superclass. The @scheme[rename-super] and @scheme[rename-inner]
declarations are rarely used, since @scheme[inherit/super] and
@scheme[inherit/inner] provide the same access. Also, superclass and
augmenting methods are typically accessed through @scheme[super] and
@scheme[inner] in a class that also declares the methods, instead of
through @scheme[inherit/super], @scheme[inherit/inner],
@scheme[rename-super], or @scheme[rename-inner].

Method names declared with @scheme[inherit], @scheme[inherit/super],
or @scheme[inherit/inner] access overriding declarations, if any, at
run time. Method names declared with @scheme[inherit/super] can also
be used with the @scheme[super] form to access the superclass
implementation, and method names declared with @scheme[inherit/inner]
can also be used with the @scheme[inner] form to access an augmenting
method, if any.
 
Method names declared with @scheme[rename-super] always access the
superclass's implementation at run-time. Methods declared with
@scheme[rename-inner] access a subclass's augmenting method, if any,
and must be called with the form

@schemeblock[
(_id (lambda () _default-expr) _arg ...)
]

so that a @scheme[default-expr] is available to evaluate when no
augmenting method is available. In such a form, @scheme[lambda] is a
literal identifier to separate the @scheme[default-expr] from the
@scheme[arg]. When an augmenting method is available, it receives the
results of the @scheme[arg] expressions as arguments.

Methods that are present in the superclass but not declared with
@scheme[inherit], @scheme[inherit/super], or @scheme[inherit/inner] or
@scheme[rename-super] are not directly accessible in the class
(through they can be called with @scheme[send]).  Every public method
in a superclass is present in a derived class, even if it is not
declared with @scheme[inherit] in the derived class; the
@scheme[inherit] clause does not control inheritance, but merely
controls lexical scope within a class expression.

If a method declared with @scheme[inherit], @scheme[inherit/super],
@scheme[inherit/inner], @scheme[rename-super], or
@scheme[rename-inner] is not present in the superclass, the
@exnraise[exn:fail:object] when the class expression is evaluated.

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

@subsubsection[#:tag "extnames"]{Internal and External Names}

Each method declared with @scheme[public], @scheme[override],
@scheme[augment], @scheme[pubment], @scheme[overment],
@scheme[augride], @scheme[public-final], @scheme[override-final],
@scheme[augment-final], @scheme[inherit], @scheme[inherit/super],
@scheme[inherit/inner], @scheme[rename-super], and
@scheme[rename-inner] can have separate internal and external names
when @scheme[(internal-id external-id)] is used for declaring the
method. The internal name is used to access the method directly within
the class expression (including within @scheme[super] or
@scheme[inner] forms), while the external name is used with
@scheme[send] and @scheme[generic] (see @secref["ivaraccess"]).  If
a single @scheme[id] is provided for a method declaration, the
identifier is used for both the internal and external names.

Method inheritance, overriding, and augmentation are based external
names, only.  Separate internal and external names are required for
@scheme[rename-super] and @scheme[rename-inner] (for historical
reasons, mainly).

Each @scheme[init], @scheme[init-field], @scheme[field], or
@scheme[inherit-field] variable similarly has an internal and an
external name. The internal name is used within the class to access
the variable, while the external name is used outside the class when
providing initialization arguments (e.g., to @scheme[instantiate]),
inheriting a field, or accessing a field externally (e.g., with
@scheme[class-field-accessor]). As for methods, when inheriting a
field with @scheme[inherit-field], the external name is matched to an
external field name in the superclass, while the internal name is
bound in the @scheme[class] expression.

A single identifier can be used as an internal identifier and an
external identifier, and it is possible to use the same identifier as
internal and external identifiers for different bindings. Furthermore,
within a single class, a single name can be used as an external method
name, an external field name, and an external initialization argument
name. Overall, each internal identifier must be distinct from all
other internal identifiers, each external method name must be distinct
from all other method names, each external field name must be distinct
from all other field names, and each initialization argument name must
be distinct from all other initialization argument names

By default, external names have no lexical scope, which means, for
example, that an external method name matches the same syntactic
symbol in all uses of @scheme[send]. The
@scheme[define-local-member-name] and @scheme[define-member-name] forms
introduce scoped external names.

When a @scheme[class] expression is compiled, identifiers used in
place of external names must be symbolically distinct (when the
corresponding external names are required to be distinct), otherwise a
syntax error is reported. When no external name is bound by
@scheme[define-member-name], then the actual external names are
guaranteed to be distinct when @scheme[class] expression is evaluated.
When any external name is bound by @scheme[define-member-name], the
@exnraise[exn:fail:object] by @scheme[class] if the actual external
names are not distinct.


@defform[(define-local-member-name id ...)]{

Unless it appears as the top-level definition, binds each @scheme[id]
so that, within the scope of the definition, each use of each
@scheme[id] as an external name is resolved to a hidden name generated
by the @scheme[define-local-member-name] declaration. Thus, methods,
fields, and initialization arguments declared with such external-name
@scheme[id]s are accessible only in the scope of the
@scheme[define-local-member-name] declaration.  As a top-level
definition, @scheme[define-local-member-name] binds @scheme[id] to its
symbolic form.

The binding introduced by @scheme[define-local-member-name] is a
syntax binding that can be exported and imported with
@scheme[module]s. Each execution of a
@scheme[define-local-member-name] declaration generates a distinct
hidden name (except as a top-level definitions). The
@scheme[interface->method-names] procedure does not expose hidden
names.

@defexamples[
#:eval class-eval
(define-values (r o)
  (let ()
    (define-local-member-name m)
    (define c% (class object%
                 (define/public (m) 10)
                 (super-new)))
    (define o (new c%))
    
    (values (send o m)
            o)))

r
(send o m)
]}


@defform[(define-member-name id key-expr)]{

Maps a single external name to an external name that is determined by
an expression. The value of @scheme[key-expr] must be the result of either a
@scheme[member-name-key] expression or a @scheme[generate-member-key] call.}


@defform[(member-name-key identifier)]{

Produces a representation of the external name for @scheme[id] in the
environment of the @scheme[member-name-key] expression.}

@defproc[(generate-member-key) member-name-key?]{

Produces a hidden name, just like the binding for
@scheme[define-local-member-name].}

@defproc[(member-name-key? [v any/c]) boolean?]{

Returns @scheme[#t] for values produced by @scheme[member-name-key]
and @scheme[generate-member-key], @scheme[#f]
otherwise.}

@defproc[(member-name-key=? [a-key member-name-key?][b-key member-name-key?]) boolean?]{

Produces @scheme[#t] if member-name keys @scheme[a-key] and
@scheme[b-key] represent the same external name, @scheme[#f]
otherwise.}


@defproc[(member-name-key-hash-code [a-key member-name-key?]) integer?]{

Produces an integer hash code consistent with
@scheme[member-name-key=?]  comparisons, analogous to
@scheme[equal-hash-code].}

@defexamples[
#:eval class-eval
(define (make-c% key)
  (define-member-name m key)
  (class object% 
    (define/public (m) 10)
    (super-new)))

(send (new (make-c% (member-name-key m))) m)
(send (new (make-c% (member-name-key p))) m)
(send (new (make-c% (member-name-key p))) p)
]

@defs+int[
#:eval class-eval
[(define (fresh-c%)
   (let ([key (generate-member-key)])
     (values (make-c% key) key)))

 (define-values (fc% key) (fresh-c%))]

(send (new fc%) m)
(let ()
  (define-member-name p key)
  (send (new fc%) p))
]


@; ------------------------------------------------------------------------

@section[#:tag "objcreation"]{Creating Objects}

The @scheme[make-object] procedure creates a new object with
by-position initialization arguments, the @scheme[new] form
creates a new object with by-name initialization arguments, and
the @scheme[instantiate] form creates a new object with both
by-position and by-name initialization arguments.


All fields in the newly created object are initially bound to the
special @|undefined-const| value (see
@secref["void"]). Initialization variables with default value
expressions (and no provided value) are also initialized to
@|undefined-const|. After argument values are assigned to
initialization variables, expressions in @scheme[field] clauses,
@scheme[init-field] clauses with no provided argument,
@scheme[init] clauses with no provided argument, private field
definitions, and other expressions are evaluated. Those
expressions are evaluated as they appear in the class expression,
from left to right.

Sometime during the evaluation of the expressions,
superclass-declared initializations must be executed once by
using the @scheme[super-make-object] procedure,
@scheme[super-new] form, or @scheme[super-instantiate] form.

By-name initialization arguments to a class that have no matching
initialization variable are implicitly added as by-name arguments
to a @scheme[super-make-object], @scheme[super-new], or
@scheme[super-instantiate] invocation, after the explicit
arguments.  If multiple initialization arguments are provided for
the same name, the first (if any) is used, and the unused
arguments are propagated to the superclass. (Note that converted
by-position arguments are always placed before explicit by-name
arguments.)  The initialization procedure for the
@scheme[object%] class accepts zero initialization arguments; if
it receives any by-name initialization arguments, then
@exnraise[exn:fail:object].

If the end of initialization is reached for any class in the
hierarchy without invoking the superclass's initialization, the
@exnraise[exn:fail:object]. Also, if superclass initialization is
invoked more than once, the @exnraise[exn:fail:object].

Fields inherited from a superclass are not initialized until the
superclass's initialization procedure is invoked. In contrast,
all methods are available for an object as soon as the object is
created; the overriding of methods is not affect by
initialization (unlike objects in C++).



@defproc[(make-object [class class?][init-v any/c] ...) object?]{

Creates an instance of @scheme[class]. The @scheme[init-v]s are
passed as initialization arguments, bound to the initialization
variables of @scheme[class] for the newly created object as
described in @secref["clinitvars"]. If @scheme[class] is not a
class, the @exnraise[exn:fail:contract].}

@defform[(new class-expr (id by-name-expr) ...)]{

Creates an instance of the value of @scheme[class-expr] (which
must be a class), and the value of each @scheme[by-name-expr] is
provided as a by-name argument for the corresponding
@scheme[id].}

@defform[(instantiate class-expr (by-pos-expr ...) (id by-name-expr) ...)]{

Creates an instance of the value of @scheme[class-expr] (which
must be a class), and the values of the @scheme[by-pos-expr]s are
provided as by-position initialization arguments. In addition,
the value of each @scheme[by-name-expr] is provided as a by-name
argument for the corresponding @scheme[id].}

@defidform[super-make-object]{

Produces a procedure that takes by-position arguments an invokes
superclass initialization. See @secref["objcreation"] for more
information.}


@defform[(super-instantiate (by-pos-expr ...) (id by-expr ...) ...)]{


Invokes superclass initialization with the specified by-position and
by-name arguments. See @secref["objcreation"] for more
information.}


@defform[(super-new (id by-name-expr ...) ...)]{

Invokes superclass initialization with the specified by-name
arguments. See @secref["objcreation"] for more information.}

@; ------------------------------------------------------------------------

@section[#:tag "ivaraccess"]{Field and Method Access}

In expressions within a class definition, the initialization
variables, fields, and methods of the class all part of the
environment. Within a method body, only the fields and other methods
of the class can be referenced; a reference to any other
class-introduced identifier is a syntax error.  Elsewhere within the
class, all class-introduced identifiers are available, and fields and
initialization variables can be mutated with @scheme[set!].

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

@subsection[#:tag "methodcalls"]{Methods}

Method names within a class can only be used in the procedure position
of an application expression; any other use is a syntax error.

To allow methods to be applied to lists of arguments, a method
application can have the following form:

@specsubform[
(method-id arg ... . arg-list-expr)
]

This form calls the method in a way analogous to @scheme[(apply
_method-id _arg ... _arg-list-expr)]. The @scheme[arg-list-expr]
must not be a parenthesized expression.

Methods are called from outside a class with the @scheme[send] and 
@scheme[send/apply] forms.

@defform*[[(send obj-expr method-id arg ...)
           (send obj-expr method-id arg ... . arg-list-expr)]]{

Evaluates @scheme[obj-expr] to obtain an object, and calls the method
with (external) name @scheme[method-id] on the object, providing the
@scheme[arg] results as arguments. Each @scheme[arg] is as for
@scheme[#%app]: either @scheme[_arg-expr] or @scheme[_keyword
_arg-expr]. In the second form, @scheme[arg-list-expr] cannot be a
parenthesized expression.

If @scheme[obj-expr] does not produce an object, the
@exnraise[exn:fail:contract]. If the object has no public method named
@scheme[method-id], the @exnraise[exn:fail:object].}

@defform[(send/apply obj-expr method-id arg ... arg-list-expr)]{

Like the dotted form of @scheme[send], but @scheme[arg-list-expr] can
be any expression.}


@defform/subs[(send* obj-expr msg ...)
              ([msg (method-id arg ...)
                    (method-id arg ... . arg-list-expr)])]{

Calls multiple methods (in order) of the same object. Each
@scheme[msg] corresponds to a use of @scheme[send].

For example,

@schemeblock[
(send* edit (begin-edit-sequence)
            (insert "Hello")
            (insert #\newline)
            (end-edit-sequence))
]

is the same as

@schemeblock[
(let ([o edit])
  (send o begin-edit-sequence)
  (send o insert "Hello")
  (send o insert #\newline)
  (send o end-edit-sequence))
]}

@defform[(with-method ((id (obj-expr method-id)) ...)
           body ...+)]{

Extracts methods from an object and binds a local name that can be
applied directly (in the same way as declared methods within a class)
for each method. The each @scheme[obj-expr] must produce an object,
which must have a public method named by the corresponding
@scheme[method-id]. The corresponding @scheme[id] is bound so that it
can be applied directly (see @secref["methodcalls"]).

Example:

@schemeblock[
(let ([s (new stack%)])
  (with-method ([push (s push!)]
                [pop (s pop!)])
    (push 10)
    (push 9)
    (pop)))
]

is the same as

@schemeblock[
(let ([s (new stack%)])
  (send s push! 10)
  (send s push! 9)
  (send s pop!))
]}

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

@subsection{Fields}

@defform[(get-field id obj-expr)]{

Extracts the field with (external) name @scheme[id] from the value of
@scheme[obj-expr].

If @scheme[obj-expr] does not produce an object, the
@exnraise[exn:fail:contract]. If the object has no @scheme[id] method,
the @exnraise[exn:fail:object].}

@defform[(field-bound? id obj-expr)]{

Produces @scheme[#t] if the object result of @scheme[obj-expr] has an
field with (external) name @scheme[id], @scheme[#f] otherwise.

If @scheme[obj-expr] does not produce an object, the
@exnraise[exn:fail:contract].}

@defform[(class-field-accessor class-expr field-id)]{

Returns an accessor procedure that takes an instance of the class
produced by @scheme[class-expr] and returns the value of the object's
field with (external) name @scheme[field-id].

If @scheme[class-expr] does not produce a class, the
@exnraise[exn:fail:contract]. If the class has no @scheme[field-id]
field, the @exnraise[exn:fail:object].}

@defform[(class-field-mutator class-expr field-id)]{

Returns a mutator procedure that takes an instance of the class
produced by @scheme[class-expr] and a value, and sets the value of the
object's field with (external) name @scheme[field-id] to the given
value. The result is @|void-const|.

If @scheme[class-expr] does not produce a class, the
@exnraise[exn:fail:contract]. If the class has no @scheme[field-id]
field, the @exnraise[exn:fail:object].}

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

@subsection{Generics}

A @deftech{generic} can be used instead of a method name to avoid the
cost of relocating a method by name within a class.

@defform[(generic class-or-interface-expr id)]{

Produces a generic that works on instances of the class or interface
produced by @scheme[class-or-interface-expr] (or an instance of a
class/interface derived from @scheme[class-or-interface]) to call the
method with (external) name @scheme[id].

If @scheme[class-or-interface-expr] does not produce a class or
interface, the @exnraise[exn:fail:contract]. If the resulting class or
interface does not contain a method named @scheme[id], the
@exnraise[exn:fail:object].}

@defform*[[(send-generic obj-expr generic-expr arg ...)
           (send-generic obj-expr generic-expr arg ... . arg-list-expr)]]{

Calls a method of the object produced by @scheme[obj-expr] as
indicated by the generic produced by @scheme[generic-expr]. Each
@scheme[arg] is as for @scheme[#%app]: either @scheme[_arg-expr] or
@scheme[_keyword _arg-expr]. The second form is analogous to calling a
procedure with @scheme[apply], where @scheme[arg-list-expr] is not a
parenthesized expression.

If @scheme[obj-expr] does not produce a object, or if
@scheme[generic-expr] does not produce a generic, the
@exnraise[exn:fail:contract]. If the result of @scheme[obj-expr] is
not an instance of the class or interface encapsulated by the result
of @scheme[generic-expr], the @exnraise[exn:fail:object].}

@defproc[(make-generic [type (or/c class? interface?)]
                       [method-name symbol?])
         generic?]{

Like the @scheme[generic] form, but as a procedure that accepts a
symbolic method name.}

@; ------------------------------------------------------------------------

@section[#:tag "mixins"]{Mixins}

@defform[(mixin (interface-expr ...) (interface-expr ...)
           class-clause ...)]{

Produces a @deftech{mixin}, which is a procedure that encapsulates a
class extension, leaving the superclass unspecified.  Each time that a
mixin is applied to a specific superclass, it produces a new derived
class using the encapsulated extension.

The given class must implement interfaces produced by the first set of
@scheme[interface-expr]s.  The result of the procedure is a subclass
of the given class that implements the interfaces produced by the
second set of @scheme[interface-expr]s. The @scheme[class-clause]s are
as for @scheme[class*], to define the class extension encapsulated by
the mixin.

Evaluation of a @scheme[mixin] form checks that the
@scheme[class-clause]s are consistent with both sets of
@scheme[interface-expr]s.}

@; ------------------------------------------------------------------------

@section[#:tag "trait"]{Traits}

@note-lib-only[scheme/trait]

A @deftech{trait} is a collection of methods that can be converted to
a @tech{mixin} and then applied to a @tech{class}. Before a trait is
converted to a mixin, the methods of a trait can be individually
renamed, and multiple traits can be merged to form a new trait.

@defform/subs[#:literals (public pubment public-final override override-final overment augment augride
                          augment-final private inherit inherit/super inherit/inner rename-super
                          inherit-field)

              (trait trait-clause ...)
              ([trait-clause (public maybe-renamed ...)
                             (pubment maybe-renamed ...)
                             (public-final maybe-renamed ...)
                             (override maybe-renamed ...)
                             (overment maybe-renamed ...)
                             (override-final maybe-renamed ...)
                             (augment maybe-renamed ...)
                             (augride maybe-renamed ...)
                             (augment-final maybe-renamed ...)
                             (inherit maybe-renamed ...)
                             (inherit/super maybe-renamed ...)
                             (inherit/inner maybe-renamed ...)
                             method-definition
                             (field field-declaration ...)
                             (inherit-field maybe-renamed ...)])]{

Creates a @tech{trait}.  The body of a @scheme[trait] form is similar to the
body of a @scheme[class*] form, but restricted to non-private method
definitions.  In particular, the grammar of
@scheme[maybe-renamed], @scheme[method-definition], and
@scheme[field-declaration] are the same as for @scheme[class*], and
every @scheme[method-definition] must have a corresponding declaration
(one of @scheme[public], @scheme[override], etc.).  As in
@scheme[class], uses of method names in direct calls, @scheme[super]
calls, and @scheme[inner] calls depend on bringing method names into
scope via @scheme[inherit], @scheme[inherit/super],
@scheme[inherit/inner], and other method declarations in the same
trait; an exception, compared to @scheme[class] is that
@scheme[overment] binds a method name only in the corresponding
method, and not in other methods of the same trait. Finally, macros
such as @scheme[public*] and @scheme[define/public] work in
@scheme[trait] as in @scheme[class].

External identifiers in @scheme[trait], @scheme[trait-exclude],
@scheme[trait-exclude-field], @scheme[trait-alias],
@scheme[trait-rename], and @scheme[trait-rename-field] forms are
subject to binding via @scheme[define-member-name] and
@scheme[define-local-member-name]. Although @scheme[private] methods
or fields are not allowed in a @scheme[trait] form, they can be
simulated by using a @scheme[public] or @scheme[field] declaration and
a name whose scope is limited to the @scheme[trait] form.}


@defproc[(trait? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a trait, @scheme[#f] otherwise.}


@defproc[(trait->mixin [tr trait?]) (class? . -> . class?)]{

Converts a @tech{trait} to a @tech{mixin}, which can be applied to a
@tech{class} to produce a new @tech{class}. An expression of the form

@schemeblock[
(trait->mixin
 (trait
   _trait-clause ...))
]

is equivalent to

@schemeblock[
(lambda (%)
  (class %
    _trait-clause ...
    (super-new)))
]

Normally, however, a trait's methods are changed and combined with
other traits before converting to a mixin.}


@defproc[(trait-sum [tr trait?] ...+) trait?]{

Produces a @tech{trait} that combines all of the methods of the given
@scheme[tr]s. For example,

@schemeblock[
(define t1
 (trait
  (define/public (m1) 1)))
(define t2
 (trait
  (define/public (m2) 2)))
(define t3 (trait-sum t1 t2))
]

creates a trait @scheme[t3] that is equivalent to

@schemeblock[
(trait
 (define/public (m1) 1)
 (define/public (m2) 2))
]

but @scheme[t1] and @scheme[t2] can still be used individually or
combined with other traits.

When traits are combined with @scheme[trait-sum], the combination
drops @scheme[inherit], @scheme[inherit/super],
@scheme[inherit/inner], and @scheme[inherit-field] declarations when a
definition is supplied for the same method or field name by another
trait. The @scheme[trait-sum] operation fails (the
@exnraise[exn:fail:contract]) if any of the traits to combine define a
method or field with the same name, or if an @scheme[inherit/super] or
@scheme[inherit/inner] declaration to be dropped is inconsistent with
the supplied definition. In other words, declaring a method with
@scheme[inherit], @scheme[inherit/super], or @scheme[inherit/inner],
does not count as defining the method; at the same time, for example,
a trait that contains an @scheme[inherit/super] declaration for a
method @scheme[m] cannot be combined with a trait that defines
@scheme[m] as @scheme[augment], since no class could satisfy the
requirements of both @scheme[augment] and @scheme[inherit/super] when
the trait is later converted to a mixin and applied to a class.}


@defform[(trait-exclude trait-expr id)]{

Produces a new @tech{trait} that is like the @tech{trait} result of
@scheme[trait-expr], but with the definition of a method named by
@scheme[id] removed; as the method definition is removed, either a
@scheme[inherit], @scheme[inherit/super], or @scheme[inherit/inner]
declaration is added:

@itemize[

 @item{A method declared with @scheme[public], @scheme[pubment], or
  @scheme[public-final] is replaced with a @scheme[inherit]
  declaration.}

 @item{A method declared with @scheme[override] or @scheme[override-final]
 is replaced with a @scheme[inherit/super] declaration.}

  @item{A method declared with @scheme[augment], @scheme[augride], or
  @scheme[augment-final] is replaced with a @scheme[inherit/inner] declaration.}

 @item{A method declared with @scheme[overment] is not replaced
  with any @scheme[inherit] declaration.}

]

If the trait produced by @scheme[trait-expr] has no method definition for
@scheme[id], the @exnraise[exn:fail:contract].}


@defform[(trait-exclude-field trait-expr id)]{

Produces a new @tech{trait} that is like the @tech{trait} result of
@scheme[trait-expr], but with the definition of a field named by
@scheme[id] removed; as the field definition is removed, an
@scheme[inherit-field] declaration is added.}


@defform[(trait-alias trait-expr id new-id)]{

Produces a new @tech{trait} that is like the @tech{trait} result of
@scheme[trait-expr], but the definition and declaration of the method
named by @scheme[id] is duplicated with the name @scheme[new-id]. The
consistency requirements for the resulting trait are the same as for
@scheme[trait-sum], otherwise the @exnraise[exn:fail:contract]. This
operation does not rename any other use of @scheme[id], such as in
method calls (even method calls to @scheme[identifier] in the cloned
definition for @scheme[new-id]).}


@defform[(trait-rename trait-expr id new-id)]{

Produces a new @tech{trait} that is like the @tech{trait} result of
@scheme[trait-expr], but all definitions and references to methods
named @scheme[id] are replaced by definitions and references to
methods named by @scheme[new-id]. The consistency requirements for the
resulting trait is the same as for @scheme[trait-sum], otherwise the
@exnraise[exn:fail:contract].}


@defform[(trait-rename-field trait-expr id new-id)]{

Produces a new @tech{trait} that is like the @tech{trait} result of
@scheme[trait-expr], but all definitions and references to fields
named @scheme[id] are replaced by definitions and references to fields
named by @scheme[new-id]. The consistency requirements for the
resulting trait is the same as for @scheme[trait-sum], otherwise the
@exnraise[exn:fail:contract].}

@; ------------------------------------------------------------------------

@section{Object and Class Contracts}

@defform/subs[
#:literals (field inherit inherit-field super inner override augment augride)

(class/c member-spec ...)

([member-spec
  method-spec
  (field field-spec ...)
  (inherit method-spec ...)
  (inherit-field field-spec ...)
  (super method-spec ...)
  (inner method-spec ...)
  (override method-spec ...)
  (augment method-spec ...)]
 
 [method-spec
  method-id
  (method-id method-contract)]
 [field-spec
  field-id
  (field-id contract-expr)])]{
Produces a contract for a class.

There are two major categories of contracts listed in a @scheme[class/c]
form: external and internal contracts. External contracts govern behavior
when methods or fields are accessed via an object of that class. Internal
contracts govern behavior when method or fields are accessed within the
class hierarchy.  This separation allows for stronger contracts for class
clients and weaker contracts for subclasses.

Method contracts must contain an additional initial argument which corresponds
to the @scheme[this] parameter of the method.  This allows for contracts which
discuss the state of the object when the method is called (or, for dependent
contracts, in other parts of the contract).

The external contracts are as follows:

@itemize[
 @item{A method contract without a tag for @scheme[method-id] describes the behavior
   of the implementation of @scheme[method-id] on method sends to an object of the
   contracted class.  This contract will continue to be checked in subclasses until
   the contracted class's implementation is no longer the entry point for dynamic
   dispatch.}
 @item{A field contract, tagged with @scheme[field], describes the behavior of the
   value contained in that field when accessed via an object of that class.  Since
   fields may be mutated, these contracts are checked on any external access and/or
   mutation of the field.}
]

The internal contracts are as follows:
@itemize[
 @item{A method contract, tagged with @scheme[inherit], describes the behavior of the
   method when invoked directly (i.e., via @scheme[inherit]) in any subclass of the
   contracted class.  This contract, like external method contracts, applies until
   the contracted class's method implementation is no longer the entry point for dynamic
   dispatch.}
 @item{A field contract, tagged with @scheme[inherit-field], describes the behavior of the
   value contained in that field when accessed directly (i.e., via @scheme[inherit-field])
   in any subclass of the contracted class.  Since fields may be mutated, these contracts are
   checked on any access and/or mutation of the field that occurs in such subclasses.}
 @item{A method contract, tagged with @scheme[super], describes the behavior of
   @scheme[method-id] when called by the @scheme[super] form in a subclass.  This contract
   only affects @scheme[super] calls in subclasses which call the contract class's
   implementation of @scheme[method-id].}
 @item{A method contract, tagged with @scheme[inner], describes the behavior the class
   expects of an augmenting method in a subclass.  This contract affects any implementations
   of @scheme[method-id] in subclasses which can be called via @scheme[inner] from the
   contracted class.  This means a subclass which implements @scheme[method-id] via
   @scheme[augment] or @scheme[overment] stop future subclasses from being affected by
   the contract, since further extension cannot be reached via the contracted class.}
 @item{A method contract, tagged with @scheme[override], describes the behavior expected by
   the contracted class for @scheme[method-id] when called directly (i.e. by the application
   @scheme[(method-id ...)]).  This form can only be used if overriding the method in subclasses
   will change the entry point to the dynamic dispatch chain (i.e., the method has never been
   augmentable).}
 @item{A method contract, tagged with either @scheme[augment] or @scheme[augride], describes the
   behavior provided by the contracted class for @scheme[method-id] when called directly from
   subclasses.  These forms can only be used if the method has previously been augmentable, which
   means that no augmenting or overriding implementation will change the entry point to the
   dynamic dispatch chain.  @scheme[augment] is used when subclasses can augment the method, and
   @scheme[augride] is used when subclasses can override the current augmentation.}
]}

@defform/subs[
#:literals (field -> ->* ->d)

(object-contract member-spec ...)

([member-spec
  (method-id method-contract)
  (field field-id contract-expr)]

 [method-contract
  (-> dom ... range)
  (->* (mandatory-dom ...)
       (optional-dom ...)
       rest
       range)
  (->d (mandatory-dependent-dom ...) 
       (optional-dependent-dom ...) 
       dependent-rest
       pre-cond
       dep-range)]

 [dom dom-expr (code:line keyword dom-expr)]
 [range range-expr (values range-expr ...) any]
 [mandatory-dom dom-expr (code:line keyword dom-expr)]
 [optional-dom dom-expr (code:line keyword dom-expr)]
 [rest (code:line) (code:line #:rest rest-expr)]
 [mandatory-dependent-dom [id dom-expr] (code:line keyword [id dom-expr])]
 [optional-dependent-dom [id dom-expr] (code:line keyword [id dom-expr])]
 [dependent-rest (code:line) (code:line #:rest id rest-expr)]
 [pre-cond (code:line) (code:line #:pre-cond boolean-expr)]
 [dep-range any
            (code:line [id range-expr] post-cond)
            (code:line (values [id range-expr] ...) post-cond)]
 [post-cond (code:line) (code:line #:post-cond boolean-expr)]
)]{

Produces a contract for an object.

Each of the contracts for a method has the same semantics as
the corresponding function contract, but the syntax of the
method contract must be written directly in the body of the
object-contract---much like the way that methods in class
definitions use the same syntax as regular function
definitions, but cannot be arbitrary procedures.  Unlike the
method contracts for @scheme[class/c], the implicit @scheme[this]
argument is not part of the contract.  To allow for the use of
@scheme[this] in dependent contracts, @scheme[->d] contracts
implicitly bind @scheme[this] to the object itself.}


@defthing[mixin-contract contract?]{

A @tech{function contract} that recognizes mixins. It guarantees that
the input to the function is a class and the result of the function is
a subclass of the input.}

@defproc[(make-mixin-contract [type (or/c class? interface?)] ...) contract?]{

Produces a @tech{function contract} that guarantees the input to the
function is a class that implements/subclasses each @scheme[type], and
that the result of the function is a subclass of the input.}

@defproc[(is-a?/c [type (or/c class? interface?)]) flat-contract?]{

Accepts a class or interface and returns a flat contract that
recognizes objects that instantiate the class/interface.}


@defproc[(implementation?/c [interface interface?]) flat-contract?]{

Returns a flat contract that recognizes classes that implement
@scheme[interface].}


@defproc[(subclass?/c [class class?]) flat-contract?]{

Returns a flat-contract that recognizes classes that
are subclasses of @scheme[class].}


@; ------------------------------------------------------------------------

@section[#:tag "objectequality"]{Object Equality and Hashing}

But default, objects that are instances of different classes or that
are instances of a non-transparent class are @scheme[equal?] only if
they are @scheme[eq?]. Like transparent structures, two objects that
are instances of the same transparent class (i.e., every superclass of
the class has @scheme[#f] as its inspector) are @scheme[equal?] when
their field values are @scheme[equal?].

To customize the way that a class instance is compared to other
instances by @scheme[equal?], implement the @scheme[equal<%>]
interface.

@definterface[equal<%> ()]{

The @scheme[equal<%>] interface includes three methods, which are
analogous to the functions provided for a structure type with
@scheme[prop:equal+hash]:

@itemize[

 @item{@scheme[equal-to?] --- Takes two arguments. The first argument
 is an object that is an instance of the same class (or a subclass
 that does not re-declare its implementation of @scheme[equal<%>])
 and that is being compared to the target object. The second argument
 is a @scheme[equal?]-like procedure of two arguments that should be
 used for recursive equality testing. The result should be a true
 value if the object and the first argument of the method are equal,
 @scheme[#f] otherwise.}

 @item{@scheme[equal-hash-code-of] --- Takes one argument, which is a
 procedure of one argument that should be used for recursive hash-code
 computation. The result should be an exact integer representing the
 target object's hash code.}

 @item{@scheme[equal-secondary-hash-code-of] --- Takes one argument,
 which is a procedure of one argument that should be used for
 recursive hash-code computation. The result should be an exact
 integer representing the target object's secondary hash code.}

]

The @scheme[equal<%>] interface is unusual in that declaring the
implementation of the interface is different from inheriting the
interface. Two objects can be equal only if they are instances of
classes whose most specific ancestor to explicitly implement
@scheme[equal<%>] is the same ancestor.

See @scheme[prop:equal+hash] for more information on equality
comparisons and hash codes. The @scheme[equal<%>] interface is
implemented with @scheme[interface*] and @scheme[prop:equal+hash].}

@; ------------------------------------------------------------------------

@section[#:tag "objectserialize"]{Object Serialization}

@defform[
(define-serializable-class* class-id superclass-expr 
                                     (interface-expr ...)
  class-clause ...)
]{

Binds @scheme[class-id] to a class, where @scheme[superclass-expr],
the @scheme[interface-expr]s, and the @scheme[class-clause]s are as in
@scheme[class*].

This forms can only be used at the top level, either within a module
or outside. The @scheme[class-id] identifier is bound to the new
class, and @schemeidfont{deserialize-info:}@scheme[class-id] is also
defined; if the definition is within a module, then the latter is
provided from the module.

Serialization for the class works in one of two ways:

@itemize[

 @item{If the class implements the built-in interface
       @scheme[externalizable<%>], then an object is serialized by
       calling its @scheme[externalize] method; the result can be
       anything that is serializable (but, obviously, should not be
       the object itself). Deserialization creates an instance of the
       class with no initialization arguments, and then calls the
       object's @scheme[internalize] method with the result of
       @scheme[externalize] (or, more precisely, a deserialized
       version of the serialized result of a previous call).

       To support this form of serialization, the class must be
       instantiable with no initialization arguments. Furthermore,
       cycles involving only instances of the class (and other such
       classes) cannot be serialized.}

 @item{If the class does not implement @scheme[externalizable<%>],
       then every superclass of the class must be either serializable
       or transparent (i.e,. have @scheme[#f] as its
       inspector). Serialization and deserialization are fully
       automatic, and may involve cycles of instances.

       To support cycles of instances, deserialization may create an
       instance of the call with all fields as the undefined value,
       and then mutate the object to set the field
       values. Serialization support does not otherwise make an
       object's fields mutable.}

]

In the second case, a serializable subclass can implement
@scheme[externalizable<%>], in which case the @scheme[externalize]
method is responsible for all serialization (i.e., automatic
serialization is lost for instances of the subclass). In the first
case, all serializable subclasses implement
@scheme[externalizable<%>], since a subclass implements all of the
interfaces of its parent class.

In either case, if an object is an immediate instance of a subclass
(that is not itself serializable), the object is serialized as if it
was an immediate instance of the serializable class. In particular,
overriding declarations of the @scheme[externalize] method are ignored
for instances of non-serializable subclasses.}


@defform[
(define-serializable-class class-id superclass-expr
  class-clause ...)
]{

Like @scheme[define-serializable-class*], but with not interface
expressions (analogous to @scheme[class]).}


@definterface[externalizable<%> ()]{

The @scheme[externalizable<%>] interface includes only the
@scheme[externalize] and @scheme[internalize] methods. See
@scheme[define-serializable-class*] for more information.}

@; ------------------------------------------------------------------------

@section[#:tag "objectprinting"]{Object Printing}

To customize the way that a class instance is printed by @scheme[write]
or @scheme[display], implement the @scheme[printable<%>] interface.

@defthing[printable<%> interface?]{

The @scheme[printable<%>] interface includes only the
@scheme[custom-write] and @scheme[custom-print] methods. Each accepts
a single argument, which is the destination port to @scheme[write] or
@scheme[display] the object.

Calls to the @scheme[custom-write] or @scheme[custom-display] are like
calls to a procedure attached to a structure type through the
@scheme[prop:custom-write] property. In particular, recursive printing
can trigger an escape from the call.

See @scheme[prop:custom-write] for more information. The
@scheme[printable<%>] interface is implemented with
@scheme[interface*] and @scheme[prop:custom-write].}

@; ------------------------------------------------------------------------

@section[#:tag "objectutils"]{Object, Class, and Interface Utilities}

@defproc[(object? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is an object, @scheme[#f] otherwise.}


@defproc[(class? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a class, @scheme[#f] otherwise.}


@defproc[(interface? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is an interface, @scheme[#f] otherwise.}


@defproc[(generic? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a @tech{generic}, @scheme[#f] otherwise.}


@defproc[(object=? [a object?] [b object?]) boolean?]{

Determines if two objects are the same object, or not; this procedure uses
@scheme[eq?], but also works properly with contracts.}


@defproc[(object->vector [object object?][opaque-v any/c #f]) vector?]{

Returns a vector representing @scheme[object] that shows its
inspectable fields, analogous to @scheme[struct->vector].}


@defproc[(class->interface [class class?]) interface?]{

Returns the interface implicitly defined by @scheme[class].}


@defproc[(object-interface [object object?]) interface?]{

Returns the interface implicitly defined by the class of
@scheme[object].}

 
@defproc[(is-a? [v any/c][type (or/c interface? class?)]) boolean?]{

Returns @scheme[#t] if @scheme[v] is an instance of a class
@scheme[type] or a class that implements an interface @scheme[type],
@scheme[#f] otherwise.}


@defproc[(subclass? [v any/c][class class?]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a class derived from (or equal
to) @scheme[class], @scheme[#f] otherwise.}


@defproc[(implementation? [v any/c][interface interface?]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a class that implements
@scheme[interface], @scheme[#f] otherwise.}


@defproc[(interface-extension? [v any/c][interface interface?]) boolean?]{

Returns @scheme[#t] if @scheme[v] is an interface that extends
@scheme[interface], @scheme[#f] otherwise.}


@defproc[(method-in-interface? [sym symbol?][interface interface?]) boolean?]{

Returns @scheme[#t] if @scheme[interface] (or any of its ancestor
interfaces) includes a member with the name @scheme[sym], @scheme[#f]
otherwise.}


@defproc[(interface->method-names [interface interface?]) (listof symbol?)]{

Returns a list of symbols for the method names in @scheme[interface],
including methods inherited from superinterfaces, but not including
methods whose names are local (i.e., declared with
@scheme[define-local-member-names]).}


@defproc[(object-method-arity-includes? [object object?][sym symbol?][cnt exact-nonnegative-integer?])
         boolean?]{

Returns @scheme[#t] if @scheme[object] has a method named @scheme[sym]
that accepts @scheme[cnt] arguments, @scheme[#f] otherwise.}


@defproc[(field-names [object object?]) (listof symbol?)]{

Returns a list of all of the names of the fields bound in
@scheme[object], including fields inherited from superinterfaces, but
not including fields whose names are local (i.e., declared with
@scheme[define-local-member-names]).}


@defproc[(object-info [object any/c]) (values (or/c class? #f) boolean?)]{

Returns two values, analogous to the return
values of @scheme[struct-info]:
@itemize[

  @item{@scheme[class]: a class or @scheme[#f]; the result is
  @scheme[#f] if the current inspector does not control any class for
  which the @scheme[object] is an instance.}

  @item{@scheme[skipped?]: @scheme[#f] if the first result corresponds
  to the most specific class of @scheme[object], @scheme[#t]
  otherwise.}

]}


@defproc[(class-info [class class?])
         (values symbol?
                 exact-nonnegative-integer?
                 (listof symbol?)
                 (any/c exact-nonnegative-integer? . -> . any/c)
                 (any/c exact-nonnegative-integer? any/c . -> . any/c)
                 (or/c class? #f)
                 boolean?)]{

Returns seven values, analogous to the return
values of @scheme[struct-type-info]:

@itemize[

  @item{@scheme[name]: the class's name as a symbol;}

  @item{@scheme[field-cnt]: the number of fields (public and private)
   defined by the class;}

  @item{@scheme[field-name-list]: a list of symbols corresponding to the
  class's public fields; this list can be larger than @scheme[field-k]
  because it includes inherited fields;}

  @item{@scheme[field-accessor]: an accessor procedure for obtaining
  field values in instances of the class; the accessor takes an
  instance and a field index between @scheme[0] (inclusive)
  and @scheme[field-cnt] (exclusive);}

  @item{@scheme[field-mutator]: a mutator procedure for modifying
  field values in instances of the class; the mutator takes an
  instance, a field index between @scheme[0] (inclusive)
  and @scheme[field-cnt] (exclusive), and a new field value;}

  @item{@scheme[super-class]: a class for the most specific ancestor of
   the given class that is controlled by the current inspector,
   or @scheme[#f] if no ancestor is controlled by the current
   inspector;}

  @item{@scheme[skipped?]: @scheme[#f] if the sixth result is the most
   specific ancestor class, @scheme[#t] otherwise.}

]}

@defstruct[(exn:fail:object exn:fail) ()]{

Raised for @scheme[class]-related failures, such as attempting to call
a method that is not supplied by an object.

}

@; ----------------------------------------------------------------------

@include-section["surrogate.scrbl"]

@close-eval[class-eval]
