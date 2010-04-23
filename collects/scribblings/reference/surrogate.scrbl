#lang scribble/doc
@(require "mz.ss"
          (for-label racket/surrogate
                     racket/class))

@title{Surrogates}
 
@note-lib-only[racket/surrogate]

The @schememodname[racket/surrogate] library provides an abstraction
for building an instance of the @deftech{proxy design pattern}. The
pattern consists of two objects, a @defterm{host} and a
@defterm{surrogate} object. The host object delegates method calls to
its surrogate object. Each host has a dynamically assigned surrogate,
so an object can completely change its behavior merely by changing the
surrogate.

@defform/subs[#:literals (override override-final)
              (surrogate method-spec ...)
              ([method-spec (method-id arg-spec ...)
                            (override method-id arg-spec ...)               
                            (override-final method-id (lambda () default-expr) 
                                            arg-spec ...)]
               [arg-spec (id ...)
                         id])]{

If neither @scheme[override] nor @scheme[override-final] is specified
for a @scheme[method-id], then @scheme[override] is assumed.

The @scheme[surrogate] form produces four values: a host mixin (a
procedure that accepts and returns a class), a host interface, a
surrogate class, and a surrogate interface.

The host mixin adds one additional field, @scheme[surrogate], to its
argument. It also adds a getter method, @scheme[get-surrogate], and a
setter method, @scheme[set-surrogate], for changing the field. The
@scheme[set-surrogate] form accepts instances the class returned by
the form or @scheme[#f], and updates the field with its
argument. Then, it calls the @scheme[on-disable-surrogate] on the
previous value of the field and @scheme[on-enable-surrogate] for the
new value of the field. The @scheme[get-surrogate] method returns the
current value of the field.

The host mixin has a single overriding method for each
@scheme[method-id] in the @scheme[surrogate] form. Each of these
methods is defined with a @scheme[case-lambda] with one arm for each
@scheme[arg-spec]. Each arm has the variables as arguments in the
@scheme[arg-spec]. The body of each method tests the
@scheme[surrogate] field. If it is @scheme[#f], the method just
returns the result of invoking the super or inner method. If the
@scheme[surrogate] field is not @scheme[#f], the corresponding method
of the object in the field is invoked. This method receives the same
arguments as the original method, plus two extras. The extra arguments
come at the beginning of the argument list. The first is the original
object. The second is a procedure that calls the super or inner method
(i.e., the method of the class that is passed to the mixin or an
extension, or the method in an overriding class), with the arguments
that the procedure receives.

The host interface has the names @scheme[set-surrogate],
@scheme[get-surrogate], and each of the @scheme[method-id]s in the
original form.

The surrogate class has a single public method for each
@scheme[method-id] in the @scheme[surrogate] form. These methods are
invoked by classes constructed by the mixin. Each has a corresponding
method signature, as described in the above paragraph. Each method
just passes its argument along to the super procedure it receives.

Note: if you derive a class from the surrogate class, do not both call
the @scheme[super] argument and the super method of the surrogate
class itself. Only call one or the other, since the default methods
call the @scheme[super] argument.

Finally, the interface contains all of the names specified in
surrogate's argument, plus @scheme[on-enable-surrogate] and
@scheme[on-disable-surrogate]. The class returned by
@scheme[surrogate] implements this interface.}
