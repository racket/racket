#lang scribble/doc
@(require "mz.rkt" (for-label racket/surrogate racket/class))

@title{Surrogates}

@note-lib-only[racket/surrogate]

The @racketmodname[racket/surrogate] library provides an abstraction
for building an instance of the @deftech{proxy design pattern}. The
pattern consists of two objects, a @defterm{host} and a
@defterm{surrogate} object. The host object delegates method calls to
its surrogate object. Each host has a dynamically assigned surrogate,
so an object can completely change its behavior merely by changing the
surrogate.

@defform/subs[#:literals (augment override override-final)
              (surrogate use-wrapper-proc method-spec ...)
              ([use-wrapper-proc #:use-wrapper-proc (code:line)]
               [method-spec (augment default-expr method-id arg-spec ...)
                            (override method-id arg-spec ...)               
                            (override-final method-id (lambda () default-expr) 
                                            arg-spec ...)]
               [arg-spec (id ...)
                         id])]{

If neither @racket[override] nor @racket[override-final] is specified
for a @racket[method-id], then @racket[override] is assumed.

The @racket[surrogate] form produces four values: a host mixin (a
procedure that accepts and returns a class), a host interface, a
surrogate class, and a surrogate interface.

If @racket[#:use-wrapper-proc] does not appear,
the host mixin adds one field @racket[surrogate].
to its argument. It also adds getter and setter methods, @racket[get-surrogate],
@racket[set-surrogate], @racket[get-surrogate-wrapper-proc], and
@racket[set-surrogate-wrapper-proc] for changing the fields. The
@racket[set-surrogate] method accepts instances of the class returned by
the @racket[surrogate] form or @racket[#f], and it updates the field with its
argument; then, @racket[set-surrogate] calls the @racket[on-disable-surrogate] on the
previous value of the field and @racket[on-enable-surrogate] for the
new value of the field. The @racket[get-surrogate] method returns the
current value of the field.

If @racket[#:use-wrapper-proc] does appear, the the host mixin adds
both the @racket[_surrogate] field (with its getters and setters) and a
a second field, @racket[_surrogate-wrapper-proc] and its getter and setter
methods, @racket[_get-surrogate-wrapper-proc] and @racket[_set-surrogate-wrapper-proc].
The @racket[_surrogate-wrapper-proc] field holds a procedure whose contract
is @racket[(-> (-> any) (-> any) any)]. The function is invoked with two thunks.
The first one is a fallback that invokes the original object's method,
skipping the surrogate. The other one invokes the surrogate.
 @racketblock[(λ (fallback-thunk surrogate-thunk)
                (surrogate-thunk))]
This means that it simply defers to the method being invoked on the surrogate.
The @racket[_surrogate-wrapper-proc] capability is part of the surrogate
so that the dynamic extent of the calls to the surrogate can be adjusted
(by, for example, changing the values of parameters). The
@racket[_surrogate-wrapper-proc] is also invoked when calling the
@racket[_on-disable-surrogate] and @racket[_on-enable-surrogate] methods
of the surrogate.

The host mixin has a single overriding method for each
@racket[method-id] in the @racket[surrogate] form (even the ones
specified with @racket[augment]). Each of these
methods is defined with a @racket[case-lambda] with one arm for each
@racket[arg-spec]. Each arm has the variables as arguments in the
@racket[arg-spec]. The body of each method tests the
@racket[surrogate] field. If it is @racket[#f], the method just
returns the result of invoking the super or inner method. If the
@racket[surrogate] field is not @racket[#f], the corresponding method
of the object in the field is invoked. This method receives the same
arguments as the original method, plus two extras. The extra arguments
come at the beginning of the argument list. The first is the original
object. The second is a procedure that calls the super or inner method
(i.e., the method of the class that is passed to the mixin or an
extension, or the method in an overriding class), with the arguments
that the procedure receives.

For example, the host-mixin for this surrogate:
@racketblock[(surrogate (override m (x y z)))]
will override the @racket[m] method and call the surrogate like this:
@racketblock[(define/override (m x y z)
               (if _surrogate
                   (send _surrogate m 
                         this 
                         (λ (x y z) (super m x y z))
                         x y z)
                   (super m x y z)))]
where @racket[_surrogate] is bound to the value most recently passed
to the host mixin's @racket[_set-surrogate] method.

The host interface has the names @racket[set-surrogate],
@racket[get-surrogate], and each of the @racket[method-id]s in the
original form.

The surrogate class has a single public method for each
@racket[method-id] in the @racket[surrogate] form. These methods are
invoked by classes constructed by the mixin. Each has a corresponding
method signature, as described in the above paragraph. Each method
just passes its argument along to the super procedure it receives.

In the example above, this is the @racket[_m] method in the surrogate class:
@racketblock[(define/public (m original-object original-super x y z)
               (original-super x y z))]

Note: if you derive a class from the surrogate class, do not both call
the @racket[super] argument and the super method of the surrogate
class itself. Only call one or the other, since the default methods
call the @racket[super] argument.

Finally, the interface contains all of the names specified in
surrogate's argument, plus @racket[on-enable-surrogate] and
@racket[on-disable-surrogate]. The class returned by
@racket[surrogate] implements this interface.
}
