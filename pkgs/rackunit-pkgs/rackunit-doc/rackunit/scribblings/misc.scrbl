#lang scribble/doc
@(require "base.rkt")

@title{Miscellaneous Utilities}

The @racket[require/expose] macro allows you to access
bindings that a module does not provide.  It is useful for
testing the private functions of modules.

@defform[(require/expose module (id ...))]{

Requires @racket[id] from @racket[module] into the current module.  It
doesn't matter if the source module provides the bindings or not;
@racket[require/expose] can still get at them.

Note that @racket[require/expose] can be a bit fragile,
especially when mixed with compiled code.  Use at your own risk!
}

This example gets @racket[make-failure-test], which is defined in a RackUnit test:

@racketblock[
(require/expose rackunit/private/check-test (make-failure-test))
]

@defproc[(dynamic-require/expose [mod (or/c module-path?
                                            module-path-index?
                                            resolved-module-path?)]
                                 [name symbol?])
         any]{

Like @racket[dynamic-require], but gets internal bindings like
@racket[require/expose].
}
