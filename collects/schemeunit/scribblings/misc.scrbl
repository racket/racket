#lang scribble/doc
@(require "base.ss")

@title{Miscellaneous Utilities}

The @scheme[require/expose] macro allows you to access
bindings that a module does not provide.  It is useful for
testing the private functions of modules.

@defform[(require/expose module (id ...))]{
Requires @scheme[id] from @scheme[module] into the current module.  It doesn't matter if the source module provides the bindings or not; @scheme[require/expose] can still get at them.

Note that @scheme[require/expose] can be a bit fragile,
especially when mixed with compiled code.  Use at your own risk!
}

This example gets @scheme[make-failure-test], which is defined in a SchemeUnit test:

@schemeblock[
(require/expose schemeunit/private/check-test (make-failure-test))
]
