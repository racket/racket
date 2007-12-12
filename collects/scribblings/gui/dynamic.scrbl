#lang scribble/doc
@require["common.ss"
         (for-label scheme/gui/dynamic)]

@title{Dynamic Loading}

@defmodule[scheme/gui/dynamic]{The @schememodname[scheme/gui/dynamic]
library provides functiosn for dynamically accessing the PLT Scheme
GUI toolbox, instead of directly requiring @scheme[scheme/gui] or
@scheme[scheme/gui/base].}

@defproc[(gui-available?) boolean?]{

Returns @scheme[#t] if dynamic access to the GUI bindings are
available---that is, that the program is being run as a
@exec{mred}-based application, as opposed to a pure
@exec{mzscheme}-based application, and that GUI modules are attached
to the namespace in which @scheme[scheme/gui/dynamic] was
instantiated.

This predicate can be used in code that optionally uses GUI elements
when they are available.}


@defproc[(gui-dynamic-require [sym symbol?]) any]{

Like @scheme[dynamic-require], but specifically to access exports of
@scheme[scheme/gui/base].}
