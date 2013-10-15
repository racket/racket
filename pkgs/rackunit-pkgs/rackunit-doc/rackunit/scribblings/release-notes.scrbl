#lang scribble/doc
@(require "base.rkt")

@title{Release Notes}

@section{Version 3.4}

This version allows arbitrary expressions within test
suites, fixing the semantics issue below.

There are also miscellaneous Scribble fixes.

@section{Version 3}

This version of RackUnit is largely backwards compatible
with version 2 but there are significant changes to the
underlying model, justifying incrementing the major version
number.  These changes are best explained in
@secref["philosophy"].

There are a few omissions in this release, that will
hopefully be corrected in later minor version releases:

@itemize[

@item{There is no graphical UI, and in particular no
integration with DrRacket.}

@item{The semantics of @racket[test-suite] are not the
desired ones.  In particular, only checks and test cases
have their evaluation delayed by a test suite; other
expressions will be evaluated before the suite is
constructed.  This won't affect tests written in the version
2 style.  In particular this doesn't effect test suites that
contain other test suites; they continue to work in the
expected way.  However people incrementally developing tests
from plain checks to test suites might be surprised.  I'm
hoping that few enough people will do this that no-one will
notice before it's fixed.}

]
