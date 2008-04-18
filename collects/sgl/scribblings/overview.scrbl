#lang scribble/doc
@(require "common.ss")

@title[#:tag "overview"]{Using OpenGL}

The @schememodname[sgl/gl] library provides direct access to the
C-style OpenGL API, whereas the @schememodname[sgl] library provides a
more Scheme-like interface.  The @schememodname[sgl/gl] library
provides a binding for each @tt{#defined} constant and for most
functions in OpenGL 1.5 and GLU 1.3.  The functions perform comparable
checking to their C-language counterparts; they check the types of
their arguments, but do not check the length of array arguments.  The
@schememodname[sgl] library provides wrappers around many of the
functions in the @schememodname[sgl/gl] library to present a more
Scheme-friendly interface, including function names that follow Scheme
conventions, and checked, symbolic enumeration arguments, and
array-length checks.

@bold{Warning on Safety:} OpenGL programming is inherently unsafe,
even when using only the @schememodname[sgl] library.  Although
@schememodname[sgl] checks the arguments to each function call,
violation of higher-level assumptions of the system's OpenGL library
can cause it to crash, bringing the entire Scheme system down.  For
example, sending a large number of vertices in a single @tt{glBegin}
causes at least some GL implementations to crash.

Some examples are available in the @filepath{examples} directory of
the @filepath{sgl} collection in the PLT Scheme installation.  For
@filepath{alpha.ss}, try choosing the @filepath{sk.jpg} image distritbuted
with PLT Scheme in the @filepath{icons} collection; you may have to
press the ``t'' key a few times if the spinning cube is blank.
