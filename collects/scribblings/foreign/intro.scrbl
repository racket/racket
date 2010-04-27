#lang scribble/doc
@(require "utils.ss")

@title[#:tag "intro"]{Overview}

Although using the FFI requires writing no new C code, it provides
very little insulation against the issues that C programmer faces
related to safety and memory management. An FFI programmer must be
particularly aware of memory management issues for data that spans the
Racket--C divide. Thus, this manual relies in many ways on the
information in @|InsideMzScheme|, which defines how Racket
interacts with C APIs in general.

Since using the FFI entails many safety concerns that Racket
programmers can normally ignore, the library name includes
@schemeidfont{unsafe}. Importing the library macro should be
considered as a declaration that your code is itself unsafe, therefore
can lead to serious problems in case of bugs: it is your
responsibility to provide a safe interface. If your library provides
an unsafe interface, then it should have @schemeidfont{unsafe} in its
name, too.

For examples of common FFI usage patterns, see the defined interfaces
in the @filepath{ffi} collection.
