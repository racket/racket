#lang scribble/doc
@(require "utils.ss")

@title[#:tag "intro"]{Overview}

Although using the FFI requires writing no new C code, it provides
very little insulation against the issues that C programmer faces
related to safety and memory management. An FFI programmer must be
particularly aware of memory management issues for data that spans the
Scheme--C divide. Thus, this manual relies in many ways on the
information in @|InsideMzScheme|, which defines how PLT Scheme
interacts with C APIs in general.

Since using the FFI entails many safety concerns that Scheme
programmers can normally ignore, merely importing
@schememodname[scheme/foreign] with @scheme[(require scheme/foreign)]
does not import all of the FFI functionality. Only safe functionality
is immediately imported. For example, @scheme[ptr-equal?] can never
cause memory corruption or an invalid memory access, so it is
immediately available on import.

Use @scheme[(@#,indexed-scheme[unsafe!])] at the top-level of a
module that imports @schememodname[scheme/foreign] to make unsafe
features accessible. (For additional safety, the @scheme[unsafe!] is
itself protected; see @secref[#:doc '(lib
"scribblings/reference/reference.scrbl") "modprotect"].)  Using this
macro should be considered as a declaration that your code is itself
unsafe, therefore can lead to serious problems in case of bugs: it is
your responsibility to provide a safe interface. Bindings that become
available only via @scheme[unsafe!] are documented in this manual in
sections with titles starting ``Unsafe.''

For examples of common FFI usage patterns, see the defined interfaces
in the @filepath{ffi} collection.
