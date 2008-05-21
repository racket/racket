#lang scribble/doc
@(require scribble/manual
          "common.ss"
          (for-label scheme/runtime-path))

@title[#:tag "exe"]{Stand-Alone Executables from Scheme Code}

The command-line flag @DFlag{exe} directs @|mzc| to embed a module,
from source or byte code, into a copy of the @exec{mzscheme}
executable. (Under Unix, the embedding executable is actually a copy
of a wrapper executable.)  The created executable invokes the embedded
module on startup. The @DFlag{gui-exe} flag is similar, but it copies
the @exec{mred} executable. If the embedded module refers to other
modules via @scheme[require], then the other modules are also included
in the embedding executable.

For example, the command

@commandline{mzc --gui-exe hello hello.ss}

produces either @filepath{hello.exe} (Windows), @filepath{hello.app}
(Mac OS X), or @filepath{hello} (Unix), which runs the same as
invoking the @filepath{hello.ss} module in @exec{mred}.

Library modules or other files that are referenced
dynamically---through @scheme[eval], @scheme[load], or
@scheme[dynamic-require]---are not automatically embedded into the
created executable. Such modules can be explicitly included using
@|mzc|'s @DFlag{lib} flag. Alternately, use
@scheme[define-runtime-path] to embed references to the run-time files
in the executable; the files are then copied and packaged together
with the executable when creating a distribution (as described in
@secref["exe-dist"]).

Modules that are implemented directly by extensions---i.e., extensions
that are automatically loaded from @scheme[(build-path "compiled"
"native" (system-library-subpath))] to satisfy a
@scheme[require]---are treated like other run-time files: a generated
executable uses them from their original location, and they are copied
and packaged together when creating a distribution.

The @DFlag{exe} and @DFlag{gui-exe} flags work only with
@scheme[module]-based programs. The @schememodname[compiler/embed]
library provides a more general interface to the embedding mechanism.

A stand-alone executable is ``stand-alone'' in the sense that you can
run it without starting @exec{mzscheme}, @exec{mred}, or
DrScheme. However, the executable depends on PLT Scheme shared
libraries, and possibly other run-time files declared via
@scheme[define-runtime-path]. The executable can be packaged with
support libraries to create a distribution, as described in
@secref["exe-dist"].

@; ----------------------------------------------------------------------

@include-section["exe-api.scrbl"]
