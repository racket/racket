#lang scribble/doc
@(require scribble/manual
          "common.ss"
          (for-label racket/runtime-path))

@title[#:tag "exe"]{@exec{raco exe}: Creating Stand-Alone Executables}

Compiled code produced by @exec{raco make} relies on Racket
executables to provide run-time support to the compiled code. However,
@exec{raco exe} can package code together with its run-time support to
form an executable, and @exec{raco distribute} can packaged the
executable into a distribution that works on other machines.

The @exec{raco make} command embeds a module, from source or byte
code, into a copy of the @exec{racket} executable. (Under Unix, the
embedding executable is actually a copy of a wrapper executable.)  The
created executable invokes the embedded module on startup. The
@DFlag{gui} flag causes the program to be embedded in a copy of the
@exec{gracket} executable. If the embedded module refers to other
modules via @racket[require], then the other modules are also included
in the embedding executable.

For example, the command

@commandline{raco exe --gui hello.rkt}

produces either @filepath{hello.exe} (Windows), @filepath{hello.app}
(Mac OS X), or @filepath{hello} (Unix), which runs the same as running
the @filepath{hello.rkt} module in @exec{gracket}.

Library modules or other files that are referenced
dynamically---through @racket[eval], @racket[load], or
@racket[dynamic-require]---are not automatically embedded into the
created executable. Such modules can be explicitly included using the
@DFlag{lib} flag to @exec{raco exe}. Alternately, use
@racket[define-runtime-path] to embed references to the run-time files
in the executable; the files are then copied and packaged together
with the executable when creating a distribution (as described in
@secref["exe-dist"]).

Modules that are implemented directly by extensions---i.e., extensions
that are automatically loaded from @racket[(build-path "compiled"
"native" (system-library-subpath))] to satisfy a
@racket[require]---are treated like other run-time files: a generated
executable uses them from their original location, and they are copied
and packaged together when creating a distribution.

The @exec{raco exe} command works only with module-based programs. The
@racketmodname[compiler/embed] library provides a more general
interface to the embedding mechanism.

A stand-alone executable is ``stand-alone'' in the sense that you can
run it without starting @exec{racket}, @exec{gracket}, or
DrRacket. However, the executable depends on Racket shared libraries,
and possibly other run-time files declared via
@racket[define-runtime-path]. The executable can be packaged with
support libraries to create a distribution using @exec{raco
distribute}, as described in @secref["exe-dist"].

@; ----------------------------------------------------------------------

@include-section["exe-api.scrbl"]
@include-section["launcher.scrbl"]
