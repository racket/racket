#lang scribble/doc
@(require scribble/manual
          "common.ss"
          (for-label scheme/runtime-path))

@title[#:tag "exe"]{Creating and Distributing Stand-Alone Executables}

Whether bytecode or native code, the compiled code produced by @|mzc|
relies on MzScheme (or MrEd) to provide run-time support to the
compiled code. However, @|mzc| can also package code together with its
run-time support to form a complete executable, and then the
executable can be packaged into a distribution that works on other
machines.

@section{Stand-Alone Executables from Scheme Code}

The command-line flag @DFlag{exe} directs @|mzc| to embed a
module, from source or byte code, into a copy of the MzScheme
executable. (Under Unix, the embedding executable is actually a copy
of a wrapper executable.)  The created executable invokes the embedded
module on startup. The @DFlag{gui-exe} flag is similar, but it
copies the MrEd executable. If the embedded module refers to other
modules via @scheme[require], then the other modules are also included
in the embedding executable.

For example, the command

@commandline{mzc --gui-exe hello hello.ss}

produces either @filepath{hello.exe} (Windows), @filepath{hello.app}
(Mac OS X), or @filepath{hello} (Unix), which runs the same as
invoking the @filepath{hello.ss} module in MrEd.

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
run it without starting MzScheme, MrEd, or DrScheme. However, the
executable depends on MzScheme and/or MrEd shared libraries, and
possibly other run-time files declared via
@scheme[define-runtime-path]. The executable can be packaged with
support libraries to create a distribution, as described in the
next section.

@; ----------------------------------------------------------------------

@section[#:tag "exe-dist"]{Distributing Stand-Alone Executables}

The command-line flag @DFlag{exe-dir} directs @|mzc| to combine a
stand-alone executable (created via @DFlag{exe} or @DFlag{gui-exe})
with all of the shared libraries that are needed to run it, along with
any run-time files declared via @scheme[define-runtime-path].  The
resulting package can be moved to other machines that run the same
operating system.

After the @DFlag{exe-dir} flag, supply a directory to contain the
combined files for a distribution. Each command-line argument is an
executable to include in the distribution, so multiple executables can
be packaged together. For example, under Windows,

@commandline{mzc --exe-dir geetings hello.exe goodbye.exe}

creates a directory @filepath{greetings} (if the directory doesn't
exist already), and it copies the executables @filepath{hello.exe} and
@filepath{goodbye.exe} into @filepath{greetings}. It also creates a
@filepath{lib} sub-directory in @filepath{greetings} to contain DLLs,
and it adjusts the copied @filepath{hello.exe} and
@filepath{goodbye.exe} to use the DLLs in @filepath{lib}.

The layout of files within a distribution directory is
platform-specific:

@itemize{

@item{Under Windows, executables are put directly into the
      distribution directory, and DLLs and other run-time files go
      into a @filepath{lib} sub-directory.}

@item{Under Mac OS X, @DFlag{gui-exe} executables go into the
      distribution directory, @DFlag{exe} executables go into a
      @filepath{bin} subdirectory, and frameworks (i.e., shared
      libraries) go into a @filepath{lib} sub-directory along with
      other run-time files. As a special case, if the distribution has
      a single @DFlag{gui-exe} executable, then the @filepath{lib}
      directory is hidden inside the application bundle.}

@item{Under Unix, executables go into a @filepath{bin} subdirectory,
      shared libraries (if any) go into a @filepath{lib} subdirectory
      along with other run-time files, and wrapped executables are
      placed into a @filepath{lib/plt} subdirectory with
      version-specific names. This layout is consistent with Unix
      installation conventions; the version-specific names for shared
      libraries and wrapped executables means that distributions can
      be safely unpacked into a standard place on target machines
      without colliding with an existing PLT Scheme installation or
      other executables created by @|mzc|.}

}

A distribution also has a @filepath{collects} directory that is used
as the main library collection directory for the packaged executables.
By default, the directory is empty. Use @|mzc|'s
@as-index{@DPFlag{copy-collects}} flag to supply a directory whose
content is copied into the distribution's @filepath{collects}
directory. The @DPFlag{copy-collects} flag can be used multiple times
to supply multiple directories.

When multiple executables are disrtibuted together, then separately
creating the executables with @DFlag{exe} and @DFlag{gui-exe} can
generate multiple copies of collection-based libraries that are used
by multiple executables. To share the library code, instead, specify a
target directory for library copies using the
@as-index{@DFlag{collects-dest}} flag with @DFlag{exe} and
@DFlag{gui-exe}, and specify the same directory for each executable
(so that the set of libraries used by all executables are pooled
together). Finally, when packaging the distribution with
@DFlag{exe-dir}, use the @DPFlag{copy-collects} flag to include the
copied libraries in the distribution.
