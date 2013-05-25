#lang scribble/doc
@(require scribble/manual "common.rkt" (for-label racket/runtime-path))

@title[#:tag "exe-dist"]{@exec{raco distribute}: Sharing Stand-Alone Executables}

The @exec{raco distribute} command combines a
stand-alone executable created by @exec{raco exe} with all of the
shared libraries that are needed to run it, along with any run-time
files declared via @racket[define-runtime-path].  The resulting
package can be moved to other machines that run the same operating
system.

After the @exec{raco distribute} command, supply a directory to
contain the combined files for a distribution. Each command-line
argument is an executable to include in the distribution, so multiple
executables can be packaged together. For example, on Windows,

@commandline{raco distribute greetings hello.exe goodbye.exe}

creates a directory @filepath{greetings} (if the directory doesn't
exist already), and it copies the executables @filepath{hello.exe} and
@filepath{goodbye.exe} into @filepath{greetings}. It also creates a
@filepath{lib} sub-directory in @filepath{greetings} to contain DLLs,
and it adjusts the copied @filepath{hello.exe} and
@filepath{goodbye.exe} to use the DLLs in @filepath{lib}.

The layout of files within a distribution directory is
platform-specific:

@itemize[

@item{On Windows, executables are put directly into the
      distribution directory, and DLLs and other run-time files go
      into a @filepath{lib} sub-directory.}

@item{On Mac OS X, GUI executables go into the distribution
      directory, other executables go into a @filepath{bin}
      subdirectory, and frameworks (i.e., shared libraries) go into a
      @filepath{lib} sub-directory along with other run-time files. As
      a special case, if the distribution has a single @DFlag{gui-exe}
      executable, then the @filepath{lib} directory is hidden inside
      the application bundle.}

@item{On Unix, executables go into a @filepath{bin} subdirectory,
      shared libraries (if any) go into a @filepath{lib} subdirectory
      along with other run-time files, and wrapped executables are
      placed into a @filepath{lib/plt} subdirectory with
      version-specific names. This layout is consistent with Unix
      installation conventions; the version-specific names for shared
      libraries and wrapped executables means that distributions can
      be safely unpacked into a standard place on target machines
      without colliding with an existing Racket installation or
      other executables created by @exec{raco exe}.}

]

A distribution also has a @filepath{collects} directory that is used
as the main library collection directory for the packaged executables.
By default, the directory is empty. Use the
@as-index{@DPFlag{collects-copy}} flag of @exec{raco distribute} to
supply a directory whose content is copied into the distribution's
@filepath{collects} directory. The @DPFlag{collects-copy} flag can be
used multiple times to supply multiple directories.

When multiple executables are distributed together, then separately
creating the executables with @exec{raco exe} can generate multiple
copies of collection-based libraries that are used by multiple
executables. To share the library code, instead, specify a target
directory for library copies using the
@as-index{@DFlag{collects-dest}} flag with @exec{raco exe}, and
specify the same directory for each executable (so that the set of
libraries used by all executables are pooled together). Finally, when
packaging the distribution with @exec{raco distribute}, use the
@DPFlag{collects-copy} flag to include the copied libraries in the
distribution.


@; ----------------------------------------------------------------------

@include-section["dist-api.scrbl"]
@include-section["bundle-api.scrbl"]
