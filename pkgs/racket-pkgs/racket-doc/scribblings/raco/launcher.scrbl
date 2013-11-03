#lang scribble/doc
@(require scribble/manual
          "common.rkt"
          (for-label racket/base racket/unit racket/contract
                     launcher/launcher
                     launcher/launcher-sig
                     launcher/launcher-unit
                     compiler/embed
                     racket/gui/base))

@title[#:tag "launcher"]{Installation-Specific Launchers}

A @deftech{launcher} is similar to a stand-alone executable, but a
launcher is usually smaller and can be created more quickly, because
it depends permanently on the local Racket installation and the
program's sources. In the case of Unix, a launcher is simply a shell
script that runs @exec{racket} or @exec{gracket}. Launchers
@emph{cannot} be packaged into a distribution using @exec{raco
distribute}. The @exec{raco exe} command creates a launcher when the
@Flag{l} or @DFlag{launcher} flag is specified.

@defmodule[launcher/launcher]

The @racketmodname[launcher/launcher] library provides functions for
creating @tech{launchers}.

@section{Creating Launchers}

@defproc[(make-gracket-launcher [args (listof string?)]
                                [dest path-string?]
                                [aux (listof (cons/c symbol? any/c)) null])
         void?]{

Creates the launcher @racket[dest], which starts GRacket with the
command-line arguments specified as strings in @racket[args]. Extra
arguments passed to the launcher at run-time are appended (modulo
special Unix/X flag handling, as described below) to this list and
passed on to GRacket. If @racket[dest] exists already, as either a file
or directory, it is replaced.

The optional @racket[aux] argument is an association list for
platform-specific options (i.e., it is a list of pairs where the first
element of the pair is a key symbol and the second element is the
value for that key). See also @racket[build-aux-from-path]. See
@racket[create-embedding-executable] for a list that applies to both
stand-alone executables and launchers on Windows and Mac OS X GRacket;
the following additional associations apply to launchers:

@itemize[

 @item{@racket['independent?] (Windows) --- a boolean; @racket[#t]
       creates an old-style launcher that work with any
       Racket or GRacket binary, like @exec{setup-plt.exe}. No other
       @racket[aux] associations are used for an old-style launcher.}

 @item{@racket['exe-name] (Mac OS X, @racket['script-3m] or
       @racket['script-cgc] variant) --- provides the base name for a
       @racket['3m]-/@racket['cgc]-variant launcher, which the script
       will call ignoring @racket[args]. If this name is not provided,
       the script will go through the GRacket executable as usual.}

 @item{@racket['exe-is-gracket] (when @racket['exe-name] is used) ---
       indicates that @racket['exe-name] refers to the GRacket
       executable, which is potentially in a @filepath{lib}
       subdirectory instead of with other GUI applications.}

 @item{@racket['relative?] (all platforms) --- a boolean, where
        @racket[#t] means that the generated launcher should find the
        base GRacket executable through a relative path.}

 @item{@racket['install-mode] (Windows, Unix) --- either
       @racket['user] or @racket['main], indicates that the launcher
       is being installed to a user-specific place or to an
       installation-wide place, which in turn determines where to
       record @racket['start-menu], @racket['extension-registry],
       and/or @racket['desktop] information.}

 @item{@racket['start-menu] (Windows) --- a boolean or real number;
       @racket[#t] indicates that the launcher should be in the
       @onscreen{Start} menu by an installer that includes the
       launcher. A number value is treated like @racket[#t], but also
       requests that the installer automatically start the
       application, where the number determines a precedence relative
       to other launchers that may request starting.  A
       @racket['start-menu] value is used only when
       @racket['install-mode] is also specified.}

 @item{@racket['extension-register] (Windows) --- a list of document
       types for file-extension registrations to be performed by an
       installer. Each document type is described by a list of six
       items:

         @itemlist[

            @item{a human-readable string describing the document
                  type, such as @racket["Racket Document"];}

            @item{a string to use as a key for the document type,
                  such as @racket["Racket.Document"];}

            @item{a list of strings, where each string is a file
                  extension without the dot, such as @racket['("rkt"
                  "rktl" "rktd")];}

            @item{a path to a file that supplies the icon, such as
                  @racket["doc.ico"];}

            @item{a string to represent the command line to handle a
                  document with a matching extension, such as
                  @racket["\"%1\""], where the string will be prefixed
                  with a path to the launcher, and where @litchar{%1}
                  will be replaced with the document path}
          ]

       An @racket['extension-registry] value is used only when
       @racket['install-mode] is also specified.}

 @item{@racket['desktop] (Unix) --- a string containing the content of
       a @filepath{.desktop} file for the launcher, where @tt{Exec}
       and @tt{Icon} entries are added automatically. If an @tt{Exec}
       entry exists in the string, and if its value starts with a
       non-empty sequence of alpha-numeric ASCII characters followed
       by a space, then the space and remainder of the value is
       appended to the automatically generated value.
       The @filepath{.desktop} file is written to the directory produced by
       @racket[(find-apps-dir)] or @racket[(find-user-apps-dir)]. A
       @racket['desktop] value is used only when
       @racket['install-mode] is also specified.}

  @item{@racket['png] (Unix) : An icon file path (suffix
        @filepath{.png}) to be referenced by a @filepath{.desktop}
        file (if any); a @racket['png] value takes precedence over a
        @racket['ico] value, but neither is used unless a
        @racket['desktop] value is also present.}

  @item{@racket['ico] (Unix, in addition to more general Windows use)
        : An icon file path (suffix @filepath{.ico}) that is used in
        the same way as @racket['png] if no @racket['png] value is
        available.}

]

For Unix/X, the script created by @racket[make-mred-launcher] detects
and handles X Windows flags specially when they appear as the initial
arguments to the script. Instead of appending these arguments to the
end of @racket[args], they are spliced in after any X Windows flags
already listed in @racket[args]. The remaining arguments (i.e.,
all script flags and arguments after the last X Windows flag or
argument) are then appended after the spliced @racket[args].}


@defproc[(make-racket-launcher [args (listof string?)]
                               [dest path-string?]
                               [aux (listof (cons/c symbol? any/c)) null])
         void?]{

Like @racket[make-gracket-launcher], but for starting Racket. On Mac
OS X, the @racket['exe-name] @racket[aux] association is ignored.}


@defproc[(make-gracket-program-launcher [file string?]
                                        [collection string?]
                                        [dest path-string?])
         void?]{

Calls @racket[make-gracket-launcher] with arguments that start the
GRacket program implemented by @racket[file] in @racket[collection]:
@racket[(list "-l-" (string-append collection "/" file))]. The
@racket[_aux] argument to @racket[make-gracket-launcher] is generated
by stripping the suffix (if any) from @racket[file], adding it to the
path of @racket[collection], and passing the result to
@racket[build-aux-from-path].}


@defproc[(make-racket-program-launcher [file string?]
                                       [collection string?]
                                       [dest path-string?])
        void?]{

Like @racket[make-gracket-program-launcher], but for
@racket[make-racket-launcher].}


@defproc[(install-gracket-program-launcher [file string?]
                                          [collection string?]
                                          [name string?])
         void?]{

Same as 

@racketblock[
(make-gracket-program-launcher 
 file collection
 (gracket-program-launcher-path name))
]}

@defproc[(install-racket-program-launcher [file string?]
                                          [collection string?]
                                          [name string?])
         void?]{

Same as 

@racketblock[
(make-racket-program-launcher 
 file collection
 (racket-program-launcher-path name))
]}


@deftogether[(
@defproc[(make-mred-launcher [args (listof string?)]
                             [dest path-string?]
                             [aux (listof (cons/c symbol? any/c)) null])
         void?]
@defproc[(make-mred-program-launcher [file string?]
                                     [collection string?]
                                     [dest path-string?])
         void?]
@defproc[(install-mred-program-launcher [file string?]
                                        [collection string?]
                                        [name string?])
         void?]
)]{

Backward-compatible version of @racket[make-gracket-launcher], etc.,
that adds @racket["-I" "scheme/gui/init"] to the start of the
command-line arguments.}

@deftogether[(
@defproc[(make-mzscheme-launcher [args (listof string?)]
                                 [dest path-string?]
                                 [aux (listof (cons/c symbol? any/c)) null])
         void?]
@defproc[(make-mzscheme-program-launcher [file string?]
                                         [collection string?]
                                         [dest path-string?])
        void?]
@defproc[(install-mzscheme-program-launcher [file string?]
                                            [collection string?]
                                            [name string?])
         void?]
)]{

Backward-compatible version of @racket[make-racket-launcher], etc.,
that adds @racket["-I" "scheme/init"] to the start of the command-line
arguments.}

@; ----------------------------------------------------------------------

@section{Launcher Path and Platform Conventions}

@defproc[(gracket-program-launcher-path [name string?]
                                        [#:user? user? any/c #f]) 
         path?]{

Returns a pathname for an executable called something like @racket[name]
in the Racket installation (if @racket[user?] is @racket[#f]) or the
user's Racket executable directory (if @racket[user?] is @racket[#t]).
For Windows, the @filepath{.exe}
suffix is automatically appended to @racket[name]. For Unix,
@racket[name] is changed to lowercase, whitespace is changed to
@litchar{-}, and the path includes the @filepath{bin} subdirectory of
the Racket installation. For Mac OS X, the @filepath{.app} suffix
is appended to @racket[name].}


@defproc[(racket-program-launcher-path [name string?]
                                       [#:user? user? any/c #f])
         path?]{

Returns the same path as @racket[(gracket-program-launcher-path name #:user? user?)]
for Unix and Windows. For Mac OS X, the result is the same as for
Unix.}


@defproc[(gracket-launcher-is-directory?) boolean?]{

Returns @racket[#t] if GRacket launchers for the current platform are
directories from the user's perspective. For all currently supported
platforms, the result is @racket[#f].}


@defproc[(racket-launcher-is-directory?) boolean?]{

Like @racket[gracket-launcher-is-directory?], but for Racket
launchers.}


@defproc[(gracket-launcher-is-actually-directory?) boolean?]{

Returns @racket[#t] if GRacket launchers for the current platform are
implemented as directories from the filesystem's perspective. The
result is @racket[#t] for Mac OS X, @racket[#f] for all other
platforms.}


@defproc[(racket-launcher-is-actually-directory?) boolean?]{

Like @racket[gracket-launcher-is-actuall-directory?], but for Racket
launchers. The result is @racket[#f] for all platforms.}


@defproc[(gracket-launcher-add-suffix [path-string? path]) path?]{

Returns a path with a suitable executable suffix added, if it's not
present already.}

@defproc[(racket-launcher-add-suffix [path-string? path]) path?]{

Like @racket[gracket-launcher-add-suffix], but for Racket launchers.}


@defproc[(gracket-launcher-put-file-extension+style+filters)
         (values (or/c string? false/c)
                 (listof (one-of/c 'packages 'enter-packages))
                 (listof (list/c string? string?)))]{

Returns three values suitable for use as the @racket[extension],
@racket[style], and @racket[filters] arguments to @racket[put-file],
respectively.

If GRacket launchers for the current platform were directories form the
user's perspective, the @racket[style] result is suitable for use with
@racket[get-directory], and the @racket[extension] result may be a
string indicating a required extension for the directory name. }


@defproc[(racket-launcher-put-file-extension+style+filters)
         (values (or/c string? false/c)
                 (listof (one-of/c 'packages 'enter-packages))
                 (listof (list/c string? string?)))]{

Like @racket[gracket-launcher-get-file-extension+style+filters], but for
Racket launchers.}

@deftogether[(
@defproc[(mred-program-launcher-path [name string?] [#:user? user? any/c #f]) path?]
@defproc[(mred-launcher-is-directory?) boolean?]
@defproc[(mred-launcher-is-actually-directory?) boolean?]
@defproc[(mred-launcher-add-suffix [path-string? path]) path?]
@defproc[(mred-launcher-put-file-extension+style+filters)
         (values (or/c string? false/c)
                 (listof (one-of/c 'packages 'enter-packages))
                 (listof (list/c string? string?)))]
)]{

Backward-compatible aliases for
@racket[gracket-program-launcher-path], etc.}

@deftogether[(
@defproc[(mzscheme-program-launcher-path [name string?] [#:user? user? any/c #f]) path?]
@defproc[(mzscheme-launcher-is-directory?) boolean?]
@defproc[(mzscheme-launcher-is-actually-directory?) boolean?]
@defproc[(mzscheme-launcher-add-suffix [path-string? path]) path?]
@defproc[(mzscheme-launcher-put-file-extension+style+filters)
         (values (or/c string? false/c)
                 (listof (one-of/c 'packages 'enter-packages))
                 (listof (list/c string? string?)))]
)]{

Backward-compatible aliases for
@racket[racket-program-launcher-path], etc.}


@defproc[(installed-executable-path->desktop-path [exec-path path-string?] [user? any/c])
         (and/c path? complete-path?)]{

Returns a path for a @filepath{.desktop} file to describe the
installed executable at @racket[exec-path]. Only the filename part of
@racket[exec-path] is used. The @racket[user?] argument should be true
if @racket[exec-path] is installed in a user-specific location (in
which case the result path will also be user-specific).}


@defproc[(installed-desktop-path->icon-path [desktop-path path-string?]
                                            [user? any/c]
                                            [suffix bytes?])
         (and/c path? complete-path?)]{

Returns a path for an icon file to be referenced by the
@filepath{desktop} file at @racket[desktop-path]. Only the filename
part of @racket[desktop-path] is used. The @racket[user?] argument
should be true if @racket[desktop-path] is installed in a
user-specific location (in which case the result path will also be
user-specific).  The @racket[suffix] argument provides the icon-file
suffix, normally either @racket[#"png"] or @racket[#"ico"].}

@; ----------------------------------------------------------------------

@section{Launcher Configuration}

@defproc[(gracket-launcher-up-to-date? [dest path-string?]
                                    [aux (listof (cons/c symbol? any/c))])
         boolean?]{

Returns @racket[#t] if the GRacket launcher @racket[dest] does not need
to be updated, assuming that @racket[dest] is a launcher and its
arguments have not changed.}

@defproc[(racket-launcher-up-to-date? [dest path-string?]
                                        [aux (listof (cons/c symbol? any/c))])
         boolean?]{

Analogous to @racket[gracket-launcher-up-to-date?], but for a Racket
launcher.}

@defproc[(build-aux-from-path [path path-string?])
         (listof (cons/c symbol? any/c))]{

Creates an association list suitable for use with
@racket[make-gracket-launcher] or
@racket[create-embedding-executable].  It builds associations by
adding to @racket[path] suffixes, such as @filepath{.icns}, checking
whether such a file exists, and calling @racket[extract-aux-from-path]
if so. The results from all recognized suffixes are appended
together.}


@defproc[(extract-aux-from-path [path path-string?])
         (listof (cons/c symbol? any/c))]{

Creates an association list suitable for use with
@racket[make-gracket-launcher] or
@racket[create-embedding-executable].  It builds associations by
recognizing the suffix of @racket[path], where the recognized suffixes
are as follows:

@itemize[

 @item{@filepath{.icns} @'rarr @racket['icns] file for use on Mac
       OS X}

 @item{@filepath{.ico} @'rarr @racket['ico] file for use on
       Windows or Unix}

 @item{@filepath{.png} @'rarr @racket['png] file for use on
       Unix}

 @item{@filepath{.lch} @'rarr @racket['independent?] as @racket[#t]
       (the file content is ignored) for use on Windows}

 @item{@filepath{.creator} @'rarr @racket['creator] as the initial
       four characters in the file for use on Mac OS X}

 @item{@filepath{.filetypes} @'rarr @racket['file-types] as
       @racket[read] content (a single S-expression), and
       @racket['resource-files] as a list constructed by finding
       @racket["CFBundleTypeIconFile"] entries in @racket['file-types]
       (and filtering duplicates); for use on Mac OS X}

 @item{@filepath{.utiexports} @'rarr @racket['uti-exports] as
       @racket[read] content (a single S-expression); for use on
       Mac OS X}

 @item{@filepath{.wmclass} @'rarr @racket['wm-class] as the literal
       content, removing a trailing newline if any; for use on Unix}

 @item{@filepath{.desktop} @'rarr @racket['desktop] as the literal
       content; for use on Unix}

 @item{@filepath{.startmenu} @'rarr @racket['start-menu] as the file
       content if it @racket[read]s as a real number, @racket[#t]
       otherwise, for use on Windows}

 @item{@filepath{.extreg} @'rarr @racket['extension-register] as
       @racket[read] content (a single S-expression), but with
       relative (to the @filepath{.extreg} file) paths converted
       absolute; for use on Windows}

]}

@defparam[current-launcher-variant variant symbol?]{

A parameter that indicates a variant of Racket or GRacket to use for
launcher creation and for generating launcher names. The default is
the result of @racket[(system-type 'gc)]. On Unix and Windows, the
possibilities are @racket['cgc] and @racket['3m]. On Mac OS X, the
@racket['script-3m] and @racket['script-cgc] variants are also
available for GRacket launchers.}

@defproc[(available-gracket-variants) (listof symbol?)]{

Returns a list of symbols corresponding to available variants of GRacket
in the current Racket installation. The list normally includes at
least one of @racket['3m] or @racket['cgc]--- whichever is the result
of @racket[(system-type 'gc)]---and may include the other, as well as
@racket['script-3m] and/or @racket['script-cgc] on Mac OS X.}

@defproc[(available-racket-variants) (listof symbol?)]{

Returns a list of symbols corresponding to available variants of
Racket in the current Racket installation. The list normally
includes at least one of @racket['3m] or @racket['cgc]---whichever is
the result of @racket[(system-type 'gc)]---and may include the other.}

@deftogether[(
@defproc[(mred-launcher-up-to-date? [dest path-string?]
                                    [aux (listof (cons/c symbol? any/c))])
         boolean?]
@defproc[(mzscheme-launcher-up-to-date? [dest path-string?]
                                        [aux (listof (cons/c symbol? any/c))])
         boolean?]
@defproc[(available-mred-variants) (listof symbol?)]
@defproc[(available-mzscheme-variants) (listof symbol?)]
)]{
Backward-compatible aliases for
@racket[gracket-launcher-up-to-date?], etc.}


@; ----------------------------------------

@section{Launcher Creation Signature}

@defmodule[launcher/launcher-sig]

@defsignature/splice[launcher^ ()]{

Includes the identifiers provided by @racketmodname[launcher/launcher].}

@; ----------------------------------------

@section{Launcher Creation Unit}

@defmodule[launcher/launcher-unit]

@defthing[launcher@ unit?]{

A unit that imports nothing and exports @racket[launcher^].}
