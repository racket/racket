#lang scribble/doc
@(require "mz.rkt"
          (for-label framework/preferences
                     racket/runtime-path
                     launcher/launcher
                     setup/dirs))

@(define file-eval (make-base-eval))
@(interaction-eval #:eval file-eval (begin (require racket/file) (define filename (make-temporary-file))))


@title{Filesystem}

@;------------------------------------------------------------------------
@section[#:tag "findpaths"]{Locating Paths}

@defproc[(find-system-path [kind symbol?]) path?]{

Returns a machine-specific path for a standard type of path specified
by @racket[kind], which must be one of the following:

@itemize[

 @item{@indexed-racket['home-dir] --- the current @deftech{user's home
 directory}.

 On all platforms, if the @indexed-envvar{PLTUSERHOME} environment
 variable is defined as a @tech{complete} path, then the path is used
 as the user's home directory.

 On Unix and Mac OS X, when @envvar{PLTUSERHOME} does not apply,
 the user's home directory is determined by
 expanding the path @filepath{~}, which is expanded by first checking
 for a @indexed-envvar{HOME} environment variable. If none is defined,
 the @indexed-envvar{USER} and @indexed-envvar{LOGNAME} environment
 variables are consulted (in that order) to find a user name, and then
 system files are consulted to locate the user's home directory.

 On Windows, when @envvar{PLTUSERHOME} does not apply,
 the user's home directory is the user-specific profile
 directory as determined by the Windows registry. If the registry
 cannot provide a directory for some reason, the value of the
 @indexed-envvar{USERPROFILE} environment variable is used instead, as
 long as it refers to a directory that exists. If @envvar{USERPROFILE}
 also fails, the directory is the one specified by the
 @indexed-envvar{HOMEDRIVE} and @indexed-envvar{HOMEPATH} environment
 variables.  If those environment variables are not defined, or if the
 indicated directory still does not exist, the directory containing
 the current executable is used as the home directory.}

 @item{@indexed-racket['pref-dir] --- the standard directory for
 storing the current user's preferences. On Unix, the directory is
 @filepath{.racket} in the @tech{user's home directory}.  On Windows,
 it is @filepath{Racket} in the @tech{user's home directory} if
 determined by @envvar{PLTUSERHOME}, otherwise in the user's
 application-data folder as specified by the Windows registry; the
 application-data folder is usually @filepath{Application Data} in the
 user's profile directory. On Mac OS X, the preferences directory
 is @filepath{Library/Preferences} in the
 @tech{user's home directory}. The preferences directory might not exist.}

 @item{@indexed-racket['pref-file] --- a file that contains a
 symbol-keyed association list of preference values. The file's
 directory path always matches the result returned for
 @racket['pref-dir]. The file name is @filepath{racket-prefs.rktd} on Unix
 and Windows, and it is @filepath{org.racket-lang.prefs.rktd} on Mac OS
 X. The file's directory might not exist. See also
 @racket[get-preference].}

 @item{@indexed-racket['temp-dir] --- the standard directory for
 storing temporary files. On @|AllUnix|, this is the directory
 specified by the @indexed-envvar{TMPDIR} environment variable, if it
 is defined, otherwise it is the first path that exists among
 @filepath{/var/tmp}, @filepath{/usr/tmp}, and @filepath{/tmp}. On
 Windows, the result is the directory specified by the
 @indexed-envvar{TMP} or @indexed-envvar{TEMP} environment variable,
 if it is defined, otherwise it is the current directory.}

 @item{@indexed-racket['init-dir] --- the directory containing the
 initialization file used by the Racket executable.
 It is the same as the @tech{user's home directory}.}

 @item{@indexed-racket['init-file] --- the file loaded at start-up by
 the Racket executable. The directory part of the
 path is the same path as returned for @racket['init-dir].  The file
 name is platform-specific:

  @itemize[

  @item{@|AllUnix|: @indexed-file{.racketrc}}

  @item{Windows: @indexed-file{racketrc.rktl}}

  ]}

 @item{@indexed-racket['config-dir] --- a directory for
 the installation's configuration. This directory is specified by the
 @indexed-envvar{PLTCONFIGDIR} environment variable, and it can be
 overridden by the @DFlag{config} or @Flag{G} command-line flag.  If no
 environment variable or flag is specified, or if the value is not a
 legal path name, then this directory defaults to an
 @filepath{etc} directory relative to the current executable.
 If the result of @racket[(find-system-path 'config-dir)] is a
 relative path, it is relative to the current executable.
 The directory might not exist.}

 @item{@indexed-racket['addon-dir] --- a directory for
 user-specific Racket configuration, packages, and extension.
 This directory is specified by the
 @indexed-envvar{PLTADDONDIR} environment variable, and it can be
 overridden by the @DFlag{addon} or @Flag{A} command-line flag.  If no
 environment variable or flag is specified, or if the value is not a
 legal path name, then this directory defaults to
 @filepath{Library/Racket} in the @tech{user's home directory} on Mac
 OS X and @racket['pref-dir] otherwise.  The directory might not
 exist.}

 @item{@indexed-racket['doc-dir] --- the standard directory for
 storing the current user's documents. On Unix, it's
 the @tech{user's home directory}.  On Windows, it is the @tech{user's
 home directory} if determined by @envvar{PLTUSERHOME}, otherwise it
 is the user's documents folder as specified by the Windows registry;
 the documents folder is usually @filepath{My Documents} in the user's
 home directory.  On Mac OS X, it's the @filepath{Documents} directory
 in the @tech{user's home directory}.}

 @item{@indexed-racket['desk-dir] --- the directory for the current user's
 desktop. On Unix, it's the @tech{user's home directory}. On
 Windows, it is the @tech{user's home directory} if determined by
 @envvar{PLTUSERHOME}, otherwise it is the user's desktop folder as
 specified by the Windows registry; the desktop folder is usually
 @filepath{Desktop} in the user's home directory. On Mac OS X, it is
 @filepath{Desktop} in the @tech{user's home directory}}

 @item{@indexed-racket['sys-dir] --- the directory containing the
 operating system for Windows. On @|AllUnix|, the
 result is @racket["/"].}

 @item{@indexed-racket['exec-file] --- the path of the Racket
 executable as provided by the operating system for the current
 invocation. For some operating systems, the path can be relative.

 @margin-note{For GRacket, the executable path is the name of a GRacket
 executable.}}

 @item{@indexed-racket['run-file] --- the path of the current
  executable; this may be different from result for
  @racket['exec-file] because an alternate path was provided through a
  @DFlag{name} or @Flag{N} command-line flag to the Racket
  (or GRacket) executable, or because an embedding executable
  installed an alternate path. In particular a ``launcher'' script
  created by @racket[make-racket-launcher] sets this path to the
  script's path.}

 @item{@indexed-racket['collects-dir] --- a path to the main
 collection of libraries (see @secref["collects"]). If this path is
 relative, then it is relative to the executable as reported by
 @racket[(find-system-path 'exec-file)]---though the latter could be a
 soft-link or relative to the user's executable search path, so that
 the two results should be combined with
 @racket[find-executable-path].  The @racket['collects-dir] path is
 normally embedded in the Racket executable, but it can be
 overridden by the @DFlag{collects} or @Flag{X} command-line flag.}

 @item{@indexed-racket['orig-dir] --- the current directory at
 start-up, which can be useful in converting a relative-path result
 from @racket[(find-system-path 'exec-file)] or
 @racket[(find-system-path 'run-file)] to a complete path.}

 ]

@history[#:changed "6.0.0.3" @elem{Added @envvar{PLTUSERHOME}.}]}

@defproc[(path-list-string->path-list [str (or/c string? bytes?)]
                                      [default-path-list (listof path?)])
         (listof path?)]{

Parses a string or byte string containing a list of paths, and returns
a list of path strings. On @|AllUnix|, paths in a path list are
separated by a @litchar{:}; on Windows, paths are separated by a
@litchar{;}, and all @litchar{"}s in the string are discarded. Whenever the path 
list contains an empty path, the list
@racket[default-path-list] is spliced into the returned list of
paths. Parts of @racket[str] that do not form a valid path are not
included in the returned list.}


@defproc[(find-executable-path [program-sub path-string?]
                               [related-sub (or/c path-string? #f) #f]
                               [deepest? any/c #f]) 
         (or/c path? #f)]{

Finds a path for the executable @racket[program-sub], returning
@racket[#f] if the path cannot be found.

If @racket[related-sub] is not @racket[#f], then it must be a relative
path string, and the path found for @racket[program-sub] must be such
that the file or directory @racket[related-sub] exists in the same
directory as the executable. The result is then the full path for the
found @racket[related-sub], instead of the path for the executable.
 
This procedure is used by the Racket executable to find the
standard library collection directory (see @secref["collects"]).  In
this case, @racket[program] is the name used to start Racket and
@racket[related] is @racket["collects"].  The @racket[related-sub]
argument is used because, on @|AllUnix|, @racket[program-sub] may
involve a sequence of soft links; in this case,
@racket[related-sub] determines which link in the chain is relevant.

If @racket[related-sub] is not @racket[#f], then when
@racket[find-executable-path] does not find a @racket[program-sub]
that is a link to another file path, the search can continue with the
destination of the link. Further links are inspected until
@racket[related-sub] is found or the end of the chain of links is
reached. If @racket[deepest?] is @racket[#f] (the default), then the
result corresponds to the first path in a chain of links for which
@racket[related-sub] is found (and further links are not actually
explored); otherwise, the result corresponds to the last link in the
chain for which @racket[related-sub] is found.

If @racket[program-sub] is a pathless name,
@racket[find-executable-path] gets the value of the
@indexed-envvar{PATH} environment variable; if this environment
variable is defined, @racket[find-executable-path] tries each path in
@envvar{PATH} as a prefix for @racket[program-sub] using the search
algorithm described above for path-containing
@racket[program-sub]s. If the @envvar{PATH} environment variable is
not defined, @racket[program-sub] is prefixed with the current
directory and used in the search algorithm above. (On Windows, the
current directory is always implicitly the first item in
@envvar{PATH}, so @racket[find-executable-path] checks the current
directory first on Windows.)}

@;------------------------------------------------------------------------
@section[#:tag "fileutils"]{Files}

@defproc[(file-exists? [path path-string?]) boolean?]{

Returns @racket[#t] if a file (not a directory) @racket[path] exists,
@racket[#f] otherwise.

On Windows, @racket[file-exists?]  reports @racket[#t] for all
variations of the special filenames (e.g., @racket["LPT1"],
@racket["x:/baddir/LPT1"]).}

@defproc[(link-exists? [path path-string?]) boolean?]{

Returns @racket[#t] if a link @racket[path] exists,
@racket[#f] otherwise.

The predicates @racket[file-exists?]  or @racket[directory-exists?]
work on the final destination of a link or series of links, while
@racket[link-exists?]  only follows links to resolve the base part of
@racket[path] (i.e., everything except the last name in the
path).

This procedure never raises the @racket[exn:fail:filesystem]
exception.

@history[#:changed "6.0.1.12" @elem{Added support for links on Windows.}]}


@defproc[(delete-file [path path-string?]) void?]{

Deletes the file with path @racket[path] if it exists, otherwise the
@exnraise[exn:fail:filesystem]. If @racket[path] is a link, the link
is deleted rather than the destination of the link.}

@defproc[(rename-file-or-directory [old path-string?]
                                   [new path-string?]
                                   [exists-ok? any/c #f]) 
         void?]{
 
Renames the file or directory with path @racket[old]---if it
exists---to the path @racket[new]. If the file or directory is not
renamed successfully, the @exnraise[exn:fail:filesystem].

This procedure can be used to move a file/directory to a different
directory (on the same disk) as well as rename a file/directory within
a directory. Unless @racket[exists-ok?]  is provided as a true value,
@racket[new] cannot refer to an existing file or directory. Even if
@racket[exists-ok?] is true, @racket[new] cannot refer to an existing
file when @racket[old] is a directory, and vice versa.

If @racket[new] exists and is replaced, the replacement is atomic
on Unix and Mac OS X, but it is not guaranteed to be atomic on
Windows. Furthermore, if @racket[new] exists and is opened by any
process for reading or writing, then attempting to replace it will
typically fail on Windows. See also @racket[call-with-atomic-output-file].

If @racket[old] is a link, the link is renamed rather than the
destination of the link, and it counts as a file for replacing any
existing @racket[new].}


@defproc[(file-or-directory-modify-seconds [path path-string?]
                                           [secs-n (or/c exact-integer? #f) #f]
                                           [fail-thunk (-> any) (lambda () (raise (make-exn:fail:filesystem ....)))])
         any]{

@index['("file modification date and time")]{Returns}
the file or directory's last modification date in seconds
since midnight UTC, January 1, 1970 (see also @secref["time"]) when
@racket[secs-n] is not provided or is @racket[#f].

For FAT filesystems on Windows, directories do not have modification
dates. Therefore, the creation date is returned for a directory, but
the modification date is returned for a file.

If @racket[secs-n] is provided and not @racket[#f], the access and
modification times of @racket[path] are set to the given time.

On error (e.g., if no such file exists), @racket[fail-thunk] is
called, and the default @racket[fail-thunk] raises
@racket[exn:fail:filesystem].}


@defproc*[([(file-or-directory-permissions [path path-string?] [mode #f #f]) (listof (or/c 'read 'write 'execute))]
           [(file-or-directory-permissions [path path-string?] [mode 'bits]) (integer-in 0 #xFFFF)]
           [(file-or-directory-permissions [path path-string?] [mode (integer-in 0 #xFFFF)]) void])]{

When given one argument or @racket[#f] as the second argument, returns
a list containing @indexed-racket['read], @indexed-racket['write],
and/or @indexed-racket['execute] to indicate permission the given file
or directory path by the current user and group. On @|AllUnix|,
permissions are checked for the current effective user instead of the
real user.

If @racket['bits] is supplied as the second argument, the result is a
platform-specific integer encoding of the file or directory properties
(mostly permissions), and the result is independent of the current
user and group. The lowest nine bits of the encoding are somewhat
portable, reflecting permissions for the file or directory's owner,
members of the file or directory's group, or other users:

@itemlist[
 @item{@racketvalfont{#o100} : owner has read permission}
 @item{@racketvalfont{#o200} : owner has write permission}
 @item{@racketvalfont{#o400} : owner has execute permission}
 @item{@racketvalfont{#o010} : group has read permission}
 @item{@racketvalfont{#o020} : group has write permission}
 @item{@racketvalfont{#o040} : group has execute permission}
 @item{@racketvalfont{#o001} : others have read permission}
 @item{@racketvalfont{#o002} : others have write permission}
 @item{@racketvalfont{#o004} : others have execute permission}
]

See also @racket[user-read-bit], etc. On Windows, permissions from
all three (owner, group, and others) are always the same, and read and
execute permission are always available. On @|AllUnix|,
higher bits have a platform-specific meaning.

If an integer is supplied as the second argument, its is used as an
encoding of properties (mostly permissions) to install for the file.

In all modes, the @exnraise[exn:fail:filesystem] on error (e.g., if no
such file exists).}


@defproc[(file-or-directory-identity [path path-string?]
                                     [as-link? any/c #f])
         exact-positive-integer?]{

@index['("inode")]{Returns} a number that represents the identity of
@racket[path] in terms of the device and file or directory that it
accesses. This function can be used to check whether two paths
correspond to the same filesystem entity under the assumption that the
path's entity selection does not change.

If @racket[as-link?] is a true value, then if @racket[path] refers to
a filesystem link, the identity of the link is returned instead of the
identity of the referenced file or directory (if any).}


@defproc[(file-size [path path-string?]) exact-nonnegative-integer?]{

Returns the (logical) size of the specified file in bytes. On Mac
OS X, this size excludes the resource-fork size. On error (e.g., if no
such file exists), the @exnraise[exn:fail:filesystem].}


@defproc[(copy-file [src path-string?] [dest path-string?] [exists-ok? any/c #f]) void?]{

Creates the file @racket[dest] as a copy of @racket[src], if
@racket[dest] does not already exist. If @racket[dest] already exists
and @racket[exists-ok?] is @racket[#f], the copy fails with
@exnraise[exn:fail:filesystem:exists?]; otherwise, if @racket[dest]
exists, its content is replaced with the content of @racket[src]. File
permissions are transferred from @racket[src] to @racket[dest]; on Windows,
the modification time of @racket[src] is also transferred to @racket[dest]. If
@racket[src] refers to a link, the target of the link is copied,
rather than the link itself; if @racket[dest] refers to a link and
@racket[exists-ok?] is true, the target of the link is updated.}


@defproc[(make-file-or-directory-link [to path-string?] [path path-string?]) 
         void?]{

Creates a link @racket[path] to @racket[to]. The
creation will fail if @racket[path] already exists. The @racket[to]
need not refer to an existing file or directory, and @racket[to] is
not expanded before writing the link. If the link is not created
successfully,the @exnraise[exn:fail:filesystem].

On Windows XP and earlier, the @exnraise[exn:fail:unsupported]. On
later versions of Windows, the creation of links tends to be
disallowed by security policies. Furthermore, a relative-path link is
parsed specially; see @secref["windowspaths"] for more information.

@history[#:changed "6.0.1.12" @elem{Added support for links on Windows.}]}

@;------------------------------------------------------------------------
@section[#:tag "directories"]{Directories}

See also: @racket[rename-file-or-directory],
@racket[file-or-directory-modify-seconds],
@racket[file-or-directory-permissions].

@defparam*[current-directory path path-string? (and/c path? complete-path?)]{

A @tech{parameter} that determines the current directory for resolving
relative paths.

When the parameter procedure is called to set the current directory,
the path argument is @tech{cleanse}d using @racket[cleanse-path],
simplified using @racket[simplify-path], and then converted to a
directory path with @racket[path->directory-path]; cleansing and
simplification raise an exception if the path is ill-formed. Thus, the
current value of @racket[current-directory] is always a cleansed,
simplified, complete, directory path.

The path is not checked for existence when the parameter is set.

On Unix and Mac OS X, the initial value of the parameter for a Racket
process is taken from the @indexed-envvar{PWD} environment
variable---if the value of the environment variable identifies the
same directory as the operating system's report of the current
directory.}

@defparam*[current-directory-for-user path path-string? (and/c path? complete-path?)]{

Like @racket[current-directory], but use only by
@racket[srcloc->string] for reporting paths relative to a
directory.

Normally, @racket[current-directory-for-user] should stay at its
initial value, reflecting the directory where a user started a
process. A tool such as DrRacket, however, implicitly lets a user
select a directory (for the file being edited), in which case updating 
@racket[current-directory-for-user] makes sense.}


@defproc[(current-drive) path?]{

Returns the current drive name Windows. For other platforms, the
@exnraise[exn:fail:unsupported]. The current drive is always the drive
of the current directory.}


@defproc[(directory-exists? [path path-string?]) boolean?]{

Returns @racket[#t] if @racket[path] refers to a directory,
@racket[#f] otherwise.}

@defproc[(make-directory [path path-string?]) void?]{

Creates a new directory with the path @racket[path].  If the directory
is not created successfully, the @exnraise[exn:fail:filesystem].}


@defproc[(delete-directory [path path-string?]) void?]{

Deletes an existing directory with the path @racket[path]. If the
directory is not deleted successfully, the
@exnraise[exn:fail:filesystem].}


@defproc[(directory-list [path path-string? (current-directory)]
                         [#:build? build? any/c #f])
         (listof path?)]{

@margin-note{See also the @racket[in-directory] sequence constructor.}

Returns a list of all files and directories in the directory specified
by @racket[path]. If @racket[build?] is @racket[#f], the resulting
paths are all @tech{path elements}; otherwise, the individual results
are combined with @racket[path] using @racket[build-path].
On Windows, an element of the result list may start with
@litchar{\\?\REL\\}.

The resulting paths are always sorted using @racket[path<?].}


@defproc[(filesystem-root-list) (listof path?)]{

Returns a list of all current root directories. Obtaining this list
can be particularly slow on Windows.}

@;------------------------------------------------------------------------
@section[#:tag "filesystem-change"]{Detecting Filesystem Changes}

Many operating systems provide notifications for filesystem changes,
and those notifications are reflected in Racket by @tech{filesystem
change events}.

@defproc[(filesystem-change-evt? [v any/c]) boolean?]{

Returns @racket[#f] if @racket[v] is a @tech{filesystem change
event}, @racket[#f] otherwise.}


@defproc*[([(filesystem-change-evt [path path-string?])
            filesystem-change-evt?]
           [(filesystem-change-evt [path path-string?]
                                   [failure-thunk (-> any)])
            any])]{

Creates a @deftech{filesystem change event}, which is a
@tech{synchronizable event} that becomes @tech{ready for
synchronization} after a change to @racket[path]:

@itemlist[

 @item{If @racket[path] refers to a file, the event becomes
       @tech{ready for synchronization} when the file's content or
       attributes change, or when the file is deleted.}

 @item{If @racket[path] refers to a directory, the event becomes
       @tech{ready for synchronization} if a file or subdirectory is
       added, renamed, or removed within the directory.}

]

The event also becomes @tech{ready for synchronization} if
it is passed to @racket[filesystem-change-evt-cancel].

Finally, depending on the precision of information available from the
operating system, the event may become @tech{ready for
synchronization} under other circumstances. For example, on
Windows, an event for a file becomes ready when any file changes
within in the same directory as the file.

After a @tech{filesystem change event} becomes @tech{ready for
synchronization}, it stays @tech{ready for synchronization}. The
event's @tech{synchronization result} is the event itself.

If the current platform does not support filesystem-change
notifications, then the @exnraise[exn:fail:unsupported] if
@racket[failure-thunk] is not provided, or @racket[failure-thunk] is
called in tail position if provided. Similarly, if there is any
operating-system error when creating the event (such as a non-existent
file), then the @exnraise[exn:fail:filesystem] or @racket[failure-thunk]
is called.

Creation of a @tech{filesystem change event} alloates resources at the
operating-system level. The resources are released at latest when the
event is sychronized and @tech{ready for synchronization} or when the
event is canceled with @racket[filesystem-change-evt-cancel].
See also @racket[system-type] in @racket['fs-change] mode.

A @tech{filesystem change event} is placed under the management of the
@tech{current custodian} when it is created. If the @tech{custodian}
is shut down, @racket[filesystem-change-evt-cancel] is applied to the
event.}


@defproc[(filesystem-change-evt-cancel [evt filesystem-change-evt?])
         void?]{

Causes @racket[evt] to become immediately @tech{ready for
synchronization}, whether it was ready or before not, and releases and
resources (at the operating-system level) for tracking filesystem
changes.}


@;------------------------------------------------------------------------
@section[#:tag "runtime-path"]{Declaring Paths Needed at Run Time}

@note-lib-only[racket/runtime-path]

The @racketmodname[racket/runtime-path] library provides forms for
accessing files and directories at run time using a path that are
usually relative to an enclosing source file. Unlike using
@racket[collection-path], @racket[define-runtime-path] exposes each
run-time path to tools like the executable and distribution creators,
so that files and directories needed at run time are carried along in
a distribution.

In addition to the bindings described below,
@racketmodname[racket/runtime-path] provides @racket[#%datum] in
@tech{phase level} 1, since string constants are often used as
compile-time expressions with @racket[define-runtime-path].

@defform[(define-runtime-path id expr)]{

Uses @racket[expr] as both a compile-time (i.e., @tech{phase} 1)
expression and a run-time (i.e., @tech{phase} 0) expression. In either
context, @racket[expr] should produce a path, a string that represents
a path, a list of the form @racket[(list 'lib _str ...+)], or a list
of the form @racket[(list 'so _str)] or @racket[(list 'so _str _vers)].

For run time, @racket[id] is bound to a path that is based on the
result of @racket[expr]. The path is normally computed by taking a
relative path result from @racket[expr] and adding it to a path for
the enclosing file (which is computed as described below). However,
tools like the executable creator can also arrange (by colluding with
@racketmodname[racket/runtime-path]) to have a different base path
substituted in a generated executable. If @racket[expr] produces an
absolute path, it is normally returned directly, but again may be
replaced by an executable creator. In all cases, the executable
creator preserves the relative locations of all paths within a given
@tech{package} (treating paths outside of any package as being together).
When @racket[expr] produces a relative or absolute path, then the path
bound to @racket[id] is always an absolute path.

If @racket[expr] produces a list of the form @racket[(list 'lib _str
...+)], the value bound to @racket[id] is an absolute path. The path
refers to a collection-based file similar to using the value as a
@tech{module path}.

If @racket[expr] produces a list of the form @racket[(list 'so _str)]
or @racket[(list 'so _str _vers)],
the value bound to @racket[id] can be either @racket[_str] or an
absolute path; it is an absolute path when searching in the
Racket-specific shared-object library directories (as determined by
@racket[get-lib-search-dirs]) locates the path. In this way, shared-object
libraries that are installed specifically for Racket get carried
along in distributions. The search tries each directory in order;
within a directory, the search tries using @racket[_str] directly,
then it tries adding each version specified by @racket[_vers]---which defaults
to @racket['(#f)]---along with
a platform-specific shared-library extension---as produced by
@racket[(system-type 'so-suffix)]. A @racket[_vers]
can be a string, or it can be a list of strings and @racket[#f].

If @racket[expr] produces a list of the form @racket[(list 'module
_module-path _var-ref)] or @racket[(list 'so _str (list
_str-or-false ...))], the value bound to @racket[id] is a
@tech{module path index}, where @racket[_module-path] is treated as
relative (if it is relative) to the module that is the home of the
@tech{variable reference} @racket[_var-ref], where @racket[_var-ref]
can be @racket[#f] if @racket[_module-path] is absolute. In an
executable, the corresponding module is carried along, including all
of its dependencies.

For compile-time, the @racket[expr] result is used by an executable
creator---but not the result when the containing module is
compiled. Instead, @racket[expr] is preserved in the module as a
compile-time expression (in the sense of
@racket[begin-for-syntax]). Later, at the time that an executable is
created, the compile-time portion of the module is executed (again),
and the result of @racket[expr] is the file to be included with the
executable. The reason for the extra compile-time execution is that
the result of @racket[expr] might be platform-dependent, so the result
should not be stored in the (platform-independent) bytecode form of
the module; the platform at executable-creation time, however, is the
same as at run time for the executable. Note that @racket[expr] is
still evaluated at run-time; consequently, avoid procedures like
@racket[collection-path], which depends on the source installation,
and instead use relative paths and forms like @racket[(list 'lib _str
...+)].

If a path is needed only on some platforms and not on others, use
@racket[define-runtime-path-list] with an @racket[expr] that produces an
empty list on platforms where the path is not needed.

Beware that @racket[define-runtime-path] in a @tech{phase level} other
than 0 does not cooperate properly with an executable creator. To work
around that limitation, put @racket[define-runtime-path] in a separate
module---perhaps a @tech{submodule} created by @racket[module]---then
export the definition, and then the module containing the definition
can be @racket[require]d into any phase level. Using
@racket[define-runtime-path] in a @tech{phase level} other than 0
logs a warning at expansion time.

The enclosing path for a @racket[define-runtime-path] is determined as
follows from the @racket[define-runtime-path] syntactic form:

@itemize[

 @item{If the form has a source module according to
       @racket[syntax-source-module], then the source location is
       determined by preserving the original expression as a syntax
       object, extracting its source module path at run time (again
       using @racket[syntax-source-module]), and then resolving the
       resulting module path index.}

 @item{If the expression has no source module, the
       @racket[syntax-source] location associated with the form is
       used, if is a string or path.}

 @item{If no source module is available, and @racket[syntax-source]
       produces no path, then @racket[current-load-relative-directory]
       is used if it is not @racket[#f]. Finally,
       @racket[current-directory] is used if all else fails.}

]

In the latter two cases, the path is normally preserved in
(platform-specific) byte form, but if the enclosing path corresponds to a
result of @racket[collection-file-path], then the path is record as
relative to the corresponding module path.

@history[#:changed "6.0.1.6" @elem{Preserve relative paths only within a package.}]

Examples:

@racketblock[
(code:comment @#,t{Access a file @filepath{data.txt} at run-time that is originally})
(code:comment @#,t{located in the same directory as the module source file:})
(define-runtime-path data-file "data.txt")
(define (read-data) 
  (with-input-from-file data-file 
    (lambda () 
      (read-bytes (file-size data-file)))))

(code:comment @#,t{Load a platform-specific shared object (using @racket[ffi-lib])})
(code:comment @#,t{that is located in a platform-specific sub-directory of the})
(code:comment @#,t{module's source directory:})
(define-runtime-path libfit-path
  (build-path "compiled" "native" (system-library-subpath #f)
              (path-replace-suffix "libfit" 
                                   (system-type 'so-suffix))))
(define libfit (ffi-lib libfit-path))

(code:comment @#,t{Load a platform-specific shared object that might be installed})
(code:comment @#,t{as part of the operating system, or might be installed})
(code:comment @#,t{specifically for Racket:})
(define-runtime-path libssl-so
  (case (system-type)
    [(windows) '(so "ssleay32")]
    [else '(so "libssl")]))
(define libssl (ffi-lib libssl-so))
]}


@defform[(define-runtime-paths (id ...) expr)]{

Like @racket[define-runtime-path], but declares and binds multiple
paths at once. The @racket[expr] should produce as many values as
@racket[id]s.}


@defform[(define-runtime-path-list id expr)]{

Like @racket[define-runtime-path], but @racket[expr] should produce a
list of paths.}


@defform[(define-runtime-module-path-index id module-path-expr)]{

Similar to @racket[define-runtime-path], but @racket[id] is bound to a
@tech{module path index} that encapsulates the result of
@racket[module-path-expr] relative to the enclosing module.

Use @racket[define-runtime-module-path-index] to bind a module path that is
passed to a reflective function like @racket[dynamic-require] while
also creating a module dependency for building and distributing
executables.}


@defform[(runtime-require module-path)]{

Similar to @racket[define-runtime-module-path-index], but creates the
distribution dependency without binding a module path index. When
@racket[runtime-require] is used multiple times within a module with
the same @racket[module-path], all but the first use expands to an
empty @racket[begin].}


@defform[(define-runtime-module-path id module-path)]{

Similar to @racket[define-runtime-path], but @racket[id] is bound to a
@tech{resolved module path}. The @tech{resolved module path} for
@racket[id] corresponds to @racket[module-path] (with the same syntax
as a module path for @racket[require]), which can be relative to the
enclosing module.

The @racket[define-runtime-module-path-index] form is usually
preferred, because it creates a weaker link to the referenced module.
Unlike @racket[define-runtime-module-path-index], the
@racket[define-runtime-module-path] form creates a @racket[for-label]
dependency from an enclosing module to @racket[module-path]. Since the
dependency is merely @racket[for-label], @racket[module-path] is not
@tech{instantiate}d or @tech{visit}ed when the enclosing module is
@tech{instantiate}d or @tech{visit}ed (unless such a dependency is
created by other @racket[require]s), but the code for the referenced
module is loaded when the enclosing module is loaded.}


@defform[(runtime-paths module-path)]{

This form is mainly for use by tools such as executable builders. It
expands to a quoted list containing the run-time paths declared by
@racket[module-path], returning the compile-time results of the
declaration @racket[expr]s, except that paths are converted to byte
strings. The enclosing module must require (directly or indirectly)
the module specified by @racket[module-path], which is an unquoted
module path. The resulting list does @emph{not} include module paths
bound through @racket[define-runtime-module-path].}

@;------------------------------------------------------------------------
@section[#:tag "file-lib"]{More File and Directory Utilities}

@note-lib[racket/file]

@defproc[(file->string [path path-string?]
                       [#:mode mode-flag (or/c 'binary 'text) 'binary])
         string?]{

Reads all characters from @racket[path] and returns them as a string.
The @racket[mode-flag] argument is the same as for
@racket[open-input-file].}

@defproc[(file->bytes [path path-string?]
                      [#:mode mode-flag (or/c 'binary 'text) 'binary])
         bytes?]{

Reads all characters from @racket[path] and returns them as a
@tech{byte string}.  The @racket[mode-flag] argument is the same as
for @racket[open-input-file].}

@defproc[(file->value [path path-string?]
                      [#:mode mode-flag (or/c 'binary 'text) 'binary])
         any]{

Reads a single S-expression from @racket[path] using @racket[read].
The @racket[mode-flag] argument is the same as for
@racket[open-input-file].}

@defproc[(file->list [path path-string?]
                     [proc (input-port? . -> . any/c) read]
                     [#:mode mode-flag (or/c 'binary 'text) 'binary])
         (listof any/c)]{
Repeatedly calls @racket[proc] to consume the contents of
@racket[path], until @racket[eof] is produced. The @racket[mode-flag]
argument is the same as for @racket[open-input-file].  }

@defproc[(file->lines [path path-string?]
                      [#:mode mode-flag (or/c 'binary 'text) 'binary]
                      [#:line-mode line-mode (or/c 'linefeed 'return 'return-linefeed 'any 'any-one) 'any])
         (listof string?)]{

Read all characters from @racket[path], breaking them into lines. The
@racket[line-mode] argument is the same as the second argument to
@racket[read-line], but the default is @racket['any] instead of
@racket['linefeed]. The @racket[mode-flag] argument is the same as for
@racket[open-input-file].}

@defproc[(file->bytes-lines [path path-string?]
                            [#:mode mode-flag (or/c 'binary 'text) 'binary]
                            [#:line-mode line-mode (or/c 'linefeed 'return 'return-linefeed 'any 'any-one) 'any])
         (listof bytes?)]{

Like @racket[file->lines], but reading bytes and collecting them into
lines like @racket[read-bytes-line].}

@defproc[(display-to-file [v any/c]
                          [path path-string?]
                      [#:mode mode-flag (or/c 'binary 'text) 'binary]
                      [#:exists exists-flag (or/c 'error 'append 'update
                                                  'replace 'truncate 'truncate/replace) 'error])
         void?]{

Uses @racket[display] to print @racket[v] to @racket[path]. The @racket[mode-flag] and
@racket[exists-flag] arguments are the same as for
@racket[open-output-file].}

@defproc[(write-to-file [v any/c]
                        [path path-string?]
                      [#:mode mode-flag (or/c 'binary 'text) 'binary]
                      [#:exists exists-flag (or/c 'error 'append 'update
                                                  'replace 'truncate 'truncate/replace) 'error])
         void?]{

Like @racket[display-to-file], but using @racket[write] instead of @racket[display].}

@defproc[(display-lines-to-file [lst list?]
                                [path path-string?]
                       [#:separator separator any/c #"\n"]
                       [#:mode mode-flag (or/c 'binary 'text) 'binary]
                       [#:exists exists-flag (or/c 'error 'append 'update
                                                   'replace 'truncate 'truncate/replace) 'error])
         void?]{

Displays each element of @racket[lst] to @racket[path], adding
@racket[separator] after each element. The @racket[mode-flag] and
@racket[exists-flag] arguments are the same as for
@racket[open-output-file].}

@defproc[(copy-directory/files [src path-string?] [dest path-string?]
                               [#:keep-modify-seconds? keep-modify-seconds? #f])
         void?]{

Copies the file or directory @racket[src] to @racket[dest], raising
@racket[exn:fail:filesystem] if the file or directory cannot be
copied, possibly because @racket[dest] exists already. If @racket[src]
is a directory, the copy applies recursively to the directory's
content. If a source is a link, the target of the link is copied
rather than the link itself.

If @racket[keep-modify-seconds?] is @racket[#f]false, then file copies
keep only the properties kept by @racket[copy-file], If
@racket[keep-modify-seconds?] is true, then each file copy also keeps
the modification date of the original.}


@defproc[(delete-directory/files [path path-string?]
                                 [#:must-exist? must-exist? #t])
         void?]{

Deletes the file or directory specified by @racket[path], raising
@racket[exn:fail:filesystem] if the file or directory cannot be
deleted. If @racket[path] is a directory, then
@racket[delete-directory/files] is first applied to each file and
directory in @racket[path] before the directory is deleted.

If @racket[must-exist?] is true, then @racket[exn:fail:filesystem] is
raised if @racket[path] does not exist. If @racket[must-exist?] is
false, then @racket[delete-directory/files] succeeds if @racket[path]
does not exist (but a failure is possible if @racket[path] initially
exists and is removed by another thread or process before 
@racket[delete-directory/files] deletes it).}


@defproc[(find-files [predicate (path? . -> . any/c)]
                     [start-path (or/c path-string? #f) #f]
                     [#:follow-links? follow-links? #f])
         (listof path?)]{

Traverses the filesystem starting at @racket[start-path] and creates a
list of all files and directories for which @racket[predicate] returns
true. If @racket[start-path] is @racket[#f], then the traversal starts
from @racket[(current-directory)]. In the resulting list, each
directory precedes its content.

The @racket[predicate] procedure is called with a single argument for
each file or directory. If @racket[start-path] is @racket[#f], the
argument is a pathname string that is relative to the current
directory. Otherwise, it is a path building on
@racket[start-path]. Consequently, supplying
@racket[(current-directory)] for @racket[start-path] is different from
supplying @racket[#f], because @racket[predicate] receives complete
paths in the former case and relative paths in the latter.  Another
difference is that @racket[predicate] is not called for the current
directory when @racket[start-path] is @racket[#f].

If @racket[follow-links?] is true, the @racket[find-files] traversal
follows links, and links are not included in the result. If
@racket[follow-links?] is @racket[#f], then links are not followed,
and links are included in the result.

If @racket[start-path] does not refer to an existing file or
directory, then @racket[predicate] will be called exactly once with
@racket[start-path] as the argument.

The @racket[find-files] procedure raises and exception if it encounters 
a directory for which @racket[directory-list] fails.}

@defproc[(pathlist-closure [path-list (listof path-string?)]
                           [#:follow-links? follow-links? #f])
         (listof path?)]{

Given a list of paths, either absolute or relative to the current
directory, returns a list such that

@itemize[

 @item{if a nested path is given, all of its ancestors are also
       included in the result (but the same ancestor is not added
       twice);}

 @item{if a path refers to directory, all of its descendants are also
       included in the result;}

 @item{ancestor directories appear before their descendants in the
       result list.}

]

If @racket[follow-links?] is true, then the traversal of directories
and files follows links, and the link paths are not included in the
result. If @racket[follow-links?] is @racket[#f], then he result list
includes paths to link and the links are not followed.}


@defproc[(fold-files [proc (or/c (path? (or/c 'file 'dir 'link) any/c 
                                   . -> . any/c)
                                 (path? (or/c 'file 'dir 'link) any/c 
                                   . -> . (values any/c any/c)))]
                     [init-val any/c]
                     [start-path (or/c path-string? #f) #f]
                     [follow-links? any/c #t])
         any]{

Traverses the filesystem starting at @racket[start-path], calling
@racket[proc] on each discovered file, directory, and link. If
@racket[start-path] is @racket[#f], then the traversal starts from
@racket[(current-directory)].

The @racket[proc] procedure is called with three arguments for each
file, directory, or link:

@itemize[

 @item{If @racket[start-path] is @racket[#f], the first argument is a
 pathname string that is relative to the current directory. Otherwise,
 the first argument is a pathname that starts with
 @racket[start-path]. Consequently, supplying
 @racket[(current-directory)] for @racket[start-path] is different
 from supplying @racket[#f], because @racket[proc] receives complete
 paths in the former case and relative paths in the latter. Another
 difference is that @racket[proc] is not called for the current
 directory when @racket[start-path] is @racket[#f].}

 @item{The second argument is a symbol, either @racket['file],
 @racket['dir], or @racket['link]. The second argument can be
 @racket['link] when @racket[follow-links?] is @racket[#f],
 in which case the filesystem traversal does not follow links. If
 @racket[follow-links?] is @racket[#t], then @racket[proc]
 will only get a @racket['link] as a second argument when it
 encounters a dangling symbolic link (one that does not resolve to an
 existing file or directory).}

 @item{The third argument is the accumulated result. For the first
 call to @racket[proc], the third argument is @racket[init-val]. For the
 second call to @racket[proc] (if any), the third argument is the result
 from the first call, and so on. The result of the last call to
 @racket[proc] is the result of @racket[fold-files].}

]

The @racket[proc] argument is used in an analogous way to the
procedure argument of @racket[foldl], where its result is used as the
new accumulated result.  There is an exception for the case of a
directory (when the second argument is @racket['dir]): in this case
the procedure may return two values, the second indicating whether the
recursive scan should include the given directory or not.  If it
returns a single value, the directory is scanned.  In the cases of 
files or links (when the second argument is @racket['file] or 
@racket['link]), a second value is permitted but ignored.

If the @racket[start-path] is provided but no such path exists, or if
paths disappear during the scan, then an exception is raised.}


@defproc[(make-directory* [path path-string?]) void?]{

Creates directory specified by @racket[path], creating intermediate
directories as necessary, and never failing if @racket[path] exists
already.}


@defproc[(make-temporary-file [template string? "rkttmp~a"]
                              [copy-from-filename (or/c path-string? #f 'directory) #f]
                              [directory (or/c path-string? #f) #f])
         path?]{

Creates a new temporary file and returns a pathname string for the
file.  Instead of merely generating a fresh file name, the file is
actually created; this prevents other threads or processes from
picking the same temporary name.

The @racket[template] argument must be a format string suitable
for use with @racket[format] and one additional string argument (where
the string contains only digits). If the resulting string is a
relative path, it is combined with the result of
@racket[(find-system-path 'temp-dir)], unless @racket[directory] is
provided and non-@racket[#f], in which case the
file name generated from @racket[template] is combined with
@racket[directory] to obtain a full path.

The @racket[template] argument's default is only the string @racket["rkttmp~a"]
when there is no source location information for the callsite of
@racket[make-temporary-file] (or if @racket[make-temporary-file] is
used in a higher-order position). If there is such information, then the template
string is based on the source location.

If @racket[copy-from-filename] is provided as path, the temporary file
is created as a copy of the named file (using @racket[copy-file]). If
@racket[copy-from-filename] is @racket[#f], the temporary file is
created as empty. If @racket[copy-from-filename] is
@racket['directory], then the temporary ``file'' is created as a
directory.

When a temporary file is created, it is not opened for reading or
writing when the pathname is returned. The client program calling
@racket[make-temporary-file] is expected to open the file with the
desired access and flags (probably using the @racket['truncate] flag;
see @racket[open-output-file]) and to delete it when it is no longer
needed.}

@defproc[(call-with-atomic-output-file [file path-string?] 
                                       [proc ([port input-port?] [tmp-path path?]  . -> . any)]
                                       [#:security-guard security-guard (or/c #f security-guard?) #f])
         any]{

Opens a temporary file for writing in the same directory as
@racket[file], calls @racket[proc] to write to the temporary file, and
then atomically moves the temporary file in place of @racket[proc].
The atomic move simply uses @racket[rename-file-or-directory] on Unix
and Mac OS X, but it uses an extra rename step (see below) on Windows
to avoid problems due to concurrent readers of @racket[file].

The @racket[proc] function is called with an output port for the
temporary file, plus the path of the temporary file. The result of
@racket[proc] is the result of @racket[call-with-atomic-output].

The @racket[call-with-atomic-output] function arranges to delete
temporary files on exceptions.

Windows prevents programs from deleting or replacing files that are
open, but it allows renaming of open files. Therefore, on Windows,
@racket[call-with-atomic-output-file] creates a second temporary file
@racket[_extra-tmp-file], renames @racket[file] to
@racket[_extra-tmp-file], renames the temporary file written by
@racket[proc] to @racket[p], and finally deletes
@racket[_extra-tmp-file].}


@defproc[(get-preference [name symbol?]
                         [failure-thunk (-> any) (lambda () #f)]
                         [flush-mode any/c 'timestamp]
                         [filename (or/c string-path? #f) #f]
                         [#:use-lock? use-lock? any/c #t]
                         [#:timeout-lock-there timeout-lock-there 
                                               (or/c (path? . -> . any) #f)
                                               #f]
                         [#:lock-there 
                          lock-there
                          (or/c (path? . -> . any) #f)
                          (make-handle-get-preference-locked 
                           0.01 name failure-thunk flush-mode filename
                           #:lock-there timeout-lock-there)])
         any]{

Extracts a preference value from the file designated by
@racket[(find-system-path 'pref-file)], or by @racket[filename] if it
is provided and is not @racket[#f].  In the former case, if the
preference file doesn't exist, @racket[get-preferences] attempts to
read an @elemref["old-prefs"]{old preferences file}, and then a
@filepath{racket-prefs.rktd} file in the configuration directory
(as reported by @racket[find-config-dir]), instead. If none of those
files exists, the preference set is empty.

The preference file should contain a list of symbol--value lists
written with the default parameter settings.  Keys
starting with @racket[racket:], @racket[mzscheme:], @racket[mred:],
and @racket[plt:] in any letter case are reserved for use by Racket
implementors. If the preference file does not contain a list
of symbol--value lists, an error is logged via @racket[log-error]
and @racket[failure-thunk] is called.

The result of @racket[get-preference] is the value associated with
@racket[name] if it exists in the association list, or the result of
calling @racket[failure-thunk] otherwise.

Preference settings are cached (weakly) across calls to
@racket[get-preference], using @racket[(path->complete-path filename)]
as a cache key. If @racket[flush-mode] is provided as @racket[#f], the
cache is used instead of re-consulting the preferences file. If
@racket[flush-mode] is provided as @racket['timestamp] (the default),
then the cache is used only if the file has a timestamp that is the
same as the last time the file was read. Otherwise, the file is
re-consulted.

On platforms for which @racket[preferences-lock-file-mode] returns
@racket['file-lock] and when @racket[use-lock?] is true,
preference-file reading is guarded by a lock; multiple readers can
share the lock, but writers take the lock exclusively. If the
preferences file cannot be read because the lock is unavailable,
@racket[lock-there] is called on the path of the lock file; if
@racket[lock-there] is @racket[#f], an exception is raised. The
default @racket[lock-there] handler retries about 5 times (with
increasing delays between each attempt) before trying 
@racket[timeout-lock-there], and the default @racket[timeout-lock-there] 
triggers an exception.

See also @racket[put-preferences]. For a more elaborate preference
system, see @racket[preferences:get].

@elemtag["old-prefs"]{@bold{Old preferences files}}: When a
@racket[filename] is not provided and the file indicated by
@racket[(find-system-path 'pref-file)] does not exist, the following
paths are checked for compatibility with old versions of Racket:

@itemlist[

 @item{Windows: @racket[(build-path (find-system-path 'pref-dir) 'up "PLT Scheme" "plt-prefs.ss")]}

 @item{Mac OS X: @racket[(build-path (find-system-path 'pref-dir) "org.plt-scheme.prefs.ss")]}

 @item{Unix: @racket[(expand-user-path "~/.plt-scheme/plt-prefs.ss")]}

]}

@defproc[(put-preferences [names (listof symbol?)]
                          [vals list?]
                          [locked-proc (or/c #f (path? . -> . any)) #f]
                          [filename (or/c #f path-string?) #f])
         void?]{

Installs a set of preference values and writes all current values to
the preference file designated by @racket[(find-system-path
'pref-file)], or @racket[filename] if it is supplied and not
@racket[#f].

The @racket[names] argument supplies the preference names, and
@racket[vals] must have the same length as @racket[names]. Each
element of @racket[vals] must be an instance of a built-in data type
whose @racket[write] output is @racket[read]able (i.e., the
@racket[print-unreadable] parameter is set to @racket[#f] while
writing preferences).

Current preference values are read from the preference file before
updating, and a write lock is held starting before the file
read, and lasting until after the preferences file is updated. The
lock is implemented by the existence of a file in the same directory
as the preference file; see @racket[preferences-lock-file-mode] for 
more information. If the directory of the preferences file does
not already exist, it is created.

If the write lock is already held, then
@racket[locked-proc] is called with a single argument: the path of the lock
file. The default @racket[locked-proc] (used when the @racket[locked-proc] 
argument is @racket[#f]) reports an error; an alternative
thunk might wait a while and try again, or give the user the choice to
delete the lock file (in case a previous update attempt encountered
disaster and locks are implemented by the presence of the lock file).

If @racket[filename] is @racket[#f] or not supplied, and the
preference file does not already exist, then values read from the
@filepath{defaults} collection (if any) are written for preferences
that are not mentioned in @racket[names].}


@defproc[(preferences-lock-file-mode) (or/c 'exists 'file-lock)]{

Reports the way that the lock file is used to implement
preference-file locking on the current platform.

The @racket['exists] mode is currently used on all platforms except
Windows. In @racket['exists] mode, the existence of the lock file
indicates that a write lock is held, and readers need no lock (because
the preferences file is atomically updated via
@racket[rename-file-or-directory]).

The @racket['file-lock] mode is currently used on Windows. In
@racket['file-lock] mode, shared and exclusive locks (in the sense of
@racket[port-try-file-lock?]) on the lock file reflect reader and
writer locks on the preference-file content. (The preference file
itself is not locked, because a lock would interfere with replacing
the file via @racket[rename-file-or-directory].)}


@defproc[(make-handle-get-preference-locked
          [delay real?]
          [name symbol?]
          [failure-thunk (-> any) (lambda () #f)]
          [flush-mode any/c 'timestamp]
          [filename (or/c path-string? #f) #f]
          [#:lock-there lock-there (or/c (path? . -> . any) #f) #f]
          [#:max-delay max-delay real? 0.2])
         (path-string? . -> . any)]{

Creates a procedure suitable for use as the @racket[#:lock-there]
argument to @racket[get-preference], where the @racket[name],
@racket[failure-thunk], @racket[flush-mode], and @racket[filename]
are all passed on to @racket[get-preference] by the result procedure
to retry the preferences lookup.

Before calling @racket[get-preference], the result procedure uses
@racket[(sleep delay)] to pause. Then, if @racket[(* 2 delay)] is less
than @racket[max-delay], the result procedure calls
@racket[make-handle-get-preference-locked] to generate a new retry
procedure to pass to @racket[get-preference], but with a
@racket[delay] of @racket[(* 2 delay)]. If @racket[(* 2 delay)] is not
less than @racket[max-delay], then @racket[get-preference] is called
with the given @racket[lock-there], instead.}

@defproc[(call-with-file-lock/timeout
          [filename (or/c path-string? #f)]
          [kind (or/c 'shared 'exclusive)]
          [thunk (-> any)]
          [failure-thunk (-> any)]
          [#:lock-file lock-file (or/c #f path-string?) #f]
          [#:delay delay (and/c real? (not/c negative?)) 0.01]
          [#:max-delay max-delay (and/c real? (not/c negative?)) 0.2])
         any]{

Obtains a lock for the filename @racket[lock-file] and then calls
@racket[thunk].  The @racket[filename] argument specifies a file path
prefix that is used only to generate the lock filename when
@racket[lock-file] is @racket[#f].  Specifically, when
@racket[lock-file] is @racket[#f], then
@racket[call-with-file-lock/timeout] uses @racket[make-lock-file-name]
to build the lock filename. If the lock file does not yet exist, it is
created; beware that the lock file is @emph{not} deleted by 
@racket[call-with-file-lock/timeout].

When @racket[thunk] returns, 
@racket[call-with-file-lock/timeout] releases the lock, returning the result of
@racket[thunk]. The @racket[call-with-file-lock/timeout] function will retry
after @racket[delay] seconds and continue retrying with exponential backoff
until delay reaches @racket[max-delay]. If
@racket[call-with-file-lock/timeout] fails to obtain the lock,
@racket[failure-thunk] is called in tail position.  The @racket[kind] argument
specifies whether the lock is @racket['shared] or @racket['exclusive]
in the sense of @racket[port-try-file-lock?].

}

@examples[
  #:eval file-eval
  (call-with-file-lock/timeout filename 'exclusive
    (lambda () (printf "File is locked\n"))
    (lambda () (printf "Failed to obtain lock for file\n")))

  (call-with-file-lock/timeout #f 'exclusive
    (lambda () 
      (call-with-file-lock/timeout filename 'shared
        (lambda () (printf "Shouldn't get here\n"))
        (lambda () (printf "Failed to obtain lock for file\n"))))
    (lambda () (printf "Shouldn't get here either\n"))
    #:lock-file (make-lock-file-name filename))]


@defproc*[([(make-lock-file-name [path (or path-string? path-for-some-system?)]) 
            path?]
           [(make-lock-file-name [dir (or path-string? path-for-some-system?)] 
                                 [name path-element?]) 
            path?])]{

Creates a lock filename by prepending @racket["_LOCK"] on Windows or
@racket[".LOCK"] on other platforms to the file portion of the path.

@examples[
  #:eval file-eval
  (make-lock-file-name "/home/george/project/important-file")]}

@deftogether[(
@defthing[user-read-bit     @#,racketvalfont{#o400}]
@defthing[user-write-bit    @#,racketvalfont{#o200}]
@defthing[user-execute-bit  @#,racketvalfont{#o100}]
@defthing[group-read-bit    @#,racketvalfont{#o040}]
@defthing[group-write-bit   @#,racketvalfont{#o020}]
@defthing[group-execute-bit @#,racketvalfont{#o010}]
@defthing[other-read-bit    @#,racketvalfont{#o004}]
@defthing[other-write-bit   @#,racketvalfont{#o002}]
@defthing[other-execute-bit @#,racketvalfont{#o001}]
)]{

Constants that are useful with @racket[file-or-directory-permissions]
and bitwise operations such as @racket[bitwise-ior], and
@racket[bitwise-and].}


@(interaction-eval #:eval file-eval (begin
                                     (delete-file filename)
                                     (delete-file (make-lock-file-name filename))))
@(close-eval file-eval)
