#reader(lib "docreader.ss" "scribble")
@require["mz.ss"]

@title{Filesystem}

@;------------------------------------------------------------------------
@section[#:tag "mz:findpaths"]{Locating Paths}

@defproc[(find-system-path [kind symbol?]) path?]{

Returns a machine-specific path for a standard type of path specified
by @scheme[kind], which must be one of the following:

@itemize{

 @item{@indexed-scheme['home-dir] --- the current user's home
 directory.

 Under Unix and Mac OS X, this directory is determined by expanding
 the path @file{~}, which is expanded by first checking for a
 @indexed-envvar{HOME} environment variable. If none is defined, the
 @indexed-envvar{USER} and @indexed-envvar{LOGNAME} environment
 variables are consulted (in that order) to find a user name, and then
 system files are consulted to locate the user's home directory.

 Under Windows, the user's home directory is the user-specific profile
 directory as determined by the Windows registry. If the registry
 cannot provide a directory for some reason, the value of the
 @indexed-envvar{USERPROFILE} environment variable is used instead, as
 long as it refers to a directory that exists. If @envvar{USERPROFILE}
 also fails, the directory is the one specified by the
 @indexed-envvar{HOMEDRIVE} and @indexed-envvar{HOMEPATH} environment
 variables.  If those environment variables are not defined, or if the
 indicated directory still does not exist, the directory containing
 the current executable is used as the home directory.}

 @item{@indexed-scheme['pref-dir] --- the standard directory for
 storing the current user's preferences. Under Unix, the directory is
 @file{.plt-scheme} in the user's home directory.  Under Windows, it
 is @file{PLT Scheme} in the user's application-data folder as
 specified by the Windows registry; the application-data folder is
 usually @file{Application Data} in the user's profile
 directory. Under Mac OS X, it is @file{Library/Preferences} in the
 user's home directory. This directory might not exist.}

 @item{@indexed-scheme['pref-file] --- a file that contains a
 symbol-keyed association list of preference values. The file's
 directory path always matches the result returned for
 @scheme['pref-dir]. The file name is @file{plt-prefs.ss} under Unix
 and Windows, and it is @file{org.plt-scheme.prefs.ss} under Mac OS
 X. The file's directory might not exist. See also
 @scheme[get-preference].}

 @item{@indexed-scheme['temp-dir] --- the standard directory for
 storing temporary files. Under @|AllUnix|, this is the directory
 specified by the @indexed-envvar{TMPDIR} environment variable, if it
 is defined.}

 @item{@indexed-scheme['init-dir] --- the directory containing the
 initialization file used by stand-alone @exec{mzscheme} executable.
 It is the same as the current user's home directory.}

 @item{@indexed-scheme['init-file] --- the file loaded at start-up by
 the stand-alone @exec{mzscheme} executable. The directory part of the
 path is the same path as returned for @scheme['init-dir].  The file
 name is platform-specific:

  @itemize{

  @item{@|AllUnix|: @indexed-file{.mzschemerc}}

  @item{Windows: @indexed-file{mzschemerc.ss}}

  }}

 @item{@indexed-scheme['addon-dir] --- a directory for installing PLT Scheme
 extensions. It's the same as @scheme['pref-dir], except under Mac OS
 X, where it is @file{Library/PLT Scheme} in the user's home
 directory. This directory might not exist.}

 @item{@indexed-scheme['doc-dir] --- the standard directory for storing the
 current user's documents. It's the same as @scheme['home-dir] under
 @|AllUnix|. Under Windows, it is the user's documents folder as
 specified by the Windows registry; the documents folder is usually
 @file{My Documents} in the user's home directory.}

 @item{@indexed-scheme['desk-dir] --- the directory for the current user's
 desktop. Under Unix, it's the same as @scheme['home-dir]. Under
 Windows, it is the user's desktop folder as specified by the Windows
 registry; the documents folder is usually @file{Desktop} in the
 user's home directory. Under Mac OS X, it is the desktop directory,
 which is specifically @file{~/Desktop} under Mac OS X.}

 @item{@indexed-scheme['sys-dir] --- the directory containing the
 operating system for Windows. Under @|AllUnix|, the
 result is @scheme["/"].}

 @item{@indexed-scheme['exec-file] --- the path of the @exec{mzscheme}
 executable as provided by the operating system for the current
 invocation.

 @margin-note{For MrEd, the executable path is the name of a MrEd
 executable.}}

 @item{@indexed-scheme['run-file] --- the path of the current
  executable; this may be different from result for
  @scheme['exec-file] because an alternate path was provided through a
  @DFlag{name} or @Flag{N} command-line flag to the @exec{mzscheme}
  (or @exec{mred}) executable, or because an embedding executable
  installed an alternate path. In particular a ``launcher'' script
  created by @scheme[make-mzscheme-launcher] sets this path to the
  script's path. In the @exec{mzscheme} executable, this path is also
  bound initially to @scheme[program].}

 @item{@indexed-scheme['collects-dir] --- a path to the main
 collection of libraries (see @secref["mz:collects"]). If this path is
 relative, it's relative to the directory of @scheme[(find-system-path
 'exec-file)].  This path is normally embedded in the @exec{mzscheme}
 executable, but it can be overridden by the @DFlag{collects} or
 @Flag{X} command-line flag.}

 @item{@indexed-scheme['orig-dir] --- the current directory at
 start-up, which can be useful in converting a relative-path result
 from @scheme[(find-system-path 'exec-file)] or
 @scheme[(find-system-path 'run-file)] to a complete path.}

 }}

@defproc[(path-list-string->path-list [str (or/c string? bytes?)]
                                      [default-path-list (listof path?)])
         (listof path?)]{
 

Parses a string or byte string containing a list of paths, and returns
a list of path strings. Under @|AllUnix|, paths in a path list are
separated by a @litchar{:}; under Windows, paths are separated by a
@litchar{;}. Whenever the path list contains an empty path, the list
@scheme[default-path-list] is spliced into the returned list of
paths. Parts of @scheme[str] that do not form a valid path are not
included in the returned list.}


@defproc[(find-executable-path [program-sub path-string?][related-sub path-string?][deepest? any/c #f]) 
         (or/c path? false/c)]{

Finds a path for the executable @scheme[program-sub], returning
@scheme[#f] if the path cannot be found.

If @scheme[related-sub] is not @scheme[#f], then it must be a relative
path string, and the path found for @scheme[program-sub] must be such
that the file or directory @scheme[related-sub] exists in the same
directory as the executable. The result is then the full path for the
found @scheme[related-sub], instead of the path for the executable.
 
This procedure is used by the @exec{mzscheme} executable to find the
standard library collection directory (see @secref["mz:collects"]).  In
this case, @scheme[program] is the name used to start MzScheme and
@scheme[related] is @scheme["collects"].  The @scheme[related-sub]
argument is used because, under @|AllUnix|, @scheme[program-sub] may
involve to a sequence of soft links; in this case,
@scheme[related-sub] determines which link in the chain is relevant.

If @scheme[related-sub] is not @scheme[#f], then when
@scheme[find-executable-path] does not finds a @scheme[program-sub]
that is a link to another file path, the search can continue with the
destination of the link. Further links are inspected until
@scheme[related-sub] is found or the end of the chain of links is
reached. If @scheme[deepest?] is @scheme[#f] (the default), then the
result corresponds to the first path in a chain of links for which
@scheme[related-sub] is found (and further links are not actually
explored); otherwise, the result corresponds to the last link in the
chain for which @scheme[related-sub] is found.

If @scheme[program-sub] is a pathless name,
@scheme[find-executable-path] gets the value of the
@indexed-envvar{PATH} environment variable; if this environment
variable is defined, @scheme[find-executable-path] tries each path in
@envvar{PATH} as a prefix for @scheme[program-sub] using the search
algorithm described above for path-containing
@scheme[program-sub]s. If the @envvar{PATH} environment variable is
not defined, @scheme[program-sub] is prefixed with the current
directory and used in the search algorithm above. (Under Windows, the
current directory is always implicitly the first item in
@envvar{PATH}, so @scheme[find-executable-path] checks the current
directory first under Windows.)}

@;------------------------------------------------------------------------
@section[#:tag "mz:fileutils"]{Files}

@defproc[(file-exists? [path path-string?]) boolean?]{

Returns @scheme[#t] if a file (not a directory) @scheme[path] exists,
@scheme[#f] otherwise.

Under Windows, @scheme[file-exists?]  reports @scheme[#t] for all
variations of the special filenames (e.g., @scheme["LPT1"],
@scheme["x:/baddir/LPT1"]).}

@defproc[(link-exists? [path path-string?]) boolean?]{

Returns @scheme[#t] if a link @scheme[path] exists (@|AllUnix|),
@scheme[#f] otherwise.

The predicates @scheme[file-exists?]  or @scheme[directory-exists?]
work on the final destination of a link or series of links, while
@scheme[link-exists?]  only follows links to resolve the base part of
@scheme[path] (i.e., everything except the last name in the
path).

This procedure never raises the @scheme[exn:fail:filesystem]
exception.}


@defproc[(delete-file [path path-string?]) void?]{

Feletes the file with path @scheme[path] if it exists, otherwise the
@exnraise[exn:fail:filesystem]. If @scheme[path] is a link, the link
is deleted rather than the destination of the link.}

@defproc[(rename-file-or-directory [old path-string?]
                                   [new path-string?]
                                   [exists-ok? any/c #f]) 
         void?]{
 
Renames the file or directory with path @scheme[old]---if it
exists---to the path @scheme[new]. If the file or directory is not
renamed successfully, the @exnraise[exn:fail:filesystem].

This procedure can be used to move a file/directory to a different
directory (on the same disk) as well as rename a file/directory within
a directory. Unless @scheme[exists-ok?]  is provided as a true value,
@scheme[new] cannot refer to an existing file or directory. Even if
@scheme[exists-ok?] is true, @scheme[new] cannot refer to an existing
file when @scheme[old] is a directory, and vice versa. (If
@scheme[new] exists and is replaced, the replacement is atomic in the
filesystem, except under Windows 95, 98, or Me. However, the check for
existence is not included in the atomic action, which means that race
conditions are possible when @scheme[exists-ok?] is false or not
supplied.)

If @scheme[old] is a link, the link is renamed rather than the
destination of the link, and it counts as a file for replacing any
existing @scheme[new].}


@defproc[(file-or-directory-modify-seconds [path path-string?]
                                           [secs-n (or/c exact-integer? false/c) #f]
                                           [fail-thunk (-> any) (lambda () (raise (make-exn:fail:filesystem ....)))])
         any]{

Returns the file or directory's last modification date as
platform-specific seconds (see also @secref["mz:time"]) when
@scheme[secs-n] is not provided or is @scheme[#f]. (For FAT
filesystems under Windows, directories do not have modification
dates. Therefore, the creation date is returned for a directory (but
the modification date is returned for a file).)

If @scheme[secs-n] is provided and not @scheme[#f], the access and
modification times of @scheme[path] are set to the given time.

On error (e.g., if no such file exists), @scheme[fail-thunk] is
called, and the default @scheme[fail-thunk] raises
@scheme[exn:fail:filesystem].}


@defproc[(file-or-directory-permissions [path path-string?]) (listof symbol?)]{

Returns a list containing @indexed-scheme['read],
@indexed-scheme['write], and/or @indexed-scheme['execute] for the
given file or directory path. On error (e.g., if no such file exists),
the @exnraise[exn:fail:filesystem]. Under @|AllUnix|, permissions are
checked for the current effective user instead of the real user.}

@defproc[(file-size [path path-string?]) nonnegative-exact-integer?]{

Returns the (logical) size of the specified file in bytes. Under Mac
OS X, this size excludes the resource-fork size. On error (e.g., if no
such file exists), the @exnraise[exn:fail:filesystem].}


@defproc[(copy-file [src path-string?][dest path-string?]) void?]{

Creates the file @scheme[dest] as a copy of @scheme[src]. If the file
is not successfully copied, the @exnraise[exn:fail:filesystem]. If
@scheme[dest] already exists, the copy will fail. File permissions are
preserved in the copy. Under Mac OS X, the resource fork is also
preserved in the copy. If @scheme[src] refers to a link, the target of
the link is copied, rather than the link itself.}

@defproc[(make-file-or-directory-link [to path-string?][path path-string?]) 
         void?]{

Creates a link @scheme[path] to @scheme[to] under @|AllUnix|. The
creation will fail if @scheme[path] already exists. The @scheme[to]
need not refer to an existing file or directory, and @scheme[to] is
not expanded before writing the link. If the link is not created
successfully,the @exnraise[exn:fail:filesystem]. Under Windows, the
@exnraise[exn:fail:unsupported] always.}

@;------------------------------------------------------------------------
@section[#:tag "mz:directories"]{Directories}

See also: @scheme[rename-file-or-directory],
@scheme[file-or-directory-modify-seconds],
@scheme[file-or-directory-permissions].

@defparam[current-directory path path-string?]{

A parameter that determines the current directory for resolving
relative paths.

When the parameter procedure is called to set the current directory,
the path argument is expanded using @scheme[expand-path], simplified
using @scheme[simplify-path], and then converted to a directory path
with @scheme[path->directory-path]; expansion and simplification raise
an exception if the path is ill-formed. Thus, the current value of
@scheme[current-directory] is always an expanded, simplified,
complete, directory path.

The path is not checked for existence when the parameter is set.}


@defproc[(current-drive) path?]{

Returns the current drive name Windows. For other platforms, the
@exnraise[exn:fail:unsupported]. The current drive is always the drive
of the current directory.}


@defproc[(directory-exists? [path path-string?]) boolean?]{

Returns @scheme[#t] if @scheme[path] refers to a directory,
@scheme[#f] otherwise.}

@defproc[(make-directory [path path-string?]) void?]{

Creates a new directory with the path @scheme[path].  If the directory
is not created successfully, the @exnraise[exn:fail:filesystem].}


@defproc[(delete-directory [path path-string?]) void?]{

Deletes an existing directory with the path @scheme[path]. If the
directory is not deleted successfully, the
@exnraise[exn:fail:filesystem].}


@defproc[(directory-list [path path-string? (current-directory)]) 
         (listof path?)]{

Returns a list of all files and directories in the directory specified
by @scheme[path]. If @scheme[path] is omitted, a list of files and
directories in the current directory is returned. Under @|AllUnix|, an
element of the list can start with @litchar{./~} if it would otherwise
start with @litchar{~}. Under Windows, an element of the list may
start with @litchar["\\\\?\\REL\\\\"].}


@defproc[(filesystem-root-list) (listof path?)]{

Returns a list of all current root directories. Obtaining this list
can be particularly slow under Windows.}
