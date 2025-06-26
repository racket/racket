#lang scribble/doc
@(require "mz.rkt" scribble/example)

@title[#:tag "pathutils" #:style 'toc]{Paths}

When a Racket procedure takes a filesystem path as an argument, the
path can be provided either as a string or as an instance of the
@deftech{path} datatype. If a string is provided, it is converted to a
path using @racket[string->path]. Beware that some paths may not
be representable as strings; see @secref["unixpathrep"] and
@secref["windowspathrep"] for more information.
A Racket procedure that generates a
filesystem path always generates a @tech{path} value.

By default, paths are created and manipulated for the current
platform, but procedures that merely manipulate paths (without using
the filesystem) can manipulate paths using conventions for other
supported platforms. The @racket[bytes->path] procedure accepts an
optional argument that indicates the platform for the path, either
@racket['unix] or @racket['windows]. For other functions, such as
@racket[build-path] or @racket[simplify-path], the behavior is
sensitive to the kind of path that is supplied. Unless otherwise
specified, a procedure that requires a path accepts only paths for the
current platform.

Two @tech{path} values are @racket[equal?] when they use the same
convention type and when their byte-string representations are
@racket[equal?]. A path string (or byte string) cannot be empty, and
it cannot contain a nul character or byte. When an empty string or a
string containing nul is provided as a path to any procedure except
@racket[absolute-path?], @racket[relative-path?], or
@racket[complete-path?], the @exnraise[exn:fail:contract].

Most Racket primitives that accept paths first @deftech{cleanse} the
path before using it. Procedures that build paths or merely check the
form of a path do not cleanse paths, with the exceptions of
@racket[cleanse-path], @racket[expand-user-path], and
@racket[simplify-path].  For more information about path cleansing and
other platform-specific details, see @secref["unixpaths"] and
@secref["windowspaths"].

@;------------------------------------------------------------------------
@section{Manipulating Paths}

@defproc[(path? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a path value for the current
platform (not a string, and not a path for a different platform),
@racket[#f] otherwise.}

@defproc[(path-string? [v any/c]) boolean?]{
 Returns @racket[#t] if @racket[v] is either a @deftech{path
  or string}: either a path for the current platform or a
 non-empty string without nul characters. Returns @racket[#f]
 otherwise.
}

@defproc[(path-for-some-system? [v any/c]) boolean?]{

Returns @racket[#t] if @racket[v] is a path value for some platform
(not a string), @racket[#f] otherwise.}


@defproc[(string->path [str string?]) path?]{

Produces a path whose byte-string encoding is
@racket[(string->bytes/locale str (char->integer #\?))] on @|AllUnix|
or @racket[(string->bytes/utf-8 str)] on Windows.

Beware that the current locale might not encode every string, in which
case @racket[string->path] can produce the same path for different
@racket[str]s. See also @racket[string->path-element], which should be
used instead of @racket[string->path] when a string represents a
single @tech{path element}. For information on how strings and byte
strings encode paths, see @secref["unixpathrep"] and
@secref["windowspathrep"].

See also @racket[string->some-system-path], and see
@secref["unixpathrep"] and @secref["windowspathrep"] for information
on how strings encode paths.

@history[#:changed "6.1.1.1" @elem{Changed Windows conversion to always use UTF-8.}]}


@defproc[(bytes->path [bstr bytes?]
                      [type (or/c 'unix 'windows) (system-path-convention-type)])
         path?]{

Produces a path (for some platform) whose byte-string encoding is
@racket[bstr], where @racket[bstr] must not contain a nul byte. The
optional @racket[type] specifies the convention to use for the path.

For converting relative @tech{path elements} from literals, use instead
@racket[bytes->path-element], which applies a suitable encoding for
individual elements.

For information on how byte strings encode paths, see
@secref["unixpathrep"] and @secref["windowspathrep"].}


@defproc[(path->string [path path?]) string?]{

Produces a string that represents @racket[path] by decoding
@racket[path]'s byte-string encoding using the current locale
on @|AllUnix| and by using UTF-8 on Windows. In the former case,
@litchar{?} is used in the result string where encoding fails, and if
the encoding result is the empty string, then the result is
@racket["?"].

The resulting string is suitable for displaying to a user,
string-ordering comparisons, etc., but it is not suitable for
re-creating a path (possibly modified) via @racket[string->path],
since decoding and re-encoding the path's byte string may lose
information.

Furthermore, for display and sorting based on individual @tech{path elements}
(such as pathless file names), use @racket[path-element->string],
instead, to avoid special encodings use to represent some relative
paths. See @secref["windowspaths"] for specific information about
the conversion of Windows paths.

See also @racket[some-system-path->string].

@history[#:changed "6.1.1.1" @elem{Changed Windows conversion to always use UTF-8.}]}


@defproc[(path->bytes [path path-for-some-system?]) bytes?]{

Produces @racket[path]'s byte-string representation. No information is
lost in this translation, so that @racket[(bytes->path (path->bytes
path) (path-convention-type path))] always produces a path that is
@racket[equal?] to @racket[path]. The @racket[path] argument can be a
path for any platform.

Conversion to and from byte values is useful for marshaling and
unmarshaling paths, but manipulating the byte form of a path is
generally a mistake. In particular, the byte string may start with a
@litchar{\\?\REL} encoding for Windows paths. Instead of
@racket[path->bytes], use @racket[split-path] and
@racket[path-element->bytes] to manipulate individual @tech{path elements}.

For information on how byte strings encode paths, see
@secref["unixpathrep"] and @secref["windowspathrep"].}


@defproc[(string->path-element [str string?]
                               [false-on-non-element? any/c #f])
         (or/c (and/c path? path-element?) #f)]{

Like @racket[string->path], except that @racket[str] corresponds to a
single relative element in a path, and it is encoded as necessary to
convert it to a path. See @secref["unixpaths"] and
@secref["windowspaths"] for more information on the conversion of
paths.

If @racket[str] does not correspond to any @tech{path element}
(e.g., it is an absolute path, or it can be split), or if it
corresponds to an up-directory or same-directory indicator on
@|AllUnix|, then either @racket[#f] is returned or @exnraise[exn:fail:contract].
A @racket[#f] is returned only when @racket[false-on-non-element?]
is true.

Like @racket[path->string], information can be lost from
@racket[str] in the locale-specific conversion to a path.

@history[#:changed "8.1.0.6" @elem{Added the @racket[false-on-non-element?] argument.}]}


@defproc[(bytes->path-element [bstr bytes?]
                              [type (or/c 'unix 'windows) (system-path-convention-type)]
                              [false-on-non-element? any/c #f])
         (or/c path-element? #f)]{

Like @racket[bytes->path], except that @racket[bstr] corresponds to a
single relative element in a path. In terms of conversions,
restrictions on @racket[bstr], and the treatment of @racket[false-on-non-element?],
@racket[bytes->path-element] is like @racket[string->path-element].

The @racket[bytes->path-element] procedure is generally the best
choice for reconstructing a path based on another path (where the
other path is deconstructed with @racket[split-path] and
@racket[path-element->bytes]) when ASCII-level manipulation of
@tech{path elements} is necessary.

@history[#:changed "8.1.0.6" @elem{Added the @racket[false-on-non-element?] argument.}]}


@defproc[(path-element->string [path path-element?]) string?]{

Like @racket[path->string], except that trailing path separators are
removed (as by @racket[split-path]). On Windows, any
@litchar{\\?\REL} encoding prefix is also removed; see
@secref["windowspaths"] for more information.

The @racket[path] argument must be such that @racket[split-path]
applied to @racket[path] would return @racket['relative] as its first
result and a path as its second result, otherwise the
@exnraise[exn:fail:contract].

The @racket[path-element->string] procedure is generally the best
choice for presenting a pathless file or directory name to a user.}


@defproc[(path-element->bytes [path path-element?]) bytes?]{

Like @racket[path->bytes], except that any encoding prefix is removed,
etc., as for @racket[path-element->string].

For any reasonable locale, consecutive ASCII characters in the printed
form of @racket[path] are mapped to consecutive byte values that match
each character's code-point value, and a leading or trailing ASCII
character is mapped to a leading or trailing byte, respectively. The
@racket[path] argument can be a path for any platform.

The @racket[path-element->bytes] procedure is generally the right
choice (in combination with @racket[split-path]) for extracting the
content of a path to manipulate it at the ASCII level (then
reassembling the result with @racket[bytes->path-element] and
@racket[build-path]).}


@defproc[(path<? [a-path path?] [b-path path?] ...) boolean?]{

Returns @racket[#t] if the arguments are sorted, where the comparison
for each pair of paths is the same as using
@racket[path->bytes] and @racket[bytes<?].

@history/arity[]}


@defproc[(path-convention-type [path path-for-some-system?])
         (or/c 'unix 'windows)]{

Accepts a path value (not a string) and returns its convention
type.}


@defproc[(system-path-convention-type)
         (or/c 'unix 'windows)]{

Returns the path convention type of the current platform:
@indexed-racket['unix] for @|AllUnix|, @indexed-racket['windows] for
Windows.}


@defproc[(build-path [base (or/c path-string? path-for-some-system? 'up 'same)]
                     [sub (or/c (and/c (or/c path-string? path-for-some-system?)
                                       (not/c complete-path?))
                                (or/c 'up 'same))] ...)
         path-for-some-system?]{

Creates a path given a base path and any number of sub-path
extensions. If @racket[base] is an absolute path, the result is an
absolute path, otherwise the result is a relative path.

The @racket[base] and each @racket[sub] must be either a relative
path, the symbol @indexed-racket['up] (indicating the relative parent
directory), or the symbol @indexed-racket['same] (indicating the
relative current directory).  For Windows paths, if @racket[base] is a
drive specification (with or without a trailing slash) the first
@racket[sub] can be an absolute (driveless) path. For all platforms,
the last @racket[sub] can be a filename.

The @racket[base] and @racket[sub] arguments can be paths for
any platform. The platform for the resulting path is inferred from the
@racket[base] and @racket[sub] arguments, where string arguments imply
a path for the current platform. If different arguments are for
different platforms, the @exnraise[exn:fail:contract]. If no argument
implies a platform (i.e., all are @racket['up] or @racket['same]), the
generated path is for the current platform.

Each @racket[sub] and @racket[base] can optionally end in a directory
separator. If the last @racket[sub] ends in a separator, it is
included in the resulting path.

If @racket[base] or @racket[sub] is an illegal path string (because it
is empty or contains a nul character), the
@exnraise[exn:fail:contract].

The @racket[build-path] procedure builds a path @italic{without}
checking the validity of the path or accessing the filesystem.

See @secref["unixpaths"] and @secref["windowspaths"] for more
information on the construction of paths.

The following examples assume that the current directory is
@filepath{/home/joeuser} for Unix examples and @filepath{C:\Joe's Files} for
Windows examples.

@racketblock[
(define p1 (build-path (current-directory) "src" "racket"))
 (code:comment @#,t{Unix: @racket[p1] is @racket["/home/joeuser/src/racket"]})
 (code:comment @#,t{Windows: @racket[p1] is @racket["C:\\Joe's Files\\src\\racket"]})
(define p2 (build-path 'up 'up "docs" "Racket"))
 (code:comment @#,t{Unix: @racket[p2] is @racket["../../docs/Racket"]})
 (code:comment @#,t{Windows: @racket[p2] is @racket["..\\..\\docs\\Racket"]})
(build-path p2 p1) 
 (code:comment @#,t{Unix and Windows: raises @racket[exn:fail:contract]; @racket[p1] is absolute})
(build-path p1 p2) 
 (code:comment @#,t{Unix: is @racket["/home/joeuser/src/racket/../../docs/Racket"]})
 (code:comment @#,t{Windows: is @racket["C:\\Joe's Files\\src\\racket\\..\\..\\docs\\Racket"]})
]}


@defproc[(build-path/convention-type
                     [type (or/c 'unix 'windows)]
                     [base (or/c path-string? path-for-some-system? 'up 'same)]
                     [sub (or/c (and/c (or/c path-string? path-for-some-system?)
                                       (not/c complete-path?))
                                (or/c 'up 'same))] ...)
         path-for-some-system?]{

Like @racket[build-path], except a path convention type is specified
explicitly.

Note that, just as with @racket[build-path], any string arguments for either
@racket[base] or @racket[sub] will be implicitly converted into a path for the
current platform before being combined with the others. For this reason, you
cannot use this function to build paths from strings for any platform other
than the current one; in such attempts, @racket[type] does not match the
inferred convention type for the strings and an @exnraise[exn:fail:contract].
(To create paths for foreign platforms, see @racket[bytes->path].)

The usefulness of @racket[build-path/convention-type] over @racket[build-path]
is limited to cases where the sub-paths contain @racket['same] or @racket['up]
elements.}


@defproc[(absolute-path? [path (or/c path? string? path-for-some-system?)]) boolean?]{

Returns @racket[#t] if @racket[path] is an absolute path, @racket[#f]
otherwise. The @racket[path] argument can be a path for any
platform. If @racket[path] is not a legal path string (e.g., it
contains a nul character), @racket[#f] is returned. This procedure
does not access the filesystem.}


@defproc[(relative-path? [path (or/c path? string? path-for-some-system?)]) boolean?]{

Returns @racket[#t] if @racket[path] is a relative path, @racket[#f]
otherwise. The @racket[path] argument can be a path for any
platform. If @racket[path] is not a legal path string (e.g., it
contains a nul character), @racket[#f] is returned. This procedure
does not access the filesystem.}


@defproc[(complete-path? [path (or/c path? string? path-for-some-system?)]) boolean?]{

Returns @racket[#t] if @racket[path] is a @deftech{complete}ly determined path
(@italic{not} relative to a directory or drive), @racket[#f]
otherwise. The @racket[path] argument can be a path for any
platform. Note that for Windows paths, an absolute path can omit the
drive specification, in which case the path is neither relative nor
complete. If @racket[path] is not a legal path string (e.g., it
contains a nul character), @racket[#f] is returned.

This procedure does not access the filesystem.}


@defproc[(path->complete-path [path (or/c path-string? path-for-some-system?)]
                              [base (or/c path-string? path-for-some-system?) (current-directory)])
         path-for-some-system?]{

Returns @racket[path] as a complete path. If @racket[path] is already
a complete path, it is returned as the result. Otherwise,
@racket[path] is resolved with respect to the complete path
@racket[base]. If @racket[base] is not a complete path, the
@exnraise[exn:fail:contract].

The @racket[path] and @racket[base] arguments can be paths for any
platform; if they are for different
platforms, the @exnraise[exn:fail:contract].

This procedure does not access the filesystem.}


@defproc[(path->directory-path [path (or/c path-string? path-for-some-system?)])
         path-for-some-system?]{

Returns @racket[path] if @racket[path] syntactically refers to a
directory and ends in a separator, otherwise it returns an extended
version of @racket[path] that specifies a directory and ends with a
separator. For example, on @|AllUnix|, the path @filepath{x/y/}
syntactically refers to a directory and ends in a separator, but
@filepath{x/y} would be extended to @filepath{x/y/}, and @filepath{x/..} would be
extended to @filepath{x/../}. The @racket[path] argument can be a path for
any platform, and the result will be for the same platform.  

This procedure does not access the filesystem.}


@defproc[(resolve-path [path path-string?]) path?]{

@tech{Cleanse}s @racket[path] and returns a path that references the
same file or directory as @racket[path]. If
@racket[path] is a soft link to another path, then the referenced path
is returned (this may be a relative path with respect to the directory
owning @racket[path]), otherwise @racket[path] is returned (after
cleansing).

On Windows, the path for a link should be simplified syntactically, so
that an up-directory indicator removes a preceding path element
independent of whether the preceding element itself refers to a
link. For relative-paths links, the path should be parsed specially;
see @secref["windowspaths"] for more information.

@history[#:changed "6.0.1.12" @elem{Added support for links on Windows.}]}


@defproc[(cleanse-path [path (or/c path-string? path-for-some-system?)])
         path-for-some-system?]{

@techlink{Cleanse}s @racket[path] (as described at the beginning of
this chapter) without consulting the filesystem.

@examples[#:eval (make-base-eval '(require racket/path))
  (let ([p (string->some-system-path "tiny//dancer" 'unix)])
    (cleanse-path p))
]}


@defproc[(expand-user-path [path path-string?]) path?]{

@techlink{Cleanse}s @racket[path]. In addition, on @|AllUnix|, a
leading @litchar{~} is treated as user's home directory and expanded;
the username follows the @litchar{~} (before a @litchar{/} or the end
of the path), where @litchar{~} by itself indicates the home directory
of the current user.}


@defproc[(simplify-path [path (or/c path-string? path-for-some-system?)]
                        [use-filesystem? boolean? #t])
         path-for-some-system?]{

Eliminates redundant path separators (except for a single trailing
separator), up-directory @litchar{..}, and same-directory @litchar{.}
indicators in @racket[path], and changes @litchar{/} separators to
@litchar{\} separators in Windows paths, such that the result
accesses the same file or directory (if it exists) as @racket[path].

In general, the pathname is normalized as much as possible---without
consulting the filesystem if @racket[use-filesystem?] is @racket[#f],
and (on Windows) without changing the case of letters within the
path.  If @racket[path] syntactically refers to a directory, the
result ends with a directory separator.

When @racket[path] is simplified other than just converting slashes
to backslashes and @racket[use-filesystem?] is true
(the default), a complete path is returned. If @racket[path] is
relative, it is resolved with respect to the current directory.
On @|AllUnix|, up-directory indicators are removed taking into account soft links (so
that the resulting path refers to the same directory as before);
on Windows, up-directory indicators are removed by deleting a
preceding @tech{path element}.

When @racket[use-filesystem?] is @racket[#f], up-directory indicators
are removed by deleting a preceding @tech{path element}, and the result can
be a relative path with up-directory indicators remaining at the
beginning of the path; up-directory indicators are dropped when they
refer to the parent of a root directory. Similarly, the result can be
the same as @racket[(build-path 'same)] (but with a trailing
separator) if eliminating up-directory indicators leaves only
same-directory indicators.

The @racket[path] argument can be a path for any platform when
@racket[use-filesystem?] is @racket[#f], and the resulting path is for
the same platform.

The filesystem might be accessed when @racket[use-filesystem?] is
true, but the source or simplified path might be a non-existent path. If
@racket[path] cannot be simplified due to a cycle of links, the
@exnraise[exn:fail:filesystem] (but a successfully simplified path may
still involve a cycle of links if the cycle did not inhibit the
simplification).

See @secref["unixpaths"] and @secref["windowspaths"] for more
information on simplifying paths.

@examples[#:eval (make-base-eval '(require racket/path))
  (let ([p (string->some-system-path "tiny//in/my/head/../../../dancer" 'unix)])
    (simplify-path p #f))
]}
 

@defproc[(normal-case-path [path (or/c path-string? path-for-some-system?)])
         path-for-some-system?]{

Returns @racket[path] with ``normalized'' case characters. For @|AllUnix|
paths, this procedure always returns the input path, because
filesystems for these platforms can be case-sensitive. For Windows
paths, if @racket[path] does not start with @litchar{\\?\}, the
resulting string uses only lowercase letters, based on the current
locale. In addition, for Windows paths when the path does not start
with @litchar{\\?\}, all @litchar{/}s are converted to
@litchar{\}s, and trailing spaces and @litchar{.}s are removed.

The @racket[path] argument can be a path for any platform, but beware
that local-sensitive decoding and conversion of the path may be
different on the current platform than for the path's platform.

This procedure does not access the filesystem.}


@defproc[(split-path [path (or/c path-string? path-for-some-system?)])
         (values (or/c path-for-some-system? 'relative #f)
                 (or/c path-for-some-system? 'up 'same)
                 boolean?)]{

Deconstructs @racket[path] into a smaller path and an immediate
directory or file name.  Three values are returned:

@itemize[

 @item{@racket[base] is either

  @itemize[
   @item{a path,} 
   @item{@indexed-racket['relative] if @racket[path] is an immediate
    relative directory or filename, or}
   @item{@racket[#f] if @racket[path] is a root directory.}
 ]}

 @item{@racket[name] is either 
  @itemize[
   @item{a directory-name path,} 
   @item{a filename,}
   @item{@racket['up] if the last part of @racket[path] specifies the parent
    directory of the preceding path (e.g., @litchar{..} on Unix), or}
   @item{@racket['same] if the last part of @racket[path] specifies the 
     same directory as the  preceding path (e.g., @litchar{.} on Unix).}
  ]}

 @item{@racket[must-be-dir?] is @racket[#t] if @racket[path] explicitly
 specifies a directory (e.g., with a trailing separator), @racket[#f]
 otherwise. Note that @racket[must-be-dir?] does not specify whether
 @racket[name] is actually a directory or not, but whether @racket[path]
 syntactically specifies a directory.}

 ]

Compared to @racket[path], redundant separators (if any) are removed
in the result @racket[base] and @racket[name].  If @racket[base] is
@racket[#f], then @racket[name] cannot be @racket['up] or
@racket['same]. The @racket[path] argument can be a path for any
platform, and resulting paths for the same platform.

This procedure does not access the filesystem.

See @secref["unixpaths"] and @secref["windowspaths"] for more
information on splitting paths.}


@defproc[(explode-path [path (or/c path-string? path-for-some-system?)])
         (listof (or/c path-for-some-system? 'up 'same))]{

Returns the list of @tech{path elements} that constitute @racket[path].  If
@racket[path] is simplified in the sense of @racket[simple-form-path],
then the result is always a list of paths, and the first element of
the list is a root.

The @racket[explode-path] function computes its result in time
proportional to the length of @racket[path] (unlike a loop that
uses @racket[split-path], which must allocate intermediate paths).}


@defproc[(path-replace-extension [path (or/c path-string? path-for-some-system?)]
                                 [ext (or/c string? bytes?)])
         path-for-some-system?]{

Returns a path that is the same as @racket[path], except that the
extension for the last element of the path (including the extension
separator) is changed to @racket[ext]. If the last element of
@racket[path] has no extension, then @racket[ext] is added to the
path.

An extension is defined as a @litchar{.} that is not at the start of
the path element followed by any number of non-@litchar{.}
characters/bytes at the end of the @tech{path element}, as long as the
path element is not a directory indicator like @racket[".."].

The @racket[path] argument can be a path for any platform, and the
result is for the same platform. If @racket[path] represents a root,
the @exnraise[exn:fail:contract]. The given @racket[ext] typically
starts with @litchar{.}, but it is not required to start with an
extension separator.

@examples[
(path-replace-extension "x/y.ss" #".rkt")
(path-replace-extension "x/y.ss" #"")
(path-replace-extension "x/y" #".rkt")
(path-replace-extension "x/y.tar.gz" #".rkt")
(path-replace-extension "x/.racketrc" #".rkt")
]

@history[#:added "6.5.0.3"]}


@defproc[(path-add-extension [path (or/c path-string? path-for-some-system?)]
                             [ext (or/c string? bytes?)]
                             [sep (or/c string? bytes?) #"_"])
         path-for-some-system?]{

Similar to @racket[path-replace-extension], but any existing extension on
@racket[path] is preserved by replacing the @litchar{.} before the extension
with @racket[sep], and then the @racket[ext] is added
to the end.

@examples[
(path-add-extension "x/y.ss" #".rkt")
(path-add-extension "x/y" #".rkt")
(path-add-extension "x/y.tar.gz" #".rkt")
(path-add-extension "x/y.tar.gz" #".rkt" #".")
(path-add-extension "x/.racketrc" #".rkt")
]

@history[#:changed "6.8.0.2" @elem{Added the @racket[sep] optional argument.}
         #:added "6.5.0.3"]}


@defproc[(path-replace-suffix [path (or/c path-string? path-for-some-system?)]
                              [ext (or/c string? bytes?)])
         path-for-some-system?]{
@deprecated[#:what "function" @racket[path-replace-extension]]

Like @racket[path-replace-extension], but treats a leading @litchar{.}
in a path element as an extension separator.}

@defproc[(path-add-suffix [path (or/c path-string? path-for-some-system?)]
                          [ext (or/c string? bytes?)])
         path-for-some-system?]{

@deprecated[#:what "function" @racket[path-add-extension]]

Like @racket[path-add-extension], but treats a leading @litchar{.}
in a path element as an extension separator.}

@defproc[(reroot-path [path (or/c path-string? path-for-some-system?)]
                      [root-path (or/c path-string? path-for-some-system?)])
         path-for-some-system?]{

Produces a path that extends @racket[root-path] based on the complete
form of @racket[path].

If @racket[path] is not already @tech{complete}, is it completed via
@racket[path->complete-path], in which case @racket[path] must be a
path for the current platform. The @racket[path] argument is also
@tech{cleanse}d and case-normalized via @racket[normal-case-path]. The
path is then appended to @racket[root-path]; in the case of Windows
paths, a root letter drive becomes a letter path element, while a root
UNC path is prefixed with @racket["UNC"] as a path element and the
machine and volume names become path elements.

@examples[
(reroot-path (bytes->path #"/home/caprica/baltar" 'unix)
             (bytes->path #"/earth" 'unix))
(reroot-path (bytes->path #"c:\\usr\\adama" 'windows)
             (bytes->path #"\\\\earth\\africa\\" 'windows))
(reroot-path (bytes->path #"\\\\galactica\\cac\\adama" 'windows)
             (bytes->path #"s:\\earth\\africa\\" 'windows))
]}

@;------------------------------------------------------------------------
@section{More Path Utilities}

@(define path-eval (make-base-eval `(require racket/path)))

@note-lib[racket/path]

@defproc[(file-name-from-path [path (or/c path-string? path-for-some-system?)])
         (or/c path-for-some-system? #f)]{

Returns the last element of @racket[path]. If @racket[path] is
syntactically a directory path (see @racket[split-path]), then the
result is @racket[#f].}


@defproc[(path-get-extension [path (or/c path-string? path-for-some-system?)])
         (or/c bytes? #f)]{

Returns a byte string that is the extension part of the filename in
@racket[path], including the @litchar{.} separator. If the path has no
extension, @racket[#f] is returned.

See @racket[path-replace-extension] for the definition of a filename
extension.

@examples[#:eval path-eval
(path-get-extension "x/y.rkt")
(path-get-extension "x/y")
(path-get-extension "x/y.tar.gz")
(path-get-extension "x/.racketrc")
]

@history[#:added "6.5.0.3"]}


@defproc[(path-has-extension? [path (or/c path-string? path-for-some-system?)]
                              [ext (or/c bytes? string?)])
         boolean?]{

Determines whether the last element of @racket[path] ends with
@racket[ext] but is not exactly the same as @racket[ext].

If @racket[ext] is a @tech{byte string} with the shape of an extension
(i.e., starting with @litchar{.} and not including another @litchar{.}), this check is equivalent to
checking whether @racket[(path-get-extension path)] produces @racket[ext].

@examples[#:eval path-eval
(path-has-extension? "x/y.rkt" #".rkt")
(path-has-extension? "x/y.ss" #".rkt")
(path-has-extension? "x/y" #".rkt")
(path-has-extension? "x/.racketrc" #".racketrc")
(path-has-extension? "x/compiled/y_rkt.zo" #"_rkt.zo")
]

@history[#:added "6.5.0.3"]}


@defproc[(filename-extension [path (or/c path-string? path-for-some-system?)])
         (or/c bytes? #f)]{

@deprecated[#:what "function" @racket[path-get-extension]]

Returns a byte string that is the extension part of the filename in
@racket[path] without the @litchar{.} separator. If @racket[path] is
syntactically a directory (see @racket[split-path]) or if the path has
no extension, @racket[#f] is returned.}


@defproc[(find-relative-path [base (or/c path-string? path-for-some-system?)]
                             [path (or/c path-string?  path-for-some-system?)]
                             [#:more-than-root? more-than-root? any/c #f]
                             [#:more-than-same? more-than-same? any/c #t]
                             [#:normalize-case? normalize-case? any/c #t])
         (or/c path-for-some-system? path-string?)]{

Finds a relative pathname with respect to @racket[base] that names the
same file or directory as @racket[path]. Both @racket[base] and
@racket[path] must be simplified in the sense of
@racket[simple-form-path]. If @racket[path] shares no subpath in
common with @racket[base], @racket[path] is returned.

If @racket[more-than-root?] is true, if @racket[base] and
@racket[path] share only a Unix root in common, and if neither
@racket[base] nor @racket[path] is just a root path, then
@racket[path] is returned.

If @racket[path] is the same as @racket[base], then
@racket[(build-path 'same)] is returned only if
@racket[more-than-same?] is @racket[#f]. Otherwise, and by default,
@racket[path] is returned when @racket[path] is the same as @racket[base].

If @racket[normalize-case?] is true (the default), then pairs of path
elements to be compared are first converted via
@racket[normal-case-path], which means that path elements are
compared case-insentively on Windows. If @racket[normalize-case?] is
@racket[#f], then path elements and the path roots match only if they
have the same case.

The result is normally a @tech{path} in the sense of @racket[path?].
The result is a string only if @racket[path] is provided a string and
also returned as the result.

@history[#:changed "6.8.0.3" @elem{Made path elements case-normalized
                                   for comparison by default, and
                                   added the @racket[#:normalize-case?]
                                   argument.}
         #:changed "6.90.0.21" @elem{Added the @racket[#:more-than-same?]
                                     argument.}]}


@defproc[(normalize-path [path path-string?]
                         [wrt (and/c path-string? complete-path?)
                              (current-directory)]) 
         path?]{

@margin-note{For most purposes, @racket[simple-form-path] is the
 preferred mechanism to normalize a path, because it works for paths
 that include non-existent directory components, and it avoids
 unnecessarily expanding soft links.}

Returns a complete version of @racket[path] by making the path
complete, expanding the complete path, and resolving all soft links
(which requires consulting the filesystem). If @racket[path] is
relative, then @racket[wrt] is used as the base path.

Letter case is @italic{not} normalized by @racket[normalize-path]. For
this and other reasons, such as whether the path is syntactically a
directory, the result of @racket[normalize-path] is not suitable for
comparisons that determine whether two paths refer to the same file or
directory (i.e., the comparison may produce false negatives).

An error is signaled by @racket[normalize-path] if the input
path contains an embedded path for a non-existent directory,
or if an infinite cycle of soft links is detected.

@examples[#:eval (make-base-eval '(require racket/path))
  (equal? (current-directory) (normalize-path "."))
]}


@defproc[(path-element? [path any/c]) boolean?]{

Returns @racket[#t] if @racket[path] is a @deftech{path element}: 
a path value for some platform (see @racket[path-for-some-system?]) such that
@racket[split-path] applied to @racket[path] would return
@racket['relative] as its first result and a path as its second
result. Otherwise, the result is @racket[#f].}


@defproc[(path-only [path (or/c path-string? path-for-some-system?)])
         (or/c #f path-for-some-system?)]{

Returns @racket[path] without its final path element in the case that
@racket[path] is not syntactically a directory; if @racket[path] has only
a single, non-directory path element, @racket[#f] is returned. If
@racket[path] is syntactically a directory, then @racket[path] is
returned unchanged (but as a path, if it was a string).

@examples[#:eval path-eval
(path-only (build-path "a" "b"))
(path-only (build-path "a"))
(path-only (path->directory-path (build-path "a")))
(path-only (build-path 'up 'up))
]}


@defproc[(simple-form-path [path path-string?]) path?]{

Returns @racket[(simplify-path (path->complete-path path))], which
ensures that the result is a complete path containing no up- or
same-directory indicators.}

@defproc[(some-system-path->string [path path-for-some-system?])
         string?]{

Converts @racket[path] to a string using a UTF-8 encoding of the
path's bytes.

Use this function when working with paths for a different system
(whose encoding of pathnames might be unrelated to the current
locale's encoding) and when starting and ending with strings.}

@defproc[(string->some-system-path [str string?]
                                   [kind (or/c 'unix 'windows)])
         path-for-some-system?]{

Converts @racket[str] to a @racket[kind] path using a UTF-8 encoding
of the path's bytes.

Use this function when working with paths for a different system
(whose encoding of pathnames might be unrelated to the current
locale's encoding) and when starting and ending with strings.}

@defproc[(shrink-path-wrt [pth path?] [other-pths (listof path?)]) (or/c #f path?)]{
  Returns a suffix of @racket[pth] that shares nothing
  in common with the suffixes of @racket[other-pths], or
  @racket[pth], if not possible (e.g. when @racket[other-pths]
  is empty or contains only paths with the same elements as @racket[pth]).
  
  @examples[#:eval path-eval
                   (shrink-path-wrt (build-path "racket" "list.rkt")
                                    (list (build-path "racket" "list.rkt")
                                          (build-path "racket" "base.rkt")))
                   
                   (shrink-path-wrt (build-path "racket" "list.rkt")
                                    (list (build-path "racket" "list.rkt")
                                          (build-path "racket" "private" "list.rkt")
                                          (build-path "racket" "base.rkt")))]

}

@close-eval[path-eval]

@;------------------------------------------------------------------------
@include-section["unix-paths.scrbl"]
@include-section["windows-paths.scrbl"]
