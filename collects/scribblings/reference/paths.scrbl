#lang scribble/doc
@(require "mz.ss")

@title[#:tag "pathutils" #:style 'toc]{Paths}

When a Scheme procedure takes a filesystem path as an argument, the
path can be provided either as a string or as an instance of the
@deftech{path} datatype. If a string is provided, it is converted to a
path using @scheme[string->path]. A Scheme procedure that generates a
filesystem path always generates a @tech{path} value.

By default, paths are created and manipulated for the current
platform, but procedures that merely manipulate paths (without using
the filesystem) can manipulate paths using conventions for other
supported platforms. The @scheme[bytes->path] procedure accepts an
optional argument that indicates the platform for the path, either
@scheme['unix] or @scheme['windows]. For other functions, such as
@scheme[build-path] or @scheme[simplify-path], the behavior is
sensitive to the kind of path that is supplied. Unless otherwise
specified, a procedure that requires a path accepts only paths for the
current platform.

Two @tech{path} values are @scheme[equal?] when they are use the same
convention type and when their byte-string representations are
@scheme[equal?]. A path string (or byte string) cannot be empty, and
it cannot contain a nul character or byte. When an empty string or a
string containing nul is provided as a path to any procedure except
@scheme[absolute-path?], @scheme[relative-path?], or
@scheme[complete-path?], the @exnraise[exn:fail:contract].

Most Scheme primitives that accept paths first @deftech{cleanse} the
path before using it. Procedures that build paths or merely check the
form of a path do not cleanse paths, with the exceptions of
@scheme[cleanse-path], @scheme[expand-user-path], and
@scheme[simplify-path].  For more information about path cleansing and
other platform-specific details, see @secref["unixpaths"] for
@|AllUnix| paths and @secref["windowspaths"] for Windows paths.

@;------------------------------------------------------------------------
@section{Manipulating Paths}

@defproc[(path? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a path value for the current
platform (not a string, and not a path for a different platform),
@scheme[#f] otherwise.}

@defproc[(path-string? [v any/c]) boolean?]{

Return @scheme[#t] if @scheme[v] is either a path value for the
current platform or a non-empty string without nul characters,
@scheme[#f] otherwise.}

@defproc[(path-for-some-system? [v any/c]) boolean?]{

Returns @scheme[#t] if @scheme[v] is a path value for some platform
(not a string), @scheme[#f] otherwise.}

@defproc[(string->path [str string?]) path?]{

Produces a path whose byte-string name is
@scheme[(string->bytes/locale string (char->integer #\?))].

Beware that the current locale might not encode every string, in which
case @scheme[string->path] can produce the same path for different
@scheme[str]s. See also @scheme[string->path-element], which should be
used instead of @scheme[string->path] when a string represents a
single path element.

See also @scheme[string->some-system-path].}

@defproc[(bytes->path [bstr bytes?]
                      [type (or/c 'unix 'windows) (system-path-convention-type)]) 
         path?]{

Produces a path (for some platform) whose byte-string name is
@scheme[bstr]. The optional @scheme[type] specifies the convention to
use for the path.

For converting relative path elements from literals, use instead
@scheme[bytes->path-element], which applies a suitable encoding for
individual elements.}

@defproc[(path->string [path path?]) string?]{

Produces a string that represents @scheme[path] by decoding
@scheme[path]'s byte-string name using the current locale's encoding;
@litchar{?} is used in the result string where encoding fails, and if
the encoding result is the empty string, then the result is
@scheme["?"].

The resulting string is suitable for displaying to a user,
string-ordering comparisons, etc., but it is not suitable for
re-creating a path (possibly modified) via @scheme[string->path],
since decoding and re-encoding the path's byte string may lose
information.

Furthermore, for display and sorting based on individual path elements
(such as pathless file names), use @scheme[path-element->string],
instead, to avoid special encodings use to represent some relative
paths. See @secref["windowspaths"] for specific information about
the conversion of Windows paths.

See also @scheme[some-system-path->string].}

@defproc[(path->bytes [path path?]) bytes?]{

Produces @scheme[path]'s byte string representation. No information is
lost in this translation, so that @scheme[(bytes->path (path->bytes
path) (path-convention-type path))] always produces a path is that is
@scheme[equal?] to @scheme[path]. The @scheme[path] argument can be a
path for any platform.

Conversion to and from byte values is useful for marshaling and
unmarshaling paths, but manipulating the byte form of a path is
generally a mistake. In particular, the byte string may start with a
@litchar{\\?\REL} encoding for Windows paths. Instead of
@scheme[path->bytes], use @scheme[split-path] and
@scheme[path-element->bytes] to manipulate individual path elements.}

@defproc[(string->path-element [str string?]) path?]{

Like @scheme[string->path], except that @scheme[str] corresponds to a
single relative element in a path, and it is encoded as necessary to
convert it to a path. See @secref["unixpaths"] for more information
on the conversion for @|AllUnix| paths, and see
@secref["windowspaths"] for more information on the conversion for
Windows paths.

If @scheme[str] does not correspond to any path element
(e.g., it is an absolute path, or it can be split), or if it
corresponds to an up-directory or same-directory indicator under
@|AllUnix|, then @exnraise[exn:fail:contract].

As for @scheme[path->string], information can be lost from
@scheme[str] in the locale-specific conversion to a path.}


@defproc[(bytes->path-element [bstr bytes?]
                              [type (or/c 'unix 'windows) (system-path-convention-type)]) 
         path?]{

Like @scheme[bytes->path], except that @scheme[bstr] corresponds to a
single relative element in a path. In terms of conversions and
restrictions on @scheme[bstr], @scheme[bytes->path-element] is like
@scheme[string->path-element].

The @scheme[bytes->path-element] procedure is generally the best
choice for reconstructing a path based on another path (where the
other path is deconstructed with @scheme[split-path] and
@scheme[path-element->bytes]) when ASCII-level manipulation of path
elements is necessary.}

@defproc[(path-element->string [path path?]) string?]{

Like @scheme[path->string], except any encoding prefix is removed. See
@secref["unixpaths"] for more information on the conversion for
@|AllUnix| paths, and see @secref["windowspaths"] for more
information on the conversion for Windows paths.
In addition, trailing path separators are removed, as by
@scheme[split-path].

The @scheme[path] argument must be such that @scheme[split-path]
applied to @scheme[path] would return @scheme['relative] as its first
result and a path as its second result, otherwise the
@exnraise[exn:fail:contract].

The @scheme[path-element->string] procedure is generally the best
choice for presenting a pathless file or directory name to a user.}

@defproc[(path-element->bytes [path path-string?]) bytes?]{

Like @scheme[path->bytes], except that any encoding prefix is removed,
etc., as for @scheme[path-element->string].

For any reasonable locale, consecutive ASCII characters in the printed
form of @scheme[path] are mapped to consecutive byte values that match
each character's code-point value, and a leading or trailing ASCII
character is mapped to a leading or trailing byte, respectively. The
@scheme[path] argument can be a path for any platform.

The @scheme[path-element->bytes] procedure is generally the right
choice (in combination with @scheme[split-path]) for extracting the
content of a path to manipulate it at the ASCII level (then
reassembling the result with @scheme[bytes->path-element] and
@scheme[build-path]).}


@defproc[(path-convention-type [path path?])
         (or/c 'unix 'windows)]{

Accepts a path value (not a string) and returns its convention
type.}


@defproc[(system-path-convention-type)
         (or/c 'unix 'windows)]{

Returns the path convention type of the current platform:
@indexed-scheme['unix] for @|AllUnix|, @indexed-scheme['windows] for
Windows.}


@defproc[(build-path [base (or/c path-string? 'up 'same)]
                     [sub (or/c (and/c path-string? 
                                       (not/c complete-path?))
                                (or/c 'up 'same))] ...)
         path?]{

Creates a path given a base path and any number of sub-path
extensions. If @scheme[base] is an absolute path, the result is an
absolute path, otherwise the result is a relative path.

The @scheme[base] and each @scheme[sub] must be either a relative
path, the symbol @indexed-scheme['up] (indicating the relative parent
directory), or the symbol @indexed-scheme['same] (indicating the
relative current directory).  For Windows paths, if @scheme[base] is a
drive specification (with or without a trailing slash) the first
@scheme[sub] can be an absolute (driveless) path. For all platforms,
the last @scheme[sub] can be a filename.

The @scheme[base] and @scheme[sub-paths] arguments can be paths for
any platform. The platform for the resulting path is inferred from the
@scheme[base] and @scheme[sub] arguments, where string arguments imply
a path for the current platform. If different arguments are for
different platforms, the @exnraise[exn:fail:contract]. If no argument
implies a platform (i.e., all are @scheme['up] or @scheme['same]), the
generated path is for the current platform.

Each @scheme[sub] and @scheme[base] can optionally end in a directory
separator. If the last @scheme[sub] ends in a separator, it is
included in the resulting path.

If @scheme[base] or @scheme[sub] is an illegal path string (because it
is empty or contains a nul character), the
@exnraise[exn:fail:contract].

The @scheme[build-path] procedure builds a path @italic{without}
checking the validity of the path or accessing the filesystem.

See @secref["unixpaths"] for more information on the construction
of @|AllUnix| paths, and see @secref["windowspaths"] for more
information on the construction of Windows paths.

The following examples assume that the current directory is
@filepath{/home/joeuser} for Unix examples and @filepath{C:\Joe's Files} for
Windows examples.

@schemeblock[
(define p1 (build-path (current-directory) "src" "scheme"))
 (code:comment @#,t{Unix: @scheme[p1] is @scheme["/home/joeuser/src/scheme"]})
 (code:comment @#,t{Windows: @scheme[p1] is @scheme["C:\\Joe's Files\\src\\scheme"]})
(define p2 (build-path 'up 'up "docs" "Scheme"))
 (code:comment @#,t{Unix: @scheme[p2] is @scheme["../../docs/Scheme"]})
 (code:comment @#,t{Windows: @scheme[p2] is @scheme["..\\..\\docs\\Scheme"]})
(build-path p2 p1) 
 (code:comment @#,t{Unix and Windows: raises @scheme[exn:fail:contract]; @scheme[p1] is absolute})
(build-path p1 p2) 
 (code:comment @#,t{Unix: is @scheme["/home/joeuser/src/racket/../../docs/Scheme"]})
 (code:comment @#,t{Windows: is @scheme["C:\\Joe's Files\\src\\scheme\\..\\..\\docs\\Scheme"]})
]}


@defproc[(build-path/convention-type [type (or/c 'unix 'windows)]
                                     [base path-string?]
                                     [sub (or/c path-string? 'up 'same)] ...)
         path?]{

Like @scheme[build-path], except a path convention type is specified
explicitly.}

@defproc[(absolute-path? [path path-string?]) boolean?]{

Returns @scheme[#t] if @scheme[path] is an absolute path, @scheme[#f]
otherwise. The @scheme[path] argument can be a path for any
platform. If @scheme[path] is not a legal path string (e.g., it
contains a nul character), @scheme[#f] is returned. This procedure
does not access the filesystem.}


@defproc[(relative-path? [path path-string?]) boolean?]{

Returns @scheme[#t] if @scheme[path] is a relative path, @scheme[#f]
otherwise. The @scheme[path] argument can be a path for any
platform. If @scheme[path] is not a legal path string (e.g., it
contains a nul character), @scheme[#f] is returned. This procedure
does not access the filesystem.}


@defproc[(complete-path? [path path-string?]) boolean?]{

Returns @scheme[#t] if @scheme[path] is a completely determined path
(@italic{not} relative to a directory or drive), @scheme[#f]
otherwise. The @scheme[path] argument can be a path for any
platform. Note that for Windows paths, an absolute path can omit the
drive specification, in which case the path is neither relative nor
complete. If @scheme[path] is not a legal path string (e.g., it
contains a nul character), @scheme[#f] is returned.

This procedure does not access the filesystem.}


@defproc[(path->complete-path [path path-string?]
                              [base path-string? (current-directory)])
         path?]{

Returns @scheme[path] as a complete path. If @scheme[path] is already
a complete path, it is returned as the result. Otherwise,
@scheme[path] is resolved with respect to the complete path
@scheme[base]. If @scheme[base] is not a complete path, the
@exnraise[exn:fail:contract].

The @scheme[path] and @scheme[base] arguments can paths for any
platform; if they are for different
platforms, the @exnraise[exn:fail:contract].

This procedure does not access the filesystem.}


@defproc[(path->directory-path [path path-string?]) path?]{

Returns @scheme[path] if @scheme[path] syntactically refers to a
directory and ends in a separator, otherwise it returns an extended
version of @scheme[path] that specifies a directory and ends with a
separator. For example, under @|AllUnix|, the path @filepath{x/y/}
syntactically refers to a directory and ends in a separator, but
@filepath{x/y} would be extended to @filepath{x/y/}, and @filepath{x/..} would be
extended to @filepath{x/../}. The @scheme[path] argument can be a path for
any platform, and the result will be for the same platform.  

This procedure does not access the filesystem.}


@defproc[(resolve-path [path path-string?]) path?]{

@tech{Cleanse}s @scheme[path] and returns a path that references the
same file or directory as @scheme[path]. Under @|AllUnix|, if
@scheme[path] is a soft link to another path, then the referenced path
is returned (this may be a relative path with respect to the directory
owning @scheme[path]), otherwise @scheme[path] is returned (after
expansion).}


@defproc[(cleanse-path [path path-string?]) path]{

@techlink{Cleanse}s @scheme[path] (as described at the beginning of
this section). The filesystem might be accessed, but the source or
expanded path might be a non-existent path.}


@defproc[(expand-user-path [path path-string?]) path]{

@techlink{Cleanse}s @scheme[path]. In addition, under @|AllUnix|, a
leading @litchar{~} is treated as user's home directory and expanded;
the username follows the @litchar{~} (before a @litchar{/} or the end
of the path), where @litchar{~} by itself indicates the home directory
of the current user.}


@defproc[(simplify-path [path path-string?][use-filesystem? boolean? #t]) path?]{

Eliminates redundant path separators (except for a single trailing
separator), up-directory @litchar{..}, and same-directory @litchar{.}
indicators in @scheme[path], and changes @litchar{/} separators to
@litchar{\} separators in Windows paths, such that the result
accesses the same file or directory (if it exists) as @scheme[path].

In general, the pathname is normalized as much as possible --- without
consulting the filesystem if @scheme[use-filesystem?] is @scheme[#f],
and (under Windows) without changing the case of letters within the
path.  If @scheme[path] syntactically refers to a directory, the
result ends with a directory separator.

When @scheme[path] is simplified and @scheme[use-filesystem?] is true
(the default), a complete path is returned; if @scheme[path] is
relative, it is resolved with respect to the current directory, and
up-directory indicators are removed taking into account soft links (so
that the resulting path refers to the same directory as before).

When @scheme[use-filesystem?] is @scheme[#f], up-directory indicators
are removed by deleting a preceding path element, and the result can
be a relative path with up-directory indicators remaining at the
beginning of the path; up-directory indicators are dropped when they
refer to the parent of a root directory. Similarly, the result can be
the same as @scheme[(build-path 'same)] (but with a trailing
separator) if eliminating up-directory indicators leaves only
same-directory indicators.

The @scheme[path] argument can be a path for any platform when
@scheme[use-filesystem?] is @scheme[#f], and the resulting path is for
the same platform.

The filesystem might be accessed when @scheme[use-filesystem?] is
true, but the source or simplified path might be a non-existent path. If
@scheme[path] cannot be simplified due to a cycle of links, the
@exnraise[exn:fail:filesystem] (but a successfully simplified path may
still involve a cycle of links if the cycle did not inhibit the
simplification).

See @secref["unixpaths"] for more information on simplifying
@|AllUnix| paths, and see @secref["windowspaths"] for more
information on simplifying Windows paths.}
 

@defproc[(normal-case-path [path path-string?]) path?]{

Returns @scheme[path] with ``normalized'' case letters. For @|AllUnix|
paths, this procedure always returns the input path, because
filesystems for these platforms can be case-sensitive. For Windows
paths, if @scheme[path] does not start @litchar{\\?\}, the
resulting string uses only lowercase letters, based on the current
locale. In addition, for Windows paths when the path does not start
@litchar{\\?\}, all @litchar{/}s are converted to
@litchar{\}s, and trailing spaces and @litchar{.}s are removed.

The @scheme[path] argument can be a path for any platform, but beware
that local-sensitive decoding and conversion of the path may be
different on the current platform than for the path's platform.

This procedure does not access the filesystem.}


@defproc[(split-path [path path-string?])
         (values (or/c path? 'relative #f)
                 (or/c path? 'up 'same)
                 boolean?)]{

Deconstructs @scheme[path] into a smaller path and an immediate
directory or file name.  Three values are returned:

@itemize[

 @item{@scheme[base] is either

  @itemize[
   @item{a path,} 
   @item{@indexed-scheme['relative] if @scheme[path] is an immediate
    relative directory or filename, or}
   @item{@scheme[#f] if @scheme[path] is a root directory.}
 ]}

 @item{@scheme[name] is either 
  @itemize[
   @item{a directory-name path,} 
   @item{a filename,}
   @item{@scheme['up] if the last part of @scheme[path] specifies the parent
    directory of the preceding path (e.g., @litchar{..} under Unix), or}
   @item{@scheme['same] if the last part of @scheme[path] specifies the 
     same directory as the  preceding path (e.g., @litchar{.} under Unix).}
  ]}

 @item{@scheme[must-be-dir?] is @scheme[#t] if @scheme[path] explicitly
 specifies a directory (e.g., with a trailing separator), @scheme[#f]
 otherwise. Note that @scheme[must-be-dir?] does not specify whether
 @scheme[name] is actually a directory or not, but whether @scheme[path]
 syntactically specifies a directory.}

 ]

Compared to @scheme[path], redundant separators (if any) are removed
in the result @scheme[base] and @scheme[name].  If @scheme[base] is
@scheme[#f], then @scheme[name] cannot be @scheme['up] or
@scheme['same]. The @scheme[path] argument can be a path for any
platform, and resulting paths for the same platform.

This procedure does not access the filesystem.

See @secref["unixpaths"] for more information on splitting
@|AllUnix| paths, and see @secref["windowspaths"] for more
information on splitting Windows paths.}


@defproc[(path-replace-suffix [path path-string?]
                              [suffix (or/c string? bytes?)])
         path?]{

Returns a path that is the same as @scheme[path], except that the
suffix for the last element of the path is changed to
@scheme[suffix]. If the last element of @scheme[path] has no suffix,
then @scheme[suffix] is added to the path. A suffix is defined as a
@litchar{.} followed by any number of non-@litchar{.} characters/bytes
at the end of the path element, as long as the path element is not
@scheme[".."] or @scheme["."]. The @scheme[path] argument can be a
path for any platform, and the result is for the same platform. If
@scheme[path] represents a root, the @exnraise[exn:fail:contract].}

@defproc[(path-add-suffix [path path-string?]
                          [suffix (or/c string? bytes?)])
         path?]{

Similar to @scheme[path-replace-suffix], but any existing suffix on
@scheme[path] is preserved by replacing every @litchar{.} in the last
path element with @litchar{_}, and then the @scheme[suffix] is added
to the end.}

@;------------------------------------------------------------------------
@section{More Path Utilities}

@note-lib[racket/path]

@defproc[(explode-path [path (or/c path-string? path-for-some-system?)]) 
         (listof (or/c path-for-some-system? 'up 'same))]{

Returns the list of path element that constitute @scheme[path].  If
@scheme[path] is simplified in the sense of @scheme[simple-form-path],
then the result is always a list of paths, and the first element of
the list is a root.}

@defproc[(file-name-from-path [path (or/c path-string? path-for-some-system?)])
         (or/c path-for-some-system? #f)]{

Returns the last element of @scheme[path]. If @scheme[path]
syntactically a directory path (see @scheme[split-path]), then then
result is @scheme[#f].}

@defproc[(filename-extension [path (or/c path-string? path-for-some-system?)])
         (or/c bytes? #f)]{

Returns a byte string that is the extension part of the filename in
@scheme[path] without the @litchar{.} separator. If @scheme[path] is
syntactically a directory (see @scheme[split-path]) or if the path has
no extension, @scheme[#f] is returned.}

@defproc[(find-relative-path [base (or/c path-string? path-for-some-system?)]
                             [path (or/c path-string?  path-for-some-system?)])
         path-for-some-system?]{

Finds a relative pathname with respect to @scheme[base] that names
the same file or directory as @scheme[path]. Both @scheme[base]
and @scheme[path] must be simplified in the sense of
@scheme[simple-form-path].  If @scheme[path] is not a proper subpath
of @scheme[base] (i.e., a subpath that is strictly longer),
@scheme[path] is returned.}

@defproc[(normalize-path [path path-string?]
                         [wrt (and/c path-string? complete-path?)
                              (current-directory)]) 
         path?]{

Returns a normalized, complete version of @scheme[path], expanding the
path and resolving all soft links. If @scheme[path] is relative, then
@scheme[wrt] is used as the base path.

Letter case is @italic{not} normalized by @scheme[normalize-path]. For
this and other reasons, such as whether the path is syntactically a
directory, the result of @scheme[normalize-path] is not suitable for
comparisons that determine whether two paths refer to the same file or
directory (i.e., the comparison may produce false negatives).

An error is signaled by @scheme[normalize-path] if the input
path contains an embedded path for a non-existent directory,
or if an infinite cycle of soft links is detected.}

@defproc[(path-only [path (or/c path-string? path-for-some-system?)])
         path-for-some-system?]{

If @scheme[path] is a filename, the file's path is returned. If
@scheme[path] is syntactically a directory, @scheme[path] is returned
(as a path, if it was a string).}

@defproc[(simple-form-path [path path-string?]) path?]{

Returns @scheme[(simplify-path (path->complete-path path))], which
ensures that the result is a complete path containing no up- or
same-directory indicators.}

@defproc[(some-system-path->string [path path-for-some-system?])
         string?]{

Converts @scheme[path] to a string using a UTF-8 encoding of the
path's bytes.

Use this function when working with paths for a different system
(whose encoding of pathnames might be unrelated to the current
locale's encoding) and when starting and ending with strings.}

@defproc[(string->some-system-path [str string?]
                                   [kind (or/c 'unix 'windows)])
         path-for-some-system?]{

Converts @scheme[str] to a @scheme[kind] path using a UTF-8 encoding
of the path's bytes.

Use this function when working with paths for a different system
(whose encoding of pathnames might be unrelated to the current
locale's encoding) and when starting and ending with strings.}

@;------------------------------------------------------------------------
@include-section["unix-paths.scrbl"]
@include-section["windows-paths.scrbl"]
