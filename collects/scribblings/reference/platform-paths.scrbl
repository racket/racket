#reader(lib "docreader.ss" "scribble")
@require[(lib "bnf.ss" "scribble")]
@require["mz.ss"]

@define[(fileFirst s) (index (list s) (file s))]
@define[MzAdd (italic "Scheme-specific:")]

@title{Platform-Specific Path Conventions}

@section[#:tag "mz:unixpaths"]{@|AllUnix| Paths}

In @|AllUnix| paths, a @litchar{/} separates elements of the path,
@litchar{.} as a path element always means the directory indicated by
preceding path, and @litchar{..} as a path element always means the
parent of the directory indicated by the preceding path. A path that
starts with a @litchar{~} indicates a user's home directory; the
username follows the @litchar{~} (before a @litchar{/} or the end of
the path), where @litchar{~} by itself indicates the home directory of
the current user. No other character or byte has a special meaning
within a path. Multiple adjacent @litchar{/} are equivalent to a
single @litchar{/} (i.e., they act as a single path separator).

A path root is either @litchar{/} or a home-directory specification
starting with @litchar{~}. A relative path whose first element starts
with a @litchar{~} is encoded by prefixing the path with @litchar{./}.

Any pathname that ends with a @litchar{/} syntactically refers to a
directory, as does any path whose last element is @litchar{.} or
@litchar{..}, or any path that contains only a root.

A @|AllUnix| path is @tech{expanded} by replacing a home-directory
specification (starting with @litchar{~}) with an absolute path, and by
replacing multiple adjacent @litchar{/}s with a single @litchar{/}.

For @scheme[(bytes->path-element _bstr)], @scheme[bstr] can start with
a @litchar{~}, and it is encoded as a literal part of the path element
using a @litchar{./} prefix. The @scheme[_bstr] argument must not
contain @litchar{/}, otherwise the @exnraise[exn:fail:contract].

For @scheme[(path-element->bytes _path)] or
@scheme[(path-element->string _path)], if the bytes form of
@scheme[_path] starts with @litchar{~/.}, the @litchar{./} prefix is
not included in the result.

For @scheme[(build-path _base-path _sub-path ...)], when a
@scheme[_sub-path] starts with @litchar{./~}, the @litchar{./} is
removed before adding the path. This conversion is performed because
an initial sequence @litchar{./~} is the canonical way of representing
relative paths whose first element's name starts with @litchar{~}.

For @scheme[(simplify-path _path _use-filesystem?)], if @scheme[_path]
starts @litchar{./~}, the leading period is the only indicator, and
there are no redundant @litchar{/}s, then @scheme[_path] is returned.

For @scheme[(split-path _path)] producing @scheme[_base],
@scheme[_name], and @scheme[_must-be-dir?], the result @scheme[name]
can start with @litchar{./~} if the result would otherwise start with
@litchar{~} and it is not the start of @scheme[_path]. Furthermore, if
@scheme[path] starts with @litchar{./~} with any non-zero number of
@litchar{/}s between @litchar{.} and @litchar{~}, then the
@litchar{./} is kept with the following element (i.e., they are not
split separately).

Under Mac OS X, Finder aliases are zero-length files.

@;------------------------------------------------------------------------
@section[#:tag "mz:windowspaths"]{Windows Path Conventions}

In general, a Windows pathname consists of an optional drive specifier
and a drive-specific path. As noted in @secref["mz:filesystem"], a
Windows path can be @defterm{absolute} but still relative to the
current drive; such paths start with a @litchar{/} or @litchar["\\"]
separator and are not UNC paths or paths that start with
@litchar["\\\\?\\"].

A path that starts with a drive specification is @defterm{complete}.
Roughly, a drive specification is either a Roman letter followed by a
colon, a UNC path of the form
@litchar["\\\\"]@nonterm{machine}@litchar["\\"]@nonterm{volume}, or a
@litchar["\\\\?\\"] form followed by something other than
@litchar["REL\\"]@nonterm{element}, or
@litchar["RED\\"]@nonterm{element}. (Variants of @litchar["\\\\?\\"]
paths are described further below.)

Scheme fails to implement the usual Windows path syntax in one
way. Outside of Scheme, a pathname @file{C:rant.txt} can be a
drive-specific relative path. That is, it names a file @file{rant.txt}
on drive @file{C:}, but the complete path to the file is determined by
the current working directory for drive @file{C:}. Scheme does not
support drive-specific working directories (only a working directory
across all drives, as reflected by the @scheme[current-directory]
parameter). Consequently, Scheme implicitly converts a path like
@file{C:rant.txt} into @file["C:\\rant.txt"].

@itemize{

 @item{@|MzAdd| Whenever a path starts with a drive specifier
       @nonterm{letter}@litchar{:} that is not followed by a
       @litchar{/} or @litchar["\\"], a @litchar["\\"] is inserted as
       the path is expanded.}

}


Otherwise, Scheme follows standard Windows path conventions, but also
 adds @litchar["\\\\?\\REL"] and @litchar["\\\\?\\RED"] conventions to
 deal with paths inexpressible in the standard conventsion, plus
 conventions to deal with excessive @litchar["\\"]s in @litchar["\\\\?\\"]
 paths.

In the following, @nonterm{letter} stands for a Roman letter (case
does not matter), @nonterm{machine} stands for any sequence of
characters that does not include @litchar["\\"] or @litchar{/} and is
not @litchar{?}, @nonterm{volume} stands for any sequence of
characters that does not include @litchar["\\"] or @litchar{/} , and
@nonterm{element} stands for any sequence of characters that does not
include @litchar["\\"].

 @itemize{

 @item{Trailing spaces and @litchar{.} in a path element are ignored
       when the element is the last one in the path, unless the path
       starts with @litchar["\\\\?\\"] or the element consists of only
       spaces and @litchar{.}s.}

 @item{The following special ``files'', which access devices, exist in
       all directories, case-insensitively, and with all possible
       endings after a period or colon, except in pathnames that start
       with @litchar["\\\\?\\"]: @fileFirst{NUL}, @fileFirst{CON},
       @fileFirst{PRN}, @fileFirst{AUX}, @fileFirst{COM1},
       @fileFirst{COM2}, @fileFirst{COM3}, @fileFirst{COM4},
       @fileFirst{COM5}, @fileFirst{COM6}, @fileFirst{COM7},
       @fileFirst{COM8}, @fileFirst{COM9}, @fileFirst{LPT1},
       @fileFirst{LPT2}, @fileFirst{LPT3}, @fileFirst{LPT4},
       @fileFirst{LPT5}, @fileFirst{LPT6}, @fileFirst{LPT7},
       @fileFirst{LPT8}, @fileFirst{LPT9}.}

 @item{Except for @litchar["\\\\?\\"] paths, @litchar{/}s are
       equivalent to @litchar["\\"]s. Except for @litchar["\\\\?\\"]
       paths and the start of UNC paths, multiple adjacent
       @litchar{/}s and @litchar["\\"]s count as a single
       @litchar["\\"]. In a path that starts @litchar["\\\\?\\"]
       paths, elements can be separated by either a single or double
       @litchar["\\"].}

 @item{A directory can be accessed with or without a trailing
       separator. In the case of a non-@litchar["\\\\?\\"] path, the
       trailing separator can be any number of @litchar{/}s and
       @litchar["\\"]s; in the case of a @litchar["\\\\?\\"] path, a
       trailing separator must be a single @litchar["\\"], except that
       two @litchar["\\"]s can follow
       @litchar["\\\\?\\"]@nonterm{letter}@litchar{:}.}

 @item{Except for @litchar["\\\\?\\"] paths, a single @litchar{.} as a
       path element means ``the current directory,'' and a
       @litchar{..} as a path element means ``the parent directory.''
       Up-directory path elements (i.e., @litchar{..}) immediately
       after a drive are ignored.}

 @item{A pathname that starts
       @litchar["\\\\"]@nonterm{machine}@litchar["\\"]@nonterm{volume}
       (where a @litchar{/} can replace any @litchar["\\"]) is a UNC
       path, and the starting
       @litchar["\\\\"]@nonterm{machine}@litchar["\\"]@nonterm{volume}
       counts as the drive specifier.}

  @item{Normally, a path element cannot contain any of the following
        characters:

        @centerline{@litchar{<} @litchar{>} @litchar{:} @litchar{"} @litchar{/} @litchar["\\"] @litchar["|"]}

        Except for @litchar["\\"], path elements containing these
        characters can be accessed using a @litchar["\\\\?\\"] path
        (assuming that the underlying filesystem allows the
        characters).}

  @item{In a pathname that starts
        @litchar["\\\\?\\"]@nonterm{letter}@litchar[":\\"], the
        @litchar["\\\\?\\"]@nonterm{letter}@litchar[":\\"] prefix
        counts as the path's drive, as long as the path does not both
        contain non-drive elements and end with two consecutive
        @litchar["\\"]s, and as long as the path contains no sequence
        of three or more @litchar["\\"]s. Two @litchar["\\"]s can
        appear in place of the @litchar["\\"] before
        @nonterm{letter}. A @litchar{/} cannot be used in place of a
        @litchar["\\"] (but @litchar{/}s can be used in element names,
        though the result typically does not name an actual directory
        or file).}
       
  @item{In a pathname that starts
        @litchar["\\\\?\\UNC\\"]@nonterm{machine}@litchar["\\"]@nonterm{volume},
        the
        @litchar["\\\\?\\UNC\\"]@nonterm{machine}@litchar["\\"]@nonterm{volume}
        prefix counts as the path's drive, as long as the path does
        not end with two consecutive @litchar["\\"]s, and as long as
        the path contains no sequence of three or more
        @litchar["\\"]s. Two @litchar["\\"]s can appear in place of
        the @litchar["\\"] before @litchar{UNC}, the @litchar["\\"]s
        after @litchar{UNC}, and/or the @litchar["\\"]s
        after@nonterm{machine}.  The letters in the @litchar{UNC} part
        can be uppercase or lowercase, and @litchar{/} cannot be used
        in place of @litchar["\\"]s (but @litchar{/} can be used in
        element names).}

  @item{@|MzAdd| A pathname that starts
        @litchar["\\\\?\\REL\\"]@nonterm{element} or
        @litchar["\\\\?\\REL\\\\"]@nonterm{element} is a relative
        path, as long as the path does not end with two consecutive
        @litchar["\\"]s, and as long as the path contains no sequence of
        three or more @litchar["\\"]s. This Scheme-specific path form
        supports relative paths with elements that are not normally
        expressible in Windows paths (e.g., a final element that ends
        in a space). The @litchar{REL} part must be exactly the three
        uppercase letters, and @litchar{/}s cannot be used in place
        of @litchar["\\"]s. If the path starts
        @litchar["\\\\?\\REL\\.."]  then for as long as the
        path continues with repetitions of @litchar["\\.."],
        each element counts as an up-directory element; a single
        @litchar["\\"] must be used to separate the up-directory
        elements. As soon as a second @litchar["\\"] is used to separate
        the elements, or as soon as a non-@litchar{..}  element is
        encountered, the remaining elements are all literals (never
        up-directory elements). When a @litchar["\\\\?\\REL"] path
        value is converted to a string (or when the path value is
        written or displayed), the string does not contain the
        starting @litchar["\\\\?\\REL"] or the immediately following
        @litchar["\\"]s; converting a path value to a byte string
        preserves the @litchar["\\\\?\\REL"] prefix.}

  @item{@|MzAdd| A pathname that starts
        @litchar["\\\\?\\RED\\"]@nonterm{element} or
        @litchar["\\\\?\\RED\\\\"]@nonterm{element} is a
        drive-relative path, as long as the path does not end with two
        consecutive @litchar["\\"]s, and as long as the path contains
        no sequence of three or more @litchar["\\"]s. This
        Scheme-specific path form supports drive-relative paths (i.e.,
        absolute given a drive) with elements that are not normally
        expressible in Windows paths. The @litchar{RED} part must be
        exactly the three uppercase letters, and @litchar{/}s cannot
        be used in place of @litchar["\\"]s. Unlike
        @litchar["\\\\?\\REL"] paths, a @litchar{..} element is always
        a literal path element. When a @litchar["\\\\?\\RED"] path
        value is converted to a string (or when the path value is
        written or displayed), the string does not contain the
        starting @litchar["\\\\?\\RED"] and it contains a single
        starting @litchar["\\"]; converting a path value to a byte
        string preserves the @litchar["\\\\?\\RED"] prefix.}

}

Three additional Scheme-specific rules provide meanings to character
sequences that are otherwise ill-formed as Windows paths:

@itemize{

  @item{@|MzAdd| In a pathname of the form
        @litchar["\\\\?\\"]@nonterm{any}@litchar["\\\\"] where
        @nonterm{any} is any non-empty sequence of characters other
        than @nonterm{letter}@litchar{:} or
        @litchar["\\"]@nonterm{letter}@litchar{:}, the entire path
        counts as the path's (non-existent) drive.}

  @item{@|MzAdd| In a pathname of the form
        @litchar["\\\\?\\"]@nonterm{any}@litchar["\\\\\\"]@nonterm{elements},
        where @nonterm{any} is any non-empty sequence of characters
        and @nonterm{elements} is any sequence that does not start
        with a @litchar["\\"], does not end with two @litchar["\\"]s,
        and does not contain a sequence of three @litchar["\\"]s, then
        @litchar["\\\\?\\"]@nonterm{any}@litchar["\\\\"] counts as the
        path's (non-existent) drive.}

  @item{@|MzAdd| In a pathname that starts @litchar["\\\\?\\"] and
        does not match any of the patterns from the preceding bullets,
        @litchar["\\\\?\\"] counts as the path's (non-existent)
        drive.}

}

Outside of Scheme, except for @litchar["\\\\?\\"] paths, pathnames are
 typically limited to 259 characters. Scheme internally converts
 pathnames to @litchar["\\\\?\\"] form as needed to avoid this
 limit. The operating system cannot access files through
 @litchar["\\\\?\\"] paths that are longer than 32,000 characters or
 so.

Where the above descriptions says ``character,'' substitute ``byte''
for interpreting byte strings as paths. The encoding of Windows paths
into bytes preserves ASCII characters, and all special characters
mentioned above are ASCII, so all of the rules are the same.

Beware that the @litchar["\\"] path separator is an escape character
in Scheme strings. Thus, the path @litchar["\\\\?\\REL\\..\\\\.."]  as
a string must be written @scheme["\\\\?\\REL\\..\\\\.."].

A path that ends with a directory separator syntactically refers to a
directory.  In addition, a path syntactcially refers to a directory if
its last element is a same-directory or up-directory indicator (not
quoted by a @litchar["\\\\?\\"] form), or if it refers to a root.

Windows paths are expanded as follows: In paths that start
@litchar["\\\\?\\"], redundant @litchar["\\"]s are removed, an extra
@litchar["\\"] is added in a @litchar["\\\\?\\REL"] if an extra one is
not already present to separate up-directory indicators from literal
path elements, and an extra @litchar["\\"] is similarly added after
@litchar["\\\\?\\RED"] if an extra one is not already present. When
@litchar["\\\\?\\"] acts as the root and the path contains, to
additional @litchar{/}s (which might otherwise be redundant) are
included after the root. For other paths, multiple @litchar{/}s are
converted to single @litchar{/}s (except at the beginning of a shared
folder name), a @litchar{/} is inserted after the colon in a drive
specification if it is missing.

For @scheme[(bytes->path-element _bstr)], @litchar{/}s, colons,
trailing dots, trailing whitespace, and special device names (e.g.,
``aux'') in @scheme[_bstr] are encoded as a literal part of the path
element by using a @litchar["\\\\?\\REL"] prefix.  The @scheme[bstr]
argument must not contain a @litchar["\\"], otherwise the
@exnraise[exn:fail:contract].

For @scheme[(path-element->bytes _path)] or
@scheme[(path-element->string _path)], if the byte-string form of
@scheme[_path] starts with a @litchar["\\\\?\\REL"], the prefix is not
included in the result.

For @scheme[(build-path _base-path _sub-path ...)], trailing spaces
and periods are removed from the last element of @scheme[_base-path]
and all but the last @scheme[_sub-path] (unless the element consists of
only spaces and peroids), except for those that start with
@litchar["\\\\?\\"]. If @scheme[_base-path] starts @litchar["\\\\?\\"],
then after each non-@litchar["\\\\?\\REL\\"] and
non-@litchar["\\\\?\\RED\\"] @scheme[_sub-path] is added, all
@litchar{/}s in the addition are converted to @litchar["\\"]s,
multiple consecutive @litchar["\\"]s are converted to a single
@litchar["\\"], added @litchar{.} elements are removed, and added
@litchar{..} elements are removed along with the preceding element;
these conversions are not performed on the original @scheme[_base-path]
part of the result or on any @litchar["\\\\?\\REL\\"] or
@litchar["\\\\?\\RED\\"] or @scheme[_sub-path].  If a
@litchar["\\\\?\\REL\\"] or @litchar["\\\\?\\RED\\"]
@scheme[_sub-path] is added to a non-@litchar["\\\\?\\"]
@scheme[_base-path], the the @scheme[_base-path] (with any additions up
to the @litchar["\\\\?\\REL\\"] or @litchar["\\\\?\\RED\\"]
@scheme[_sub-path]) is simplified and converted to a
@litchar["\\\\?\\"] path.  In other cases, a @litchar["\\"] may be
added or removed before combining paths to avoid changing the root
meaning of the path (e.g., combining @litchar{//x} and @litchar{y}
produces @litchar{/x/y}, because @litchar{//x/y} would be a UNC path
instead of a drive-relative path).

For @scheme[(simplify-path _path _use-filesystem?)], @scheme[_path] is
expanded, and if @scheme[_path] does not start with
@litchar["\\\\?\\"], trailing spaces and periods are removed, a
@litchar{/} is inserted after the colon in a drive specification if it
is missing, and a @litchar["\\"] is inserted after @litchar["\\\\?\\"]
as a root if there are elements and no extra @litchar["\\"]
already. Otherwise, if no indicators or redundant separators are in
@scheme[_path], then @scheme[_path] is returned.

For @scheme[(split-path _path)] producing @scheme[_base],
@scheme[_name], and @scheme[_must-be-dir?], splitting a path that does
not start with @litchar["\\\\?\\"] can produce parts that start with
@litchar["\\\\?\\"]. For example, splitting @litchar{C:/x~/aux/}
produces @litchar["\\\\?\\C:\\x~\\"] and @litchar["\\\\?\\REL\\\\aux"];
the @litchar["\\\\?\\"] is needed in these cases to preserve a
trailing space after @litchar{x} and to avoid referring to the AUX
device instead of an @file{aux} file.
