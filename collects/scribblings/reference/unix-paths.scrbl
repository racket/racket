#reader(lib "docreader.ss" "scribble")
@require[(lib "bnf.ss" "scribble")]
@require["mz.ss"]

@title[#:tag "unixpaths"]{@|AllUnix| Paths}

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

A @|AllUnix| path is @tech{path-expand}ed by replacing a
home-directory specification (starting with @litchar{~}) with an
absolute path, and by replacing multiple adjacent @litchar{/}s with a
single @litchar{/}.

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
