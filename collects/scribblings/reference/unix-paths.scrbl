#lang scribble/doc
@(require scribble/bnf "mz.rkt")

@title[#:tag "unixpaths"]{@|AllUnix| Paths}

In @|AllUnix| paths, a @litchar{/} separates elements of the path,
@litchar{.} as a path element always means the directory indicated by
preceding path, and @litchar{..} as a path element always means the
parent of the directory indicated by the preceding path. A leading
@litchar{~} in a path is not treated specially, but
@racket[expand-user-path] can be used to convert a leading @litchar{~}
element to a user-specific directory. No other character or byte has a
special meaning within a path. Multiple adjacent @litchar{/} are
equivalent to a single @litchar{/} (i.e., they act as a single path
separator).

A path root is always @litchar{/}. A path starting with @litchar{/} is
an absolute, complete path, and a path starting with any other
character is a relative path.

Any pathname that ends with a @litchar{/} syntactically refers to a
directory, as does any path whose last element is @litchar{.} or
@litchar{..}.

A @|AllUnix| path is @techlink{cleanse}d by replacing multiple adjacent
@litchar{/}s with a single @litchar{/}.

For @racket[(bytes->path-element _bstr)], @racket[bstr] must not
contain any @litchar{/}, otherwise the @exnraise[exn:fail:contract].
The result of @racket[(path-element->bytes _path)] or
@racket[(path-element->string _path)] is always the same as the result
of @racket[(path->bytes _path)] and @racket[(path->string
_path)]. Since that is not the case for other platforms, however,
@racket[path-element->bytes] and @racket[path-element->string] should
be used when converting individual path elements.

On Mac OS X, Finder aliases are zero-length files.
