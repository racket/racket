#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@title[#:tag "paths"]{Paths}

A @deftech{path} encapsulates a filesystem path that (potentially)
names a file or directory. Although paths can be converted to and from
strings and byte strings, neither strings nor byte strings are
suitable for representing general paths. The problem is that paths are
represented in the filesystem as either byte sequences or UTF-16
sequences (depending on the operating systems); the sequences are not
always human-readable, and not all sequences can be decoded to Unicode
scalar values.

Despite the occasional encoding problems, most paths can be converted
to and from strings. Thus, procedures that accept a path argument
always accept a string, and the printed form of a path uses the string
decoding of the path inside @litchar{#<path:} and @litchar{>}. The
@racket[display] form of a path is the same as the @racket[display]
form of its string encodings.

@examples[
(string->path "my-data.txt")
(file-exists? "my-data.txt")
(file-exists? (string->path "my-data.txt"))
(display (string->path "my-data.txt"))
]

Procedures that produce references to the filesystem always produce path
values, instead of strings.

@examples[
(path-replace-suffix "foo.scm" #".rkt")
]

Although it's sometimes tempting to directly manipulate strings that
represent filesystem paths, correctly manipulating a path can be
surprisingly difficult. Windows path manipulation is especially
tricky, because path elements like @filepath{aux} can have special
meanings.

@refdetails/gory["windows-path"]{Windows filesystem paths}

Use procedures like @racket[split-path] and @racket[build-path] to
deconstruct and construct paths. When you must manipulate the name of
a specific path element (i.e., a file or directory component in a
path), use procedures like @racket[path-element->bytes] and
@racket[bytes->path-element].

@examples[
(build-path "easy" "file.rkt")
(split-path (build-path "easy" "file.rkt"))
]
