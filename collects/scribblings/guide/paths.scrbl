#lang scribble/doc
@(require scribble/manual
          scribble/eval
          "guide-utils.ss")

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
@scheme[display] form of a path is the same as the @scheme[display]
form of its string encodings.

@examples[
(string->path "my-data.txt")
(file-exists? "my-data.txt")
(file-exists? (string->path "my-data.txt"))
(display (string->path "my-data.txt"))
]

Produces that produce references to the filesystem always produce path
values, instead of strings.

@examples[
(path-replace-suffix "foo.scm" #".ss")
]

Although it's sometimes tempting to directly manipulate strings that
represent filesystem paths, correctly manipulating a path can be
surprisingly difficult. For example, if you start under Unix with the
absolute path @filepath{/tmp/~} and take just the last part, you end up
with @filepath{~}---which looks like a reference to the current user's
home directory, instead of a relative path to a file of directory
named @filepath{~}. Windows path manipulation, furthermore, is far
trickier, because path elements like @filepath{aux} can have special
meanings.

@refdetails/gory["windows-path"]{Windows filesystem paths}

Use procedures like @scheme[split-path] and @scheme[build-path] to
deconstruct and construct paths. When you must manipulate the name of
a specific path element (i.e., a file or directory component in a
path), use procedures like @scheme[path-element->bytes] and
@scheme[bytes->path-element].

@examples[
(build-path "easy" "file.ss")
(split-path (build-path "easy" "file.ss"))
]
