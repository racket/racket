#lang scribble/doc
@require["common.rkt"
         scribble/examples
         (for-label file/glob racket/sequence)]

@(define glob-eval (make-base-eval '(require file/glob)))

@title[#:tag "glob"]{Globbing}

@defmodule[file/glob]{The @racketmodname[file/glob] library implements
@hyperlink["https://en.wikipedia.org/wiki/Glob_(programming)"]{globbing}
for @racket[path-string?] values. A @emph{glob} is a path string that matches
a set of path strings using the following @deftech[#:key "glob-wildcard"]{wildcards}:
@itemlist[
@item{
  A sextile (@tt{*}) matches any sequence of characters in a file or directory
  name.
}
@item{
  Two sextiles (@tt{**}) match any sequence of characters and any number of
  path separators.
}
@item{
  A question mark (@tt{?}) matches any single character in a file or directory
  name.
}
@item{
  Square bracket-delimited character groups, e.g. @tt{[abc]}, match any
  character within the group. The square brackets have the same meaning in globs
  as in regular expressions, see
  @secref["regexp-syntax" #:doc '(lib "scribblings/reference/reference.scrbl")].
}
@item{
  If the glob ends with a path separator (@tt{/} on any @racket[(system-type)],
  additionally @tt{\} on @racket['windows])
  then it only matches directories.
}
]

By default, wildcards will not match files or directories whose name begins
with a period (aka "dotfiles").  To override, set the parameter
@racket[glob-capture-dotfiles?] to a non-@racket[#f] value or supply a similar
value using the @racket[#:capture-dotfiles?] keyword.
}

@defthing[glob/c (or/c path-string? (sequence/c path-string?))]{
A @tech[#:doc '(lib "scribblings/reference/reference.scrbl")]{flat contract}
that accepts a glob or a sequence of globs.
}

All @racketmodname[file/glob] functions accept @racket[glob/c] values.
These functions also recognize braces (@tt|{{}}|) as a @deftech[#:key "glob-meta-wildcard"]{meta-wildcard} for
describing multiple globs.
@margin-note{Braces are interpreted @emph{before} any other wildcards.}

@itemlist[
@item{
Brace-delimited, comma-separated character groups, e.g. @tt|{{foo,bar}}|,
expand to multiple globs before the @racketmodname[file/glob] module begins
matching. For example, the @racket[glob/c] value @racket|{{foo,bar}.rkt}|
has the same meaning as @racket['("foo.rkt" "bar.rkt")].
}
]

@defproc[(glob [pattern glob/c] [#:capture-dotfiles? capture-dotfiles? boolean? (glob-capture-dotfiles?)]) (listof path-string?)]{
  Builds a list of all paths on the current filesystem that match any glob
  in @racket[pattern]. The order of paths in the result is unspecified.

  If @racket[pattern] contains the wildcard @tt{**}, then @racket[glob]
  recursively searches the filesystem to find matches.
  For example, the glob @racket{/**.rkt} will search the @emph{entire filesystem}
  for files or directories with a @racket{.rkt} suffix (aka, Racket files).

Examples:
@codeblock{
> (glob "*.rkt")
;; Lists all Racket files in current directory

> (glob "*/*.rkt")
;; Lists all Racket files in all sub-directories of the current directory.
;; (Does not search sub-sub-directories, etc.)

> (glob (build-path (find-system-path 'home-dir) "**" "*.rkt"))
;; Recursively searches home directory for Racket files, lists all matches.

> (glob "??.rkt")
;; Lists all Racket files in current directory with 2-character names.

> (glob "[a-z0-9].rkt")
;; Lists all Racket files in current directory with single-character,
;; alphanumeric names.

> (glob '("foo-bar.rkt" "foo-baz.rkt" "qux-bar.rkt" "qux-baz.rkt"))
;; Filters the list to contain only files or directories that exist.

> (glob "{foo,qux}-{bar,baz}.rkt")
;; Same as above, returns at most 4 files.
}
}

@defproc[(in-glob [pattern glob/c] [#:capture-dotfiles? capture-dotfiles? boolean? (glob-capture-dotfiles?)]) (sequence/c path-string?)]{
  Returns a stream of all paths matching the glob @racket[pattern],
  instead of eagerly building a list.
}

@defproc[(glob-match? [pattern glob/c]
                      [path path-string?]
                      [#:capture-dotfiles? capture-dotfiles? boolean? (glob-capture-dotfiles?)]) boolean?]{
  Analogous to @racket[regexp-match?]; returns @racket[#true] if @racket[path]
  matches any glob in @racket[pattern].

  @racket[(glob-match? pattern path)] is @emph{not} the same as:
  @racketblock[
    (member path (glob pattern))
  ]
  because @racket[glob] only returns files/directories that exist, whereas
  @racket[glob-match?] does not check that @racket[path] exists.

  This operation accesses the filesystem.
}

@defproc*[([(glob-quote [str string?]) string?]
           [(glob-quote [path path?]) path?]
)]{
  Escapes all @tech{glob wildcards} and @tech{glob meta-wildcards} in the given
  string or path string.

  @examples[#:eval glob-eval
    (glob-quote "*.rkt")
    (glob-quote "[Ff]ile?{zip,tar.gz}")
    (glob-quote "]")
  ]
}

@defparam[glob-capture-dotfiles? capture-dotfiles? boolean? #:value #f]{
  Determines whether wildcards match names that begin with a @racket[#\.]
  character.  If @racket[#t], the wildcards will match dotfiles.  If
  @racket[#f], use a glob such as @racket{.*} to match dotfiles explicitly.
}

