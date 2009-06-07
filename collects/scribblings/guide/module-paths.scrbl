#lang scribble/doc
@(require scribble/manual
          scribble/eval
          "guide-utils.ss")

@title[#:tag "module-paths"]{Module Paths}

A @deftech{module path} is a reference to a module, as used with
@scheme[require] or as the @scheme[_initial-module-path] in a
@scheme[module] form. It can be any of several forms:

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@specsubform[#:literals (quote) (#,(scheme quote) id)]{

A @tech{module path} that is a quoted identifier refers to a non-file
@scheme[module] declaration using the identifier. This form of module
reference makes the most sense in a @tech{REPL}.

@examples[
(module m scheme
  (provide color)
  (define color "blue"))
(module n scheme
  (require 'm)
  (printf "my favorite color is ~a\n" color))
(require 'n)
]}

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@specsubform[id]{

A @tech{module path} that is an unquoted identifier refers to an
installed library. The @scheme[id] is constrained to contain only
ASCII letters, ASCII numbers, @litchar{+}, @litchar{-}, @litchar{_},
and @litchar{/}, where @litchar{/} separates path elements within the
identifier. The elements refer to @tech{collection}s and
sub-@tech{collections}, instead of directories and sub-directories.

An example of this form is @scheme[scheme/date]. It refers to the
module whose source is the @filepath{date.ss} file in the
@filepath{scheme} collection, which is installed as part of PLT
Scheme. The @filepath{.ss} suffix is added automatically.

Another example of this form is @scheme[scheme], which is commonly
used at the initial import. The path @scheme[scheme] is shorthand for
@scheme[scheme/main]; when an @scheme[id] has no @litchar{/}, then
@scheme[/main] is automatically added to the end. Thus,
@scheme[scheme] or @scheme[scheme/main] refers to the module whose
source is the @filepath{main.ss} file in the @filepath{scheme}
collection.

@examples[
(module m scheme
  (require scheme/date)

  (printf "Today is ~s\n"
          (date->string (seconds->date (current-seconds)))))
(require 'm)
]}

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@specsubform[rel-string]{

A string @tech{module path} is a relative path using Unix-style
conventions: @litchar{/} is the path separator, @litchar{..} refers to
the parent directory, and @litchar{.} refers to the same
directory. The @scheme[rel-string] must not start or end with a path
separator.

The path is relative to the enclosing file, if any, or it is relative
to the current directory. (More precisely, the path is relative to the
value of @scheme[(current-load-relative-directory)], which is set
while loading a file.)

@secref["module-basics"] shows examples using relative paths.
}

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@specsubform[#:literals (lib)
             (lib rel-string)]{

Like an unquoted-identifier path, but expressed as a string instead of
an identifier. Also, the @scheme[rel-string] can end with a file
suffix, in case the relevant suffix is not @filepath{.ss}.

Example of this form include @scheme[(lib "scheme/date.ss")] and
@scheme[(lib "scheme/date")], which are equivalent to
@scheme[scheme/date]. Other examples include @scheme[(lib "scheme")],
@scheme[(lib "scheme/main")], and @scheme[(lib "scheme/main.ss")],
which are all equivalent to @scheme[scheme].

@examples[
(module m (lib "scheme")
  (require (lib "scheme/date.ss"))

  (printf "Today is ~s\n"
          (date->string (seconds->date (current-seconds)))))
(require 'm)
]}

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@specsubform[#:literals (planet)
             (planet id)]{

Accesses a third-party library that is distributed through the
@|PLaneT| server. The library is downloaded the first time that it is
needed, and then the local copy is used afterward.

The @scheme[id] encodes several pieces of information separated by a
@litchar{/}: the package owner, then package name with optional
version information, and an optional path to a specific library with
the package. Like @scheme[id] as shorthand for a @scheme[lib] path, a
@filepath{.ss} suffix is added automatically, and @schemeidfont{/main}
is used as the path if no sub-path element is supplied.

@examples[
(eval:alts
 (module m (lib "scheme")
   (code:comment @#,t{Use @filepath{schematics}'s @filepath{random.plt} 1.0, file @filepath{random.ss}:})
   (require (planet schematics/random:1/random))
   (display (random-gaussian)))
 (void))
(eval:alts
 (require 'm)
 (display 0.9050686838895684))
]
}

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@specsubform[#:literals (planet)
             (planet package-string)]{

Like the symbol form of a @scheme[planet], but using a string instead
of an identifier. Also, the @scheme[package-string] can end with a
file suffix, in case the relevant suffix is not @filepath{.ss}.
}

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@specsubform/subs[#:literals (planet = + -)
                  (planet rel-string (user-string pkg-string vers ...))
                  ([vers nat
                         (nat nat)
                         (= nat)
                         (+ nat)
                         (- nat)])]{

A more general form to access a library from the @|PLaneT| server. In
this general form, a @|PLaneT| reference starts like a @scheme[lib]
reference with a relative path, but the path is followed by
information about the producer, package, and version of the
library. The specified package is downloaded and installed on demand.

The @scheme[vers]es specify a constraint on the acceptable version of
the package, where a version number is a sequence of non-negative
integers, and the constraints determine the allowable values for each
element in the sequence. If no constraint is provided for a particular
element, then any version is allowed; in particular, omitting all
@scheme[vers]es means that any version is acceptable. Specifying at
least one @scheme[vers] is strongly recommended.

For a version constraint, a plain @scheme[nat] is the same as
@scheme[(+ nat)], which matches @scheme[nat] or higher for the
corresponding element of the version number.  A @scheme[(_start-nat
_end-nat)] matches any number in the range @scheme[_start-nat] to
@scheme[_end-nat], inclusive. A @scheme[(= nat)] matches only exactly
@scheme[nat]. A @scheme[(- nat)] matches @scheme[nat] or lower.

@examples[
(eval:alts
 (module m (lib "scheme")
   (require (planet "random.ss" ("schematics" "random.plt" 1 0)))
   (display (random-gaussian)))
 (void))
(eval:alts
 (require 'm)
 (display 0.9050686838895684))
]
}

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@specsubform[#:literals (file)
             (file string)]{

Refers to a file, where @scheme[string] is a relative or absolute path
using the current platform's conventions. This form is not portable,
and it should @italic{not} be used when a plain, portable
@scheme[rel-string] suffices.

}
