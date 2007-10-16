#reader(lib "docreader.ss" "scribble")
@require[(lib "manual.ss" "scribble")]
@require[(lib "eval.ss" "scribble")]
@require["guide-utils.ss"]

@title[#:tag "module-paths"]{Module Paths}

A @deftech{module path} is a reference to a module, as used with
@scheme[require] or as the @scheme[_initial-module-path] in a
@scheme[module] form. It can be any of several forms:

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@specsubform[id]{

A @tech{module path} that is just an identifier refers to a non-file
@scheme[module] declaration using the identifier. This form of module
reference makes the most sense in a @tech{REPL}.

@examples[
(module m (lib "big/lang.ss")
  (provide color)
  (define color "blue"))
(module n (lib "big/lang.ss")
  (require m)
  (printf "my favorite color is ~a\n" color))
(require n)
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

Like a plain-string @scheme[rel-path], but the elements of
@scheme[rel-path] refer to @tech{collection}s and
sub-@tech{collections}, instead of directories and sub-directories. A
@deftech{collection} is represented by a directory in one of several
installation-specific locations.

An example of this form is @scheme[(lib "big/lang.ss")], which is
commonly uses at the initial import. The path @scheme[(lib
"big/lang.ss")], refers to the module whose source is the
@file{lang.ss} file in the @file{big} collection, which is installed
as part of PLT Scheme.

@examples[
(module m (lib "big/lang.ss")
  (require (lib "mzlib/date.ss"))

  (printf "Today is ~s\n"
          (date->string (seconds->date (current-seconds)))))
(require m)
]}

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@specsubform/subs[#:literals (planet)
                  (planet rel-string (user-string pkg-string vers ...))
                  ([vers nat
                         (nat nat)
                         (= nat)
                         (+ nat)
                         (- nat)])]{

Accesses a third-party library that is distributed through the
@|PLaneT| server. A @|PLaneT| reference starts like a @scheme[lib]
reference, with a relative path, but the path is followed by
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
 (module m (lib "big/lang.ss")
   (require (planet "random.ss" ("schematics" "random.plt" 1 0)))
   (display (random-gaussian)))
 (void))
(eval:alts
 (require m)
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
