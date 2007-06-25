#reader(lib "docreader.ss" "scribble")
@require[(lib "manual.ss" "scribble")]
@require[(lib "eval.ss" "scribble")]
@require["guide-utils.ss"]

@title[#:tag "guide:module-paths"]{Module Paths}

A @deftech{module path} is a reference to a module, as used with
@scheme[require] or as the @scheme[_initial-module-path] in a
@scheme[module] form. It can be any of several forms:

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@specsubform[id]{

A @tech{module path} that is just an identifier refers to a
@scheme[module] declaration using the identifier. This form of module
reference makes the most sense in a REPL, where a module can be
declared independent of any file.

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
to the current directory. (More precisely, it is relative to the value
of @scheme[(current-load-relative-directory)], which is set while
loading a file.)

@secref["guide:module-basics"] shows examples using relative paths.
}

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@specsubform[#:literals (lib)
             (lib rel-string)]{

Like a plain-string @scheme[rel-path], but the elements of
@scheme[rel-path] refer to @tech{collection}s and
sub-@tech{collections}, instead of directories and sub-directories. A
collection is represented by a directory in one of several
installation-specific locations.

The common initial import @scheme[(lib "big/lang.ss")] uses this form;
it refers to the module whose source is the @file{lang.ss} file in the
@file{big} collection, which is installed as part of PLT Scheme.

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
@|PLaneT| server. The specified package is downloaded and installed on
demand. A @|PLaneT| reference starts like a @scheme[lib] reference,
with a relative path, but the path is followed by information about
the producer, package, and version of the library.

The version is expressed as a constraint on the acceptable version,
where a version number is a sequence of non-negative integers, and the
constraints determine the allowable values for each element in the
sequence. If no constraint is provided for a particular component,
then any version is allowed; in particular, omitting all
@scheme[vers]es means that any version is acceptable. Specifying at
least one @scheme[vers] is strongly recommended.

For a version constraint, a plain @scheme[nat] is the same as
@scheme[(+ nat)], which matches @scheme[nat] or higher for the
corresponding component of the version number.  A @scheme[(_start-nat
_end-nat)] matches any number in the range @scheme[_start-nat] to
@scheme[_end-nat] inclusive. A @scheme[(= nat)] matches only exactly
@scheme[nat]. A @scheme[(- nat)] matches @scheme[nat] or lower.

@examples[
(module m (lib "big/lang.ss")
  (require (planet "random.ss" ("schematics" "random.plt" 1 0)))
  (display (random-gaussian)))
]
}
