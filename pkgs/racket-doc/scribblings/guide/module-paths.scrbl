#lang scribble/doc
@(require scribble/manual scribble/eval "guide-utils.rkt")

@title[#:tag "module-paths"]{Module Paths}

A @deftech{module path} is a reference to a module, as used with
@racket[require] or as the @racket[_initial-module-path] in a
@racket[module] form. It can be any of several forms:

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@specsubform[#:literals (quote) (#,(racket quote) id)]{

A @tech{module path} that is a quoted identifier refers to a non-file
@racket[module] declaration using the identifier. This form of module
reference makes the most sense in a @tech{REPL}.

@examples[
(module m racket
  (provide color)
  (define color "blue"))
(module n racket
  (require 'm)
  (printf "my favorite color is ~a\n" color))
(require 'n)
]}

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@specsubform[rel-string]{

A string @tech{module path} is a relative path using Unix-style
conventions: @litchar{/} is the path separator, @litchar{..} refers to
the parent directory, and @litchar{.} refers to the same
directory. The @racket[rel-string] must not start or end with a path
separator. If the path has no suffix, @filepath{.rkt} is added
automatically.

The path is relative to the enclosing file, if any, or it is relative
to the current directory. (More precisely, the path is relative to the
value of @racket[(current-load-relative-directory)], which is set
while loading a file.)

@secref["module-basics"] shows examples using relative paths.

If a relative path ends with a @filepath{.ss} suffix, it is converted
to @filepath{.rkt}. If the file that implements the referenced module
actually ends in @filepath{.ss}, the suffix will be changed back when
attempting to load the file (but a @filepath{.rkt} suffix takes
precedence). This two-way conversion provides compatibility with older
versions of Racket.}

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@specsubform[id]{

A @tech{module path} that is an unquoted identifier refers to an
installed library. The @racket[id] is constrained to contain only
ASCII letters, ASCII numbers, @litchar{+}, @litchar{-}, @litchar{_},
and @litchar{/}, where @litchar{/} separates path elements within the
identifier. The elements refer to @tech{collection}s and
sub-@tech{collections}, instead of directories and sub-directories.

An example of this form is @racket[racket/date]. It refers to the
module whose source is the @filepath{date.rkt} file in the
@filepath{racket} collection, which is installed as part of
Racket. The @filepath{.rkt} suffix is added automatically.

Another example of this form is @racketmodname[racket], which is commonly
used at the initial import. The path @racketmodname[racket] is shorthand for
@racket[racket/main]; when an @racket[id] has no @litchar{/}, then
@racket[/main] is automatically added to the end. Thus,
@racketmodname[racket] or @racket[racket/main] refers to the module whose
source is the @filepath{main.rkt} file in the @filepath{racket}
collection.

@examples[
(module m racket
  (require racket/date)

  (printf "Today is ~s\n"
          (date->string (seconds->date (current-seconds)))))
(require 'm)
]

When the full path of a module ends with @filepath{.rkt}, if no such
file exists but one does exist with the @filepath{.ss} suffix, then
the @filepath{.ss} suffix is substituted automatically. This
transformation provides compatibility with older versions of Racket.}

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@specsubform[#:literals (lib)
             (lib rel-string)]{

Like an unquoted-identifier path, but expressed as a string instead of
an identifier. Also, the @racket[rel-string] can end with a file
suffix, in which case @filepath{.rkt} is not automatically added.

Example of this form include @racket[(lib "racket/date.rkt")] and
@racket[(lib "racket/date")], which are equivalent to
@racket[racket/date]. Other examples include @racket[(lib "racket")],
@racket[(lib "racket/main")], and @racket[(lib "racket/main.rkt")],
which are all equivalent to @racketmodname[racket].

@examples[
(module m (lib "racket")
  (require (lib "racket/date.rkt"))

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

The @racket[id] encodes several pieces of information separated by a
@litchar{/}: the package owner, then package name with optional
version information, and an optional path to a specific library with
the package. Like @racket[id] as shorthand for a @racket[lib] path, a
@filepath{.rkt} suffix is added automatically, and @racketidfont{/main}
is used as the path if no sub-path element is supplied.

@examples[
(eval:alts
 (module m (lib "racket")
   (code:comment @#,t{Use @filepath{schematics}'s @filepath{random.plt} 1.0, file @filepath{random.rkt}:})
   (require (planet schematics/random:1/random))
   (display (random-gaussian)))
 (void))
(eval:alts
 (require 'm)
 (display 0.9050686838895684))
]

As with other forms, an implementation file ending with @filepath{.ss}
can be substituted automatically if no implementation file ending with
@filepath{.rkt} exists.}

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@specsubform[#:literals (planet)
             (planet package-string)]{

Like the symbol form of a @racket[planet], but using a string instead
of an identifier. Also, the @racket[package-string] can end with a
file suffix, in which case @filepath{.rkt} is not added.

As with other forms, an @filepath{.ss} extension is converted to
@filepath{.rkt}, while an implementation file ending with
@filepath{.ss} can be substituted automatically if no implementation
file ending with @filepath{.rkt} exists.}

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@specsubform/subs[#:literals (planet = + -)
                  (planet rel-string (user-string pkg-string vers ...))
                  ([vers nat
                         (nat nat)
                         (= nat)
                         (+ nat)
                         (- nat)])]{

A more general form to access a library from the @|PLaneT| server. In
this general form, a @|PLaneT| reference starts like a @racket[lib]
reference with a relative path, but the path is followed by
information about the producer, package, and version of the
library. The specified package is downloaded and installed on demand.

The @racket[vers]es specify a constraint on the acceptable version of
the package, where a version number is a sequence of non-negative
integers, and the constraints determine the allowable values for each
element in the sequence. If no constraint is provided for a particular
element, then any version is allowed; in particular, omitting all
@racket[vers]es means that any version is acceptable. Specifying at
least one @racket[vers] is strongly recommended.

For a version constraint, a plain @racket[nat] is the same as
@racket[(+ nat)], which matches @racket[nat] or higher for the
corresponding element of the version number.  A @racket[(_start-nat
_end-nat)] matches any number in the range @racket[_start-nat] to
@racket[_end-nat], inclusive. A @racket[(= nat)] matches only exactly
@racket[nat]. A @racket[(- nat)] matches @racket[nat] or lower.

@examples[
(eval:alts
 (module m (lib "racket")
   (require (planet "random.rkt" ("schematics" "random.plt" 1 0)))
   (display (random-gaussian)))
 (void))
(eval:alts
 (require 'm)
 (display 0.9050686838895684))
]

The automatic @filepath{.ss} and @filepath{.rkt} conversions apply as
with other forms.}

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@specsubform[#:literals (file)
             (file string)]{

Refers to a file, where @racket[string] is a relative or absolute path
using the current platform's conventions. This form is not portable,
and it should @italic{not} be used when a plain, portable
@racket[rel-string] suffices.

The automatic @filepath{.ss} and @filepath{.rkt} conversions apply as
with other forms.}

@; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
@specsubform/subs[#:literals (submod)
                  (@#,elemtag["submod"]{@racket[submod]} base element ...+)
                  ([base module-path
                         "."
                         ".."]
                   [element id
                            ".."])]{

Refers to a submodule of @racket[base]. The sequence of
@racket[element]s within @racket[submod] specify a path of submodule
names to reach the final submodule. 

@examples[
  (module zoo racket
    (module monkey-house racket
      (provide monkey)
      (define monkey "Curious George")))
  (require (submod 'zoo monkey-house))
  monkey
]

Using @racket["."] as @racket[base] within @racket[submod] stands for
the enclosing module. Using @racket[".."] as @racket[base] is
equivalent to using @racket["."] followed by an extra
@racket[".."]. When a path of the form @racket[(#,(racket quote) id)]
refers to a submodule, it is equivalent to @racket[(submod "."  id)].

Using @racket[".."] as an @racket[element] cancels one submodule step, effectively
referring to the enclosing module. For example, @racket[(submod "..")]
refers to the enclosing module of the submodule in which the path
appears.

@examples[
  (module zoo racket
    (module monkey-house racket
      (provide monkey)
      (define monkey "Curious George"))
    (module crocodile-house racket
      (require (submod ".." monkey-house))
      (provide dinner)
      (define dinner monkey)))
  (require (submod 'zoo crocodile-house))
  dinner
]}
