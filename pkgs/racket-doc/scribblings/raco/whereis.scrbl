#lang scribble/doc
@(require scribble/manual
          racket/list
          scribble/bnf
          "common.rkt"
          (for-label racket/base racket/contract))

@title[#:tag "whereis"]{@exec{raco whereis}: Finding Racket Local Paths}

@(define-syntax-rule (ttlist e ...)
   (apply elem (add-between (list (tt (format "~a" 'e)) ...) ", ")))

The @exec{raco whereis} command prints the local filesystem path
corresponding to Racket modules, collections, packages, etc.

Command-line flags:

@itemlist[

@item{@Flag{m} @nonterm{module-path} or @DFlag{module}
@nonterm{module-path} --- Prints the path containing the declaration
of the given module. The @nonterm{module-path} argument must contain
exactly one @racket[read]able S-expression, otherwise an error is
reported.

Examples:
@itemlist[
@item{@exec{raco whereis -m racket/list}}
@item{@exec{raco whereis -m racket/draw}}
@item{@exec{raco whereis -m '(submod racket reader)'} ---
same as @exec{raco whereis -m racket}}
]

An error is reported if @nonterm{module-path} cannot be resolved or if
it ``resolves'' to a nonexistant file.}

@item{@Flag{c} @nonterm{collection} or @DFlag{collect}
@nonterm{collection} --- Prints the directory paths corresponding to
the given collection. Note that a collection may have multiple
directories, depending on collection roots and installed packages.

Examples:
@itemlist[
@item{@exec{raco whereis -c json}}
@item{@exec{raco whereis -c racket/gui}}
@item{@exec{raco whereis -c drracket}}
]

An error is reported if @nonterm{collection} is invalid (see
@racket[collection-name?]) or if no directories are found.}

@item{@Flag{p} @nonterm{package} or @DFlag{pkg} @nonterm{package} ---
Prints the path of the directory containing @nonterm{package}, which
must be installed in some scope.

Examples:
@itemlist[
@item{@exec{raco whereis -p base}}
@item{@exec{raco whereis -p pict-lib}}
]

If @nonterm{package} is not installed, an error is reported.}

@item{@Flag{r} @nonterm{command} or @DFlag{raco} @nonterm{command} ---
Prints the path of the module implementing the given @exec{raco} command.

Examples:
@itemlist[
@item{@exec{raco whereis -r exe}}
@item{@exec{raco whereis -r whereis}}
]

An error is reported if the given @exec{raco} command is not registered.}

@item{@Flag{s} @nonterm{name} or @DFlag{system} @nonterm{name} ---
Prints the path or paths corresponding to the given system location.

The following location names are supported:
@itemlist[
@item{@ttlist[home-dir pref-dir pref-file temp-dir init-dir init-file
              addon-dir doc-dir desk-dir sys-dir]
--- Same as @racket[(find-system-path _name)].

Example: @exec{raco whereis -s temp-dir}}

@item{@ttlist[exec-file config-dir host-config-dir collects-dir host-collects-dir]
      --- Like @racket[(find-system-path _name)], but relative paths are
converted into absolute paths by interpreting them with respect to the
path of the @exec{racket} executable.

Example: @exec{raco whereis -s config-dir}.}

@item{the name of a procedure from @racketmodname[setup/dirs] that
returns a path or list of paths.

Example: @exec{raco whereis -s find-config-dir}.}

]

If @nonterm{name} is unknown or if the corresponding procedure returns
@racket[#f], an error is reported.}

@item{@Flag{b} @nonterm{providing-module} @nonterm{name} or
      @DFlag{binding} @nonterm{providing-module} @nonterm{name}
--- Prints the path of the module that defines the binding exported by
@nonterm{providing-module} as @nonterm{name}. Note that the defined name
might be different due to renamings.

Examples:
@itemlist[
@item{@exec{raco whereis -b racket define}}
@item{@exec{raco whereis -b racket in-list}}
]

Note that @exec{whereis} does not see through @racket[contract-out].
That is, @racket[contract-out] defines and exports an auxiliary macro
to perform contract checking, and @exec{whereis} reports the
definition site of the macro (the site where @racket[contract-out] is
used) instead of the definition site of the binding being protected.

If @nonterm{name} is not provided by @nonterm{providing-module}, or if
the binding was defined by a built-in module (such as
@racketmodname['#%kernel]), an error is reported.}

]

@history[#:added "7.2.0.9"]
