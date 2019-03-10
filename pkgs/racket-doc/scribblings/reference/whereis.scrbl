#lang scribble/doc
@(require racket/list "mz.rkt" scribble/example
          (for-label racket/whereis setup/collection-paths pkg/lib))

@title[#:tag "whereis"]{Finding Racket Paths}

@(define-syntax-rule (symlist e ...)
   (apply elem (add-between (list (racket (quote e)) ...) ", ")))

@(begin
  ;; NOTE: "whereis.rkt" hand-edited to replace path prefixes with /home/me/dev
  (define my-eval (make-log-based-eval "whereis.rktd" 'replay))
  (my-eval '(require racket/whereis))
  (define-syntax-rule (my-examples e ...)
    (examples #:eval my-eval e ...)))

@defmodule[racket/whereis]
@history[#:added "7.2.0.9"]

The procedures provided by @racketmodname[racket/whereis] return local
filesystem paths corresponding to Racket modules, collections,
packages, etc.

See also @secref["whereis" #:doc '(lib "scribblings/raco/raco.scrbl")].

@defproc[(whereis-module [modpath (or/c module-path? module-path-index?)])
         path?]{

Returns the path containing the declaration of the given module.

@margin-note{The example results are for a development build of racket in
@tt{/home/me/dev}. Your results will differ.}
@my-examples[
(whereis-module 'racket/list)
(whereis-module 'racket/draw)
(code:line (whereis-module '(submod racket reader)) (code:comment "same as racket"))
]

If @racket[modpath] cannot be resolved or resolves to a nonexistant
file, an exception is raised.}


@defproc[(whereis-collection [collection collection-name?])
         (listof path?)]{

Returns a list of directory paths corresponding to the given
collection. Note that a collection may have multiple directories,
depending on collection roots and installed packages.

Equivalent to @racket[(collection-paths collection)].

@my-examples[
(whereis-collection "json")
(whereis-collection "racket/gui")
(whereis-collection "drracket")
]

If no directories are found for @racket[collection], an exception is
raised.}


@defproc[(whereis-pkg [pkg string?])
         path?]{

Returns the directory containing @racket[pkg], which must be installed
in some scope.

Like @racket[(pkg-directory pkg)], but simplifies the result path.

@my-examples[
(whereis-pkg "base")
(whereis-pkg "pict-lib")
]

If @racket[pkg] is not installed, an exception is raised.}


@defproc[(whereis-raco [command string?])
         path?]{

Returns the path of the module implementing the given @exec{raco}
command.

@my-examples[
(whereis-raco "exe")
(whereis-raco "whereis")
]

An error is reported if the given @exec{raco} command is not
registered.}


@defproc[(whereis-system [name symbol?])
         (or/c path? (listof path?))]{

Prints the path or paths corresponding to the given system location.

The following location names are supported:
@itemlist[

@item{@symlist[home-dir pref-dir pref-file temp-dir init-dir init-file
               addon-dir doc-dir desk-dir sys-dir]
--- Equivalent to @racket[(find-system-path _name)].

Example: @racket[(whereis-system 'temp-dir)].}

@item{@symlist[exec-file config-dir host-config-dir collects-dir host-collects-dir]
--- Like @racket[(find-system-path _name)], but relative paths are
converted into absolute paths by interpreting them with respect to the
path of the @exec{racket} executable.

Example: @racket[(whereis-system 'config-dir)].}

@item{the name of a procedure from @racketmodname[setup/dirs] that
returns a path or list of paths.

Example: @racket[(whereis-system 'get-config-dir)].}

]

If @racket[name] is unknown or if the implementation returns
@racket[#f], an exception is raised.}


@defproc[(whereis-binding [id identifier?]
                          [phase (or/c exact-integer? #f) (syntax-local-phase-level)])
         path?]{

Returns the path of the module that defines the binding referred to by
@racket[id] (at phase @racket[phase]). Note that the defined name
might be different due to renamings.

@my-examples[
(whereis-binding #'lambda)
(whereis-binding #'in-list)
]

Note that this procedure does not see through @racket[contract-out].
That is, @racket[contract-out] defines and exports an auxiliary macro
to perform contract checking, and this procedure reports the
definition site of the macro (the site where @racket[contract-out] is
used) instead of the definition site of the binding being protected.

If @racket[id] does not refer to a module export at phase
@racket[phase], or if the binding was defined by a built-in module
(such as @racketmodname['#%kernel]), an error is reported.}


@defproc[(whereis-binding/symbol [providing-mod module-path?]
                                 [name symbol?])
         path?]{

Like @racket[whereis-binding], but the binding is @racket[name]
exported by @racket[providing-mod]. Note that the defined name might
be different due to renamings.

@my-examples[
(whereis-binding/symbol 'racket 'define)
(whereis-binding/symbol 'racket 'in-list)
]

If @racket[providing-mod] does not have an export named @racket[name],
or if the binding was defined by a built-in module (such as
@racketmodname['#%kernel]), an error is reported.}
