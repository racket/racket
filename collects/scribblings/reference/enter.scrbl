#lang scribble/doc
@(require "mz.rkt" (for-label racket/enter))

@title[#:tag "enter"]{Interactive Module Loading}

@note-init-lib[racket/enter]

@defform*/subs[[(enter! module-path)
                (enter! #f)
                (enter! module-path flag ...+)]
               ([flag #:quiet
                      #:verbose-reload
                      #:verbose
                      #:dont-re-require-enter])]{

Intended for use in a @tech{REPL}, such as when @exec{racket} is
started in interactive mode. When a @racket[module-path] is provided
(in the same sense as for @racket[require]), the corresponding module
is loaded or invoked, and the current @tech{namespace} is changed to
the body of the module via @racket[module->namespace]. When
@racket[#f] is provided, then the current @tech{namespace} is restored
to the original one.

If invoking @racket[module-path] requires loading any files, then
modification dates of the files are recorded. If the file is modified,
then a later @racket[enter!] re-loads the module from source; see also
@secref["module-redeclare"]. Similarly if a later @racket[enter!]
transitively @racket[require]s a modified module, then the required
module is re-loaded. Re-loading support works only for modules that
are first loaded (either directly or indirectly through transitive
@racket[require]s) via @racket[enter!].

Additional @racket[flag]s can customize aspects of @racket[enter!]:
@itemize[

 @item{When @racket[enter!] loads or re-loads a module from a file, it
  can print a message to @racket[(current-error-port)].  Use the
  @racket[#:verbose] flag to print a message about such loads and
  re-loads, @racket[#:verbose-reload] to print a message only for
  re-loaded modules, and @racket[#:quiet] for no printouts.  The default
  reporting corresponds to @racket[#:verbose-reload].}

 @item{After switching namespaces to the designated module,
  @racket[enter!] automatically requires @racket[racket/enter] into the
  namespace, so that @racket[enter!] can be used to switch namespaces
  again.  In some cases, requiring @racket[racket/enter] 
  might not be desirable (e.g., in a tool
  that uses @racket[racket/enter]); use the
  @racket[#:dont-re-require-enter] flag to disable the require.}]
}
