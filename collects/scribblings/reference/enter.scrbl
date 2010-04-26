#lang scribble/doc
@(require "mz.ss"
          (for-label racket/enter))

@title[#:tag "enter"]{Interactive Module Loading}

@note-init-lib[racket/enter]

@defform*[[(enter! module-path)
           (enter! #f)]]{

Intended for use in a @tech{REPL}, such as when @exec{mzscheme} is
started in interactive mode. When a @scheme[module-path] is provided
(in the same sense as for @scheme[require]), the corresponding module
is loaded or invoked, and the current @tech{namespace} is changed to
the body of the module via @scheme[module->namespace]. When
@scheme[#f] is provided, then the current @tech{namespace} is restored
to the original one.

If invoking @scheme[module-path] requires loading any files, then
modification dates of the files are recorded. If the file is modified,
then a later @scheme[enter!] re-loads the module from source; see also
@secref["module-redeclare"]. Similarly if a later @scheme[enter!]
transitively @scheme[require]s a modified module, then the required
module is re-loaded. Re-loading support works only for modules that
are first loaded (either directly or indirectly through transitive
@scheme[require]s) via @scheme[enter!].

After switching namespaces to the designated module, @scheme[enter!]
automatically requires @scheme[racket/enter] into the namespace, so
that @scheme[enter!] can be used to switch namespaces again.

When it loads or re-loads a module from a file, @scheme[enter!] prints
a message to @scheme[(current-error-port)].}
