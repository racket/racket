#lang scribble/doc
@(require "mz.rkt" (for-label racket/rerequire))

@title[#:tag "rerequire"]{Loading and Reloading Modules}

@note-lib-only[racket/rerequire]

@defproc[(dynamic-rerequire [module-path module-path?]
                            [#:verbosity verbosity (or/c 'all 'reload 'none) 'reload])
	 (listof path?)]{

Like @racket[(dynamic-require module-path 0)], but with reloading
support. The @racket[dynamic-rerequire] function is intended for use
in an interactive environment, especially via @racket[enter!].

If invoking @racket[module-path] requires loading any files, then
modification dates of the files are recorded. If the file is modified,
then a later @racket[dynamic-rerequire] re-loads the module from source; see also
@secref["module-redeclare"]. Similarly if a later @racket[dynamic-rerequire]
transitively @racket[require]s a modified module, then the required
module is re-loaded. Re-loading support works only for modules that
are first loaded (either directly or indirectly through transitive
@racket[require]s) via @racket[dynamic-rerequire].

The returned list contains the absolute paths to the modules that were
reloaded on this call to @racket[dynamic-rerequire]. If the returned
list is empty, no modules were changed or loaded.

When @racket[enter!] loads or re-loads a module from a file, it can
print a message to @racket[(current-error-port)], depending on
@racket[verbosity]: @racket['all] prints a message for all loads and
re-loads, @racket['reload] prints a message only for
re-loaded modules, and @racket['none] disables printouts.}
