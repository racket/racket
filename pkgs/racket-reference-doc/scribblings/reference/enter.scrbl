#lang scribble/doc
@(require "mz.rkt"
          (for-label racket/enter
                     racket/rerequire))

@title[#:tag "enter"]{Entering Modules}

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
is loaded or invoked via @racket[dynamic-rerequire], and the current @tech{namespace} is changed to
the body of the module via @racket[module->namespace]. When
@racket[#f] is provided, then the current @tech{namespace} is restored
to the original one.

Additional @racket[flag]s can customize aspects of @racket[enter!]:
@itemize[

 @item{The @racket[#:verbose], @racket[#:verbose-reload], and
  @racket[#:quiet] flags correspond to @racket['all],
  @racket['reload], and @racket['none] verbosity for
  @racket[dynamic-rerequire]. The default corresponds to
  @racket[#:verbose-reload].}

 @item{After switching namespaces to the designated module,
  @racket[enter!] automatically requires @racket[racket/enter] into the
  namespace, so that @racket[enter!] can be used to switch namespaces
  again.  In some cases, requiring @racket[racket/enter] 
  might not be desirable (e.g., in a tool
  that uses @racket[racket/enter]); use the
  @racket[#:dont-re-require-enter] flag to disable the require.}]
}

@defproc[(dynamic-enter! [mod (or/c module-path? #f)]
                         [#:verbosity verbosity (or/c 'all 'reload 'none) 'reload]
                         [#:re-require-enter? re-require-enter? any/c #t])
         void?]{

Procedure variant of @racket[enter!], where @racket[verbosity] is
passed along to @racket[dynamic-rerequire] and
@racket[re-require-enter?]  determines whether @racket[dynamic-enter!]
requires @racket[racket/enter] in a newly entered namespace.

@history[#:added "6.0.0.1"]}

