#lang scribble/manual

@(require (for-label setup/plt-installer
                     setup/plt-installer-sig
                     setup/plt-installer-unit))
                    
@title[#:tag "gui-unpacking"]{GUI @filepath{.plt} Installer}

@defmodule[setup/plt-installer]{ The
  @racketmodname[setup/plt-installer] library in the setup collection
  defines procedures for installing a @filepath{.plt} archive with a
  GUI (using the facilities of @racketmodname[racket/gui/base]).}

@defproc[(run-installer (filename path-string?)) void?]{
  Run the installer on the @filepath{.plt} file
  in @racket[filename] and show the output in a window. This is a
  composition of @racket[with-installer-window] and
  @racket[run-single-installer] with a @racket[get-dir-proc] that prompts
  the user for a directory (turning off the busy cursor while the dialog
  is active).}

@defparam[on-installer-run thunk (-> any)]{
  A thunk that is run after a @filepath{.plt} file is installed.}

@defproc[(with-installer-window
          [do-install (-> (or/c (is-a?/c dialog%) (is-a?/c frame%))
                          void?)]
          [cleanup-thunk (-> any)])
         void?]{

  Equivalent to
  @racketblock[(define installer-run (on-installer-run))
               (parameterize ([on-terminal-run 
                               (λ ()
                                 (printf "\nInstallation complete.\n")
                                 (installer-run))])
                 (in-terminal
                  (λ (custodian tlw) (do-install tlw))
                  #:title (string-constant plt-installer-progress-window-title)
                  #:cleanup-thunk cleanup-thunk))]
  }

@defproc[(run-single-installer (file path-string?)
                               (get-dir-proc (-> (or/c path-string? #f)))
                               [#:show-beginning-of-file? show-beginning-of-file? any/c #f])
         void?]{
  The same as the export from @racketmodname[setup/plt-single-installer], 
  but with a GUI.}

@; ----------------------------------------

@section{GUI Unpacking Signature}
      
@defmodule[setup/plt-installer-sig]{
  @defsignature[setup:plt-installer^ ()]{
  Provides two names: @racket[run-installer] and @racket[on-installer-run].}
}

@; ----------------------------------------

@section{GUI Unpacking Unit}

@defmodule[setup/plt-installer-unit]{

Imports @racket[mred^] and exports @racket[setup:plt-installer^]. }

