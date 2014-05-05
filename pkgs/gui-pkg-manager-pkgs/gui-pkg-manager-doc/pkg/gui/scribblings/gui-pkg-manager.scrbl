#lang scribble/manual
@(require scribble/bnf
          scribble/core
          (for-label pkg racket/base))

@title{Package Management GUI Libraries}

@author[@author+email["Matthew Flatt" "mflatt@racket-lang.org"]
        @author+email["Jay McCarthy" "jay@racket-lang.org"]
        @author+email["Robert Bruce Findler" "robby@racket-lang.org"]]

@declare-exporting[pkg/gui]

The @tt{gui-pkg-manager} provides GUI support for package management.

@defproc[(make-pkg-gui
          [#:wrap-terminal-action wrap-terminal-action (-> (-> any) any) (λ (t) (t))]
          [#:initial-tab initial-tab (or/c 'by-source 'installed 'from-list 'migrate) 'by-source])
         (is-a?/c top-level-window<%>)]{
 Opens the package manager GUI starting with the @racket[initial-tab] selected. 
                                                   
 The @racket[wrap-terminal-action] function is passed a thunk that it is expected to 
 invoke, possibly after redirecting the @racket[current-output-port] and @racket[current-error-port]
 to point to an existing GUI window.
}

@defproc[(make-pkg-installer
          [#:parent parent (or/c #f (is-a?/c top-level-window<%>)) #f]
          [#:wrap-terminal-action wrap-terminal-action (-> (-> any) any) (λ (t) (t))]
          [#:package-to-offer package-to-offer (or/c #f string?) #f])
         (is-a?/c top-level-window<%>)]{
                                        
 Opens a specialized version of the @racket[make-pkg-gui] window that contains only the 
 @racket['by-source] panel.
  
 The @racket[wrap-terminal-action] function is passed a thunk that it is expected to 
 invoke, possibly after redirecting the @racket[current-output-port] and @racket[current-error-port]
 to point to an existing GUI window.
}

@defproc[(pkg-catalog-update-local/simple-status-dialog
          [#:parent parent (or/c (is-a?/c frame%) (is-a?/c dialog%) #f) #f])
         void?]{
  Calls @racket[pkg-catalog-update-local] with a simple @racket[dialog%]
  that shows which catalog servers are being consulted.
}
