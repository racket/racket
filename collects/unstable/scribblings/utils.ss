#lang at-exp scheme/base

(require scribble/base scribble/manual)

(provide unstable addition)
(define (unstable . authors)
  (begin
    (apply author authors)
    @para{This library is @emph{unstable}; compatibility will not be maintained.
	       See @secref{unstable} for more information.}))

(define (addition name)
  @margin-note{The subsequent bindings were added by @|name|.})


