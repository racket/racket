#lang at-exp scheme/base

(require scribble/base scribble/manual scribble/core)

(provide unstable addition)
(define (unstable . authors)
  (make-compound-paragraph 
   plain
   (list (apply author authors)
	 @para{This library is @emph{unstable}
	       ; compatibility will not be maintained.
	       See @secref{unstable} for more information.})))

(define (addition name)
  @margin-note{The subsequent bindings were added by @|name|.})


