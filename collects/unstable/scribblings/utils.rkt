#lang at-exp racket/base
(require scribble/base scribble/manual scribble/core scribble/eval)
(provide unstable
         unstable-header
         addition)

#|
Place either @unstable or @unstable-header immediately after the
@title form, before any @defmodule form or introductory/summary text.
|#

(define (unstable . authors)
  (make-compound-paragraph plain
    (list (apply author authors)
          (unstable-header))))

(define (unstable-header)
  @para{This library is @emph{unstable};
        compatibility will not be maintained.
        See @secref{unstable} for more information.})

(define (addition name)
  @margin-note{The subsequent bindings were added by @|name|.})
