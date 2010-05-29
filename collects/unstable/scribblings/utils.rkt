#lang at-exp racket/base
(require scribble/base scribble/manual scribble/core scribble/eval)
(provide unstable
         unstable-header
         addition
         eval/require)

(define (unstable . authors)
  (make-compound-paragraph 
   plain
   (list (apply author authors)
         (unstable-header))))

(define (unstable-header)
  @para{This library is @emph{unstable};
        compatibility will not be maintained.
        See @secref{unstable} for more information.})

(define (addition name)
  @margin-note{The subsequent bindings were added by @|name|.})

(define (eval/require . paths)
  (let* ([e (make-base-eval)])
    (for ([path (in-list paths)])
      (e `(require ,path)))
    e))
