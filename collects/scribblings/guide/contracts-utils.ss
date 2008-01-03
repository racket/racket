#lang scheme/base
(require scribble/basic)

(provide question)

(define (question #:tag [tag #f] . rest)
  (keyword-apply section
                 '(#:tag)
                 (list (and tag (format "contracts-~a" tag)))
                 rest))

