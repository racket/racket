#lang scheme/base

;; Use this module to create literate doc wrappers -- files that require the
;; literate code in a way that makes it a scribble file.

(provide include chunk
         (all-from-out scribble/manual))

(require scribble/manual scribble/decode scheme/include)

;; define `chunk' as a macro that typesets the code
(define-syntax-rule (chunk name expr ...)
  (make-splice (list (subsection #:tag (format "~a" 'name)
                                 (scheme name))
                     (schemeblock expr ...))))

;; HACK: provide a fake `module', which makes it possible to include a module
;; and get only its code in.
(provide module)
(define-syntax-rule (module name base body ...)
  (begin body ...))
