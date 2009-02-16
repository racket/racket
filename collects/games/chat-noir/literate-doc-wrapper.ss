#lang scheme/base

;; Use this module to create literate doc wrappers -- files that require the
;; literate code in a way that makes it a scribble file.

(provide include chunk (all-from-out scribble/manual))

(require scribble/manual scribble/decode scheme/include
         (for-syntax scheme/base syntax/boundmap))

(begin-for-syntax
  ;; maps chunk identifiers to a counter, so we can distinguish multiple uses
  ;; of the same name
  (define chunk-number (make-free-identifier-mapping)))

;; This is the doc-view implementation of `chunk', see "literate-lang.ss" for
;; the cide-view implementation.  Defines `chunk' as a macro that typesets the
;; contained code.
(define-syntax (chunk stx)
  (syntax-case stx ()
    [(_ #:part #f name expr ...)
     #'(make-splice (list (bold (scheme name) " ::=")
                          (schemeblock expr ...)))]
    [(_ #:part part-function name expr ...)
     (let ([n (add1 (free-identifier-mapping-get
                     chunk-number #'name (lambda () 0)))])
       (free-identifier-mapping-put! chunk-number #'name n)
       (with-syntax ([tag (format "~a~a" (syntax->datum #'name)
                                  (if (n . > . 1) (format ":~a" n) ""))]
                     [(more ...) (if (n . > . 1)
                                   #`((subscript #,(format "~a" n)))
                                   #`())])
         #'(make-splice (list (part-function #:tag tag (scheme name) more ...)
                              (schemeblock expr ...)))))]
    [(_ name expr ...) #'(chunk #:part subsection name expr ...)]))

;; HACK: provide a fake `module', which makes it possible to include a module
;; and get only its code in.
(provide module)
(define-syntax-rule (module name base body ...)
  (begin body ...))
