#lang racket/base
(require scribble/xref
         racket/fasl
         setup/dirs)

(define xref (load-xref (list 
                         (lambda ()
                           (cadr
                            (call-with-input-file*
                             (build-path (find-doc-dir)
                                         "reference"
                                         "out.sxref")
                             fasl->s-exp))))))

(unless (equal? '(form ((lib "racket/contract/base.rkt") ->))
                (xref-binding->definition-tag 
                 xref 
                 (list '(lib "contract.rkt" "racket") '->)
                 #f))
  (error "failed"))

