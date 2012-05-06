#lang racket/base
(require scribble/xref
         racket/fasl
         setup/dirs
         tests/eli-tester)

(provide xref-tests)
(module+ main (xref-tests))
(define (xref-tests)
  (define xref
    (load-xref (list
                (lambda ()
                  (cadr
                   (call-with-input-file*
                       (build-path (find-doc-dir) "reference" "out.sxref")
                     fasl->s-exp))))))
  (test (xref-binding->definition-tag
         xref (list '(lib "contract.rkt" "racket") '->) #f)
        => '(form ((lib "racket/contract/base.rkt") ->))))
