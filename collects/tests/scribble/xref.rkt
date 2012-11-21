#lang racket/base
(require scribble/xref
         racket/fasl
         setup/dirs
         tests/eli-tester)

;; FIXME: need to look for out<i>.sxref files

(provide xref-tests)
(module+ main (xref-tests))
(define (xref-tests)
  (define sxref (build-path (find-doc-dir) "reference" "out.sxref"))
  (when (file-exists? sxref)
    (define xref
      (load-xref (list (λ () (cadr (call-with-input-file* sxref fasl->s-exp))))))
    (test (xref-binding->definition-tag
           xref (list '(lib "contract.rkt" "racket") '->) #f)
          => '(form ((lib "racket/contract/base.rkt") ->)))))
