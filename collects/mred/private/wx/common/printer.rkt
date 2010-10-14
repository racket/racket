#lang racket/base
(require racket/class)

(provide make-run-printout)

(define ((make-run-printout printer-dc%)
         parent
         interactive? ; currently ignored
         fit-to-page? ; ignored
         begin-doc-proc
         has-page?-proc
         print-page-proc
         end-doc-proc)
  (let ([dc (make-object printer-dc% parent)])
    (send dc start-doc "printing")
    (begin-doc-proc dc)
    (let loop ([i 1])
      (when (has-page?-proc dc i)
        (begin
          (send dc start-page)
          (print-page-proc dc i)
          (send dc end-page)
          (loop (add1 i)))))
    (end-doc-proc)
    (send dc end-doc)))
