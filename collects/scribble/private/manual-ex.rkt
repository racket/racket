#lang scheme/base
(require "../struct.rkt")

; XXX unknown contracts
(provide (struct-out exporting-libraries)
         current-signature)

(define-struct (exporting-libraries element) (libs source-libs))

(define current-signature (make-parameter #f))
