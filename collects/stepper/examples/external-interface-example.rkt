#lang racket

(require stepper/external-interface
         stepper/private/marks
         racket/runtime-path)

;; this handler just prints out some information about 
;; the topmost mark in the list.
(define (handler mark-list kind value-list)
  (printf "handling a break\n")
  (printf "break kind: ~s\n" kind)
  (when mark-list
    (printf "~a" (display-mark (first mark-list)))
    (define source (mark-source (first mark-list)))
    (printf "top-mark line: ~s\n" (syntax-line source))
    (printf "top-mark column: ~s\n" (syntax-column source)))
  (when value-list
    (printf "values in value-list:\n")
    (for ([v value-list])
      (printf "~s\n" v)))
  (newline))

;; the string interface:
(step-program-string "globby"
                     "#lang racket 
(+ 3 4)"
                     handler)

;; the file interface:
(define-runtime-path bobby "./bobby.rkt")
(step-program-file bobby handler)
