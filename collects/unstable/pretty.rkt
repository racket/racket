#lang racket/base
(require racket/pretty)

(provide pretty-format/write
         pretty-format/display
         pretty-format/print)

(define ((pretty-format/pretty f) v [columns (pretty-print-columns)])
  (parameterize ([current-output-port (open-output-string)]
                 [pretty-print-columns columns])
    (f v)
    (get-output-string (current-output-port))))

(define pretty-format/write (pretty-format/pretty pretty-write))
(define pretty-format/display (pretty-format/pretty pretty-display))
(define pretty-format/print (pretty-format/pretty pretty-print))

;; Ryan: There doesn't seem to be any 'format' going on. Perhaps call
;; them pretty-write-to-string, etc?
;; Bleh, just saw pretty-format in racket/pretty :(
