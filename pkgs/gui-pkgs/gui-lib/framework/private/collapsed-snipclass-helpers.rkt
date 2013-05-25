#lang racket/base
(require racket/gui/base
         racket/class)

(provide make-sexp-snipclass%)

(define (make-sexp-snipclass% sexp-snip%)
  (class snip-class%
    (define/override (read in)
      (define left-bracket (integer->char (bytes-ref (send in get-bytes) 0)))
      (define right-bracket (integer->char (bytes-ref (send in get-bytes) 0)))
      (define snip-count (send in get-exact))
      (define saved-snips
        (for/list ([in-range snip-count])
          (define classname (bytes->string/utf-8 (send in get-bytes)))
          (define snipclass (send (get-the-snip-class-list) find classname))
          (send snipclass read in)))
      (new sexp-snip%
           [left-bracket left-bracket]
           [right-bracket right-bracket]
           [saved-snips saved-snips]))
    (super-new)))
