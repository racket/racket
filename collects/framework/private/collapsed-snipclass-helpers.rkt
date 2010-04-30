#lang scheme/base
(require scheme/gui/base
         scheme/class)

(provide make-sexp-snipclass%)

(define (make-sexp-snipclass% sexp-snip%)
  (class snip-class%
    (define/override (read in)
      (let* ([left-bracket (integer->char (bytes-ref (send in get-bytes) 0))]
             [right-bracket (integer->char (bytes-ref (send in get-bytes) 0))]
             [snip-count (send in get-exact)]
             [saved-snips
              (let loop ([n snip-count])
                (cond
                  [(zero? n) null]
                  [else
                   (let* ([classname (bytes->string/utf-8 (send in get-bytes))]
                          [snipclass (send (get-the-snip-class-list) find classname)])
                     (cons (send snipclass read in)
                           (loop (- n 1))))]))])
        (instantiate sexp-snip% ()
          (left-bracket left-bracket)
          (right-bracket right-bracket)
          (saved-snips saved-snips))))
    (super-new)))
