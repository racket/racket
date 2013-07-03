#lang racket/base
(require stepper/private/xml-snip-helpers
         mzlib/class
         mred)

(provide snip-class scheme-snip%)

(define scheme-snip%
  (class* editor-snip% (scheme-snip<%> readable-snip<%>)
    (init-field splice?)
    (define/public (get-splice?) splice?)

    (define/public (read-special file line col pos)
      (scheme-read-special this
                           file
                           line
                           col
                           pos))

    (super-instantiate ())))

(define scheme-snipclass%
  (class snip-class%
    (define/override (read stream-in)
      (let* ([splice? (zero? (send stream-in get-exact))]
             [snip (instantiate scheme-snip% ()
                                (splice? splice?))])
        (send (send snip get-editor) read-from-file stream-in #f)
        snip))
    (super-instantiate ())))

(define snip-class (make-object scheme-snipclass%))
(send snip-class set-version 1)
(send snip-class set-classname (format "~s" '(lib "scheme-snipclass.rkt" "xml")))
(send (get-the-snip-class-list) add snip-class)
