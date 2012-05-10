#lang racket/base
(require stepper/private/xml-snip-helpers
         mzlib/class
         mred)

(provide snip-class xml-snip%)

(define xml-snip%
  (class* editor-snip% (xml-snip<%> readable-snip<%>)
    (init-field eliminate-whitespace-in-empty-tags?)

    (define/public (read-special file line col pos)
      (xml-read-special eliminate-whitespace-in-empty-tags?
                        this
                        file
                        line
                        col
                        pos))

    (super-new)))

(define xml-snipclass%
  (class snip-class%
    (define/override (read stream-in)
      (let* ([eliminate-whitespace-in-empty-tags? (zero? (send stream-in get-exact))]
             [snip (instantiate xml-snip% ()
                                (eliminate-whitespace-in-empty-tags? eliminate-whitespace-in-empty-tags?))])
        (send (send snip get-editor) read-from-file stream-in #f)
        snip))
    (super-new)))

(define snip-class (make-object xml-snipclass%))
(send snip-class set-version 1)
(send snip-class set-classname (format "~s" '(lib "xml-snipclass.rkt" "xml")))
(send (get-the-snip-class-list) add snip-class)
