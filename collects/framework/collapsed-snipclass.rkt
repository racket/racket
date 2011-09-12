#lang racket/base
(require racket/gui/base
         racket/class
         "private/collapsed-snipclass-helpers.rkt")

(provide snip-class)

(define simple-sexp-snip%
  (class* snip% (readable-snip<%>)
    (init-field left-bracket right-bracket saved-snips)
    (define/public (read-special file line col pos)
      (define text (make-object text%))
      (for ([s (in-list saved-snips)])
        (send text insert (send s copy)
              (send text last-position)
              (send text last-position)))
      (datum->syntax
       #f
       (read (open-input-text-editor text))
       (list file line col pos 1)))
    (define/override (copy)
      (new simple-sexp-snip% 
           [left-bracket left-bracket]
           [right-bracket right-bracket]
           [saved-snips saved-snips]))
    (super-new)))

(define sexp-snipclass% (make-sexp-snipclass% simple-sexp-snip%))

(define snip-class (make-object sexp-snipclass%))
(send snip-class set-classname (format "~s" '((lib "collapsed-snipclass.ss" "framework")
                                              (lib "collapsed-snipclass-wxme.ss" "framework"))))
(send snip-class set-version 0)
(send (get-the-snip-class-list) add snip-class)
