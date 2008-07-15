#lang scheme/base

(require scribble/manual
         scribble/decode
         scribble/struct
         setup/main-collects)
(provide (all-from-out scribble/manual)
         selflink
         gametitle
         game)

(define (selflink str) (link str (tt str)))

(define game onscreen)

(define (gametitle name subcol subtitle)
  (make-splice
   (list
    (title #:tag subcol 
           (make-element
            "noborder"
            (list
             (image (path->main-collects-relative
                     (build-path (collection-path "games" subcol)
                                 (format "~a.png" subcol))))))
           " " (onscreen name) " --- " subtitle)
    (margin-note "To play "
                 (onscreen name)
                 ", run the "
                 (exec "PLT Games") " program."
                 " (Under Unix, it's called " (exec "plt-games") ")."))))
