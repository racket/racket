#lang racket/base

(require scribble/manual
         scribble/decode
         scribble/struct
         setup/collects)
(provide (all-from-out scribble/manual)
         selflink
         gametitle gametitle* play-margin-note
         game)

(define (selflink str) (link str (tt str)))

(define game onscreen)

(define (gametitle name subcol subtitle 
                   #:style [style #f])
  (make-splice
   (list
    (gametitle* name subcol subtitle #:style style)
    (play-margin-note name))))

(define (gametitle* name subcol subtitle 
                    #:style [style #f])
  (title #:tag subcol 
         #:style style
         (make-element
          "noborder"
          (list
           (image (path->collects-relative
                   (build-path (collection-path "games" subcol)
                               (format "~a.png" subcol))))))
         " " (onscreen name) " --- " subtitle))

(define (play-margin-note name)
  (margin-note "To play "
               (onscreen name)
               ", run the "
               (exec "PLT Games") " program."
               " (Under Unix, it's called " (exec "plt-games") ")."))

